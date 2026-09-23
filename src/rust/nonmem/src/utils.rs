use extendr_api::Result;
use extendr_api::prelude::*;
use extendr_api::serializer::to_robj;

use fs_err as fs;
use std::path::{Path, PathBuf};

// pharos config and nonmem crate
use config::{CONFIG_FILENAME, CommentType, Config, NonmemConfig, to_root_relative};
use nonmem::output_files::resolve_estimation_files;
use nonmem::{Model, ModelLayout, RunStartFile, validate_model_extension};

// hyperion core
use hyperion_core::{OptionExt, ResultExt, extendr_err, find_config_dir};

/// Finds the correct output file path with the specified extension.
///
/// Resolves the input to its model layout (see [`resolve_model_layout`]) and
/// names the output file from the model's stem inside its run directory, so
/// templated run directories resolve the same as conventional ones.
///
/// # Examples:
/// ```
/// # use hyperion_nonmem::utils::find_output_file;
/// // Directory input returns expected path
/// let result = find_output_file("models/run001", "ext");
/// // Should return "models/run001/run001.ext"
///
/// // .mod file input returns expected path
/// let result = find_output_file("models/run001.mod", "ext");
/// // Should return "models/run001/run001.ext"
/// ```
///
/// # Arguments:
/// * `input_path` - The input path (directory, .mod file, or output file)
/// * `extension` - The desired file extension (without dot, e.g. "ext", "grd")
///
/// # Returns:
/// * `Ok(PathBuf)` - The path to the output file
/// * `Err(Error)` - If the model cannot be located or the output file doesn't exist
pub fn find_output_file(input_path: impl AsRef<Path>, extension: &str) -> Result<PathBuf> {
    let path = input_path.as_ref();

    // The input is already the file being asked for.
    if path.extension().is_some_and(|e| e == extension) {
        return if path.exists() {
            Ok(path.to_path_buf())
        } else {
            Err(extendr_err!("File not found: {}", path.display()))
        };
    }

    let (layout, run_dir) = resolve_model_run(path)?;
    let output_path = layout.output_file(&run_dir, extension);

    if output_path.exists() {
        Ok(output_path)
    } else {
        Err(extendr_err!(
            "Output file not found: {}\nExpected location based on input: {}",
            output_path.display(),
            path.display()
        ))
    }
}

/// Name of the run-start file pharos writes into every run output directory.
const RUN_START_FILENAME: &str = "pharos_start.json";

/// Resolve a model input to its pharos [`ModelLayout`].
///
/// `search_path` comes from [`path_from_robj`] and may be a `.mod`/`.ctl` file,
/// a run directory, a `_metadata.json` file, or an output file inside a run
/// directory. Every shape is normalized to the *source* model first, because
/// [`ModelLayout::from_model_file`] is defined on source models: a layout built
/// from a model copied into a run directory cannot find its own outputs.
pub fn resolve_model_layout(search_path: &Path) -> Result<ModelLayout> {
    let source = source_model_path(search_path)?;
    ModelLayout::from_model_file(&source).map_to_extendr_err("Failed to resolve model file")
}

/// Resolve the source layout and the selected run together. Explicit run
/// directories and output files select that run, even if the source has several
/// recorded runs. Source models and metadata files use discovery/configuration.
pub fn resolve_model_run(search_path: &Path) -> Result<(ModelLayout, PathBuf)> {
    let layout = resolve_model_layout(search_path)?;
    let explicit_dir = if search_path.is_dir() {
        Some(search_path)
    } else if matches!(
        search_path.extension().and_then(|ext| ext.to_str()),
        Some("lst" | "ext" | "grd" | "shk" | "cor")
    ) || (validate_model_extension(search_path).is_ok()
        && fs::canonicalize(search_path).ok().as_deref() != Some(layout.model_path()))
    {
        search_path.parent()
    } else {
        None
    };
    let run_dir = match explicit_dir {
        Some(dir) => fs::canonicalize(dir).map_to_extendr_err("Failed to resolve run directory")?,
        None => resolve_run_dir(&layout)?,
    };
    Ok((layout, run_dir))
}

/// The source `.mod`/`.ctl` file behind any of the accepted input shapes.
fn source_model_path(search_path: &Path) -> Result<PathBuf> {
    // A run directory, or any file inside one. The run-start file records the
    // model path relative to the project root, which is the only link from a
    // run back to the model that produced it.
    let run_dir_candidate = if search_path.is_dir() {
        Some(search_path.to_path_buf())
    } else {
        search_path.parent().map(Path::to_path_buf)
    };
    if let Some(dir) = run_dir_candidate
        && let Ok(start) = RunStartFile::load(dir.join(RUN_START_FILENAME))
        && let Some(root) = find_config_dir().map_to_extendr_err("Failed to find config dir")?
    {
        let source = root.join(&start.model_path);
        if source.exists() {
            return Ok(source);
        }
    }

    // A model file given directly.
    if validate_model_extension(search_path).is_ok() && search_path.exists() {
        return Ok(search_path.to_path_buf());
    }

    // A `_metadata.json` beside its model, or a run directory pharos did not
    // create: probe for a model of the same name beside the input, then inside it.
    let name = search_path
        .file_stem()
        .ok_or_extendr_err("Could not determine file stem")?
        .to_string_lossy()
        .to_string();
    let reference = name.strip_suffix("_metadata").unwrap_or(&name);
    let parent = search_path
        .parent()
        .ok_or_extendr_err("Could not determine parent directory")?;

    let mut probe = ModelLayout::try_locate(reference, parent)
        .map_to_extendr_err("Failed to locate model file")?;
    if probe.is_none() && search_path.is_dir() {
        probe = ModelLayout::try_locate(reference, search_path)
            .map_to_extendr_err("Failed to locate model file")?;
    }

    probe
        .map(|layout| layout.model_path().to_path_buf())
        .ok_or_extendr_err("Could not find a .mod or .ctl file for the given input")
}

/// The model's run output directory: the one pharos recorded for it, otherwise
/// the configured `output_dir` convention.
pub fn resolve_run_dir(layout: &ModelLayout) -> Result<PathBuf> {
    if let Some(root) = find_config_dir().map_to_extendr_err("Failed to find config dir")? {
        let root = fs::canonicalize(&root).unwrap_or(root);
        // Only models inside the project have recorded runs to discover; asking
        // about one outside it is an error in pharos, not a missing run.
        if layout.model_path().starts_with(&root)
            && let Some(dir) = layout
                .discover_output_dir(&root)
                .map_to_extendr_err("Failed to discover run output directory")?
        {
            return Ok(dir);
        }
    }

    layout
        .resolve_output_dir(get_output_dir_template().as_deref())
        .map_to_extendr_err("Failed to resolve run output directory")
}

/// Reads `nonmem.output_dir` from the pharos config, if there is one.
fn get_output_dir_template() -> Option<String> {
    find_config_dir()
        .ok()
        .flatten()
        .map(|dir| dir.join(CONFIG_FILENAME))
        .and_then(|path| Config::load(path).ok())
        .and_then(|config| config.nonmem.and_then(|n| n.output_dir))
}

/// Parse the model at `path`.
pub fn parse_model_file(path: &Path) -> Result<Model> {
    let content = fs::read_to_string(path).map_to_extendr_err("Failed to read model file")?;
    Model::parse(path, &content)
        .map_err(|_| extendr_err!("Failed to parse model: {}", path.display()))
}

/// Resolve the final `.ext` file path for a model, honoring `$EST FILE=`.
///
/// Returns the `.ext` path that the *last* `$EST` writes to, after applying
/// NONMEM's inheritance rule (an `$EST` without `FILE=` continues writing to
/// the previous `$EST`'s file). Falls back to `{stem}.ext` in `run_dir` when
/// the model has no `$EST FILE=` overrides.
pub fn resolve_ext_path(model: &Model, run_dir: &Path, stem: &str) -> PathBuf {
    let default = run_dir.join(format!("{stem}.ext"));
    resolve_estimation_files(model, run_dir, &default)
        .pop()
        .unwrap_or(default)
}

/// Validate and resolve a model input path (.mod or .ctl).
/// Returns error if path is not a .mod/.ctl file or doesn't exist.
pub fn validate_model_path(input_path: impl AsRef<Path>) -> Result<PathBuf> {
    let path = input_path.as_ref();

    if path.is_dir() {
        return Err(extendr_err!(
            "Expected .mod or .ctl file path, got directory: {}",
            path.display()
        ));
    }

    let ext = match path.extension().and_then(|e| e.to_str()) {
        Some("mod") => "mod",
        Some("ctl") => "ctl",
        _ => {
            return Err(extendr_err!(
                "Expected .mod or .ctl file path: {}",
                path.display()
            ));
        }
    };

    if path.exists() {
        let stem = path
            .file_stem()
            .ok_or_extendr_err("Could not determine model file stem")?
            .to_string_lossy()
            .to_string();
        if let Some(parent) = path.parent()
            && parent
                .file_name()
                .and_then(|name| name.to_str())
                .is_some_and(|name| name == stem.as_str())
        {
            let candidate = parent.with_extension(ext);
            return Err(extendr_err!(
                "Expected input model file, got output model file: {}\n\
                 Try: {}",
                path.display(),
                candidate.display()
            ));
        }

        return Ok(path.to_path_buf());
    }

    Err(extendr_err!("File not found: {}", path.display()))
}

/// Convert a path to a project-relative identifier (forward-slash form).
///
/// Delegates the actual prefix-stripping to pharos's `to_root_relative` for
/// consistency with how the rest of pharos generates project-relative keys.
/// We can't call pharos's `to_config_relative` directly because it uses
/// pharos's own `find_config_dir` and so wouldn't honor the
/// `hyperion.config_dir` R option override. Canonicalizes both sides
/// here so relative inputs (resolved against CWD) line up with the
/// canonical config root before stripping.
///
/// Returns the original path if no config directory is found.
pub fn to_config_relative(path: impl AsRef<Path>) -> Result<String> {
    let path = path.as_ref();
    let config_dir = find_config_dir().map_to_extendr_err("Failed to find config dir")?;

    if let Some(dir) = config_dir {
        let canonical_path = fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
        let canonical_dir = fs::canonicalize(&dir).unwrap_or(dir);
        return to_root_relative(&canonical_path, &canonical_dir)
            .map_to_extendr_err("Failed to make path config-relative");
    }

    Ok(path.to_string_lossy().to_string())
}

/// Convert a config-relative path to an absolute path.
/// If the path is already absolute, returns it unchanged.
/// The config dir is canonicalized first so the result is absolute, which keeps
/// repeated calls on the same path idempotent.
pub fn from_config_relative(source: impl AsRef<Path>) -> Result<PathBuf> {
    let source_path = source.as_ref();
    if source_path.is_absolute() {
        return Ok(source_path.to_path_buf());
    }

    if let Some(dir) = find_config_dir().map_to_extendr_err("Failed to find config dir")? {
        let canonical_dir = fs::canonicalize(&dir).unwrap_or(dir);
        return Ok(canonical_dir.join(source_path));
    }

    Ok(source_path.to_path_buf())
}

/// Extract model_source attribute from Robj and resolve to absolute path.
fn extract_model_source(model: &Robj) -> Result<PathBuf> {
    let source = model
        .get_attrib("model_source")
        .ok_or_extendr_err("Model object is missing model_source attribute")?;
    let source_str = source
        .as_str()
        .ok_or_extendr_err("model_source attribute must be a string")?;
    from_config_relative(source_str)
}

/// Extract a path from an Robj (string path or hyperion_nonmem_model object).
///
/// If `validate_model` is true, validates the path is an existing .mod/.ctl file.
pub fn path_from_robj(input: &Robj, validate_model: bool) -> Result<PathBuf> {
    let path = if input.inherits("hyperion_nonmem_model") {
        extract_model_source(input)?
    } else if let Some(s) = input.as_str() {
        PathBuf::from(s)
    } else {
        return Err(extendr_err!(
            "Input must be a path or a hyperion_nonmem_model object"
        ));
    };

    if validate_model {
        validate_model_path(path)
    } else {
        Ok(path)
    }
}

/// Gives Some(Model) if model path is found
pub fn try_parse_model(path: &str) -> Option<Model> {
    let layout = resolve_model_layout(Path::new(path)).ok()?;
    parse_model_file(layout.model_path()).ok()
}

/// Gets the comment type from pharos.toml configuration
///
/// Map an arbitrary string to an R-syntactic column name by replacing any
/// non-alphanumeric character with `_`, collapsing runs of `_`, and trimming
/// leading/trailing `_`. E.g. `"SIGMA(1,1)"` -> `"SIGMA_1_1"`.
pub(crate) fn to_syntactic_name(s: &str) -> String {
    let mut out: String = s
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect();
    while out.contains("__") {
        out = out.replace("__", "_");
    }
    out.trim_matches('_').to_string()
}

/// @return Option<CommentType> from pharos config, None if not found or config doesn't exist
pub fn get_comment_type() -> Option<CommentType> {
    find_config_dir()
        .ok()
        .flatten()
        .map(|dir| dir.join(CONFIG_FILENAME))
        .and_then(|path| Config::load(path).ok())
        .and_then(|config| config.nonmem.as_ref().and_then(|n| n.comments.r#type))
}

pub fn load_nonmem_config(run_nonmem_version: Option<&str>) -> Result<(PathBuf, NonmemConfig)> {
    let p = if let Some(root_dir) =
        find_config_dir().map_to_extendr_err("Failed to find config dir")?
    {
        root_dir.join(CONFIG_FILENAME)
    } else {
        std::env::current_dir()
            .map_to_extendr_err("Failed to get current directory")?
            .join(CONFIG_FILENAME)
    };

    if !p.exists() {
        return Err(extendr_err!(
            "pharos config file not found in current or parent directories",
        ));
    }

    let config = Config::load(&p).map_to_extendr_err("Failed to load config")?;

    let nonmem_config = config
        .nonmem
        .ok_or_extendr_err("pharos config file does not contain nonmem configuration")?;

    if let Some(version) = run_nonmem_version
        && !nonmem_config.versions.contains_key(version)
    {
        return Err(extendr_err!(
            "nonmem version {version} not found in config file"
        ));
    }

    Ok((p, nonmem_config))
}

/// Gets the pharos.toml configuration as an R object
///
/// @return pharos config as nested list structure
/// @export
///
/// @examples \dontrun{
/// config <- get_pharos_config()
/// config$nonmem$summary$high_correlation_threshold
/// config$nonmem$summary$high_condition_threshold
/// }
#[extendr]
pub fn get_pharos_config() -> Result<Robj> {
    let config_path = find_config_dir()
        .map_to_extendr_err("Failed to find config dir")?
        .ok_or_extendr_err("Could not find pharos config directory")?
        .join(CONFIG_FILENAME);

    let config = Config::load(config_path).map_to_extendr_err("Failed to load config")?;

    // Extract the values we need and build R-compatible structure manually
    let correlation_threshold = config
        .nonmem
        .as_ref()
        .map(|n| n.summary.high_correlation_threshold)
        .unwrap_or(0.95);

    let condition_threshold = config
        .nonmem
        .as_ref()
        .map(|n| n.summary.high_condition_threshold as f64)
        .unwrap_or(1000.0);

    // Build nested list structure: config$nonmem$summary$...
    let summary_list = list!(
        high_correlation_threshold = correlation_threshold,
        high_condition_threshold = condition_threshold
    );

    let nonmem_list = list!(summary = summary_list);

    let result = list!(nonmem = nonmem_list);

    Ok(result.into_robj())
}

/// Get the comment type from pharos.toml config file
///
///
/// @return CommentType R object
/// @export
///
/// @examples \dontrun{
/// get_comment_type()
/// }
#[extendr(r_name = "get_comment_type")]
pub fn get_comment_type_wrap() -> Result<Robj> {
    let comment_type = get_comment_type();
    let robj = to_robj(&comment_type).map_to_extendr_err("Failed to serialize to Robj")?;

    Ok(robj)
}

/// Validate and resolve a model path (.mod or .ctl).
///
/// @keywords internal
/// @noRd
#[extendr(r_name = "validate_model_path")]
pub fn validate_model_path_wrap(path: &str) -> Result<Robj> {
    let path = validate_model_path(path)?;
    Ok(path.to_string_lossy().into_robj())
}

/// Convert a config-relative path to an absolute path.
///
/// Resolves a path stored relative to the project configuration directory
/// (the directory containing `pharos.toml`) to an absolute path. If the
/// path is already absolute, it is returned unchanged.
///
/// @details hyperion model objects store paths relative to the project
/// configuration directory. Use this function to resolve them to absolute
/// paths for file system operations.
///
/// @param path A config-relative or absolute file path.
/// @return The absolute file path as a character string.
/// @export
#[extendr(r_name = "from_config_relative")]
pub fn from_config_relative_wrap(path: &str) -> Result<Robj> {
    let path = from_config_relative(path)?;
    Ok(path.to_string_lossy().into_robj())
}

/// Convert an absolute path to be relative to the pharos config directory.
///
/// @param path Absolute path to make relative.
/// @return Path relative to pharos.toml directory, or original path if not under config dir.
/// @keywords internal
/// @noRd
#[extendr(r_name = "to_config_relative")]
pub fn to_config_relative_wrap(path: &str) -> Result<Robj> {
    let rel_path = to_config_relative(path)?;
    Ok(rel_path.into_robj())
}

extendr_module! {
    mod utils;

    fn get_pharos_config;
    fn get_comment_type_wrap;
    fn validate_model_path_wrap;
    fn from_config_relative_wrap;
    fn to_config_relative_wrap;
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::glob;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_resolve_model_layout_prefers_the_source_model() {
        // No run-start file, so resolution falls back to probing: the source
        // model beside the run directory wins over the copy inside it.
        let temp_dir = TempDir::new().unwrap();
        let source = temp_dir.path().join("run001.mod");
        fs::write(&source, "$PROBLEM source").unwrap();
        let run_dir = temp_dir.path().join("run001");
        fs::create_dir(&run_dir).unwrap();
        fs::write(run_dir.join("run001.mod"), "$PROBLEM copy").unwrap();

        for input in [&run_dir, &source] {
            let layout = resolve_model_layout(input).unwrap();
            assert_eq!(layout.stem(), "run001");
            assert_eq!(layout.model_path(), fs::canonicalize(&source).unwrap());
        }
    }

    #[test]
    fn test_find_output_file_already_correct() {
        let temp_dir = TempDir::new().unwrap();
        let ext_file = temp_dir.path().join("run001.ext");
        fs::write(&ext_file, "test content").unwrap();

        let result = find_output_file(&ext_file, "ext").unwrap();
        assert_eq!(result, ext_file);
    }

    #[test]
    fn test_find_output_file_not_found() {
        let temp_dir = TempDir::new().unwrap();
        let run_dir = temp_dir.path().join("run001");

        let result = find_output_file(&run_dir, "ext");
        assert!(result.is_err());
    }

    #[test]
    fn test_try_parse_model_success() {
        // Use real test data instead of creating temporary files
        let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data");
        glob!(test_dir, "**/*.mod", |path| {
            let result = try_parse_model(path.to_str().unwrap());
            assert!(
                result.is_some(),
                "Expected Some(Model) when valid mod file exists in test data"
            );
        })
    }

    #[test]
    fn test_try_parse_model_success_for_output_file() {
        let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data");
        glob!(test_dir, "**/*.grd", |path| {
            // Skip directories where file stem doesn't match directory name
            if path.to_string_lossy().contains("run001-running") {
                return;
            }
            let result = try_parse_model(path.to_str().unwrap());
            assert!(
                result.is_some(),
                "Expected Some(Model) when valid mod file exists in test data"
            );
        })
    }

    #[test]
    fn test_try_parse_model_no_mod_file() {
        let temp_dir = TempDir::new().unwrap();
        let run_dir = temp_dir.path().join("run001");
        fs::create_dir(&run_dir).unwrap();

        // Don't create a mod file - should return None
        let result = try_parse_model(run_dir.to_str().unwrap());
        assert!(
            result.is_none(),
            "Expected None when mod file doesn't exist"
        );
    }

    #[test]
    fn test_validate_model_path_ok() {
        let temp_dir = TempDir::new().unwrap();
        let mod_file = temp_dir.path().join("run001.mod");
        fs::write(&mod_file, "test content").unwrap();

        let result = validate_model_path(&mod_file).unwrap();
        assert_eq!(result, mod_file);
    }

    #[test]
    fn test_validate_model_path_rejects_output_model() {
        let temp_dir = TempDir::new().unwrap();
        let run_dir = temp_dir.path().join("run001");
        fs::create_dir(&run_dir).unwrap();
        let output_mod = run_dir.join("run001.mod");
        fs::write(&output_mod, "test content").unwrap();

        let err = validate_model_path(&output_mod).unwrap_err();
        let message = format!("{err}");
        assert!(message.contains("Expected input model file"));
        assert!(message.contains("Try:"));
    }

    #[test]
    fn test_validate_model_path_rejects_wrong_extension() {
        let temp_dir = TempDir::new().unwrap();
        let txt_file = temp_dir.path().join("run001.txt");
        fs::write(&txt_file, "test content").unwrap();

        let err = validate_model_path(&txt_file).unwrap_err();
        let message = format!("{err}");
        assert!(message.contains("Expected .mod or .ctl"));
    }

    #[test]
    fn test_from_config_relative_absolute() {
        let temp_dir = TempDir::new().unwrap();
        let mod_file = temp_dir.path().join("run001.mod");
        fs::write(&mod_file, "test content").unwrap();

        let result = from_config_relative(mod_file.to_string_lossy().as_ref()).unwrap();
        assert_eq!(result, mod_file);
    }
}
