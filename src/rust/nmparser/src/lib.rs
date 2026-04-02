use extendr_api::Result;
use extendr_api::deserializer::from_robj;
use extendr_api::prelude::*;
use extendr_api::serializer::to_robj;

use fs_err as fs;
use std::path::Component;
use std::path::Path;
use std::path::PathBuf;

use hyperion_core::find_config_dir;
use hyperion_core::{OptionExt, ResultExt, extendr_err};
use nmparser::Model;

/// Helper to convert Model to Robj for read_model and other model readers.
pub fn model_to_robj(model: &Model, path: impl AsRef<Path>) -> Result<Robj> {
    let path = path.as_ref();

    let mut model_robj = to_robj(model).map_to_extendr_err("failed to create Robj from Model")?;

    add_filename_attr(&mut model_robj, path)?;
    add_model_source_attr(&mut model_robj, path)?;

    set_model_class(&mut model_robj)
}

fn add_filename_attr(model_robj: &mut Robj, path: &Path) -> Result<()> {
    if let Some(n) = path.file_stem().and_then(|name| name.to_str()) {
        model_robj
            .set_attrib("filename", n.into_robj())
            .map_to_extendr_err("Failed to set filename attribute")?;
    }
    Ok(())
}

fn add_model_source_attr(model_robj: &mut Robj, path: &Path) -> Result<()> {
    let source_path = to_config_relative(path)?;
    model_robj
        .set_attrib("model_source", source_path.into_robj())
        .map_to_extendr_err("Failed to set model source attribute")?;

    Ok(())
}

fn set_model_class(model_robj: &mut Robj) -> Result<Robj> {
    let result = model_robj
        .set_class(["hyperion_nonmem_model"])
        .map_to_extendr_err("Failed to set class")?;

    Ok(result.to_owned())
}

/// Helper function to reconstruct a parser Model from hyperion_nonmem_model Robj
pub fn robj_to_model(model: &Robj) -> Result<Model> {
    from_robj(model).map_to_extendr_err("Failed to create Model from Robj")
}

fn validate_model_path(input_path: impl AsRef<Path>) -> Result<PathBuf> {
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

fn make_relative_path(base: &Path, target: &Path) -> PathBuf {
    let base_components: Vec<Component<'_>> = base.components().collect();
    let target_components: Vec<Component<'_>> = target.components().collect();

    if base_components.first() != target_components.first() {
        return target.to_path_buf();
    }

    let mut idx = 0;
    let max = base_components.len().min(target_components.len());
    while idx < max && base_components[idx] == target_components[idx] {
        idx += 1;
    }

    let mut rel = PathBuf::new();
    for _ in idx..base_components.len() {
        rel.push("..");
    }
    for comp in target_components.iter().skip(idx) {
        rel.push(comp.as_os_str());
    }

    rel
}

fn to_config_relative(path: impl AsRef<Path>) -> Result<String> {
    let path = path.as_ref();
    let config_dir = find_config_dir().map_to_extendr_err("Failed to find config dir")?;

    if let Some(dir) = config_dir {
        let rel = make_relative_path(&dir, path);
        return Ok(rel.to_string_lossy().to_string());
    }

    Ok(path.to_string_lossy().to_string())
}

/// Gets model object
///
/// @param path path to mod or ctl file.
///
/// @return hyperion_nonmem_model S3 object with `model_source` attribute
/// @export
///
/// @examples \dontrun{
/// read_model("model/nonmem/run001.mod")
/// }
#[extendr]
pub fn read_model(path: &str) -> Result<Robj> {
    let mod_path = validate_model_path(path)?;
    let content = fs::read_to_string(&mod_path).map_to_extendr_err("")?;

    let model = match Model::parse(&content) {
        Ok(model) => model,
        Err(diags) => {
            let msg = diags
                .iter()
                .map(|d| d.to_string())
                .collect::<Vec<_>>()
                .join("\n");
            return Err(extendr_err!("Failed to read model file:\n{msg}"));
        }
    };
    let robj_model = model_to_robj(&model, mod_path)?;
    Ok(robj_model)
}

// Generate extendr module for R integration
extendr_module! {
    mod hyperion_nmparser;
    fn read_model;
}
