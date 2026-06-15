use extendr_api::Result;
use extendr_api::deserializer::from_robj;
use extendr_api::prelude::*;
use extendr_api::serializer::to_robj;
use fs_err as fs;
use nmparser::Model;
use nonmem::output_files::lst;
use std::path::Path;

use crate::model::run_status::determine_run_status;
use crate::utils::{find_output_file, get_comment_type, to_config_relative, validate_model_path};
use hyperion_core::{OptionExt, ResultExt, extendr_err};

pub mod check;
pub mod comment_info;
pub mod comparison;
pub mod copy;
pub mod lineage;
pub mod metadata;
pub mod parameters;
pub mod run_status;
pub mod summary;

/// Convert a parsed Model into a hyperion_nonmem_model Robj.
pub fn model_to_robj(model: &mut Model, path: impl AsRef<Path>) -> Result<Robj> {
    let path = path.as_ref();

    if let Some(ct) = get_comment_type() {
        model.parse_comments(ct);
    }

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

fn add_run_status_attr(model_robj: &mut Robj, path: &Path) -> Result<()> {
    if let Some(ext) = path.extension().and_then(|e| e.to_str())
        && (ext == "mod" || ext == "ctl" || ext == "lst")
    {
        let stem = path
            .file_stem()
            .ok_or_extendr_err("Could not determine model file stem")?
            .to_string_lossy()
            .to_string();
        let parent = path
            .parent()
            .ok_or_extendr_err("Could not determine parent directory")?;
        let run_dir = match ext {
            "lst" => parent.to_path_buf(),
            _ => parent.join(&stem),
        };
        let run_status = determine_run_status(&run_dir, &stem)?;
        model_robj
            .set_attrib("run_status", run_status.to_string().into_robj())
            .map_to_extendr_err("Failed to set run_status attribute")?;
    }

    Ok(())
}
/// Read a model from an .lst file (internal)
///
/// @param path path to an .lst file, model output directory, or metadata.json file.
///
/// @return A `hyperion_nonmem_model` S3 object with attributes:
///   - `filename`: the model stem (e.g. `"run001"`)
///   - `model_source`: path to the source file, relative to the pharos config dir
///   - `run_status`: `"run"`, `"running"`, or `"not_run"` determined from output files on disk
/// @keywords internal
#[extendr]
pub fn read_model_from_lst(path: &str) -> Result<Robj> {
    let path = find_output_file(path, "lst")?;
    let mut model =
        lst::extract_model(&path).map_to_extendr_err("Failed to extract Model from lst file")?;
    let mut robj_model = model_to_robj(&mut model, &path)?;
    add_run_status_attr(&mut robj_model, &path)?;

    Ok(robj_model)
}

/// Read a NONMEM model from a .mod or .ctl file
///
/// @param path path to a .mod or .ctl file.
///
/// @return A `hyperion_nonmem_model` S3 object with attributes:
///   - `filename`: the model stem (e.g. `"run001"`)
///   - `model_source`: path to the source file, relative to the pharos config dir
///   - `run_status`: `"run"`, `"running"`, or `"not_run"` determined from output files on disk
/// @export
///
/// @examples \dontrun{
/// read_model("model/nonmem/run001.mod")
/// }
#[extendr]
pub fn read_model(path: &str) -> Result<Robj> {
    let mod_path = validate_model_path(path)?;
    let content = fs::read_to_string(&mod_path).map_to_extendr_err("")?;

    let mut model = match Model::parse(path, &content) {
        Ok(model) => model,
        Err(e) => return Err(extendr_err!("Failed to read model file:\n{e}")),
    };

    let mut robj_model = model_to_robj(&mut model, &mod_path)?;
    add_run_status_attr(&mut robj_model, &mod_path)?;

    Ok(robj_model)
}

/// Get the full text content of a model
///
/// Reconstructs the NONMEM model file text from a `hyperion_nonmem_model`
/// object by joining the parsed model tokens.
///
/// @param model A `hyperion_nonmem_model` object from `read_model()`
///
/// @return Character string with the full model file content
/// @export
///
/// @examples \dontrun{
/// mod <- read_model("model/nonmem/run001.mod")
/// get_model_content(mod)
/// }
#[extendr]
pub fn get_model_content(model: Robj) -> Result<String> {
    let model: Model = from_robj(&model)?;
    Ok(model.model_content())
}

extendr_module! {
    mod model;
    use copy;
    use summary;
    use check;
    use lineage;
    use parameters;
    use comment_info;
    use comparison;
    use metadata;
    use run_status;

    fn read_model;
    fn read_model_from_lst;
    fn get_model_content;
}
