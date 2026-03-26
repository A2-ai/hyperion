use extendr_api::Result;
use extendr_api::deserializer::from_robj;
use extendr_api::prelude::*;
use extendr_api::serializer::to_robj;

use fs_err as fs;
use std::path::Path;

//pharos nonmem crate
use nmparser::Model;

use crate::utils::{to_config_relative, validate_model_path};
use hyperion_core::ResultExt;

/// Helper to convert Model to Robj for read_model and read_model_from_lst
///
/// This handles comment parsing and Model -> Robj + S3 setting
fn model_to_robj(model: &mut Model, path: impl AsRef<Path>) -> Result<Robj> {
    let path = path.as_ref();

    let mut model_robj = to_robj(&model).map_to_extendr_err("failed to create Robj from Model")?;

    add_filename_attr(&mut model_robj, path)?;
    add_model_source_attr(&mut model_robj, path)?;
    add_run_status_attr(&mut model_robj, path)?;

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

fn add_run_status_attr(model_robj: &mut Robj, path: &Path) -> Result<()> {
    if let Some(ext) = path.extension().and_then(|e| e.to_str())
        && (ext == "mod" || ext == "ctl" || ext == "lst")
    {
        let run_status = "run";
        model_robj
            .set_attrib("run_status", run_status.to_string().into_robj())
            .map_to_extendr_err("Failed to set run_status attribute")?;
    }

    Ok(())
}

fn set_model_class(model_robj: &mut Robj) -> Result<Robj> {
    let result = model_robj
        .set_class(["hyperion_nonmem_nmmodel"])
        .map_to_extendr_err("Failed to set class")?;

    Ok(result.to_owned())
}

/// Helper function to reconstruct a pharos Model from hyperion_nonmem_model Robj
pub fn robj_to_model(model: &Robj) -> Result<Model> {
    from_robj(model).map_to_extendr_err("Failed to create Model from Robj")
}

/// Gets model object
///
/// @param path path to mod or ctl file.
///
/// @return hyperion_nonmem_model S3 object with `model_source` and `run_status` attributes
/// @export
///
/// @examples \dontrun{
/// read_nmmodel("model/nonmem/run001.mod")
/// }
#[extendr]
pub fn read_nmmodel(path: &str) -> Result<Robj> {
    let mod_path = validate_model_path(path)?;
    let content = fs::read_to_string(&mod_path).map_to_extendr_err("")?;

    let (mut model, _diagonstics) =
        Model::parse(&content).map_to_extendr_err("Failed to read model file")?;
    let robj_model = model_to_robj(&mut model, mod_path)?;
    Ok(robj_model)
}

extendr_module! {
    mod nmmodel;
    fn read_nmmodel;
}
