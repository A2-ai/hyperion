use extendr_api::Result;
use extendr_api::prelude::*;
use nonmem::output_files::lst;
use std::path::Path;

use crate::model::run_status::determine_run_status;
use crate::utils::find_output_file;
use hyperion_core::ResultExt;
use hyperion_nmparser::model_to_robj;

pub mod check;
pub mod copy;
pub mod lineage;
pub mod metadata;
pub mod parameters;
pub mod run_status;
pub mod summary;

fn add_run_status_attr(model_robj: &mut Robj, path: &Path) -> Result<()> {
    if let Some(ext) = path.extension().and_then(|e| e.to_str())
        && (ext == "mod" || ext == "ctl" || ext == "lst")
    {
        let run_status = determine_run_status(path)?;
        model_robj
            .set_attrib("run_status", run_status.to_string().into_robj())
            .map_to_extendr_err("Failed to set run_status attribute")?;
    }

    Ok(())
}
/// Gets model object from lst file (internal)
///
/// @param path path to lst file, model output directory, or metadata.json file.
///
/// @return hyperion_nonmem_model S3 object with `model_source` attribute for the source file
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

extendr_module! {
    mod model;
    use copy;
    use summary;
    use check;
    use lineage;
    use parameters;
    use metadata;
    use run_status;

    fn read_model_from_lst;
}
