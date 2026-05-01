use extendr_api::Result;
use extendr_api::prelude::*;
use extendr_api::serializer::to_robj;
use hyperion_core::{OptionExt, ResultExt};

// pharos config and nonmem crates
use nonmem::{check_dataset as nonmem_check_dataset, check_model};

use crate::model::robj_to_model;

use crate::utils::{load_nonmem_config, path_from_robj};
use hyperion_core::extendr_err;

/// Checks mod file for nmtran errors
///
/// @param model_path path to nonmem model file, or a hyperion_nonmem_model object
///
/// @return exit code of NMTRAN
/// @export
///
/// @examples \dontrun{
/// check_model("model/nonmem/1001.mod")
/// model <- read_model("model/nonmem/1001.mod")
/// check_model(model)
/// }
#[extendr(r_name = "check_model")]
pub fn check_model_wrap(model_path: Robj) -> Result<i32> {
    let model_path = path_from_robj(&model_path, true)?;

    let (_config_path, nonmem_config) =
        load_nonmem_config(None).map_to_extendr_err("Failed to create NonmemConfig")?;

    let res = match check_model(&nonmem_config, &model_path) {
        Ok(r) => r,
        Err(e) => {
            let error_msg = e.to_string();
            if error_msg.contains("NMTRAN.exe not found") {
                reprintln!("{}", error_msg.trim());
                return Ok(-1);
            } else {
                return Err(extendr_err!("Failed to run NMTRAN.exe: {e}"));
            }
        }
    };

    rprintln!("{}", res.stdout.trim());

    Ok(res.exit_code)
}

/// Checks model dataset
///
/// @param model hyperion_nonmem_model object from `read_model`
///
/// @return Dataset check results
/// @export
#[extendr]
pub fn check_dataset(model: Robj) -> Result<Robj> {
    let model_path = path_from_robj(&model, false)?;
    let model_dir = model_path
        .parent()
        .ok_or_extendr_err("Could not determine model directory")?;

    let model = robj_to_model(&model)?;
    let dataset = nonmem_check_dataset(&model, model_dir).map_to_extendr_err("")?;

    let mut robj = to_robj(&dataset).map_to_extendr_err("Failed to serialize to Robj")?;
    robj.set_class(["hyperion_nonmem_dataset"])
        .map_to_extendr_err("Failed to set class")?;

    Ok(robj)
}

extendr_module! {
    mod check;

    fn check_model_wrap;
    fn check_dataset;
}
