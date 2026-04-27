use extendr_api::Result;
use extendr_api::prelude::*;
use extendr_api::serializer::to_robj;

use nonmem::ModelMetadata;
//pharos nonmem crate
use nonmem::{clear_metadata_file, update_metadata_file};

use crate::utils::path_from_robj;
use hyperion_core::{OptionExt, ResultExt, extendr_err};

const METADATA_FILENAME_SUFFIX: &str = "_metadata.json";

/// Creates a metadata file for a NONMEM model
///
/// This function creates a metadata file that stores information about a NONMEM model,
/// including its description, tags, and lineage information. The metadata is stored
/// in a structured format that can be used for model tracking and documentation.
///
/// @param model_path Path to the NONMEM model file, or a hyperion_nonmem_model object (required)
/// @param description Optional description of the model and its purpose
/// @param tags Character vector of tags to categorize or label the model
/// @param based_on Character vector of model names/paths that this model is based on
/// @param copied_from Optional model name/path this model was mechanically copied from
///
/// @return Returns invisibly after creating the metadata file
/// @export
///
/// @examples
/// \dontrun{
/// # Create basic metadata for a model
/// set_metadata_file("run001.mod", description = "Base population PK model")
///
/// # Create metadata using a model object
/// model <- read_model("run001.mod")
/// set_metadata_file(model, description = "Base population PK model")
///
/// # Create metadata with tags and lineage
/// set_metadata_file(
///   "run002.mod",
///   description = "PK model with covariate effects",
///   tags = c("population", "pk", "covariates"),
///   based_on = c("run001.mod")
/// )
/// }
#[extendr]
pub fn set_metadata_file(
    model_path: Robj,
    #[extendr(default = "NULL")] description: Option<String>,
    #[extendr(default = "NULL")] tags: Option<Vec<String>>,
    #[extendr(default = "NULL")] based_on: Option<Vec<String>>,
    #[extendr(default = "NULL")] copied_from: Option<String>,
) -> Result<()> {
    if let Some(d) = &description
        && d.trim().is_empty()
    {
        return Err(extendr_err!(
            "Description cannot be empty. Please provide a description for the model."
        ));
    };

    let model_path = path_from_robj(&model_path, true)?;

    let tags = tags.unwrap_or_default();
    let based_on = based_on.unwrap_or_default();

    update_metadata_file(model_path, description, tags, based_on, copied_from, true)
        .map_to_extendr_err("Failed to create metadata file")?;

    Ok(())
}

/// Updates a metadatafile
///
/// @param model_path path to model file or metadata file to update, or a hyperion_nonmem_model object
/// @param description Optional description to add to metadata
/// @param tags Optional character vector of tags to add to tags field
/// @param based_on character vector of models to add to based_on field
///
/// @return Invisibly after updaing
/// @export
///
/// @examples \dontrun{
/// update_metadata_file("model/nonmem/run001.mod", tags = "key model")
/// update_metadata_file("model/nonmem/run004.mod", tags = "key model", based_on = "1002")
/// model <- read_model("model/nonmem/run001.mod")
/// update_metadata_file(model, tags = "key model")
/// }
#[extendr(r_name = "update_metadata_file")]
pub fn append_to_metadata_file(
    model_path: Robj,
    #[extendr(default = "NULL")] description: Option<String>,
    #[extendr(default = "NULL")] tags: Option<Vec<String>>,
    #[extendr(default = "NULL")] based_on: Option<Vec<String>>,
) -> Result<()> {
    let path = path_from_robj(&model_path, true)?;

    let tags = tags.unwrap_or_default();
    let based_on = based_on.unwrap_or_default();

    update_metadata_file(path, description, tags, based_on, None, false)
        .map_to_extendr_err("Failed to update metadata file")?;

    Ok(())
}

/// Get model metadata from metadata JSON file
///
/// Loads metadata for a NONMEM model from the model's sibling
/// `<model_stem>_metadata.json` file.
///
/// @param model A hyperion_nonmem_model object from `read_model()`
///
/// @return hyperion_model_metadata object containing:
/// \itemize{
///   \item description - Model description string
///   \item tags - Character vector of metadata tags
///   \item based_on - Character vector of parent model references
/// }
/// @export
///
/// @examples \dontrun{
/// mod <- read_model("model/nonmem/run001.mod")
/// meta <- get_model_metadata(mod)
/// meta$description
/// meta$tags
/// meta$based_on
/// }
#[extendr(r_name = "get_model_metadata")]
pub fn load_model_metadata(model: Robj) -> Result<Robj> {
    let model_path =
        path_from_robj(&model, true).map_to_extendr_err("Failed to get path from model object")?;

    let metadata = ModelMetadata::load_from_model_path(model_path)
        .map_to_extendr_err("Failed to load ModelMetadata")?;

    let mut meta_robj =
        to_robj(&metadata).map_to_extendr_err("failed to create Robj from Model")?;
    let result = meta_robj
        .set_class(["hyperion_model_metadata"])
        .map_to_extendr_err("Failed to set class")?;

    Ok(result.to_owned())
}

/// Clear fields in a model's metadata file
///
/// Selectively clears the `based_on`, `copied_from`, and/or `tags` fields in
/// the metadata file associated with a model. Fields not selected are left
/// unchanged.
///
/// @param model_path Path to the NONMEM model file, or a hyperion_nonmem_model object
/// @param based_on If TRUE, clear the based_on field. Default FALSE.
/// @param copied_from If TRUE, clear the copied_from field. Default FALSE.
/// @param tags If TRUE, clear the tags field. Default FALSE.
///
/// @return Returns invisibly after updating the metadata file
/// @export
///
/// @examples \dontrun{
/// clear_metadata_file("model/nonmem/run001.mod", tags = TRUE)
/// model <- read_model("model/nonmem/run001.mod")
/// clear_metadata_file(model, based_on = TRUE, copied_from = TRUE)
/// }
#[extendr(r_name = "clear_metadata_file")]
pub fn clear_metadata_file_wrap(
    model_path: Robj,
    #[extendr(default = "FALSE")] based_on: bool,
    #[extendr(default = "FALSE")] copied_from: bool,
    #[extendr(default = "FALSE")] tags: bool,
) -> Result<()> {
    let model_path = path_from_robj(&model_path, true)?;

    let model_name = model_path
        .file_stem()
        .ok_or_extendr_err("Could not determine model file stem")?
        .to_string_lossy()
        .to_string();
    let model_dir = model_path
        .parent()
        .ok_or_extendr_err("Could not determine model directory")?
        .to_path_buf();
    let metadata_path = model_dir.join(format!("{model_name}{METADATA_FILENAME_SUFFIX}"));

    if !metadata_path.exists() {
        return Err(extendr_err!(
            "Metadata file does not exist: {}",
            metadata_path.display()
        ));
    }

    clear_metadata_file(
        model_name,
        &model_dir,
        &metadata_path,
        based_on,
        copied_from,
        tags,
    )
    .map_to_extendr_err("Failed to clear metadata file")?;

    Ok(())
}

extendr_module! {
    mod metadata;

    fn set_metadata_file;
    fn append_to_metadata_file;
    fn load_model_metadata;
    fn clear_metadata_file_wrap;
}
