use extendr_api::Result;
use extendr_api::prelude::*;
use extendr_api::serializer::to_robj;

use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

use nonmem::{LineageTree, ModelMetadata, OutputFileHash, RunEndFile, RunStartFile};

use crate::utils::{path_from_robj, to_config_relative};
use hyperion_core::{OptionExt, ResultExt, extendr_err, find_config_dir};

/// R-compatible version of RunEndFile with u128 -> f64 conversion
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct RRunEndFile {
    pub start: String,
    pub end: String,
    pub runtime_ms: f64, // Changed from u128 to f64 for R compatibility
    pub files_copied: HashSet<String>,
    pub output_files_rewrites: HashMap<String, String>,
    pub output_files_hashes: Vec<OutputFileHash>,
}

/// R-compatible version of LineageTree
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct RLineageTree {
    pub nodes: HashMap<String, ModelMetadata>,
    pub metadata: HashMap<String, (RunStartFile, Option<RRunEndFile>)>,
    pub source_dir: String,
}

impl RLineageTree {
    /// Set the source directory for this lineage tree
    pub fn with_source_dir(mut self, source_dir: String) -> Self {
        self.source_dir = source_dir;
        self
    }
}

impl From<RunEndFile> for RRunEndFile {
    fn from(run_end: RunEndFile) -> Self {
        RRunEndFile {
            start: run_end.start,
            end: run_end.end,
            runtime_ms: run_end.runtime_ms as f64, // Convert u128 to f64
            files_copied: run_end.files_copied,
            output_files_rewrites: run_end.output_files_rewrites,
            output_files_hashes: run_end.output_files_hashes,
        }
    }
}

impl From<LineageTree> for RLineageTree {
    fn from(lineage: LineageTree) -> Self {
        let r_metadata = lineage
            .metadata
            .into_iter()
            .map(|(key, (start_file, opt_end_file))| {
                let r_end_file = opt_end_file.map(|end_file| end_file.into());
                (key, (start_file, r_end_file))
            })
            .collect();

        RLineageTree {
            nodes: lineage.nodes,
            metadata: r_metadata,
            source_dir: String::new(), // Set by caller via with_source_dir()
        }
    }
}

/// Get's model lineage
///
/// @param model_or_dir a hyperion_nonmem_model object (or a path to a `.mod`/`.ctl`
/// file), or a path to a directory. When a model is supplied the result is
/// filtered to that model and its ancestors only (chain leading up to the
/// model). When a directory is supplied no filter is applied.
/// @param scope `"project"` (default) walks the entire pharos project rooted at
/// `pharos.toml`; `"directory"` walks only the directory inferred from
/// `model_or_dir`. Node keys are always relative to the pharos config dir so
/// `based_on` references resolve consistently.
///
/// @return hyperion_nonmem_tree S3 object
/// @export
///
/// @examples \dontrun{
/// model <- read_model("model/nonmem/run001.mod")
/// get_model_lineage(model)                          # ancestors of run001
/// get_model_lineage(model, scope = "directory")     # ancestors, walking only model's dir
/// get_model_lineage("model/nonmem/")                # full project tree
/// get_model_lineage("model/nonmem/", scope = "directory")  # only models under that dir
/// }
#[extendr]
pub fn get_model_lineage(
    model_or_dir: Robj,
    #[extendr(default = "\"project\"")] scope: &str,
) -> Result<Robj> {
    let path = path_from_robj(&model_or_dir, false)?;
    let user_passed_model = path.is_file();
    // If it's a file, use parent directory; if directory, use as-is
    let model_dir = if user_passed_model {
        path.parent()
            .ok_or_extendr_err("Could not determine model directory")?
            .to_path_buf()
    } else {
        path.clone()
    };

    // Use the pharos config dir as project_root so node keys are
    // config-relative, matching how `based_on` is stored. Without this, a
    // model under e.g. `struct/` walked in isolation would key as `1001.mod`
    // while based_on says `struct/1001.mod` and parent links never resolve.
    // Falls back to model_dir for projects without a pharos.toml.
    let project_root = find_config_dir()?.unwrap_or_else(|| model_dir.clone());

    let start = match scope {
        "project" => project_root.clone(),
        "directory" => model_dir.clone(),
        other => {
            return Err(extendr_err!(
                "Invalid scope '{}': must be 'project' or 'directory'",
                other
            ));
        }
    };

    let lineage = LineageTree::build(&start, &project_root, true)
        .map_to_extendr_err("Pharos failed to create lineage tree")?;

    // When the user passes a model file, filter the tree to just that model
    // and its ancestors (the chain leading up to it). Mirrors `pharos nonmem
    // lineage --to <model>`.
    let lineage = if user_passed_model {
        let abs_path =
            std::fs::canonicalize(&path).map_to_extendr_err("Failed to canonicalize model path")?;
        let abs_root = std::fs::canonicalize(&project_root)
            .map_to_extendr_err("Failed to canonicalize project root")?;
        let model_key = abs_path
            .strip_prefix(&abs_root)
            .map_err(|_| {
                extendr_err!(
                    "Model '{}' is outside the project root '{}'",
                    abs_path.display(),
                    abs_root.display()
                )
            })?
            .to_string_lossy()
            .to_string();

        let chain = lineage.get_tree_up_to(&model_key);
        let chain_keys: HashSet<String> = chain.iter().map(|(k, _)| k.clone()).collect();
        LineageTree {
            nodes: lineage
                .nodes
                .into_iter()
                .filter(|(k, _)| chain_keys.contains(k))
                .collect(),
            metadata: lineage
                .metadata
                .into_iter()
                .filter(|(k, _)| chain_keys.contains(k))
                .collect(),
        }
    } else {
        lineage
    };

    // Convert to R-compatible version (u128 -> f64) and attach source directory (relative to pharos.toml)
    let source_dir = to_config_relative(&model_dir)?;
    let r_lineage: RLineageTree = RLineageTree::from(lineage).with_source_dir(source_dir);

    // Serialize R-compatible lineage to Robj
    let mut lineage_robj =
        to_robj(&r_lineage).map_to_extendr_err("Failed to create Robj from RLineageTree")?;

    // Set S3 class
    let hyperion_nonmem_tree = lineage_robj
        .set_class(["hyperion_nonmem_tree"])
        .map_to_extendr_err("Failed to set class")?;

    Ok(hyperion_nonmem_tree.to_owned())
}

extendr_module! {
    mod lineage;

    fn get_model_lineage;
}
