use extendr_api::Result;
use extendr_api::prelude::*;
use extendr_api::serializer::to_robj;

use fs_err as fs;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::Path;

use nonmem::{LineageTree, ModelMetadata, OutputFileHash, RunEndFile, RunStartFile};

use crate::utils::path_from_robj;
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

/// Run-specific info (start file + optional end file) for one model.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RunInfo {
    pub start: RunStartFile,
    pub end: Option<RRunEndFile>,
}

/// One entry in the lineage tree.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LineageNode {
    pub name: String,
    pub model: ModelMetadata,
    pub run: Option<RunInfo>,
}

/// R-compatible version of LineageTree.
///
/// `nodes` is an ordered `Vec` so iteration order is deterministic and
/// matches pharos's `topological_order` (parents before children,
/// alphabetical tie-breaks). Each entry bundles the model metadata and the
/// optional run info, so R doesn't have to navigate parallel maps.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct RLineageTree {
    pub nodes: Vec<LineageNode>,
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

impl TryFrom<LineageTree> for RLineageTree {
    type Error = extendr_api::Error;

    fn try_from(lineage: LineageTree) -> Result<Self> {
        let all_keys: HashSet<String> = lineage.nodes.keys().cloned().collect();
        let chain = lineage
            .topological_order(all_keys)
            .map_to_extendr_err("Could not get topological order of LineageTree")?;

        let mut project_metadata = lineage.metadata;
        let nodes = chain
            .into_iter()
            .map(|(name, model)| {
                let run = project_metadata
                    .remove(&name)
                    .map(|(start, end_opt)| RunInfo {
                        start,
                        end: end_opt.map(Into::into),
                    });
                LineageNode { name, model, run }
            })
            .collect();

        Ok(RLineageTree { nodes })
    }
}

/// Show model lineage and relationships.
///
/// With no arguments, returns the full project lineage tree. Supplying a
/// model path returns that model's full lineage (ancestors and descendants).
/// The `from` and `to` arguments filter the tree from a model downward, up
/// to a model, or to the slice between two models. The project is always
/// rooted at the directory containing `pharos.toml`.
///
/// @param model Optional `hyperion_nonmem_model` object or model file path.
/// Returns the model's full lineage (ancestors and descendants). Conflicts
/// with `from`/`to`.
/// @param from Filter the tree to this model and everything downstream.
/// Accepts a `hyperion_nonmem_model` object or a model file path.
/// @param to Filter the tree to this model and everything upstream.
/// Accepts a `hyperion_nonmem_model` object or a model file path.
///
/// @return hyperion_nonmem_tree S3 object
/// @export
///
/// @examples \dontrun{
/// get_model_lineage()                                            # whole project
/// get_model_lineage("model/nonmem/run003.mod")                   # full lineage of run003
/// get_model_lineage(from = "model/nonmem/run001.mod")            # run001 and descendants
/// get_model_lineage(to = "model/nonmem/run003.mod")              # run003 and ancestors
/// get_model_lineage(from = "model/nonmem/run001.mod",
///                   to   = "model/nonmem/run003.mod")            # slice between two models
/// }
#[extendr]
pub fn get_model_lineage(
    #[extendr(default = "NULL")] model: Option<&Robj>,
    #[extendr(default = "NULL")] from: Option<&Robj>,
    #[extendr(default = "NULL")] to: Option<&Robj>,
) -> Result<Robj> {
    // extendr passes the R NULL default as Some(&null_robj), not None. Filter
    // null Robjs out so downstream `is_some()` checks reflect user intent.
    let model = model.filter(|r| !r.is_null());
    let from = from.filter(|r| !r.is_null());
    let to = to.filter(|r| !r.is_null());

    if model.is_some() && (from.is_some() || to.is_some()) {
        return Err(extendr_err!("model conflicts with from/to"));
    }

    let project_root = find_config_dir()?
        .ok_or_extendr_err("No pharos.toml found; lineage requires a pharos project root")?;
    // Canonicalize so `load_run_metadata`'s strip_prefix against each
    // model_canonical_path succeeds (pharos silently drops mismatches).
    let project_root = fs::canonicalize(&project_root)
        .map_to_extendr_err("Failed to canonicalize project root")?;

    let lineage = LineageTree::from_project_root(project_root.clone())
        .map_to_extendr_err("Pharos failed to build lineage tree")?;

    let mut focal: Vec<String> = Vec::new();
    let chain = if let Some(model) = model {
        let model_path = path_from_robj(model, false)?;
        focal.push(focal_key_for(&model_path, &project_root));
        lineage
            .lineage_of(&model_path)
            .map_to_extendr_err("Failed to compute model lineage")?
    } else {
        let from_path = from.map(|r| path_from_robj(r, false)).transpose()?;
        let to_path = to.map(|r| path_from_robj(r, false)).transpose()?;
        if let Some(p) = &from_path {
            focal.push(focal_key_for(p, &project_root));
        }
        if let Some(p) = &to_path {
            focal.push(focal_key_for(p, &project_root));
        }
        lineage
            .slice(from_path.as_deref(), to_path.as_deref())
            .map_to_extendr_err("Failed to slice lineage tree")?
    };

    let lineage = filter_lineage(lineage, chain);

    let r_lineage: RLineageTree = lineage.try_into()?;

    let mut lineage_robj =
        to_robj(&r_lineage).map_to_extendr_err("Failed to create Robj from RLineageTree")?;

    lineage_robj
        .set_attrib("focal", focal)
        .map_to_extendr_err("Failed to set focal attribute")?;

    let hyperion_nonmem_tree = lineage_robj
        .set_class(["hyperion_nonmem_tree"])
        .map_to_extendr_err("Failed to set class")?;

    Ok(hyperion_nonmem_tree.to_owned())
}

/// Compute the project-relative key for a user-supplied path, used to set
/// the "focal" attribute on the returned tree. Mirrors pharos's
/// `model_identity_for` resolution: canonicalize + strip the project root,
/// joining components with forward slashes. Falls back to the raw input
/// when canonicalization fails (e.g., the user passed an already-keyed
/// string).
fn focal_key_for(path: &Path, project_root: &Path) -> String {
    if let Ok(canonical) = fs::canonicalize(path)
        && let Ok(rel) = canonical.strip_prefix(project_root)
    {
        return rel
            .components()
            .map(|c| c.as_os_str().to_string_lossy().into_owned())
            .collect::<Vec<_>>()
            .join("/");
    }
    path.to_string_lossy().into_owned()
}

/// Trim a `LineageTree` to just the nodes in `chain`. Both `nodes` and
/// `metadata` are filtered to those keys so the downstream `From` impl sees
/// a coherent project containing only the slice.
fn filter_lineage(lineage: LineageTree, chain: Vec<(String, ModelMetadata)>) -> LineageTree {
    let chain_keys: HashSet<String> = chain.into_iter().map(|(k, _)| k).collect();
    // `project_root` is private on `LineageTree`; build via Default and
    // mutate the pub fields. The downstream `From` impl only needs
    // identity-keyed (string) access; it doesn't call path-based queries.
    let mut filtered = LineageTree::default();
    filtered.nodes = lineage
        .nodes
        .into_iter()
        .filter(|(k, _)| chain_keys.contains(k))
        .collect();
    filtered.metadata = lineage
        .metadata
        .into_iter()
        .filter(|(k, _)| chain_keys.contains(k))
        .collect();
    filtered
}

extendr_module! {
    mod lineage;

    fn get_model_lineage;
}
