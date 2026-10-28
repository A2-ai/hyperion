//! Stepwise covariate modeling (SCM): the reader side of `pharos nonmem scm`
//! (init, summary) and the helpers every SCM wrapper shares. The slurm side
//! (plan, submit, status, stop) lives in `hyperion-scheduler`.

use extendr_api::Result;
use extendr_api::prelude::*;
use extendr_api::serializer::to_robj;
use serde::Serialize;
use std::path::{Path, PathBuf};

use config::find_config_dir_from;
use nonmem::scm::{self, ScmStatus, SummaryOptions};
use nonmem::validate_model_extension;

use crate::utils::{from_config_relative, get_comment_type, path_from_robj};
use hyperion_core::{OptionExt, ResultExt, extendr_err};

/// The S3 class of the SCM config object `read_scm()` returns.
const SCM_CLASS: &str = "hyperion_nonmem_scm";
pub const PLAN_CLASS: &str = "hyperion_nonmem_scm_plan";
pub const STATUS_CLASS: &str = "hyperion_nonmem_scm_status";
const SUMMARY_CLASS: &str = "hyperion_nonmem_scm_summary";
/// Shared by the status and summary objects: both are the process record.
pub const RECORD_CLASS: &str = "hyperion_nonmem_scm_record";

/// Map a pharos SCM error to an R error the way the CLI shows it: one line,
/// paths relative to the project `near` sits in, with a hint appended when a
/// covariate name did not resolve under the project's comment dialect.
pub trait ScmResultExt<T> {
    fn map_scm_err(self, near: &Path) -> Result<T>;
}

impl<T> ScmResultExt<T> for std::result::Result<T, anyhow::Error> {
    fn map_scm_err(self, near: &Path) -> Result<T> {
        self.map_err(|e| Error::Other(dialect_hint(scm::shown_error(&e, near))))
    }
}

/// pharos names candidates by their `$THETA` comment under the project's
/// comment dialect; a name that does not resolve is most often a comment
/// the dialect does not parse as a name.
fn dialect_hint(text: String) -> String {
    if text.contains("no theta named")
        && let Some(ct) = get_comment_type()
    {
        let ct = format!("{ct:?}").to_lowercase();
        return format!(
            "{text}\n  (comments are parsed as {ct}: name a candidate theta `; NAME` or `; NAME (unit)` in its $THETA comment)"
        );
    }
    text
}

/// The `Config/PharosVersion` field of hyperion's DESCRIPTION: the pharos
/// version recorded in the plans and summaries hyperion writes or reads.
pub fn pharos_version() -> Result<String> {
    let v = R!(r#"utils::packageDescription("hyperion", fields = "Config/PharosVersion")"#)?;
    Ok(v.as_str()
        .filter(|s| !s.is_empty() && *s != "NA")
        .map(str::to_string)
        .unwrap_or_default())
}

/// Resolve the `scm` argument every SCM function takes: a
/// `hyperion_nonmem_scm` object (its `config_source` attribute, relative to
/// the pharos config dir) or a path given by the user.
fn scm_path_from_robj(input: &Robj) -> Result<PathBuf> {
    if input.inherits(SCM_CLASS) {
        let source = input
            .get_attrib("config_source")
            .ok_or_extendr_err("SCM object is missing config_source attribute")?;
        let source = source
            .as_str()
            .ok_or_extendr_err("config_source attribute must be a string")?;
        return from_config_relative(source);
    }
    if let Some(s) = input.as_str() {
        if s.is_empty() {
            return Err(extendr_err!("`scm` is an empty path"));
        }
        return Ok(PathBuf::from(s));
    }
    Err(extendr_err!(
        "`scm` must be a hyperion_nonmem_scm object or a path to an SCM config (<stem>scm.toml) or output directory"
    ))
}

fn reject_model_file(path: &Path) -> Result<()> {
    if validate_model_extension(path).is_ok() {
        return Err(extendr_err!(
            "{} is a model file; pass the SCM config (<stem>scm.toml), its output directory, or a hyperion_nonmem_scm object",
            path.display()
        ));
    }
    Ok(())
}

/// The SCM config for `input`: the path given (or the object's), or the
/// config beside an output directory.
pub fn scm_config_path(input: &Robj) -> Result<PathBuf> {
    let path = scm_path_from_robj(input)?;
    reject_model_file(&path)?;
    if path.is_dir() {
        return scm::config_beside(&path).map_scm_err(&path);
    }
    Ok(path)
}

/// The SCM output directory for `input`: the directory itself, its
/// `<stem>scm.toml`, or a file inside it.
pub fn scm_out_dir(input: &Robj) -> Result<PathBuf> {
    let path = scm_path_from_robj(input)?;
    reject_model_file(&path)?;
    scm::out_dir_named(&path).map_scm_err(&path)
}

fn absolute_string(path: &Path) -> String {
    std::path::absolute(path)
        .unwrap_or_else(|_| path.to_path_buf())
        .to_string_lossy()
        .into_owned()
}

/// Turn an already serialized record into an SCM object: pharos' rendering
/// in `text`, the absolute `out_dir` every record path is relative to, the
/// project root, and the S3 class.
pub fn scm_obj_from_robj(
    mut robj: Robj,
    text: String,
    out_dir: &Path,
    classes: &[&str],
) -> Result<Robj> {
    robj.set_attrib("text", text)
        .map_to_extendr_err("Failed to set text attribute")?;
    robj.set_attrib("out_dir", absolute_string(out_dir))
        .map_to_extendr_err("Failed to set out_dir attribute")?;
    if let Some(root) = find_config_dir_from(out_dir).ok().flatten() {
        robj.set_attrib("project_root", absolute_string(&root))
            .map_to_extendr_err("Failed to set project_root attribute")?;
    }
    let result = robj
        .set_class(classes)
        .map_to_extendr_err("Failed to set class")?;
    Ok(result.to_owned())
}

/// `scm_obj_from_robj` for a pharos record.
pub fn scm_obj(
    value: &impl Serialize,
    text: String,
    out_dir: &Path,
    classes: &[&str],
) -> Result<Robj> {
    let robj = to_robj(value).map_to_extendr_err("Failed to create Robj from SCM record")?;
    scm_obj_from_robj(robj, text, out_dir, classes)
}

/// Read the SCM process in an out_dir, with the pharos version hyperion
/// records on a process that has not started (pharos reports its crate
/// version there).
pub fn read_scm_status(out_dir: &Path) -> Result<ScmStatus> {
    let mut status = ScmStatus::read(out_dir).map_scm_err(out_dir)?;
    if !status.process.started {
        status.summary.pharos_version = pharos_version()?;
    }
    Ok(status)
}

/// A TOML value as `tomledit::from_toml()` returns it
fn toml_to_robj(value: &toml::Value) -> Robj {
    use toml::Value;
    match value {
        Value::String(s) => Robj::from(s.as_str()),
        Value::Integer(i) => match i32::try_from(*i) {
            Ok(i) => Robj::from(i),
            Err(_) => Robj::from(*i as f64),
        },
        Value::Float(f) => Robj::from(*f),
        Value::Boolean(b) => Robj::from(*b),
        Value::Datetime(d) => Robj::from(d.to_string()),
        Value::Array(items) => {
            if items.is_empty() {
                return Robj::from(());
            }
            if items.iter().all(|v| v.is_str()) {
                return Robj::from(items.iter().filter_map(|v| v.as_str()).collect::<Vec<_>>());
            }
            if items.iter().all(|v| v.is_integer()) && items.iter().all(|v| i32::try_from(v.as_integer().unwrap()).is_ok()) {
                return Robj::from(items.iter().map(|v| v.as_integer().unwrap() as i32).collect::<Vec<_>>());
            }
            if items.iter().all(|v| v.is_float() || v.is_integer()) {
                return Robj::from(items.iter().map(|v| v.as_float().or_else(|| v.as_integer().map(|i| i as f64)).unwrap()).collect::<Vec<_>>());
            }
            if items.iter().all(|v| v.is_bool()) {
                return Robj::from(items.iter().map(|v| v.as_bool().unwrap()).collect::<Vec<_>>());
            }
            let values: Vec<Robj> = items.iter().map(toml_to_robj).collect();
            List::from_values(values).into_robj()
        }
        Value::Table(table) => {
            let pairs: Vec<(&str, Robj)> = table.iter().map(|(k, v)| (k.as_str(), toml_to_robj(v))).collect();
            List::from_pairs(pairs).into_robj()
        }
    }
}

/// Read an SCM config file as a list (internal implementation)
///
/// @keywords internal
/// @noRd
#[extendr(r_name = "read_scm_toml_impl")]
pub fn read_scm_toml_wrap(path: &str) -> Result<Robj> {
    let text = std::fs::read_to_string(path)
        .map_err(|e| extendr_err!("cannot read the SCM config {path}: {e}"))?;
    // The whole document (`str::parse` would read a single value).
    let value: toml::Value = toml::from_str(&text)
        .map_err(|e| extendr_err!("cannot parse the SCM config {path}: {e}"))?;
    Ok(toml_to_robj(&value))
}

/// Write the starter SCM config and output directory for a model.
///
/// @keywords internal
/// @noRd
#[extendr(r_name = "init_scm_impl")]
pub fn init_scm_wrap(model: Robj, #[extendr(default = "FALSE")] overwrite: bool) -> Result<Robj> {
    let model_path = path_from_robj(&model, true)?;
    let near = model_path
        .parent()
        .map(Path::to_path_buf)
        .unwrap_or_default();
    let init = scm::init_scm(&model_path, overwrite).map_scm_err(&near)?;
    Ok(list!(
        config_path = absolute_string(&init.config_path),
        out_dir = absolute_string(&init.out_dir)
    )
    .into_robj())
}

/// Read an SCM process's summary record (internal implementation)
///
/// @param scm A `hyperion_nonmem_scm` object, the SCM config, or the output directory
/// @param round,candidate,long,timing,files See `summary.hyperion_nonmem_scm()`
///
/// @return hyperion_nonmem_scm_summary S3 object
/// @keywords internal
/// @noRd
#[extendr]
pub fn get_scm_summary(
    scm: Robj,
    #[extendr(default = "NULL")] round: Option<String>,
    #[extendr(default = "NULL")] candidate: Option<String>,
    #[extendr(default = "FALSE")] long: bool,
    #[extendr(default = "FALSE")] timing: bool,
    #[extendr(default = "FALSE")] files: bool,
) -> Result<Robj> {
    let out_dir = scm_out_dir(&scm)?;
    let status = read_scm_status(&out_dir)?;
    let opts = SummaryOptions {
        round,
        candidate,
        long,
        timing,
        files,
    };
    let text = status.summary.render_text(&opts).map_scm_err(&out_dir)?;
    scm_obj(
        &status.summary,
        text,
        &out_dir,
        &[SUMMARY_CLASS, RECORD_CLASS],
    )
}

extendr_module! {
    mod scm;

    fn init_scm_wrap;
    fn read_scm_toml_wrap;
    fn get_scm_summary;
}
