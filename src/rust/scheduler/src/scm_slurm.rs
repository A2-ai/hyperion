use extendr_api::Result;
use extendr_api::prelude::*;
use std::path::Path;
use which::which;

use nonmem::scm;
use scheduler::{
    DriverRecord, Liveness, ScmHints, ScmJob, ScmSubmitArgs, cancel_recorded_fits, fit_place_lookup,
};

use crate::slurm::PartitionTable;
use hyperion_core::{OptionExt, ResultExt, extendr_err};
use hyperion_nonmem::scm::{
    PLAN_CLASS, RECORD_CLASS, STATUS_CLASS, ScmResultExt, pharos_version, read_scm_status,
    scm_config_path, scm_obj, scm_obj_from_robj, scm_out_dir,
};
use hyperion_nonmem::utils::load_nonmem_config;

/// How hyperion's user does what a refusal asks for next. pharos's
/// [`ScmJob`] decides what a submit checks and refuses; its hints name the
/// CLI's commands, and these name the R functions instead, with the project
/// chosen through `options(hyperion.config_dir)`.
struct RHints;

impl ScmHints for RHints {
    fn status(&self, _out_dir: &Path, scm_config: &Path) -> String {
        format!("get_scm_status(\"{}\")", scm::shown(scm_config))
    }

    fn stop(&self, _out_dir: &Path, scm_config: &Path) -> String {
        format!("stop_scm(\"{}\")", scm::shown(scm_config))
    }

    fn overwrite(&self) -> String {
        "overwrite = TRUE".to_string()
    }

    fn use_project(&self, root: &Path) -> String {
        format!(
            "run from that project, or set options(hyperion.config_dir = \"{}\")",
            root.display()
        )
    }
}

/// Resolve an SCM config as hyperion does: under the pharos version the
/// package was built against, with the R functions as the hints, and the
/// pharos.toml hyperion finds. The error shows paths relative to the
/// config's project, as every SCM message does.
fn load_job(args: &ScmSubmitArgs) -> Result<ScmJob> {
    let near = args
        .config
        .parent()
        .map(Path::to_path_buf)
        .unwrap_or_default();
    let version = pharos_version()?;
    ScmJob::load(args, &version, Box::new(RHints), || {
        load_nonmem_config(None).map_err(|e| anyhow::anyhow!("{e}"))
    })
    .map_scm_err(&near)
}

/// An optional value as R sees it: the value, or NULL. (`list!` turns a
/// `None` into `NA`, which `is.null()` on the R side does not catch.)
fn opt_robj<T: Into<Robj>>(value: Option<T>) -> Robj {
    value.map(Into::into).unwrap_or_else(|| Robj::from(()))
}

/// Check an SCM config (internal implementation)
///
/// @keywords internal
/// @noRd
#[extendr(r_name = "check_scm_impl")]
pub fn check_scm_wrap(
    scm: Robj,
    #[extendr(default = "NULL")] num_rounds: Option<usize>,
) -> Result<Robj> {
    let config = scm_config_path(&scm)?;
    // A check only reads the out_dir: it reports what a submit would do
    // with the SCM process already there, and discards nothing.
    let args = ScmSubmitArgs {
        config,
        num_rounds,
        ..Default::default()
    };
    let job = load_job(&args)?;
    let text = job.built.render_text();
    let mut robj = scm_obj(
        &job.built.plan,
        text,
        &job.out_dir,
        &[PLAN_CLASS, RECORD_CLASS],
    )?;
    robj.set_attrib("warnings", job.built.warnings.clone())
        .map_to_extendr_err("Failed to set warnings attribute")?;
    robj.set_attrib("resumable", job.built.is_resumable())
        .map_to_extendr_err("Failed to set resumable attribute")?;
    Ok(robj)
}

fn driver_robj(record: &DriverRecord) -> Robj {
    let liveness = match record.liveness() {
        Liveness::Alive => "alive".to_string(),
        Liveness::Gone => "gone".to_string(),
        Liveness::Unknown(why) => format!("unknown ({why})"),
    };
    list!(
        mode = record.mode.clone(),
        job_id = record.job_id.map(|j| j as f64),
        host = record.host.clone(),
        pid = record.pid.map(|p| p as f64),
        allocation = record.allocation.map(|a| a as f64),
        log = record.log.as_ref().map(|l| scm::shown(l)),
        started = record.started.clone(),
        liveness = liveness
    )
    .into_robj()
}

/// Get the status of an SCM process (internal implementation)
///
/// Returns `list(status = <hyperion_nonmem_scm_status>, warnings =
/// <character>)`: the warnings are raised on the R side, where
/// `withCallingHandlers()` and `suppressWarnings()` can see them (a
/// `warning()` called from Rust through `call!` is signalled outside R's
/// handler stack and cannot be caught).
///
/// @keywords internal
/// @noRd
#[extendr(r_name = "get_scm_status_impl")]
pub fn get_scm_status_wrap(scm: Robj) -> Result<Robj> {
    let out_dir = scm_out_dir(&scm)?;
    let record = DriverRecord::read(&out_dir);
    let status = match read_scm_status(&out_dir) {
        Ok(status) => status,
        // A driver was submitted but has not written scm_state.json yet, and
        // the config is not beside the out_dir (`[nonmem.scm] out_dir` points
        // elsewhere): pharos cannot read the process from the out_dir alone.
        Err(e) if record.is_some() && !out_dir.join(scm::STATE_FILENAME).exists() => {
            return Err(extendr_err!(
                "{e}\nThe SCM process in {} has a driver that has not written its state yet; try again once it has",
                scm::shown(&out_dir)
            ));
        }
        Err(e) => return Err(e),
    };
    let place_of = fit_place_lookup();
    let mut text = status.render(&place_of);
    let mut warnings: Vec<String> = Vec::new();
    if record.is_none() && DriverRecord::path(&out_dir).exists() {
        warnings.push(format!(
            "{} is unreadable; the driver is treated as never started",
            scm::shown(&DriverRecord::path(&out_dir))
        ));
    }
    let driver = match &record {
        Some(r) => {
            for line in r.status_lines(&status.summary.status, &|p: &Path| scm::shown(p)) {
                text.push_str(&line);
                text.push('\n');
            }
            driver_robj(r)
        }
        None => Robj::from(()),
    };
    let summary = extendr_api::serializer::to_robj(&status.summary)
        .map_to_extendr_err("Failed to create Robj from SCM record")?;
    let summary = summary
        .as_list()
        .ok_or_extendr_err("SCM record is not a list")?;
    let mut pairs: Vec<(String, Robj)> = summary.iter().map(|(k, v)| (k.to_string(), v)).collect();
    pairs.push(("driver".to_string(), driver));
    let status = scm_obj_from_robj(
        List::from_pairs(pairs).into_robj(),
        text,
        &out_dir,
        &[STATUS_CLASS, RECORD_CLASS],
    )?;
    Ok(list!(status = status, warnings = warnings).into_robj())
}

/// Submit an SCM process to slurm (internal implementation)
///
/// @keywords internal
/// @noRd
#[extendr(r_name = "submit_scm_to_slurm_impl")]
#[allow(clippy::too_many_arguments)]
pub fn submit_scm_to_slurm_wrap(
    scm: Robj,
    #[extendr(default = "NULL")] num_rounds: Option<usize>,
    #[extendr(default = "NULL")] partition: Option<String>,
    #[extendr(default = "NULL")] driver_partition: Option<String>,
    #[extendr(default = "NULL")] account: Option<String>,
    #[extendr(default = "NULL")] max_concurrent: Option<usize>,
    #[extendr(default = "FALSE")] shared_node: bool,
    #[extendr(default = "FALSE")] overwrite: bool,
    #[extendr(default = "FALSE")] verbose: bool,
) -> Result<Robj> {
    // Everything that can refuse the submit, before anything is written.
    let pharos_exe = which("pharos").map_to_extendr_err("Failed to locate pharos executable")?;
    let config = scm_config_path(&scm)?;
    let args = ScmSubmitArgs {
        config,
        num_rounds,
        partition,
        account,
        max_concurrent,
        shared_node,
        overwrite,
    };
    let job = load_job(&args)?;
    let fits_partition = job
        .fits_partition()
        .map_to_extendr_err("Failed to resolve the fits' partition")?;
    let config_dir = job
        .config_path
        .parent()
        .ok_or_extendr_err("pharos.toml has no parent directory")?;
    let cpus_per_fit = i32::from(job.nonmem_config.parallel.resolve_num_cpus(config_dir));
    let table = PartitionTable::from_slurm()?;
    if let Some(active) = table.find_partition(&fits_partition)
        && cpus_per_fit > active.cpus as i32
    {
        let advice = table.partition_advice(cpus_per_fit, &fits_partition, 1, false);
        call!(
            "stop",
            format!(
                "each fit needs {cpus_per_fit} CPUs ([nonmem.parallel] in pharos.toml) but partition `{fits_partition}` has {} per node. {advice}",
                active.cpus
            )
        )?;
    }
    let discard_note = job.screen_for_submit(overwrite).map_scm_err(&job.out_dir)?;
    let submitted = job
        .submit_to_slurm(&args, driver_partition, pharos_exe, verbose)
        .map_scm_err(&job.out_dir)?;
    Ok(list!(
        job_id = submitted.job_id as f64,
        job_name = submitted.job_name,
        log = scm::shown(&submitted.log_path),
        config = scm::shown(&job.scm_config),
        warnings = job.built.warnings.clone(),
        discard_note = opt_robj(discard_note)
    )
    .into_robj())
}

/// Stop an SCM process (internal implementation)
///
/// @keywords internal
/// @noRd
#[extendr(r_name = "stop_scm_impl")]
pub fn stop_scm_wrap(scm: Robj) -> Result<Robj> {
    let config = scm_config_path(&scm)?;
    let out_dir = scm::out_dir_named(&config).map_scm_err(&config)?;
    let shown = scm::shown(&out_dir);
    let Some(record) = DriverRecord::read(&out_dir) else {
        return Err(extendr_err!(
            "no driver has been started for the SCM process in {shown}"
        ));
    };
    let was_alive = record.liveness() == Liveness::Alive;
    // The driver first, so it submits nothing more; then every fit still
    // in the queue; then a node allocation a gone driver left held.
    record.stop().map_scm_err(&out_dir)?;
    let cancelled: Vec<f64> = cancel_recorded_fits(&out_dir)
        .map_scm_err(&out_dir)?
        .into_iter()
        .map(|j| j as f64)
        .collect();
    let allocation = record.release_allocation().map_scm_err(&out_dir)?;
    Ok(list!(
        out_dir = shown,
        was_alive = was_alive,
        fits_cancelled = cancelled,
        allocation = opt_robj(allocation),
        config = scm::shown(&config)
    )
    .into_robj())
}

extendr_module! {
    mod scm_slurm;

    fn check_scm_wrap;
    fn get_scm_status_wrap;
    fn submit_scm_to_slurm_wrap;
    fn stop_scm_wrap;
}
