use extendr_api::Result;
use extendr_api::prelude::*;
use std::path::{Path, PathBuf};
use which::which;

use config::{CONFIG_FILENAME, NonmemConfig};
use nonmem::check_model;
use nonmem::scm::{self, BuiltPlan, PlanContext, ScmOverrides, ScmRunStatus};
use scheduler::slurm::resolve_partition;
use scheduler::{
    DriverRecord, Liveness, ScmDriver, SubmittedDriver, cancel_recorded_fits, fit_place_lookup,
};

use crate::slurm::PartitionTable;
use hyperion_core::{OptionExt, ResultExt, extendr_err};
use hyperion_nonmem::scm::{
    PLAN_CLASS, RECORD_CLASS, STATUS_CLASS, ScmResultExt, pharos_version, read_scm_status,
    scm_config_path, scm_obj, scm_obj_from_robj, scm_out_dir,
};
use hyperion_nonmem::utils::load_nonmem_config;

/// What a submit (and `check_scm`, which reports what a submit would do)
/// takes: the CLI's `ScmSubmitArgs`.
#[derive(Default)]
pub struct ScmSubmitArgs {
    pub config: PathBuf,
    pub num_rounds: Option<usize>,
    pub partition: Option<String>,
    pub account: Option<String>,
    pub max_concurrent: Option<usize>,
    pub shared_node: bool,
    pub overwrite: bool,
}

/// An SCM config resolved to its plan, and the settings it runs under:
/// arguments over `[nonmem.scm]`.
pub struct ScmJob {
    /// The SCM config, as submitted (canonical)
    scm_config: PathBuf,
    built: BuiltPlan,
    /// The project's pharos.toml
    config_path: PathBuf,
    nonmem_config: NonmemConfig,
    out_dir: PathBuf,
    /// The fits' partition
    partition: Option<String>,
    account: Option<String>,
    /// Unset: no cap (a shared node fills its CPUs)
    max_concurrent: Option<usize>,
}

impl ScmJob {
    /// Resolve the config to its plan (so validate it, and read where the
    /// SCM process in its out_dir stands) before anything is touched.
    pub fn load(args: &ScmSubmitArgs) -> Result<Self> {
        let scm_config = std::fs::canonicalize(&args.config)
            .map_err(|e| extendr_err!("no SCM config at {}: {e}", scm::shown(&args.config)))?;
        let near = scm_config
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or_default();
        let overrides = ScmOverrides {
            num_rounds: args.num_rounds,
        };
        let built = scm::build_plan_from_config(&scm_config, &overrides, &pharos_version()?)
            .map_scm_err(&near)?;
        let (config_path, nonmem_config) = load_nonmem_config(None)?;
        // The plan was built under the model's project; the fits run under
        // the pharos.toml hyperion found. They have to be the same file.
        let project_root = config_path
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or_default()
            .canonicalize()
            .unwrap_or_default();
        let plan_root = built.plan.root.canonicalize().unwrap_or_default();
        if project_root != plan_root {
            return Err(extendr_err!(
                "the pharos.toml in use ({}) is not the project the model belongs to ({}); \
                 run from that project, or set options(hyperion.config_dir = \"{}\")",
                config_path.display(),
                plan_root.join(CONFIG_FILENAME).display(),
                plan_root.display()
            ));
        }
        let scm_settings = &nonmem_config.scm;
        let max_concurrent = args.max_concurrent.or(scm_settings.max_concurrent());
        Ok(Self {
            partition: args
                .partition
                .clone()
                .or_else(|| scm_settings.partition.clone()),
            account: args
                .account
                .clone()
                .or_else(|| scm_settings.account.clone()),
            max_concurrent,
            out_dir: built.plan.out_dir_path(),
            config_path: config_path
                .canonicalize()
                .map_to_extendr_err("Failed to resolve pharos.toml")?,
            nonmem_config,
            scm_config,
            built,
        })
    }

    fn model_stem(&self) -> String {
        self.built
            .plan
            .model_path()
            .file_stem()
            .map(|s| s.to_string_lossy().into_owned())
            .unwrap_or_else(|| "scm".to_string())
    }

    /// Whether the SCM process in the out_dir has already run to completion:
    /// a submit without `overwrite` has nothing to do and is refused.
    fn completed(&self) -> bool {
        self.built
            .context
            .progress
            .as_ref()
            .is_some_and(|p| p.state.status == ScmRunStatus::Completed)
    }

    /// The note `overwrite` earns: the SCM process in the out_dir `who`
    /// discards, as `<status>, N rounds complete`; `None` when none has started.
    pub fn discard_note(&self, who: &str) -> Option<String> {
        self.built.context.progress.as_ref().map(|p| {
            let n = p.state.completed_rounds();
            let s = if n == 1 { "" } else { "s" };
            format!(
                "overwrite = TRUE: {who} discards the SCM process in {} ({}, {n} round{s} complete) and starts fresh",
                scm::shown(&self.out_dir),
                p.state.status
            )
        })
    }

    /// The initial model through NM-TRAN, as every fit's script runs it
    /// before `sbatch`: nothing when the project has no NM-TRAN to check
    /// with, the failure's output as the error.
    fn nmtran_check(&self) -> Result<()> {
        if self
            .nonmem_config
            .get_nmtrans_executable_path(None)
            .is_err()
        {
            return Ok(());
        }
        let model = self.built.plan.model_path();
        let result =
            check_model(&self.nonmem_config, &model).map_to_extendr_err("Failed to run NM-TRAN")?;
        if !result.success {
            return Err(extendr_err!(
                "NM-TRAN rejects the initial model {}:\n{}",
                scm::shown(&model),
                result.stdout.trim()
            ));
        }
        Ok(())
    }

    /// The screening every submit does before it drives; returns the discard
    /// note an `overwrite` earns.
    pub fn screen(&self, overwrite: bool) -> Result<Option<String>> {
        let out_dir = scm::shown(&self.out_dir);
        if !overwrite && self.completed() {
            return Err(extendr_err!(
                "the SCM process in {out_dir} is already complete; submit with overwrite = TRUE to start it over"
            ));
        }
        if !overwrite && !self.built.is_resumable() {
            return Err(extendr_err!(
                "the SCM process in {out_dir} cannot resume under this plan; submit with overwrite = TRUE to discard that process and start fresh\n{}",
                self.built.context.render_text().trim_end()
            ));
        }
        self.nmtran_check()?;
        self.built.write_gitignore().map_scm_err(&self.out_dir)?;
        Ok(overwrite.then(|| self.discard_note("the driver")).flatten())
    }

    /// The driver job's `scm run` flags: the settings already resolved
    /// here, so the job runs with exactly what was submitted.
    fn driver_job_flags(&self, args: &ScmSubmitArgs) -> Vec<String> {
        let mut flags = Vec::new();
        // As given, not as the plan holds it: `0` (run all rounds) overrides
        // a cap the config sets, and the driver reads the config again.
        if let Some(n) = args.num_rounds {
            flags.extend(["--num-rounds".to_string(), n.to_string()]);
        }
        if let Some(n) = self.max_concurrent {
            flags.extend(["--max-concurrent".to_string(), n.to_string()]);
        }
        for (flag, value) in [
            ("--partition", &self.partition),
            ("--account", &self.account),
        ] {
            if let Some(v) = value {
                flags.extend([flag.to_string(), v.clone()]);
            }
        }
        if args.shared_node {
            flags.push("--shared-node".to_string());
        }
        if args.overwrite {
            flags.push("--overwrite".to_string());
        }
        flags
    }

    /// One driver per SCM process: refuse while another is queued or
    /// running. The record, not the driver's job name, identifies it.
    pub fn refuse_second_driver(&self) -> Result<()> {
        let Some(record) = DriverRecord::read(&self.out_dir) else {
            return Ok(());
        };
        if record.liveness() != Liveness::Alive {
            return Ok(());
        }
        let out_dir = scm::shown(&self.out_dir);
        Err(extendr_err!(
            "the SCM process in {out_dir} is already running ({}); follow it with get_scm_status(\"{out_dir}\"), or stop it with stop_scm(\"{out_dir}\") before resubmitting",
            record.describe()
        ))
    }

    /// `overwrite` discards the process, so it is refused while its last
    /// driver may still be running: alive, or not checkable from here.
    pub fn refuse_overwrite_under_driver(&self) -> Result<()> {
        let Some(record) = DriverRecord::read(&self.out_dir) else {
            return Ok(());
        };
        let out_dir = scm::shown(&self.out_dir);
        match record.liveness() {
            Liveness::Gone => Ok(()),
            Liveness::Alive => Err(extendr_err!(
                "the SCM process in {out_dir} is still running; stop it with stop_scm(\"{out_dir}\") before discarding it with overwrite = TRUE"
            )),
            Liveness::Unknown(why) => Err(extendr_err!(
                "cannot tell whether the SCM process in {out_dir} is still running: {why}; once it has stopped, delete {out_dir}/scm_driver.json and submit again with overwrite = TRUE"
            )),
        }
    }

    /// Queue the driver as a slurm job and return: held until the record
    /// naming it is on disk, released then, cancelled if that fails.
    pub fn submit_to_slurm(
        &self,
        args: &ScmSubmitArgs,
        driver_partition: Option<String>,
        pharos_exe: PathBuf,
        verbose: bool,
    ) -> Result<SubmittedDriver> {
        // A driver alone gets the cluster default partition: it only waits.
        // On a shared node it is where the fits run.
        let driver_partition = if args.shared_node {
            self.partition.clone()
        } else {
            driver_partition.or_else(|| self.nonmem_config.scm.driver_partition.clone())
        };
        let driver = ScmDriver {
            config_path: self.config_path.clone(),
            nonmem_config: self.nonmem_config.clone(),
            pharos_exe,
            scm_config: self.scm_config.clone(),
            model_stem: self.model_stem(),
            partition: driver_partition,
            account: self.account.clone(),
            shared_node: args.shared_node,
            run_flags: self.driver_job_flags(args),
            verbose,
        };
        let submitted = driver.submit().map_scm_err(&self.out_dir)?;
        let record = DriverRecord::slurm(
            submitted.job_id,
            args.shared_node,
            submitted.log_path.clone(),
        );
        if let Err(e) = record
            .save(&self.out_dir)
            .and_then(|_| submitted.release())
        {
            let _ = submitted.cancel();
            return Err(extendr_err!(
                "the SCM driver job {} was cancelled before it started: {e:#}",
                submitted.job_id
            ));
        }
        Ok(submitted)
    }
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
    #[extendr(default = "FALSE")] overwrite: bool,
) -> Result<Robj> {
    let config = scm_config_path(&scm)?;
    let args = ScmSubmitArgs {
        config,
        num_rounds,
        overwrite,
        ..Default::default()
    };
    let job = ScmJob::load(&args)?;
    let mut text = String::new();
    if overwrite {
        // The process in the out_dir is gone before the plan meets it.
        if let Some(note) = job.discard_note("submit") {
            text.push_str(&format!("{note}\n\n"));
        }
        text.push_str(&job.built.plan.render_text(&PlanContext::default()));
    } else {
        text.push_str(&job.built.render_text());
    }
    let mut robj = scm_obj(&job.built.plan, text, &job.out_dir, &[PLAN_CLASS, RECORD_CLASS])?;
    robj.set_attrib("warnings", job.built.warnings.clone())
        .map_to_extendr_err("Failed to set warnings attribute")?;
    robj.set_attrib("resumable", job.built.is_resumable())
        .map_to_extendr_err("Failed to set resumable attribute")?;
    let changes: Vec<Robj> = job
        .built
        .context
        .changes
        .iter()
        .map(|c| list!(field = c.field.clone(), detail = c.detail.clone()).into_robj())
        .collect();
    robj.set_attrib("changes", List::from_values(changes))
        .map_to_extendr_err("Failed to set changes attribute")?;
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
                "{e}\nThe SCM process in {} has a driver that has not written its state yet; until it has, pass the SCM config (<stem>scm.toml) rather than the output directory",
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
    let mut pairs: Vec<(String, Robj)> = summary
        .iter()
        .map(|(k, v)| (k.to_string(), v))
        .collect();
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
    let job = ScmJob::load(&args)?;
    let fits_partition = resolve_partition(
        job.partition.as_deref(),
        job.nonmem_config.slurm.partition.as_deref(),
    )
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
    job.refuse_second_driver()?;
    if overwrite {
        job.refuse_overwrite_under_driver()?;
    }
    let discard_note = job.screen(overwrite)?;
    let submitted = job.submit_to_slurm(&args, driver_partition, pharos_exe, verbose)?;
    Ok(list!(
        job_id = submitted.job_id as f64,
        job_name = submitted.job_name,
        log = scm::shown(&submitted.log_path),
        out_dir = scm::shown(&job.out_dir),
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
    let out_dir = scm_out_dir(&scm)?;
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
    let config = scm::config_beside(&out_dir).ok().map(|c| scm::shown(&c));
    Ok(list!(
        out_dir = shown,
        was_alive = was_alive,
        fits_cancelled = cancelled,
        allocation = opt_robj(allocation),
        config = opt_robj(config)
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
