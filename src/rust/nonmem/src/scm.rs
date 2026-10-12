//! Stepwise covariate modeling (SCM) wrappers.
//!
//! `scm_init_wrap` writes the starter config beside a model, `scm_plan_wrap`
//! resolves a config to its validated plan and reports what a submit of it
//! would do, through the same Rust code the pharos CLI runs (`pharos nonmem
//! scm plan`), and `scm_status_wrap` / `scm_summary_wrap` read an SCM process
//! wherever it stands, exactly as `scm status` and `scm summary` do.
//! `scm_stop_wrap` stops a process's driver as `scm stop` does. Running
//! happens through the pharos CLI in the background (see `scm_run()` on the
//! R side), never in-process.

use std::path::{Path, PathBuf};

use extendr_api::Result;
use extendr_api::prelude::*;
use extendr_api::serializer::to_robj;

use nonmem::scm::{
    self as pharos_scm, Compatibility, PlanChange, PlanContext, Retuning, ScmStatus,
    SummaryOptions,
    plan::BuiltPlan,
    state::ScmProcess,
    summary::{CandidateSummary, Fits, RoundSummary},
};
use scheduler::{
    DriverRecord, Liveness, cancel_recorded_fits, driver_job_name, fit_place_lookup, running_driver,
};

use hyperion_core::{ResultExt, extendr_err};

/// Set up an SCM process for a model (runs nothing)
///
/// Internal engine behind [scm_init()]; use that instead.
///
/// @param model path to the initial model (.mod / .ctl); the output
///   directory lands beside it, and the config inside that
/// @param overwrite replace an existing `<model stem>scm.toml`
///
/// @return a list with `config` (the config file written) and `out_dir`
///   (the output directory created)
/// @keywords internal
#[extendr(r_name = "scm_init_impl")]
pub fn scm_init_wrap(model: &str, #[extendr(default = "FALSE")] overwrite: bool) -> Result<Robj> {
    let init = pharos_scm::init_scm(Path::new(model), overwrite)
        .map_to_extendr_err("Failed to set up the SCM process")?;

    Ok(list!(
        config = init.config_path.to_string_lossy().to_string(),
        out_dir = init.out_dir.to_string_lossy().to_string()
    )
    .into_robj())
}

/// Resolve an SCM config to its plan and say what running it would do
///
/// Internal engine behind [scm_plan()]; use that instead. A dry run: the
/// out_dir is only read, nothing is written and nothing runs.
///
/// @param config path to the SCM config file (TOML) written by
///   [scm_init()] into the SCM out_dir: model, direction, forward_alpha,
///   backward_alpha, max_retries, num_rounds, cov_step,
///   forward_final_cov_step, final_cov_step, and the `[covariates]` section
///   (fixed, the per-type `continuous` / `categorical` tables, effects).
///   Relative paths resolve against the config file
/// @param num_rounds pause after this many rounds per run, over the
///   config's `num_rounds`; `0` runs all rounds (NULL = as the config says)
/// @param overwrite preview a run with `overwrite = TRUE`: the SCM process
///   already in the out_dir is discarded when the driver starts, so the
///   plan is reported against an empty out_dir and the `discards`
///   attribute says what goes
///
/// @return a `hyperion_scm_plan` object; its `config` attribute is the
///   config it was built from, `out_dir` where the SCM process runs,
///   `context` where the SCM process in the out_dir already stands plus what
///   this plan changes about the plan that process runs under, `resumable`
///   whether that process resumes under this plan, `discards` what
///   `overwrite` would throw away (`NULL` when nothing), `refusals` why
///   [scm_run()] would be refused right now (empty when it would not be),
///   `nmtran` NM-TRAN's verdict on the initial model, `settings` the
///   project's `[nonmem.scm]` settings a run gets (`partition`,
///   `driver_partition`, `account`, `max_concurrent`, `poll_interval`;
///   `NULL` outside a pharos project), and `overwrite` whether the plan
///   previews a run with `overwrite = TRUE`
/// @keywords internal
#[extendr(r_name = "scm_plan_impl")]
pub fn scm_plan_wrap(
    config: &str,
    #[extendr(default = "NULL")] num_rounds: Option<i32>,
    #[extendr(default = "FALSE")] overwrite: bool,
) -> Result<Robj> {
    // Guard before the i32 -> usize cast: a negative would wrap to a huge
    // cap, silently meaning "never pause".
    if let Some(n) = num_rounds
        && n < 0
    {
        return Err(extendr_err!(
            "num_rounds must be 0 (run all rounds) or more, got {n}"
        ));
    }

    let overrides = pharos_scm::ScmOverrides {
        num_rounds: num_rounds.map(|n| n as usize),
    };

    // The config as submitted: absolute, so `scm_run()` can hand it to
    // pharos from any working directory.
    let config_path =
        std::fs::canonicalize(config).map_to_extendr_err(format!("no SCM config at {config}"))?;

    // Built, and so validated, before anything else is looked at — the
    // same order as `pharos nonmem scm plan`.
    let built =
        pharos_scm::build_plan_from_config(&config_path, &overrides, env!("CARGO_PKG_VERSION"))
            .map_to_extendr_err("Failed to build SCM plan")?;
    let out_dir = built.plan.out_dir_path();

    // What a submit of this config would meet in the out_dir.
    let discards = discardable(&built);
    // With `overwrite` the process is discarded, so there is nothing to
    // resume under: the plan starts fresh.
    let resumable = overwrite || built.is_resumable();
    let mut refusals: Vec<String> = Vec::new();
    if overwrite {
        // `overwrite` discards the process, so it is refused while its
        // last driver may still be running.
        if let Some(record) = DriverRecord::read(&out_dir)
            && let Err(e) =
                record.refuse_if_alive(&out_dir.display().to_string(), "overwrite = TRUE")
        {
            refusals.push(format!("{e:#}"));
        }
    } else if let Some(verdict) = built
        .context
        .compatibility
        .as_ref()
        .filter(|c| c.is_incompatible())
    {
        // pharos says the same in CLI terms; this spells out what to do in R.
        let reasons: String = verdict
            .reasons
            .iter()
            .map(|r| format!("\n  - {r}"))
            .collect();
        refusals.push(format!(
            "the SCM process in {} belongs to the plan it was submitted under and cannot \
             resume under this one:{reasons}\nrun with `overwrite = TRUE` to discard that \
             process and start fresh",
            built.plan.out_dir
        ));
    }
    if let Some(why) = second_driver(&built, &out_dir) {
        refusals.push(why);
    }
    let nmtran = nmtran_verdict(&built, &out_dir);
    // The settings a run gets from the project's `[nonmem.scm]` — what
    // `pharos nonmem scm plan` shows under `submit`. There are no flags over
    // them at plan time; scm_run()'s arguments override them at run time.
    // `None` when the out_dir is not inside a pharos project.
    let settings = pharos_scm::project_config(&out_dir).ok().map(|config| {
        let scm = &config.scm;
        list!(
            partition = or_null(scm.partition.clone()),
            driver_partition = or_null(scm.driver_partition.clone()),
            account = or_null(scm.account.clone()),
            max_concurrent = or_null(scm.max_concurrent().map(|n| n as i32)),
            poll_interval = scm.poll_interval() as i32
        )
        .into_robj()
    });

    let mut robj = to_robj(&built.plan).map_to_extendr_err("Failed to convert plan to Robj")?;
    robj.set_attrib("warnings", built.warnings.iter().collect_robj())?;
    // The worst-case model count is derived from the plan, not stored in it,
    // so pharos's own arithmetic rides along as an attribute.
    let max_models = pharos_scm::max_models_for(
        built.plan.candidates.len(),
        built.plan.options.phases().len(),
    );
    robj.set_attrib("max_models", (max_models as i32).into_robj())?;
    // The digest of the SCM-defining options: what an scm_state.json in the
    // out_dir has to carry for the SCM process to resume under this plan.
    robj.set_attrib("plan_digest", built.plan.digest().into_robj())?;
    // Retries restart from the previous attempt's estimates, jittered by
    // this much; pharos owns the figure, so the display asks it rather than
    // quoting one of its own.
    robj.set_attrib("retry_jitter", nonmem::scm::round::RETRY_JITTER.into_robj())?;
    robj.set_attrib("config", config_path.to_string_lossy().into_robj())?;
    robj.set_attrib("out_dir", out_dir.to_string_lossy().into_robj())?;
    // Where the SCM process in the out_dir stands and what this plan changes
    // about the plan it runs under. With `overwrite` that process is gone
    // before the plan meets it, so the plan is the whole story — as
    // `pharos nonmem scm plan --overwrite` shows it.
    let context = if overwrite {
        PlanContext::default()
    } else {
        built.context.clone()
    };
    robj.set_attrib("context", context_robj(&context)?)?;
    robj.set_attrib("resumable", resumable.into_robj())?;
    robj.set_attrib("discards", or_null(discards))?;
    robj.set_attrib("refusals", refusals.into_robj())?;
    robj.set_attrib("nmtran", nmtran.into_robj())?;
    robj.set_attrib("settings", or_null(settings))?;
    robj.set_attrib("overwrite", overwrite.into_robj())?;
    let robj = robj.set_class(["hyperion_scm_plan"])?.to_owned();
    Ok(robj)
}

/// The SCM process in the out_dir that `overwrite` discards, as
/// `<status>, N rounds complete`; `None` when none has started.
fn discardable(built: &BuiltPlan) -> Option<String> {
    built.context.progress.as_ref().map(|p| {
        let n = p.state.completed_rounds();
        let s = if n == 1 { "" } else { "s" };
        format!("{}, {n} round{s} complete", p.state.status)
    })
}

/// Why a run would be refused right now: another driver for this SCM
/// process is queued or running. The same check every submit makes.
fn second_driver(built: &BuiltPlan, out_dir: &Path) -> Option<String> {
    let stem = built
        .plan
        .model_path()
        .file_stem()
        .map(|s| s.to_string_lossy().into_owned())
        .unwrap_or_else(|| "scm".to_string());
    let shown = out_dir.display();
    if let Some(job_id) = running_driver(&driver_job_name(&stem), &built.plan.root) {
        return Some(format!(
            "the SCM process in {shown} already has a driver in the queue: slurm job {job_id} \
             ({}); follow it with `scm_status()`, or `scm_stop()` it before running again",
            driver_job_name(&stem)
        ));
    }
    if let Some(record) = DriverRecord::read(out_dir)
        && record.job_id.is_none()
        && record.liveness() == Liveness::Alive
    {
        return Some(format!(
            "the SCM process in {shown} is already being driven by {} on the login node; \
             follow it with `scm_status()`, or `scm_stop()` it before running again",
            record.describe()
        ));
    }
    None
}

/// The initial model through NM-TRAN, as every fit's script runs it before
/// `sbatch`: what `pharos nonmem scm plan` reports under `nmtran`.
fn nmtran_verdict(built: &BuiltPlan, out_dir: &Path) -> String {
    let Ok(settings) = pharos_scm::project_config(out_dir) else {
        return "not checked: this out_dir is not inside a pharos project".to_string();
    };
    if settings.get_nmtrans_executable_path(None).is_err() {
        return "not checked: this project has no NM-TRAN executable to check with".to_string();
    }
    let model = built.plan.model_path();
    match nonmem::check_model(&settings, &model) {
        Ok(result) if result.success => "the initial model passes NM-TRAN".to_string(),
        Ok(result) => format!(
            "a run would be refused: NM-TRAN rejects the initial model {}:\n{}",
            model.display(),
            result.stdout.trim()
        ),
        Err(e) => format!("a run would be refused: NM-TRAN could not be run: {e:#}"),
    }
}

/// The SCM config in an out_dir, where [scm_init()] puts it
///
/// Internal helper behind [scm_run()] and [scm_stop()].
///
/// @param out_dir the SCM out_dir
/// @return the path of the one `<stem>scm.toml` in it
/// @keywords internal
#[extendr(r_name = "scm_config_impl")]
pub fn scm_config_wrap(out_dir: &str) -> Result<String> {
    let config = pharos_scm::config_in(Path::new(out_dir)).map_err(|e| extendr_err!("{e:#}"))?;
    Ok(config.to_string_lossy().into_owned())
}

/// The SCM-defining plan fields — the ones the plan digest covers, so moving
/// any of them is a different SCM process and the state in the out_dir
/// cannot resume under it.
const DIGEST_FIELDS: [&str; 7] = [
    "model",
    "direction",
    "forward_alpha",
    "backward_alpha",
    "max_retries",
    "cov_step",
    "final_cov_step",
];

/// `NULL` for a missing value, so an absent field reads as `NULL` in R
/// rather than as an empty vector.
fn or_null<T: Into<Robj>>(value: Option<T>) -> Robj {
    value.map_or_else(|| r!(NULL), Into::into)
}

/// Where the SCM process in the out_dir stands, as the R display code reads
/// it.
fn progress_robj(p: &ScmProcess) -> Robj {
    let s = &p.state;
    let current = match s.open_round() {
        Some(cur) => list!(
            name = cur.name.clone(),
            concluded = cur.concluded() as i32,
            total = cur.candidates.len() as i32,
            // the candidates the open round holds, so a retune can say which
            // of them that round refits
            candidates = cur
                .candidates
                .iter()
                .map(|c| c.candidate.clone())
                .collect::<Vec<_>>()
        )
        .into_robj(),
        None => r!(NULL),
    };
    list!(
        status = s.status.to_string(),
        phase = or_null(s.phase.map(|d| d.to_string())),
        rounds_complete = s.completed_rounds() as i32,
        current_round = current,
        retained = s.retained.clone(),
        removed = s
            .removed_roster()
            .map(|e| e.removal_label())
            .collect::<Vec<_>>(),
        final_model = or_null(s.final_model.clone()),
        models_running = p.models_running.len() as i32,
        updated = s.updated.clone()
    )
    .into_robj()
}

/// The `context` attribute a freshly built plan carries: where the SCM
/// process in its out_dir already stands, and what this plan changes about
/// the plan that process runs under (the one its `scm_state.json` records).
/// Empty — no progress, no changes — when no SCM process has started there.
///
/// pharos renders its `PlanContext` straight to text for the CLI and never
/// serializes it, so hyperion takes it apart here and hands R the pieces its
/// own print and knit_print methods are built on.
fn context_robj(ctx: &PlanContext) -> Result<Robj> {
    // No state behind the plan reads as the empty verdict: nothing removed,
    // nothing retuned, no reason it cannot resume.
    let nothing = Compatibility::default();
    let verdict = ctx.compatibility.as_ref().unwrap_or(&nothing);
    let (removals, retunes, reasons): (&[String], &[Retuning], &[String]) =
        (&verdict.removals, &verdict.retunes, &verdict.reasons);

    // A change is SCM-defining when the SCM process in the out_dir cannot
    // carry it: a plan-digest field, or a candidate change that is neither a
    // removal nor a retune (the two the process absorbs). With no state
    // behind the plan there is nothing to lose, so nothing is flagged.
    let has_state = ctx.compatibility.is_some();
    let scm_defining = |c: &PlanChange| -> bool {
        if !has_state {
            return false;
        }
        if DIGEST_FIELDS.contains(&c.field.as_str()) {
            return true;
        }
        if c.field != "candidates" {
            return false;
        }
        // pharos writes a candidates change as "<name> <what changed>"
        let name = c.detail.split_whitespace().next().unwrap_or_default();
        !removals.iter().any(|r| r == name) && !retunes.iter().any(|r| r.name() == name)
    };

    let changes: Vec<Robj> = ctx
        .changes
        .iter()
        .map(|c| {
            list!(
                field = c.field.clone(),
                detail = c.detail.clone(),
                scm_defining = scm_defining(c)
            )
            .into_robj()
        })
        .collect();

    let retunes: Vec<Robj> = retunes
        .iter()
        .map(|r| {
            list!(
                candidate = list!(name = r.name().to_string()),
                changes = r.changes.clone()
            )
            .into_robj()
        })
        .collect();

    Ok(list!(
        changes = List::from_values(changes),
        progress = ctx
            .progress
            .as_ref()
            .map_or_else(|| r!(NULL), progress_robj),
        removals = removals.to_vec(),
        retunes = List::from_values(retunes),
        state_is_stale = verdict.is_incompatible(),
        stale_reasons = reasons.to_vec()
    )
    .into_robj())
}

// Driver ---------------------------------------------------------------------

/// The driver record as R sees it: what `scm_driver.json` holds, plus
/// whether that driver is still alive.
fn driver_robj(record: &DriverRecord) -> Robj {
    let (liveness, detail) = match record.liveness() {
        Liveness::Alive => ("alive", None),
        Liveness::Gone => ("gone", None),
        Liveness::Unknown(why) => ("unknown", Some(why)),
    };
    list!(
        mode = record.mode.clone(),
        job_id = or_null(record.job_id.map(|j| j as i32)),
        host = or_null(record.host.clone()),
        pid = or_null(record.pid.map(|p| p as i32)),
        allocation = or_null(record.allocation.map(|a| a as i32)),
        log = or_null(record.log.as_ref().map(|l| l.to_string_lossy().to_string())),
        started = record.started.clone(),
        liveness = liveness,
        detail = or_null(detail)
    )
    .into_robj()
}

/// The config of the SCM process in `out_dir`, when it holds exactly one.
fn config_of(out_dir: &Path) -> Option<PathBuf> {
    pharos_scm::config_in(out_dir).ok()
}

/// Stop the driver of an SCM process
///
/// Internal engine behind [scm_stop()]; use that instead. What
/// `pharos nonmem scm stop` does: `scancel` the driver's Slurm job, or
/// SIGTERM its login-node process (which has to be on this host); the
/// driver records the SCM process as paused and exits once its executor
/// hands the running fits back. Fits that are Slurm jobs of their own keep
/// running unless `fits`.
///
/// @param path the SCM out_dir, or its `<stem>scm.toml`
/// @param fits also cancel the fits still in the Slurm queue
///
/// @return a list: `out_dir`; `driver`, what was done to it; `was_alive`,
///   whether it was running when told to stop; `fits`, the Slurm jobs
///   cancelled (`NULL` unless `fits`); `allocation`, what became of a
///   shared-node allocation (`NULL` when there is none); `config`, the
///   process's config, to resume it with (`NULL` when the out_dir holds
///   none)
/// @keywords internal
#[extendr(r_name = "scm_stop_impl")]
pub fn scm_stop_wrap(path: &str, #[extendr(default = "FALSE")] fits: bool) -> Result<Robj> {
    let out_dir = pharos_scm::out_dir_named(Path::new(path));
    let Some(record) = DriverRecord::read(&out_dir) else {
        return Err(extendr_err!(
            "no driver has been started for the SCM process in {}",
            out_dir.display()
        ));
    };
    let was_alive = record.liveness() == Liveness::Alive;
    let driver = record.stop().map_err(|e| extendr_err!("{e:#}"))?;
    let cancelled = if fits {
        Some(
            cancel_recorded_fits(&out_dir)
                .map_err(|e| extendr_err!("Failed to cancel the fits in the slurm queue: {e:#}"))?
                .into_iter()
                .map(|j| j as i32)
                .collect::<Vec<_>>(),
        )
    } else {
        None
    };
    let allocation = record
        .release_allocation()
        .map_err(|e| extendr_err!("{e:#}"))?;

    Ok(list!(
        out_dir = out_dir.to_string_lossy().to_string(),
        driver = driver,
        was_alive = was_alive,
        fits = or_null(cancelled),
        allocation = or_null(allocation),
        config = or_null(config_of(&out_dir).map(|c| c.to_string_lossy().to_string()))
    )
    .into_robj())
}

/// Read the status of an SCM process
///
/// Internal engine behind [scm_status()]; use that instead.
///
/// @param path the SCM out_dir, or its `<stem>scm.toml`
///
/// @return a `hyperion_scm_status` object: the summary record, with the
///   text `pharos nonmem scm status` prints as its `rendered` attribute, the
///   out_dir it was read from as `out_dir`, the config in that out_dir as
///   `config` (`NULL` when it holds none), whether the SCM process has
///   started as `started`, and the driver record (`scm_driver.json`, with
///   its liveness) as `driver` — `NULL` before a driver was ever started
/// @keywords internal
#[extendr(r_name = "scm_status_impl")]
pub fn scm_status_wrap(path: &str) -> Result<Robj> {
    let out_dir = pharos_scm::out_dir_named(Path::new(path));
    // The process as its driver's terminal shows it, read off disk now: the
    // state reconciled with what the fits have left behind, then summarized
    // — the same reader `scm_summary()` uses, so the two never describe
    // different SCM processes. A process that has not started is read under
    // what its config resolves to now; a started one under the plan its
    // state records.
    let status = ScmStatus::read(&out_dir).map_to_extendr_err("Failed to read SCM status")?;
    let record = DriverRecord::read(&out_dir);
    let driver_lines = record
        .as_ref()
        .map(|r| r.status_lines(&status.summary.status, &|p| p.display().to_string()))
        .unwrap_or_default();
    let place_of = fit_place_lookup();
    let rendered = status.render(&driver_lines, &place_of);

    let mut robj =
        to_robj(&status.summary).map_to_extendr_err("Failed to convert status to Robj")?;
    robj.set_attrib("rendered", rendered.into_robj())?;
    // The record's own `out_dir` is written relative to the pharos project
    // root, so the directory it was read from is the one to go back to.
    robj.set_attrib("out_dir", out_dir.to_string_lossy().into_robj())?;
    robj.set_attrib(
        "config",
        or_null(config_of(&out_dir).map(|c| c.to_string_lossy().to_string())),
    )?;
    robj.set_attrib("started", status.process.started.into_robj())?;
    robj.set_attrib("driver", or_null(record.as_ref().map(driver_robj)))?;
    let robj = robj.set_class(["hyperion_scm_status"])?.to_owned();
    Ok(robj)
}

/// The SCM summary: every round to date, or a selection, rendered
///
/// Internal engine behind [scm_summary()]; use that instead.
///
/// @param path the SCM out_dir, or its `<stem>scm.toml`
/// @param round only this round: the Nth SCM round ("2" / "round 2"), a
///   round name (forward_round1, backward_round1), or "reference"; NULL for
///   every round
/// @param candidate trace one candidate through every round it was tested in
/// @param long,timing,files the `scm summary` detail flags
///
/// @return a `hyperion_scm_summary` object: the summary record (the rounds
///   selected), with the rendered text as its `rendered` attribute and the
///   markdown rendering as `markdown`
/// @keywords internal
#[extendr(r_name = "scm_summary_impl")]
pub fn scm_summary_wrap(
    path: &str,
    #[extendr(default = "NULL")] round: Option<String>,
    #[extendr(default = "NULL")] candidate: Option<String>,
    #[extendr(default = "FALSE")] long: bool,
    #[extendr(default = "FALSE")] timing: bool,
    #[extendr(default = "FALSE")] files: bool,
) -> Result<Robj> {
    let opts = SummaryOptions {
        round,
        candidate,
        long,
        timing,
        files,
    };

    let summary = pharos_scm::read_summary(&pharos_scm::out_dir_named(Path::new(path)))
        .map_to_extendr_err("Failed to read SCM summary")?;
    let rendered = summary
        .render_text(&opts)
        .map_to_extendr_err("Failed to render SCM summary")?;

    // The rounds the options select, with the candidate rows they show: the
    // record behind the rendering, for `as.data.frame()` to walk.
    let names: Vec<String> = summary
        .select_rounds(&opts)
        .map_to_extendr_err("Failed to select SCM rounds")?
        .iter()
        .map(|r| r.round.clone())
        .collect();
    let mut selected = summary.clone();
    selected.rounds.retain(|r| names.contains(&r.round));
    if let Some(name) = &opts.candidate {
        for r in &mut selected.rounds {
            r.candidates
                .retain(|c| c.candidate.eq_ignore_ascii_case(name));
        }
    }

    // Each round's markdown is a `## <round>` section, as in pharos's
    // scm_summary.md; the selection decides which ones, and the fits the
    // read already loaded for all of them carry over.
    let fits = &summary.fits;
    let mut markdown = String::from("# SCM summary\n\n");
    for r in &selected.rounds {
        markdown.push_str(&round_markdown(r, fits));
        markdown.push('\n');
    }

    let mut robj = to_robj(&selected).map_to_extendr_err("Failed to convert summary to Robj")?;
    robj.set_attrib("rendered", rendered.into_robj())?;
    robj.set_attrib("markdown", markdown.into_robj())?;
    let robj = robj.set_class(["hyperion_scm_summary"])?.to_owned();
    Ok(robj)
}

// Markdown -------------------------------------------------------------------
//
// pharos renders each round's section of `scm_summary.md` from the same
// record (`summary.rs`, `round_markdown` and `add_candidate_table`), but
// keeps that renderer to itself; this is the same rendering, built over the
// record's public types, so a knitted summary reads like the file on disk.

/// Accumulates the lines of a rendered section
#[derive(Default)]
struct Lines(String);

impl Lines {
    fn add(&mut self, line: impl AsRef<str>) {
        self.0.push_str(line.as_ref().trim_end());
        self.0.push('\n');
    }

    fn blank(&mut self) {
        self.0.push('\n');
    }
}

/// Every rendering gives its numbers three decimals.
const DIGITS: usize = 3;

fn yes_no(flag: bool) -> &'static str {
    if flag { "yes" } else { "no" }
}

fn none_or_list(items: &[String]) -> String {
    if items.is_empty() {
        "none".to_string()
    } else {
        items.join(", ")
    }
}

fn ofv_suffix(ofv: Option<f64>) -> String {
    ofv.map(|o| format!(" (OFV {o:.DIGITS$})"))
        .unwrap_or_default()
}

/// `0.412 (14.2%)`, or `0.412 (N/A)` when the fit carries no standard error
/// to make an RSE from — same as `pharos nonmem summary` with covariance step off
fn fmt_estimate(e: &nonmem::output_files::ext::ThetaEstimate) -> String {
    match e.rse {
        Some(rse) => format!("{:.DIGITS$} ({rse:.1}%)", e.estimate),
        None => format!("{:.DIGITS$} (N/A)", e.estimate),
    }
}

/// The condition number of a candidate's fit's last `$EST`.
fn condition_number(c: &CandidateSummary, fits: &Fits) -> Option<f64> {
    fits.get(c.files.summary_json.as_deref()?)?
        .minimization_results
        .last()?
        .condition_number
        .filter(|v| v.is_finite())
}

/// The markdown record of one round: its facts, then its candidate table.
fn round_markdown(round: &RoundSummary, fits: &Fits) -> String {
    let mut out = Lines::default();
    out.add(format!("## {}", round.round));
    out.blank();
    if !round.has_reference() {
        reference_markdown(&mut out, round);
        return out.0;
    }
    out.add(format!(
        "- reference: `{}`{}",
        round.reference_model,
        ofv_suffix(round.reference_ofv)
    ));
    if let Some(a) = round.alpha {
        out.add(format!("- alpha: {a}"));
    }
    let c = &round.counts;
    out.add(format!(
        "- all models minimized: {}",
        yes_no(c.succeeded + c.withdrawn == c.candidates)
    ));
    out.add(format!(
        "- heuristic checks fired: {}",
        yes_no(round.candidates.iter().any(|c| !c.heuristics.is_empty()))
    ));
    if c.withdrawn > 0 {
        out.add(format!("- withdrawn candidates: {}", c.withdrawn));
    }
    if !round.removed_before.is_empty() {
        out.add(format!(
            "- removed before this round: {}",
            round.removed_before.join(", ")
        ));
    }
    if !round.decision.is_empty() {
        out.add(format!("- decision: {}", round.decision));
    }
    out.add(format!(
        "- retained before this round: {}",
        none_or_list(&round.retained_before)
    ));
    // An open round has changed nothing yet
    if round.complete {
        out.add(format!("- {}", round.change_label()));
    }
    if let Some(w) = round.timing.wall_seconds {
        out.add(format!("- wall time: {}", utils::format_duration(Some(w))));
    }
    out.blank();
    add_candidate_table(&mut out, round, fits);
    if c.unusable > 0 {
        out.blank();
        out.add(
            "_Unusable candidates are reported above; they are never scored as insignificant._",
        );
    }
    out.0
}

/// The reference fit's markdown: one model, so its facts and no table.
fn reference_markdown(out: &mut Lines, round: &RoundSummary) {
    for c in &round.candidates {
        out.add(format!("- model: `{}` ({})", c.model, c.status));
        if c.attempts.len() > 1 {
            let tries: Vec<String> = c
                .attempts
                .iter()
                .map(|a| format!("`{}` {}", a.model, a.outcome))
                .collect();
            out.add(format!("- attempts: {}", tries.join("; ")));
        }
        out.add(format!(
            "- heuristic checks fired: {}",
            none_or_list(&c.heuristics)
        ));
    }
    if !round.decision.is_empty() {
        out.add(format!("- decision: {}", round.decision));
    }
    if let Some(w) = round.timing.wall_seconds {
        out.add(format!("- wall time: {}", utils::format_duration(Some(w))));
    }
}

/// A markdown table of one round's candidates. The attempts, cond# and
/// heuristic checks columns appear only when some candidate in the round
/// was retried, has a condition number or tripped a check.
fn add_candidate_table(out: &mut Lines, round: &RoundSummary, fits: &Fits) {
    let shown = [
        ("candidate", true),
        ("model", true),
        (
            "attempts",
            round.candidates.iter().any(|c| c.attempts.len() > 1),
        ),
        ("status", true),
        ("OFV", true),
        ("\u{394}OFV", true),
        ("p", true),
        ("significant", true),
        ("selected", true),
        ("estimate (RSE%)", true),
        (
            "cond#",
            round
                .candidates
                .iter()
                .any(|c| condition_number(c, fits).is_some()),
        ),
        (
            "heuristic checks",
            round.candidates.iter().any(|c| !c.heuristics.is_empty()),
        ),
    ];
    let pick = |cells: Vec<String>| {
        let kept: Vec<String> = cells
            .into_iter()
            .zip(&shown)
            .filter_map(|(cell, (_, on))| on.then_some(cell))
            .collect();
        format!("| {} |", kept.join(" | "))
    };
    out.add(pick(shown.iter().map(|(h, _)| h.to_string()).collect()));
    out.add(format!(
        "|{}",
        "---|".repeat(shown.iter().filter(|(_, on)| *on).count())
    ));
    let num =
        |v: Option<f64>, digits: usize| v.map(|v| format!("{v:.digits$}")).unwrap_or_default();
    for c in &round.candidates {
        out.add(pick(vec![
            c.candidate.clone(),
            if c.model.is_empty() {
                String::new()
            } else {
                format!("`{}`", c.model)
            },
            c.attempts.len().to_string(),
            c.status.to_string(),
            num(c.ofv, DIGITS),
            c.delta_ofv.map(|v| format!("{v:+.3}")).unwrap_or_default(),
            c.p_value.map(|p| format!("{p:.4e}")).unwrap_or_default(),
            c.significant.map(yes_no).unwrap_or("").to_string(),
            if c.selected { "**yes**" } else { "" }.to_string(),
            round
                .effect_of(c, fits)
                .map(fmt_estimate)
                .unwrap_or_default(),
            num(condition_number(c, fits), 0),
            c.heuristics.join("; "),
        ]));
    }
}

extendr_module! {
    mod scm;
    fn scm_init_wrap;
    fn scm_plan_wrap;
    fn scm_config_wrap;
    fn scm_stop_wrap;
    fn scm_status_wrap;
    fn scm_summary_wrap;
}
