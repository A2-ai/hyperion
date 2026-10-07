//! Stepwise covariate modeling (SCM) wrappers.
//!
//! `scm_init_wrap` writes the starter config beside a model, `scm_plan_wrap`
//! builds the validated plan and writes `pharos_scm_plan.json` through the same Rust
//! code the pharos CLI runs (`pharos nonmem scm plan`), and `scm_status_wrap`
//! / `scm_summary_wrap` read an SCM process wherever it stands, exactly as
//! `scm status` and `scm summary` do. Running happens through the pharos CLI
//! in the background (see `scm_run()` on the R side), never in-process.

use std::path::{Path, PathBuf};

use extendr_api::Result;
use extendr_api::prelude::*;
use extendr_api::serializer::to_robj;

use nonmem::scm::{
    self as pharos_scm, Compatibility, PlanChange, PlanContext, Retuning, ScmStatus,
    SummaryOptions,
    state::ScmProcess,
    summary::{CandidateSummary, Fits, RoundSummary},
};
use scheduler::{DriverRecord, Liveness, fit_place_lookup, scm_driver::DRIVER_RECORD_FILENAME};

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

/// Build and validate an SCM plan (runs nothing) and write its pharos_scm_plan.json
///
/// Internal engine behind [scm_plan()]; use that instead.
///
/// @param config path to the SCM config file (TOML) written by
///   [scm_init()] into the SCM out_dir: model, direction, forward_alpha,
///   backward_alpha, max_retries, cov_step, forward_final_cov_step,
///   final_cov_step, and the `[covariates]` section (fixed, the
///   per-type `continuous` / `categorical` tables, effects). Relative paths
///   resolve against the config file
/// @param num_rounds pause after this many rounds per run (NULL = no cap)
/// @param overwrite discard the SCM process already in the out_dir (its
///   fits, state and summaries) once the plan has validated, so this plan
///   starts fresh. Refused while that process's driver may still be running
///
/// @return a `hyperion_scm_plan` object; its `plan_path` attribute is the
///   `pharos_scm_plan.json` just written, its `context` attribute is where the SCM
///   process in the out_dir already stands plus what this plan changed about
///   the pharos_scm_plan.json it replaced, and its `discarded` attribute says what
///   `overwrite` threw away (`NULL` when nothing was). A plan the SCM
///   process already in the out_dir cannot resume under is an error, and is
///   not written
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
        && n < 1
    {
        return Err(extendr_err!("num_rounds must be at least 1, got {n}"));
    }

    let overrides = pharos_scm::ScmPlanOverrides {
        num_rounds: num_rounds.map(|n| n as usize),
    };

    // Built, and so validated, before anything in the out_dir is touched —
    // the same order as `pharos nonmem scm plan`.
    let mut built = pharos_scm::build_plan_from_config(
        Path::new(config),
        &overrides,
        env!("CARGO_PKG_VERSION"),
    )
    .map_to_extendr_err("Failed to build SCM plan")?;

    // `overwrite` acts on the out_dir once the plan is built: it discards the
    // SCM process there, unless its driver may still be running.
    let mut discarded: Option<String> = None;
    if overwrite {
        let out_dir = built.plan.out_dir_path();
        refuse_live_driver(&out_dir)?;
        discarded = built.context.progress.as_ref().map(|p| {
            let n = p.state.completed_rounds();
            let s = if n == 1 { "" } else { "s" };
            format!("{}, {n} round{s} complete", p.state.status)
        });
        built
            .clear_previous_output()
            .map_to_extendr_err("Failed to discard the SCM process in the out_dir")?;
    }

    // A plan the SCM process already in the out_dir cannot resume under is
    // not written: `scm status` reads that process under whatever pharos_scm_plan.json
    // says, so the file stays the one it ran under. pharos says the same in
    // CLI terms; this spells out what to do in R.
    if let Some(verdict) = built
        .context
        .compatibility
        .as_ref()
        .filter(|c| c.is_incompatible())
    {
        let reasons: String = verdict
            .reasons
            .iter()
            .map(|r| format!("\n  - {r}"))
            .collect();
        return Err(extendr_err!(
            "plan not written: the SCM process in {} belongs to the previous plan and cannot \
             resume under this one:{reasons}\nre-plan with `overwrite = TRUE` to discard that \
             process and start fresh",
            built.plan.out_dir
        ));
    }

    let written = built
        .write()
        .map_to_extendr_err("Failed to write pharos_scm_plan.json")?;

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
    robj.set_attrib("plan_path", written.to_string_lossy().into_robj())?;
    // Read while the plan was built (and again after `overwrite` cleared the
    // out_dir), i.e. before the write above replaced the pharos_scm_plan.json it
    // compares against: how far the SCM process in the out_dir got, and what
    // this plan changed. Printing leans on it; a fresh out_dir has nothing
    // to say and renders exactly as it always did.
    robj.set_attrib("context", context_robj(&built.context)?)?;
    robj.set_attrib("discarded", or_null(discarded))?;
    let robj = robj.set_class(["hyperion_scm_plan"])?.to_owned();
    Ok(robj)
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
/// process in its out_dir already stands, and what this plan changed about
/// the pharos_scm_plan.json it replaced.
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
        had_previous_plan = ctx.had_previous_plan,
        changes = List::from_values(changes),
        progress = ctx.progress.as_ref().map_or_else(|| r!(NULL), progress_robj),
        removals = removals.to_vec(),
        retunes = List::from_values(retunes),
        state_is_stale = verdict.is_incompatible(),
        stale_reasons = reasons.to_vec()
    )
    .into_robj())
}

// Driver ---------------------------------------------------------------------
//
// pharos keeps these next to its CLI rather than in the `scm` module, so
// hyperion carries its own copies: the same checks, worded for R.

/// The SCM out_dir a status / summary argument names: the directory itself,
/// or the directory holding a pharos_scm_plan.json.
fn scm_out_dir(path: &Path) -> PathBuf {
    if path.is_file() {
        path.parent()
            .map(Path::to_path_buf)
            .unwrap_or_else(|| PathBuf::from("."))
    } else {
        path.to_path_buf()
    }
}

/// Refuse to discard an SCM process whose driver may still be running: its
/// fits would land in an out_dir that no longer records them.
fn refuse_live_driver(out_dir: &Path) -> Result<()> {
    let Some(record) = DriverRecord::read(out_dir) else {
        return Ok(());
    };
    let dir = out_dir.display();
    match record.liveness() {
        Liveness::Gone => Ok(()),
        Liveness::Alive => {
            let stop = match record.job_id {
                Some(id) => format!("`scancel {id}`"),
                None => "Ctrl-C in its terminal, or kill the process".to_string(),
            };
            Err(extendr_err!(
                "the SCM process in {dir} is still being driven by {}; stop it ({stop}) \
                 before discarding it with overwrite = TRUE",
                record.describe()
            ))
        }
        Liveness::Unknown(why) => Err(extendr_err!(
            "cannot tell whether the driver of the SCM process in {dir} ({}) is still \
             running: {why}; once it has stopped, delete {dir}/{DRIVER_RECORD_FILENAME} and \
             re-plan with overwrite = TRUE",
            record.describe()
        )),
    }
}

/// What `scm status` adds about the driver: whether it is still there, and
/// when it is not while the process says it is running, where to look.
fn driver_status(record: &DriverRecord, scm_status: &str) -> Vec<String> {
    let mut lines = Vec::new();
    let log = record
        .log
        .as_ref()
        .map(|l| format!("; see {}", l.display()))
        .unwrap_or_default();
    let liveness = record.liveness();
    match &liveness {
        Liveness::Alive => lines.push(format!("driver     : running ({})", record.describe())),
        Liveness::Gone if scm_status == "running" || scm_status == "planned" => {
            lines.push(format!(
                "driver     : NOT RUNNING ({}, {}) — the SCM process stopped before finishing{log}",
                record.describe(),
                record.mode
            ));
            lines.push(
                "             resubmit the same plan to resume; fits still in the queue are waited for, not rerun"
                    .to_string(),
            );
        }
        Liveness::Gone => lines.push(format!("driver     : exited ({}){log}", record.describe())),
        Liveness::Unknown(why) => lines.push(format!(
            "driver     : {} — cannot check it from here ({why})",
            record.describe()
        )),
    }
    if let Some(allocation) = record.allocation
        && liveness != Liveness::Alive
        && scheduler::slurm::job_is_queued(allocation)
    {
        lines.push(format!(
            "allocation : slurm allocation {allocation} is still held; release it with `scancel {allocation}`"
        ));
    }
    lines
}

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

/// Read the status of an SCM process
///
/// Internal engine behind [scm_status()]; use that instead.
///
/// @param path the SCM out_dir, or its pharos_scm_plan.json
///
/// @return a `hyperion_scm_status` object: the summary record, with the
///   text `pharos nonmem scm status` prints as its `rendered` attribute, the
///   out_dir it was read from as `out_dir`, whether the SCM process has
///   started as `started`, and the driver record (`scm_driver.json`, with
///   its liveness) as `driver` — `NULL` before a driver was ever started
/// @keywords internal
#[extendr(r_name = "scm_status_impl")]
pub fn scm_status_wrap(path: &str) -> Result<Robj> {
    let out_dir = scm_out_dir(Path::new(path));
    // The process as its driver's terminal shows it, read off disk now: the
    // state reconciled with what the fits have left behind, then summarized
    // — the same reader `scm_summary()` uses, so the two never describe
    // different SCM processes.
    let status = ScmStatus::read(&out_dir).map_to_extendr_err("Failed to read SCM status")?;
    let record = DriverRecord::read(&out_dir);
    let driver_lines = record
        .as_ref()
        .map(|r| driver_status(r, &status.summary.status))
        .unwrap_or_default();
    let place_of = fit_place_lookup();
    let rendered = status.render(&driver_lines, &place_of);

    let mut robj =
        to_robj(&status.summary).map_to_extendr_err("Failed to convert status to Robj")?;
    robj.set_attrib("rendered", rendered.into_robj())?;
    // The record's own `out_dir` is written relative to the pharos project
    // root, so the directory it was read from is the one to go back to.
    robj.set_attrib("out_dir", out_dir.to_string_lossy().into_robj())?;
    robj.set_attrib("started", status.process.started.into_robj())?;
    robj.set_attrib("driver", or_null(record.as_ref().map(driver_robj)))?;
    let robj = robj.set_class(["hyperion_scm_status"])?.to_owned();
    Ok(robj)
}

/// The SCM summary: every round to date, or a selection, rendered
///
/// Internal engine behind [scm_summary()]; use that instead.
///
/// @param path the SCM out_dir, or its pharos_scm_plan.json
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
        brief: false,
        long,
        timing,
        files,
        extra: Vec::new(),
    };

    let summary = pharos_scm::read_summary(&scm_out_dir(Path::new(path)))
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
            r.candidates.retain(|c| c.candidate.eq_ignore_ascii_case(name));
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
    fn scm_status_wrap;
    fn scm_summary_wrap;
}
