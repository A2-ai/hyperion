//! Stepwise covariate modeling (SCM) wrappers.
//!
//! `scm_init_wrap` writes the starter config beside a model, `scm_plan_wrap`
//! builds the validated plan and writes `plan.json` through the same Rust
//! serializer the pharos CLI reads, and `scm_status_wrap` /
//! `scm_summary_wrap` read an SCM process wherever it stands. Running happens through the pharos CLI in the background (see
//! `scm_run()` on the R side), never in-process.

use std::path::Path;

use extendr_api::Result;
use extendr_api::prelude::*;
use extendr_api::serializer::to_robj;

use nonmem::scm::{
    self as pharos_scm, Compatibility, PlanChange, PlanContext, Retuning, SummaryOptions,
    state::ScmProcess,
    summary::round_summary_md,
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

/// Build and validate an SCM plan (runs nothing) and write its plan.json
///
/// Internal engine behind [scm_plan()]; use that instead.
///
/// @param config path to the SCM config file (TOML) written by
///   [scm_init()] into the SCM out_dir: model, direction, forward_alpha,
///   backward_alpha, max_retries, cov_step, final_cov_step, and the
///   `[covariates]` section (fixed, the per-type `continuous` /
///   `categorical` tables, effects). Relative paths resolve against the
///   config file
/// @param num_rounds pause after this many rounds per run (NULL = no cap)
/// @param overwrite replace existing SCM output from a different plan
///
/// @return a `hyperion_scm_plan` object; its `plan_path` attribute is the
///   `plan.json` just written, and its `context` attribute is where the
///   SCM process in the out_dir already stands plus what this plan changed about
///   the plan.json it replaced
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
        overwrite,
    };

    let built = pharos_scm::build_plan_from_config(
        Path::new(config),
        &overrides,
        env!("CARGO_PKG_VERSION"),
    )
    .map_to_extendr_err("Failed to build SCM plan")?;

    let written = built
        .plan
        .save()
        .map_to_extendr_err("Failed to write plan.json")?;

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
    // Read while the plan was built, i.e. before the save above replaced the
    // plan.json it compares against: how far the SCM process in the out_dir got,
    // and what this plan changed. Printing leans on it; a fresh out_dir has
    // nothing to say and renders exactly as it always did.
    robj.set_attrib("context", context_robj(&built.context)?)?;
    let robj = robj.set_class(["hyperion_scm_plan"])?.to_owned();
    Ok(robj)
}

/// What `scm status` shows: the summary header and one line per round.
const BRIEF: SummaryOptions = SummaryOptions {
    round: None,
    candidate: None,
    brief: true,
    long: false,
    timing: false,
    files: false,
};

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
/// the plan.json it replaced.
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

/// Read the status of an SCM process
///
/// Internal engine behind [scm_status()]; use that instead.
///
/// @param path the SCM out_dir
///
/// @return a `hyperion_scm_status` object
/// @keywords internal
#[extendr(r_name = "scm_status_impl")]
pub fn scm_status_wrap(path: &str) -> Result<Robj> {
    // Status is the summary read briefly — the same record, the same reader,
    // so status and summary can never describe different SCM processes.
    let status = pharos_scm::read_summary(Path::new(path))
        .map_to_extendr_err("Failed to read SCM status")?;
    let rendered = status
        .render_text(&BRIEF)
        .map_to_extendr_err("Failed to render SCM status")?;

    let mut robj = to_robj(&status).map_to_extendr_err("Failed to convert status to Robj")?;
    robj.set_attrib("rendered", rendered.into_robj())?;
    // The record's own `out_dir` is written relative to the pharos project
    // root, so the directory it was read from is the one to go back to.
    robj.set_attrib("out_dir", path.into_robj())?;
    let robj = robj.set_class(["hyperion_scm_status"])?.to_owned();
    Ok(robj)
}

/// The SCM summary: every round to date, or a selection, rendered
///
/// Internal engine behind [scm_summary()]; use that instead.
///
/// @param path the SCM out_dir
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
    };

    let summary =
        pharos_scm::read_summary(Path::new(path)).map_to_extendr_err("Failed to read SCM summary")?;
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

    // pharos renders each round's markdown as a `## <round>` section; the
    // selection decides which ones, and the fits the read already loaded for
    // all of them carry over.
    let fits = &summary.fits;
    let mut markdown = String::from("# SCM summary\n\n");
    for r in &selected.rounds {
        markdown.push_str(&round_summary_md(r, fits));
        markdown.push('\n');
    }

    let mut robj = to_robj(&selected).map_to_extendr_err("Failed to convert summary to Robj")?;
    robj.set_attrib("rendered", rendered.into_robj())?;
    robj.set_attrib("markdown", markdown.into_robj())?;
    let robj = robj.set_class(["hyperion_scm_summary"])?.to_owned();
    Ok(robj)
}

extendr_module! {
    mod scm;
    fn scm_init_wrap;
    fn scm_plan_wrap;
    fn scm_status_wrap;
    fn scm_summary_wrap;
}
