//! Stepwise covariate modeling (SCM) wrappers.
//!
//! `scm_plan_wrap` builds the validated plan and writes `plan.json` through
//! the same Rust serializer the pharos CLI reads, and `scm_status_wrap` /
//! `scm_decision_log_wrap` read a search wherever it stands. Running happens
//! through the pharos CLI in the background (see `scm_run()` on the R side),
//! never in-process.

use std::path::Path;

use extendr_api::Result;
use extendr_api::prelude::*;
use extendr_api::serializer::to_robj;

use nonmem::scm::{
    self as pharos_scm, ScmPlan, decision_log_rows, log as scm_log, state::ScmState,
};

use hyperion_core::{ResultExt, extendr_err};

/// Build and validate an SCM plan (runs nothing) and write its plan.json
///
/// Internal engine behind [scm_plan()]; use that instead.
///
/// @param config path to the SCM config file (TOML): model, out_dir,
///   covariates, direction, forward_alpha, backward_alpha, max_retries,
///   cov_step, release_init. Relative paths resolve against the config file
/// @param num_rounds pause after this many rounds per run (NULL = no cap)
/// @param max_retries override the config's retries per failed fit
/// @param cov_step override whether generated models run the covariance step
/// @param release_init override the initial estimate a newly released
///   covariate theta starts at
/// @param overwrite replace existing SCM output from a different plan
///
/// @return a `hyperion_scm_plan` object; its `plan_path` attribute is the
///   `plan.json` just written
/// @keywords internal
#[extendr(r_name = "scm_plan_impl")]
pub fn scm_plan_wrap(
    config: &str,
    #[extendr(default = "NULL")] num_rounds: Option<i32>,
    #[extendr(default = "NULL")] max_retries: Option<i32>,
    #[extendr(default = "NULL")] cov_step: Option<bool>,
    #[extendr(default = "NULL")] release_init: Option<f64>,
    #[extendr(default = "FALSE")] overwrite: bool,
) -> Result<Robj> {
    if let Some(m) = max_retries
        && m < 0
    {
        return Err(extendr_err!("max_retries must be non-negative, got {m}"));
    }
    // Guard before the i32 -> usize cast: a negative would wrap to a huge
    // cap, silently meaning "never pause".
    if let Some(n) = num_rounds
        && n < 1
    {
        return Err(extendr_err!("num_rounds must be at least 1, got {n}"));
    }

    let overrides = pharos_scm::ScmPlanOverrides {
        num_rounds: num_rounds.map(|n| n as usize),
        max_retries: max_retries.map(|m| m as usize),
        cov_step,
        release_init,
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
    robj.set_attrib("plan_path", written.to_string_lossy().into_robj())?;
    let robj = robj.set_class(["hyperion_scm_plan"])?.to_owned();
    Ok(robj)
}

/// Read the status of an SCM search
///
/// Internal engine behind [scm_status()]; use that instead.
///
/// @param path the SCM out_dir
///
/// @return a `hyperion_scm_status` object
/// @keywords internal
#[extendr(r_name = "scm_status_impl")]
pub fn scm_status_wrap(path: &str) -> Result<Robj> {
    let status = pharos_scm::read_status(Path::new(path))
        .map_to_extendr_err("Failed to read SCM status")?;

    let mut robj = to_robj(&status).map_to_extendr_err("Failed to convert status to Robj")?;
    robj.set_attrib("rendered", status.render_text().into_robj())?;
    let robj = robj.set_class(["hyperion_scm_status"])?.to_owned();
    Ok(robj)
}

#[derive(Debug, IntoDataFrameRow)]
struct DecisionLogRow {
    round: String,
    direction: String,
    candidate: String,
    model: String,
    attempts: i32,
    status: String,
    reference_ofv: Rfloat,
    delta_ofv: Rfloat,
    df: i32,
    p_value: Rfloat,
    significant: Rbool,
    selected: bool,
    heuristics: String,
    decision: String,
}

impl From<pharos_scm::DecisionLogRow> for DecisionLogRow {
    fn from(r: pharos_scm::DecisionLogRow) -> Self {
        Self {
            round: r.round,
            direction: r.direction,
            candidate: r.candidate,
            model: r.model,
            attempts: r.attempts as i32,
            status: r.status,
            reference_ofv: r.reference_ofv.map_or(Rfloat::na(), Rfloat::from),
            delta_ofv: r.delta_ofv.map_or(Rfloat::na(), Rfloat::from),
            df: r.df as i32,
            p_value: r.p_value.map_or(Rfloat::na(), Rfloat::from),
            significant: r.significant.map_or(Rbool::na(), Rbool::from),
            selected: r.selected,
            heuristics: r.heuristics,
            decision: r.decision,
        }
    }
}

/// Detailed view of one round of an SCM search
///
/// Internal engine behind [scm_summary()]; use that instead.
///
/// @param path the SCM out_dir
/// @param round which round: the Nth search round ("2" / "round 2"), a round
///   name (forward_round1, backward_round1), or "reference"
///
/// @return a `hyperion_scm_round` object
/// @keywords internal
#[extendr(r_name = "scm_summary_impl")]
pub fn scm_summary_wrap(path: &str, round: &str) -> Result<Robj> {
    let detail = pharos_scm::read_round_detail(Path::new(path), round)
        .map_to_extendr_err("Failed to read SCM round")?;

    let mut robj =
        to_robj(&detail).map_to_extendr_err("Failed to convert round detail to Robj")?;
    robj.set_attrib("rendered", detail.render_text().into_robj())?;
    let robj = robj.set_class(["hyperion_scm_round"])?.to_owned();
    Ok(robj)
}

/// Build the SCM decision log
///
/// Internal engine behind [summary.hyperion_scm_status()]; use that instead.
///
/// @param path the SCM out_dir
/// @param write whether to (re)write scm_decision_log.csv / .md into out_dir
///
/// @return the decision log as a data.frame
/// @keywords internal
#[extendr(r_name = "scm_decision_log_impl")]
pub fn scm_decision_log_wrap(path: &str, #[extendr(default = "TRUE")] write: bool) -> Result<Robj> {
    let out_dir = Path::new(path);
    let plan = ScmPlan::load(out_dir.join(pharos_scm::PLAN_FILENAME))
        .map_to_extendr_err("Failed to load plan.json")?;
    let state = ScmState::load(out_dir)
        .map_to_extendr_err("Failed to read scm_state.json")?
        .ok_or_else(|| extendr_err!("No scm_state.json yet — the search has not started"))?;

    let mut written: Vec<String> = vec![];
    if write {
        let (csv, md) = scm_log::write_decision_log(out_dir, &plan, &state)
            .map_to_extendr_err("Failed to write decision log")?;
        written.push(csv.to_string_lossy().to_string());
        written.push(md.to_string_lossy().to_string());
    }

    let rows: Vec<DecisionLogRow> = decision_log_rows(&state)
        .into_iter()
        .map(DecisionLogRow::from)
        .collect();

    let mut df = if rows.is_empty() {
        // an empty but correctly-typed data.frame
        R!("data.frame(round = character(), direction = character(), candidate = character(), model = character(), attempts = integer(), status = character(), reference_ofv = numeric(), delta_ofv = numeric(), df = integer(), p_value = numeric(), significant = logical(), selected = logical(), heuristics = character(), decision = character())")?
    } else {
        rows.into_dataframe()
            .map_to_extendr_err("Failed to build decision log df")?
            .into_robj()
    };

    df.set_attrib("files_written", written.iter().collect_robj())?;
    df.set_attrib("retained", state.retained.iter().collect_robj())?;
    if let Some(f) = &state.final_model {
        df.set_attrib("final_model", f.into_robj())?;
    }
    Ok(df)
}

extendr_module! {
    mod scm;
    fn scm_plan_wrap;
    fn scm_status_wrap;
    fn scm_summary_wrap;
    fn scm_decision_log_wrap;
}
