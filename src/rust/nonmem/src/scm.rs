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
    self as pharos_scm, Direction, ScmOptions, ScmPlan, decision_log_rows, log as scm_log,
    state::ScmState,
};

use hyperion_core::{ResultExt, extendr_err};

fn parse_directions(direction: &[String]) -> Result<Vec<Direction>> {
    direction
        .iter()
        .map(|d| d.parse::<Direction>().map_err(|e| extendr_err!("{e}")))
        .collect()
}

/// Build and validate an SCM plan (runs nothing) and write its plan.json
///
/// Internal engine behind [scm_plan()]; use that instead.
///
/// @param model path to the template control stream
/// @param covariates integer vector of 1-based THETA numbers
/// @param direction character vector: "forward", "backward", or both
/// @param out_dir output directory (NULL = scm/<model name> beside the model)
/// @param forward_alpha significance level for forward selection
/// @param backward_alpha significance level for backward elimination
/// @param num_rounds pause after this many rounds per run (NULL = no cap)
/// @param max_retries retries per failed fit
/// @param release_init initial estimate for a released covariate theta
/// @param cov_step whether generated models run the covariance step
/// @param overwrite replace existing SCM output from a different plan
///
/// @return a `hyperion_scm_plan` object; its `plan_path` attribute is the
///   `plan.json` just written
/// @keywords internal
#[extendr(r_name = "scm_plan_impl")]
#[allow(clippy::too_many_arguments)]
pub fn scm_plan_wrap(
    model: &str,
    covariates: Vec<i32>,
    direction: Vec<String>,
    #[extendr(default = "NULL")] out_dir: Option<&str>,
    #[extendr(default = "0.05")] forward_alpha: f64,
    #[extendr(default = "0.001")] backward_alpha: f64,
    #[extendr(default = "NULL")] num_rounds: Option<i32>,
    #[extendr(default = "3")] max_retries: i32,
    #[extendr(default = "0.1")] release_init: f64,
    #[extendr(default = "TRUE")] cov_step: bool,
    #[extendr(default = "FALSE")] overwrite: bool,
) -> Result<Robj> {
    let covariates: Vec<usize> = covariates
        .into_iter()
        .map(|c| {
            if c < 1 {
                Err(extendr_err!(
                    "covariates must be positive THETA numbers, got {c}"
                ))
            } else {
                Ok(c as usize)
            }
        })
        .collect::<Result<_>>()?;

    if max_retries < 0 {
        return Err(extendr_err!("max_retries must be non-negative"));
    }

    let options = ScmOptions {
        direction: parse_directions(&direction)?,
        forward_alpha,
        backward_alpha,
        num_rounds: num_rounds.map(|n| n as usize),
        max_retries: max_retries as usize,
        release_init,
        cov_step,
        overwrite,
    };

    let built = pharos_scm::build_plan(
        Path::new(model),
        &covariates,
        out_dir.map(Path::new),
        options,
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
    fn scm_decision_log_wrap;
}
