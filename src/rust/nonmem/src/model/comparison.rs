use extendr_api::Result;
use extendr_api::prelude::*;

// Pharos nonmem crate
use nonmem::comparisons::{Lrt, ModelComparison};
use nonmem::metrics::InformationCriteria;
use nonmem::output_files::get_summary;

use crate::model::summary::parse_summary_directory;
use crate::utils::get_comment_type;
use hyperion_core::{OptionExt, ResultExt};

/// Convert an InformationCriteria into a named R list.
fn ic_to_list(ic: &InformationCriteria) -> Robj {
    list!(
        ofv = ic.ofv,
        n_parameters = ic.n_estimated_parameters as i32,
        n_observations = ic.n_observations as i32,
        aic = ic.aic,
        bic = ic.bic
    )
    .into_robj()
}

/// Information criteria for a model's final estimation method (internal)
///
/// @param model path to a model file, run output directory, or a
/// hyperion_nonmem_model object
///
/// @param penalty AIC penalty per parameter; `NULL` uses pharos' default of 2
///
/// @return a named list with `ofv`, `n_parameters`, `n_observations`,
/// `aic`, and `bic` for the final estimation method
/// @keywords internal
#[extendr]
pub fn get_information_criteria(model: Robj, penalty: Option<f64>) -> Result<Robj> {
    let directory = parse_summary_directory(model)?;
    let summary = get_summary(&directory, get_comment_type(), false)
        .map_to_extendr_err("Failed to get summary")?;
    let mut ic = summary
        .final_information_criteria()
        .ok_or_extendr_err("No information criteria available for the final estimation method")?;

    if let Some(p) = penalty {
        ic = ic.with_penalty(p);
    }

    Ok(ic_to_list(&ic))
}

/// Compare two NONMEM runs (internal)
///
/// @param first path/run directory/model object for the first run
/// @param second path/run directory/model object for the second run
///
/// @return a named list with the per-run information criteria (`first`,
/// `second`), the deltas (`first - second`), and the likelihood ratio test
/// fields (`lrt_status`, `lrt_df`, `lrt_p_value`)
/// @keywords internal
#[extendr]
pub fn compare_runs_impl(first: Robj, second: Robj) -> Result<Robj> {
    let first_dir = parse_summary_directory(first)?;
    let second_dir = parse_summary_directory(second)?;

    let comparison = ModelComparison::compare_runs(&first_dir, &second_dir)
        .map_to_extendr_err("Failed to compare runs")?;

    let (lrt_status, lrt_df, lrt_p_value) = match comparison.lrt {
        Lrt::Computed(lrt) => (
            "computed",
            (lrt.df as i32).into_robj(),
            lrt.p_value.into_robj(),
        ),
        Lrt::NotNested => ("not_nested", NA_INTEGER.into_robj(), NA_REAL.into_robj()),
        Lrt::NoAddedParameters => (
            "no_added_parameters",
            NA_INTEGER.into_robj(),
            NA_REAL.into_robj(),
        ),
    };

    let result = list!(
        first = ic_to_list(&comparison.first_ic),
        second = ic_to_list(&comparison.second_ic),
        delta_ofv = comparison.delta_ofv,
        delta_aic = comparison.delta_aic,
        delta_bic = comparison.delta_bic,
        lrt_status = lrt_status,
        lrt_df = lrt_df,
        lrt_p_value = lrt_p_value
    );

    Ok(result.into_robj())
}

extendr_module! {
    mod comparison;
    fn get_information_criteria;
    fn compare_runs_impl;
}
