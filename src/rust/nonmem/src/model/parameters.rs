use extendr_api::Result;
use extendr_api::deserializer::from_robj;
use extendr_api::prelude::*;

use std::cmp::Ordering;

// pharos nonmem crate
use nmparser::ParameterOrdering;
use nonmem::Model;
use nonmem::output_files::{ext::get_parameter_estimates, shk::ShkReader};

use crate::{
    output_files::ext::create_ext_reader,
    output_files::{OMEGA, ParameterRow, ParameterRowBuilder, SIGMA, THETA, build_parameters_df},
    utils::{
        get_comment_type, parse_model_file, path_from_robj, resolve_ext_path, resolve_model_layout,
    },
};
use hyperion_core::{ResultExt, extendr_err};

/// Extract numeric indices from a parameter name for sorting.
///
/// Handles formats like:
/// - "THETA1", "THETA10" -> (1, 0, 0) or (10, 0, 0)
/// - "OMEGA(1,1)", "OMEGA(10,10)" -> (1, 1, 0) or (10, 10, 0)
/// - "SIGMA(1,1)", "SIGMA(2,2)" -> (1, 1, 0) or (2, 2, 0)
///
/// Returns a tuple of (first_num, second_num, param_type_order) for sorting.
fn extract_param_sort_key(name: &str) -> (u32, u32, u8) {
    // Determine parameter type order: THETA=0, OMEGA=1, SIGMA=2
    let type_order = if name.starts_with("THETA") {
        0
    } else if name.starts_with("OMEGA") {
        1
    } else if name.starts_with("SIGMA") {
        2
    } else {
        3
    };

    // Try to extract numbers from THETA format (e.g., "THETA1", "THETA10")
    if let Some(stripped) = name.strip_prefix("THETA")
        && let Ok(num) = stripped.parse::<u32>()
    {
        return (num, 0, type_order);
    }

    // Try to extract numbers from matrix format (e.g., "OMEGA(1,1)", "SIGMA(10,10)")
    if let Some(start) = name.find('(')
        && let Some(end) = name.find(')')
    {
        let inner = &name[start + 1..end];
        let parts: Vec<&str> = inner.split(',').collect();
        if parts.len() == 2
            && let (Ok(row), Ok(col)) = (parts[0].parse::<u32>(), parts[1].parse::<u32>())
        {
            return (row, col, type_order);
        }
    }

    // Fallback: return high values to sort unknown formats to the end
    (u32::MAX, u32::MAX, type_order)
}

/// Compare two parameter names for numeric sorting.
pub(crate) fn compare_param_names(a: &str, b: &str) -> Ordering {
    let key_a = extract_param_sort_key(a);
    let key_b = extract_param_sort_key(b);

    // First sort by parameter type (THETA, OMEGA, SIGMA)
    match key_a.2.cmp(&key_b.2) {
        Ordering::Equal => {}
        other => return other,
    }

    // Then sort by first number (row for matrices, index for THETA)
    match key_a.0.cmp(&key_b.0) {
        Ordering::Equal => {}
        other => return other,
    }

    // Then sort by second number (column for matrices)
    key_a.1.cmp(&key_b.1)
}

/// Gets parameter estimates from model run
///
/// @param path path to model file, model output directory, ext file or metadata json file,
/// or a hyperion_nonmem_model object
/// @param hide_off_diagonal_params boolean, if TRUE will not display the unfixed off-diagonal
/// estimated parameters
/// @param only_method character, filter for getting estimates from specified method only.
/// Available methods are Fo, Foce, Saems, Bayes, Imp, ImpMap, Its, Nuts
/// @param only_last boolean, for grabbing only last estimation method parameters
/// @param show_table_idx boolean, if TRUE include table_idx column in output
/// @param show_method boolean, if TRUE include method column in output
///
/// @return data.frame of parameter estimates
/// @export
///
/// @examples \dontrun{
/// get_parameters("model/nonmem/run001/run001.ext")
/// model <- read_model("model/nonmem/run001.mod")
/// get_parameters(model)
/// }
#[extendr]
pub fn get_parameters(
    path: Robj,
    #[extendr(default = "FALSE")] hide_off_diagonal_params: bool,
    #[extendr(default = "NULL")] only_method: Option<&str>,
    #[extendr(default = "TRUE")] only_last: Option<bool>,
    #[extendr(default = "FALSE")] show_table_idx: bool,
    #[extendr(default = "FALSE")] show_method: bool,
) -> Result<Robj> {
    let ext_reader = create_ext_reader(None, None, only_method, only_last)?;

    let search_path = path_from_robj(&path, false)?;
    let (layout, run_dir) = resolve_model_layout(&search_path)?;
    let model = parse_model_file(layout.model_path())?;

    let shk_path = layout.output_file(&run_dir, "shk");
    let shk_data = if shk_path.exists() {
        ShkReader.parse_file(shk_path).unwrap_or_default()
    } else {
        Vec::new()
    };

    let ext_path = resolve_ext_path(&model, &run_dir, layout.stem());
    if !ext_path.exists() {
        return Err(extendr_err!(
            "Output file not found: {}",
            ext_path.display()
        ));
    }

    let comment_type = get_comment_type();
    let parameter_names = model
        .get_parameter_names(comment_type)
        .map_to_extendr_err("Failed to get model parameter names")?;

    let tables = get_parameter_estimates(
        ext_path,
        &ext_reader,
        Some(shk_data),
        hide_off_diagonal_params,
        Some(&parameter_names),
    )
    .map_to_extendr_err("")?;

    // Build rows using the builder pattern
    let rows: Vec<ParameterRow> = tables
        .iter()
        .enumerate()
        .flat_map(|(i, tp)| {
            let table_idx = (i as i32) + 1;
            let method = tp
                .method
                .as_ref()
                .map(|m| m.to_string())
                .unwrap_or_default();

            // Collect parameters from theta, omega, and sigma
            let mut all_params = Vec::new();

            // Add theta parameters
            all_params.extend(tp.theta.iter().map(|p| {
                ParameterRowBuilder::new(THETA, p.name.clone(), p.estimate)
                    .with_stderr_rse(p.stderr, p.rse, p.fixed)
                    .with_table_idx(table_idx)
                    .with_method(method.clone())
                    .build()
            }));

            // Add omega parameters
            all_params.extend(tp.random_effects.iter().filter(|r| r.is_omega()).map(|p| {
                ParameterRowBuilder::new(OMEGA, p.name.clone(), p.estimate)
                    .with_stderr_rse(p.stderr, p.rse, p.fixed)
                    .with_sd(p.sd)
                    .with_corr(p.corr)
                    .with_shrinkage(p.shrinkage, p.fixed)
                    .with_random_effect(p.random_effect.clone())
                    .with_diagonal(p.diagonal)
                    .with_table_idx(table_idx)
                    .with_method(method.clone())
                    .build()
            }));
            // Add sigma parameters
            all_params.extend(tp.random_effects.iter().filter(|r| r.is_sigma()).map(|p| {
                ParameterRowBuilder::new(SIGMA, p.name.clone(), p.estimate)
                    .with_stderr_rse(p.stderr, p.rse, p.fixed)
                    .with_sd(p.sd)
                    .with_corr(p.corr)
                    .with_shrinkage(p.shrinkage, p.fixed)
                    .with_random_effect(p.random_effect.clone())
                    .with_diagonal(p.diagonal)
                    .with_table_idx(table_idx)
                    .with_method(method.clone())
                    .build()
            }));

            all_params.into_iter()
        })
        .collect();

    build_parameters_df(rows, show_table_idx, show_method)
}

/// Gets parameter names from model using typed comment parsing (internal)
///
/// This function extracts parameter names using the pharos typed comment parser.
/// For general use, prefer the R-side get_parameter_names() which handles both
/// typed and raw comment formats.
///
/// @param model hyperion_nonmem_model object from read_model()
///
/// @return Named list with NONMEM names as names and user-friendly names as character values
/// @keywords internal
#[extendr]
pub fn get_model_parameter_names(model: Robj) -> Result<Robj> {
    let model: Model = from_robj(&model)?;

    let comment_type = get_comment_type();
    let parameter_names = model
        .get_parameter_names(comment_type)
        .map_to_extendr_err("Failed to get model parameter names")?;

    // Convert BTreeMap to named character vector, sorting keys numerically
    // BTreeMap sorts keys alphabetically, but we need numeric order
    // (e.g., OMEGA(1,1), OMEGA(2,2), ..., OMEGA(10,10) instead of
    //  OMEGA(1,1), OMEGA(10,10), OMEGA(2,2), ...)
    let mut keys: Vec<String> = parameter_names.keys().cloned().collect();
    keys.sort_by(|a, b| compare_param_names(a, b));

    // Collect values in the same sorted order
    let values: Vec<String> = keys
        .iter()
        .map(|k| {
            parameter_names
                .get(k)
                .and_then(|v| v.clone())
                .unwrap_or_default()
        })
        .collect();

    // Create named character vector
    let result = List::from_names_and_values(keys, values).into_robj();

    Ok(result)
}

/// Gets names of parameters declared FIX in the model
///
/// Fixed status is read from the control stream, so it is available before a run
/// completes. Omega and sigma are fixed per record, so a fixed BLOCK contributes
/// its off-diagonal names as well as its diagonals.
///
/// @param model hyperion_nonmem_model object from read_model()
/// @param kind character, restrict to "THETA", "OMEGA" or "SIGMA". NULL returns
/// every kind, ordered theta, omega, sigma.
///
/// @return character vector of NONMEM parameter names
/// @export
///
/// @examples \dontrun{
/// model <- read_model("model/nonmem/run001.mod")
/// get_fixed_parameters(model, kind = "OMEGA")
/// }
#[extendr]
pub fn get_fixed_parameters(
    model: Robj,
    #[extendr(default = "NULL")] kind: Option<&str>,
) -> Result<Robj> {
    let model: Model = from_robj(&model)?;

    let kind = match kind {
        Some(k) => match k.to_uppercase().as_str() {
            THETA => Some(THETA),
            OMEGA => Some(OMEGA),
            SIGMA => Some(SIGMA),
            _ => {
                return Err(extendr_err!(
                    "kind must be one of \"THETA\", \"OMEGA\", \"SIGMA\", got: {k}"
                ));
            }
        },
        None => None,
    };
    let wanted = |k: &str| kind.is_none_or(|selected| selected == k);

    let mut names: Vec<String> = Vec::new();

    if wanted(THETA) {
        names.extend(
            model
                .thetas
                .iter()
                .enumerate()
                .filter(|(_, theta)| theta.fixed)
                .map(|(i, _)| format!("THETA{}", i + 1)),
        );
    }

    if wanted(OMEGA) {
        let entries = model
            .get_omega_parameters(ParameterOrdering::RowMajor)
            .map_to_extendr_err("Failed to get omega parameters")?;
        names.extend(
            entries
                .into_iter()
                .filter(|entry| entry.block_fixed)
                .map(|entry| entry.param_name),
        );
    }

    if wanted(SIGMA) {
        let entries = model
            .get_sigma_parameters(ParameterOrdering::RowMajor)
            .map_to_extendr_err("Failed to get sigma parameters")?;
        names.extend(
            entries
                .into_iter()
                .filter(|entry| entry.block_fixed)
                .map(|entry| entry.param_name),
        );
    }

    names.sort_by(|a, b| compare_param_names(a, b));

    Ok(names.into_robj())
}

extendr_module! {
    mod parameters;

    fn get_parameters;
    fn get_model_parameter_names;
    fn get_fixed_parameters;
}
