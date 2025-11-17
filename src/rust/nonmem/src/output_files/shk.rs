use extendr_api::prelude::*;

//pharos nonmem crate
use nonmem::estimation::EstimationMethod;
use nonmem::output_files::shk::{RawShkTable, ShkReader};

use crate::utils::find_output_file;

#[derive(Debug, IntoDataFrameRow)]
pub struct EtaShkRow {
    pub method: String,
    pub subpop: i32,
    pub eta_number: i32,
    pub etabar: Rfloat,
    pub etabar_se: Rfloat,
    pub etabar_pval: Rfloat,
    pub shrinkage_sd: Rfloat,
    pub shrinkage_vr: Rfloat,
    pub rel_info: Rfloat,
    pub ebv_shrinkage_sd: Rfloat,
    pub ebv_shrinkage_vr: Rfloat,
    pub n_individuals: Rint,
}

#[derive(Debug, IntoDataFrameRow)]
pub struct EpsShkRow {
    pub method: String,
    pub subpop: i32,
    pub eps_number: i32,
    pub shrinkage_sd: Rfloat,
    pub shrinkage_vr: Rfloat,
    pub n_individuals: Rint,
}

/// Helper function to build ExtReader
pub fn create_shk_reader(only_method: Option<&str>, only_last: Option<bool>) -> Result<ShkReader> {
    let mut reader = ShkReader::default();
    // Add estimation method filter
    if let Some(method) = only_method {
        // Handle common aliases
        let normalized_method = match method.to_lowercase().as_str() {
            "importance" => "imp",
            "focei" => "foce",
            _ => method,
        };
        let m: EstimationMethod = normalized_method
            .parse()
            .map_err(|e: String| Error::Other(e))?;
        reader = reader.only_method(m);
        return Ok(reader);
    }

    if let Some(last) = only_last
        && last
    {
        reader = reader.only_last();
        return Ok(reader);
    }

    // If only_method and only_last not provided keep all
    reader = reader.keep_all_tables();
    Ok(reader)
}

/// Helper function to convert EstimationTable vector to R dataframe
fn raw_shk_tables_to_dataframe(tables: Vec<RawShkTable>) -> Result<Robj> {
    if tables.is_empty() {
        return Err(Error::Other("No tables found in shk file".to_string()));
    }

    // Get parameter names from the first table
    let param_names = tables[0].parameters.clone();

    let flat_data: Vec<(String, i32, i32, Vec<f64>)> = tables
        .into_iter()
        .flat_map(|table| {
            let method_name = table.method.unwrap().to_string();
            table.rows.into_iter().map(move |row| {
                (
                    method_name.clone(),
                    row.type_num as i32,
                    row.subpop as i32,
                    row.values,
                )
            })
        })
        .collect();

    // Extract columns
    let type_nums: Vec<i32> = flat_data.iter().map(|(_, iter, _, _)| *iter).collect();
    let subpops: Vec<i32> = flat_data.iter().map(|(_, _, iter, _)| *iter).collect();
    let methods: Vec<String> = flat_data
        .iter()
        .map(|(method, _, _, _)| method.clone())
        .collect();

    // Build column pairs
    let mut pairs = vec![
        ("type", type_nums.into_robj()),
        ("subpop", subpops.into_robj()),
    ];

    // Add parameter columns dynamically
    for (param_idx, param_name) in param_names.iter().enumerate() {
        let values: Vec<f64> = flat_data
            .iter()
            .map(|(_, _, _, row_vals)| row_vals.get(param_idx).copied().unwrap_or(f64::NAN))
            .collect();
        pairs.push((param_name.as_str(), values.into_robj()));
    }
    pairs.push(("method", methods.into_robj()));

    let list = List::from_pairs(pairs);

    // Post-process: fix parameter values for fixed parameters and NaNs
    //let fixed_list = fix_parameter_values(list, &)?;

    let df = data_frame!(list);

    Ok(df)
}

/// Reads shk file
///
/// @param path path to model file, model output directory, shk file or metadata json file.
/// @param only_method character, filter for getting estimates from specified method only
/// @param only_last boolean, for grabbing only last estimation method parameters
///
/// @return data.frame of shk file
/// @export
///
/// @examples \dontrun{
/// read_shk_file("model/nonmem/run001/run001.shk")
/// }
#[extendr]
pub fn read_shk_file(
    path: &str,
    #[default = "NULL"] only_method: Option<&str>,
    #[default = "TRUE"] only_last: Option<bool>,
) -> Result<Robj> {
    let shk_reader = create_shk_reader(only_method, only_last)?;
    let path = find_output_file(path, "shk")?;

    let tables = shk_reader
        .parse_file(path)
        .map_err(|e| Error::Other(format!("Failed to read shk file: {e}")))?;

    raw_shk_tables_to_dataframe(tables)
}

/// Gets ETA shrinkage metrics from .shk file
///
/// @param path path to model file, model output directory, shk file or metadata json file.
///
/// @return data.frame of ETA shrinkage metrics
/// @export
///
/// @examples \dontrun{
/// get_eta_shrinkage("model/nonmem/run001/run001.shk")
/// }
#[extendr]
pub fn get_eta_shrinkage(path: &str) -> Result<Robj> {
    let shk_reader = ShkReader::default();
    let path = find_output_file(path, "shk")?;

    let tables = shk_reader
        .parse_file_semantic(path)
        .map_err(|e| Error::Other(e.to_string()))?;

    if tables.is_empty() {
        return Err(Error::Other("No tables found in shk file".to_string()));
    }

    let mut eta_rows = Vec::new();

    // Process each table group and table
    for table_group in &tables {
        for table in table_group {
            let method_name = table
                .method
                .as_ref()
                .map(|m| m.to_string())
                .unwrap_or_else(|| "Unknown".to_string());

            let subpop = table.subpop as i32;
            let n_individuals = table
                .n_individuals
                .map_or(Rint::na(), |n| Rint::from(n as i32));

            // Determine maximum ETA parameters from any ETA-related field
            let max_eta_params = [
                table.etabar.as_ref().map(|v| v.len()),
                table.etabar_se.as_ref().map(|v| v.len()),
                table.etabar_pval.as_ref().map(|v| v.len()),
                table.eta_shrinkage_sd.as_ref().map(|v| v.len()),
                table.eta_shrinkage_vr.as_ref().map(|v| v.len()),
                table.ebv_shrinkage_sd.as_ref().map(|v| v.len()),
                table.ebv_shrinkage_vr.as_ref().map(|v| v.len()),
                table.relative_information.as_ref().map(|v| v.len()),
            ]
            .iter()
            .filter_map(|&x| x)
            .max()
            .unwrap_or(0);

            // Create ETA rows
            for eta_idx in 0..max_eta_params {
                let eta_row = EtaShkRow {
                    method: method_name.clone(),
                    subpop,
                    eta_number: (eta_idx + 1) as i32,
                    etabar: table
                        .etabar
                        .as_ref()
                        .and_then(|v| v.get(eta_idx))
                        .map_or(Rfloat::na(), |&x| Rfloat::from(x)),
                    etabar_se: table
                        .etabar_se
                        .as_ref()
                        .and_then(|v| v.get(eta_idx))
                        .map_or(Rfloat::na(), |&x| Rfloat::from(x)),
                    etabar_pval: table
                        .etabar_pval
                        .as_ref()
                        .and_then(|v| v.get(eta_idx))
                        .map_or(Rfloat::na(), |&x| Rfloat::from(x)),
                    shrinkage_sd: table
                        .eta_shrinkage_sd
                        .as_ref()
                        .and_then(|v| v.get(eta_idx))
                        .map_or(Rfloat::na(), |&x| Rfloat::from(x)),
                    shrinkage_vr: table
                        .eta_shrinkage_vr
                        .as_ref()
                        .and_then(|v| v.get(eta_idx))
                        .map_or(Rfloat::na(), |&x| Rfloat::from(x)),
                    rel_info: table
                        .relative_information
                        .as_ref()
                        .and_then(|v| v.get(eta_idx))
                        .map_or(Rfloat::na(), |&x| Rfloat::from(x)),
                    ebv_shrinkage_sd: table
                        .ebv_shrinkage_sd
                        .as_ref()
                        .and_then(|v| v.get(eta_idx))
                        .map_or(Rfloat::na(), |&x| Rfloat::from(x)),
                    ebv_shrinkage_vr: table
                        .ebv_shrinkage_vr
                        .as_ref()
                        .and_then(|v| v.get(eta_idx))
                        .map_or(Rfloat::na(), |&x| Rfloat::from(x)),
                    n_individuals,
                };
                eta_rows.push(eta_row);
            }
        }
    }

    // Build dataframe
    let eta_df = if eta_rows.is_empty() {
        // Create empty dataframe with correct structure
        let empty_rows: Vec<EtaShkRow> = vec![];
        empty_rows
            .into_dataframe()
            .map_err(|e| Error::Other(format!("Failed to build empty eta dataframe: {e}")))?
    } else {
        eta_rows
            .into_dataframe()
            .map_err(|e| Error::Other(format!("Failed to build eta dataframe: {e}")))?
    };

    Ok(eta_df.into_robj())
}

/// Gets EPS shrinkage metrics from .shk file
///
/// @param path path to model file, model output directory, shk file or metadata json file.
///
/// @return data.frame of EPS shrinkage metrics
/// @export
///
/// @examples \dontrun{
/// get_eps_shrinkage("model/nonmem/run001/run001.shk")
/// }
#[extendr]
pub fn get_eps_shrinkage(path: &str) -> Result<Robj> {
    let shk_reader = ShkReader::default();
    let path = find_output_file(path, "shk")?;

    let tables = shk_reader
        .parse_file_semantic(path)
        .map_err(|e| Error::Other(e.to_string()))?;

    if tables.is_empty() {
        return Err(Error::Other("No tables found in shk file".to_string()));
    }

    let mut eps_rows = Vec::new();

    // Process each table group and table
    for table_group in &tables {
        for table in table_group {
            let method_name = table
                .method
                .as_ref()
                .map(|m| m.to_string())
                .unwrap_or_else(|| "Unknown".to_string());

            let subpop = table.subpop as i32;
            let n_individuals = table
                .n_individuals
                .map_or(Rint::na(), |n| Rint::from(n as i32));

            // Determine maximum EPS parameters
            let max_eps_params = [
                table.eps_shrinkage_sd.as_ref().map(|v| v.len()),
                table.eps_shrinkage_vr.as_ref().map(|v| v.len()),
            ]
            .iter()
            .filter_map(|&x| x)
            .max()
            .unwrap_or(0);

            // Create EPS rows
            for eps_idx in 0..max_eps_params {
                let eps_row = EpsShkRow {
                    method: method_name.clone(),
                    subpop,
                    eps_number: (eps_idx + 1) as i32,
                    shrinkage_sd: table
                        .eps_shrinkage_sd
                        .as_ref()
                        .and_then(|v| v.get(eps_idx))
                        .map_or(Rfloat::na(), |&x| Rfloat::from(x)),
                    shrinkage_vr: table
                        .eps_shrinkage_vr
                        .as_ref()
                        .and_then(|v| v.get(eps_idx))
                        .map_or(Rfloat::na(), |&x| Rfloat::from(x)),
                    n_individuals,
                };
                eps_rows.push(eps_row);
            }
        }
    }

    // Build dataframe
    let eps_df = if eps_rows.is_empty() {
        // Create empty dataframe with correct structure
        let empty_rows: Vec<EpsShkRow> = vec![];
        empty_rows
            .into_dataframe()
            .map_err(|e| Error::Other(format!("Failed to build empty eps dataframe: {e}")))?
    } else {
        eps_rows
            .into_dataframe()
            .map_err(|e| Error::Other(format!("Failed to build eps dataframe: {e}")))?
    };

    Ok(eps_df.into_robj())
}

extendr_module! {
    mod shk;

    fn get_eta_shrinkage;
    fn get_eps_shrinkage;
    fn read_shk_file;
}
