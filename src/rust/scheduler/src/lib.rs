pub mod slurm;
use slurm::PartitionTable;

use extendr_api::Result;
use extendr_api::prelude::*;

use std::path::PathBuf;

// pharos scheduler crate
use nonmem::{RunOptions, expand_model_pattern};
use scheduler::{
    SchedulerType,
    sge::SubmitOptions as SgeSubmitOptions,
    slurm::{SubmitOptions as SlurmSubmitOptions, resolve_partition},
};

use hyperion_core::{ResultExt, extendr_err};
use hyperion_nonmem::utils::{load_nonmem_config, path_from_robj};

/// Helper function to process Robj model input and expand patterns
///
/// Takes an Robj that can be either:
/// - A hyperion_nonmem_model object
/// - A single string (e.g., "run001.mod" or "run[001:003].mod")
/// - A character vector of strings
/// - A list of strings or model objects
///
/// Returns a Vec<PathBuf> with all expanded model paths
fn process_model_robj(model: Robj) -> Result<Vec<PathBuf>> {
    let expand = |pattern: &str| {
        expand_model_pattern(pattern).map_to_extendr_err(format!("model pattern '{pattern}'"))
    };

    // Handle hyperion_nonmem_model object
    if model.inherits("hyperion_nonmem_model") {
        let path = path_from_robj(&model, true)?;
        return Ok(vec![path]);
    }

    if let Some(s) = model.as_str() {
        expand(s)
    } else if let Some(strings) = model.as_str_vector() {
        strings
            .into_iter()
            .try_fold(Vec::new(), |mut acc, pattern| {
                acc.extend(expand(pattern)?);
                Ok(acc)
            })
    } else if let Some(list) = model.as_list() {
        // Handle R lists (can contain strings or model objects)
        list.values().try_fold(Vec::new(), |mut acc, item| {
            if item.inherits("hyperion_nonmem_model") {
                let path = path_from_robj(&item, true)?;
                acc.push(path);
                Ok(acc)
            } else if let Some(pattern) = item.as_str() {
                acc.extend(expand(pattern)?);
                Ok(acc)
            } else {
                Err(extendr_err!(
                    "All list elements must be strings or model objects, found: {:?}",
                    item.rtype()
                ))
            }
        })
    } else {
        Err(extendr_err!(
            "model must be a model object, string, character vector, or list"
        ))
    }
}

/// Submits NONMEM models to SLURM (internal implementation)
///
/// Called by the exported `submit_model_to_slurm()` in `R/submit.R`, which
/// resolves `pharos_exe_path` to an absolute path with `resolve_pharos()`.
/// The path is written into the job script, so this function never looks
/// pharos up itself.
///
/// @keywords internal
/// @noRd
#[extendr(r_name = ".submit_model_to_slurm")]
#[allow(clippy::too_many_arguments)]
pub fn submit_model_to_slurm(
    model: Robj,
    #[extendr(default = "FALSE")] overwrite: bool,
    #[extendr(default = "FALSE")] dry_run: bool,
    #[extendr(default = "FALSE")] run_in_output_dir: bool,
    #[extendr(default = "1")] ncpu: Option<u8>,
    #[extendr(default = "NULL")] partition: Option<String>,
    #[extendr(default = "1")] clean_level: Option<u8>,
    #[extendr(default = "NULL")] parafile: Option<String>,
    #[extendr(default = "NULL")] template: Option<String>,
    #[extendr(default = "NULL")] account: Option<String>,
    #[extendr(default = "FALSE")] verbose: bool,
    pharos_exe_path: String,
) -> Result<()> {
    // Process model input to get list of model files
    let model_files = process_model_robj(model)?;

    let (config_path, nonmem_config) = load_nonmem_config(None)?;

    // check partition and give advice if needed
    let model_count = model_files.len();
    let ncpu_i32 = i32::from(ncpu.unwrap_or(1));
    let table = PartitionTable::from_slurm()?;
    let partition_name = resolve_partition(
        partition.as_deref(),
        nonmem_config.slurm.partition.as_deref(),
    )
    .map_to_extendr_err("Failed to get requested partition")?;
    let active = table.find_partition(&partition_name);

    if let Some(active) = active {
        if ncpu_i32 > active.cpus as i32 {
            let advice = table.partition_advice(ncpu_i32, &partition_name, model_count, false);
            call!("stop", advice)?;
        } else if table.is_underutilized(&partition_name, ncpu_i32, model_count) {
            let advice = table.partition_advice(ncpu_i32, &partition_name, model_count, true);
            call!("warning", advice)?;
        }
    }

    let submit_options = SlurmSubmitOptions {
        // process_model_robj is handling model paths so SubmitOptions doesn't need it.
        model: String::new(),
        partition,
        account,
        template: template.map(PathBuf::from),
        dry_run,
        ..SlurmSubmitOptions::default()
    };

    let scheduler = SchedulerType::new_slurm(submit_options);
    let parallel = ncpu.is_some_and(|n| n > 1);

    let run_options = RunOptions {
        run_in_output_dir,
        overwrite,
        clean_level,
        parallel,
        num_mpi_cpus: ncpu,
        parafile: parafile.map(PathBuf::from),
        verbose,
        ..RunOptions::default() // nonmem_version: (),
                                // output_dir: (),
                                // num_parallel: (),
                                // extra_files: (),
                                // mpi_timeout: (),
    };

    let pharos_exe_path = PathBuf::from(pharos_exe_path);

    let res = scheduler
        .submit(
            &config_path,
            model_files,
            run_options,
            nonmem_config,
            pharos_exe_path,
        )
        .map_to_extendr_err("Failed to submit job to slurm")?;

    for (p, job_id) in res {
        rprintln!("Model {p:?} -> job ID {job_id}");
    }
    Ok(())
}

/// Submits NONMEM models to SGE (internal implementation)
///
/// Called by the exported `submit_model_to_sge()` in `R/submit.R`, which
/// resolves `pharos_exe_path` to an absolute path with `resolve_pharos()`.
/// The path is written into the job script, so this function never looks
/// pharos up itself.
///
/// @keywords internal
/// @noRd
#[extendr(r_name = ".submit_model_to_sge")]
#[allow(clippy::too_many_arguments)]
pub fn submit_model_to_sge(
    model: Robj,
    #[extendr(default = "FALSE")] overwrite: bool,
    #[extendr(default = "FALSE")] dry_run: bool,
    #[extendr(default = "FALSE")] run_in_output_dir: bool,
    #[extendr(default = "1")] ncpu: Option<u8>,
    #[extendr(default = "1")] clean_level: Option<u8>,
    #[extendr(default = "NULL")] parafile: Option<String>,
    #[extendr(default = "NULL")] template: Option<String>,
    #[extendr(default = "FALSE")] verbose: bool,
    pharos_exe_path: String,
) -> Result<()> {
    // Process model input to get list of model files
    let model_files = process_model_robj(model)?;

    let submit_options = SgeSubmitOptions {
        // process_model_robj is handling model paths so SubmitOptions doesn't need it.
        model: String::new(),
        template: template.map(PathBuf::from),
        dry_run,
        ..SgeSubmitOptions::default()
    };

    let scheduler = SchedulerType::new_sge(submit_options);
    let (config_path, nonmem_config) = load_nonmem_config(None)?;
    let parallel = ncpu.is_some_and(|n| n > 1);

    let run_options = RunOptions {
        run_in_output_dir,
        overwrite,
        clean_level,
        parallel,
        num_mpi_cpus: ncpu,
        parafile: parafile.map(PathBuf::from),
        verbose,
        ..RunOptions::default() // nonmem_version: (),
                                // output_dir: (),
                                // num_parallel: (),
                                // extra_files: (),
                                // mpi_timeout: (),
    };

    let pharos_exe_path = PathBuf::from(pharos_exe_path);

    let res = scheduler
        .submit(
            &config_path,
            model_files,
            run_options,
            nonmem_config,
            pharos_exe_path,
        )
        .map_to_extendr_err("Failed to submit job to sge")?;

    for (p, job_id) in res {
        rprintln!("Model {p:?} submitted: job id {job_id}");
    }

    Ok(())
}

extendr_module! {
    mod hyperion_scheduler;

    use slurm;

    fn submit_model_to_slurm;
    fn submit_model_to_sge;
}
