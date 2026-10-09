#' Submits a NONMEM model to SLURM for execution
#'
#' This function submits a NONMEM model file to a SLURM cluster for execution,
#' allowing for parallel processing and job queue management. The function handles
#' job configuration, resource allocation, and job submission through pharos
#'
#' @param model A hyperion_nonmem_model object, path to the NONMEM model file,
#' or character vector of model paths/patterns (required)
#' @param overwrite Whether to overwrite existing output files (default: FALSE)
#' @param dry_run Whether to perform a dry run without actually submitting the job (default: FALSE)
#' @param run_in_output_dir Whether to run the job in the output directory (default: FALSE)
#' @param ncpu Number of CPUs to allocate for the job (default: 1)
#' @param partition SLURM partition to submit the job to (default: NULL, uses cluster default)
#' @param clean_level Level of cleanup to perform after job completion (default: 1)
#' @param parafile Path to parameter file for parallel runs (default: NULL)
#' @param template Path to SLURM template file for job submission (default: NULL)
#' @param account SLURM account to charge the job to (default: NULL)
#' @param verbose Whether to include DEBUG logs in output log file (default: FALSE)
#'
#' @details
#' The job runs the pharos executable returned by [pharos_path()]: by default
#' the pharos bundled with hyperion, then `pharos` on `PATH`. Set the option
#' `hyperion.pharos_exec_path` to use a different one. The path is written into
#' the job script, so it must be readable from the compute nodes.
#'
#' @return Returns invisibly after printing job submission results. Prints model path and corresponding SLURM job ID for each submitted job.
#' @export
#'
#' @examples
#' \dontrun{
#' # Submit a basic NONMEM model
#' submit_model_to_slurm("model.mod")
#'
#' # Submit using a model object
#' model <- read_model("model.mod")
#' submit_model_to_slurm(model)
#'
#' # Dry run to test submission without actually running
#' submit_model_to_slurm("model.mod", dry_run = TRUE)
#'
#' # Submit to specific partition with account
#' submit_model_to_slurm("model.mod", partition = "gpu", account = "myproject")
#'
#' # Use a specific pharos executable
#' options(hyperion.pharos_exec_path = "/opt/pharos/bin/pharos")
#' submit_model_to_slurm("model.mod")
#' }
submit_model_to_slurm <- function(
  model,
  overwrite = FALSE,
  dry_run = FALSE,
  run_in_output_dir = FALSE,
  ncpu = 1,
  partition = NULL,
  clean_level = 1,
  parafile = NULL,
  template = NULL,
  account = NULL,
  verbose = FALSE
) {
  pharos <- resolve_pharos()
  # By name, so a reordered Rust signature cannot shift arguments.
  .submit_model_to_slurm(
    model = model,
    overwrite = overwrite,
    dry_run = dry_run,
    run_in_output_dir = run_in_output_dir,
    ncpu = ncpu,
    partition = partition,
    clean_level = clean_level,
    parafile = parafile,
    template = template,
    account = account,
    verbose = verbose,
    pharos_exe_path = pharos$path
  )
}

#' Submits a NONMEM model to SGE for execution
#'
#' This function submits a NONMEM model file to a SGE cluster for execution,
#' allowing for parallel processing and job queue management. The function handles
#' job configuration, resource allocation, and job submission through pharos
#'
#' @param model A hyperion_nonmem_model object, path to the NONMEM model file,
#' or character vector of model paths/patterns (required)
#' @param overwrite Whether to overwrite existing output files (default: FALSE)
#' @param dry_run Whether to perform a dry run without actually submitting the job (default: FALSE)
#' @param run_in_output_dir Whether to run the job in the output directory (default: FALSE)
#' @param ncpu Number of CPUs to allocate for the job (default: 1)
#' @param clean_level Level of cleanup to perform after job completion (default: 1)
#' @param parafile Path to parameter file for parallel runs (default: NULL)
#' @param template Path to SGE template file for job submission (default: NULL)
#' @param verbose Whether to include DEBUG logs in output log file (default: FALSE)
#'
#' @inherit submit_model_to_slurm details
#'
#' @return Returns invisibly after printing job submission results. Prints model path and corresponding SGE job ID for each submitted job.
#' @export
#'
#' @examples
#' \dontrun{
#' # Submit a basic NONMEM model
#' submit_model_to_sge("model.mod")
#'
#' # Submit using a model object
#' model <- read_model("model.mod")
#' submit_model_to_sge(model)
#'
#' # Dry run to test submission without actually running
#' submit_model_to_sge("model.mod", dry_run = TRUE)
#'
#' # Use a specific pharos executable
#' options(hyperion.pharos_exec_path = "/opt/pharos/bin/pharos")
#' submit_model_to_sge("model.mod")
#'}
submit_model_to_sge <- function(
  model,
  overwrite = FALSE,
  dry_run = FALSE,
  run_in_output_dir = FALSE,
  ncpu = 1,
  clean_level = 1,
  parafile = NULL,
  template = NULL,
  verbose = FALSE
) {
  pharos <- resolve_pharos()
  # By name, so a reordered Rust signature cannot shift arguments.
  .submit_model_to_sge(
    model = model,
    overwrite = overwrite,
    dry_run = dry_run,
    run_in_output_dir = run_in_output_dir,
    ncpu = ncpu,
    clean_level = clean_level,
    parafile = parafile,
    template = template,
    verbose = verbose,
    pharos_exe_path = pharos$path
  )
}
