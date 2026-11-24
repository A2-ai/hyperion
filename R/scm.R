SCM_CONFIG_SUFFIX <- "scm.toml"
SCM_NO_EFFECTS <- "No effects yet: pass {.arg effects} to {.fn init_scm} or add them with {.fn add_scm_effect}."

# SLURM job variables a nested `sbatch` would inherit as defaults.
SLURM_INHERITED_VARS <- c(
  "SLURM_CPUS_PER_TASK",
  "SLURM_DISTRIBUTION",
  "SLURM_JOB_NUM_NODES",
  "SLURM_MEM_PER_CPU",
  "SLURM_MEM_PER_GPU",
  "SLURM_MEM_PER_NODE",
  "SLURM_NNODES",
  "SLURM_NPROCS",
  "SLURM_NTASKS",
  "SLURM_NTASKS_PER_NODE"
)

#' Read an SCM config
#'
#' @param path Path to the SCM config (`<stem>scm.toml`) written by
#'   [init_scm()].
#' @return A `hyperion_nonmem_scm` S3 object: the config's keys as a list,
#'   with the candidate effects as [ScmEffect] objects under
#'   `covariates$effects`.
#' @export
#'
#' @examples \dontrun{
#' scm <- read_scm("model/nonmem/scm/run001scm.toml")
#' scm$covariates$effects
#' }
read_scm <- function(path) {
  check_scm_config_path(path, must_exist = TRUE)
  x <- read_scm_toml_impl(path)
  x$covariates <- normalize_covariates(x$covariates)
  path <- normalizePath(path, mustWork = FALSE)
  attr(x, "config_source") <- to_config_relative(path)
  attr(x, "filename") <- sub(paste0(SCM_CONFIG_SUFFIX, "$"), "", basename(path))
  class(x) <- "hyperion_nonmem_scm"
  x
}

#' Validate an SCM config path
#'
#' @param path Path to an SCM config (`<stem>scm.toml`)
#' @param must_exist Whether the file must already exist
#' @return `path`, invisibly
#' @keywords internal
#' @noRd
check_scm_config_path <- function(path, must_exist = FALSE) {
  if (
    !is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)
  ) {
    rlang::abort(
      "`path` must be a single path to an SCM config (<stem>scm.toml)"
    )
  }
  if (!endsWith(basename(path), SCM_CONFIG_SUFFIX)) {
    rlang::abort(paste0(
      "`path` must be an SCM config named <stem>",
      SCM_CONFIG_SUFFIX,
      ", got: ",
      path
    ))
  }
  if (must_exist && !file.exists(path)) {
    rlang::abort(paste0("SCM config not found: ", path))
  }
  invisible(path)
}

#' Validate the config path passed to an SCM editing function
#'
#' Editing functions take the config file, not the `hyperion_nonmem_scm`
#' object, so that every edit is written to the file.
#'
#' @param path Path to an existing SCM config (`<stem>scm.toml`)
#' @return `path`, invisibly
#' @keywords internal
#' @noRd
check_scm_edit_path <- function(path) {
  if (inherits(path, "hyperion_nonmem_scm")) {
    rlang::abort(c(
      "`path` must be the SCM config (<stem>scm.toml), not the object",
      i = "Edits are written to the file. Use read_scm() to read them back."
    ))
  }
  check_scm_config_path(path, must_exist = TRUE)
}

#' Write items into an SCM config
#'
#' @param scm A hyperion_nonmem_scm object
#' @param ... Items to insert, passed to `tomledit::insert_items()`
#' @return The config path, invisibly
#' @keywords internal
#' @noRd
write_scm_items <- function(scm, ...) {
  path <- from_config_relative(attr(scm, "config_source"))
  toml <- tomledit::insert_items(tomledit::read_toml(path), ...)
  tomledit::write_toml(toml, path)
  # tomledit writes a one-element `direction` as a string; pharos reads an array.
  lines <- readLines(path)
  scalar <- grepl('^direction = "', lines)
  if (any(scalar)) {
    lines[scalar] <- sub(
      '^direction = "(.*)"$',
      'direction = ["\\1"]',
      lines[scalar]
    )
    writeLines(lines, path)
  }
  invisible(path)
}

#' Write the candidate effects into an SCM config
#'
#' @param scm A hyperion_nonmem_scm object
#' @param effects List of ScmEffect objects
#' @return The config path, invisibly
#' @keywords internal
#' @noRd
write_scm_effects <- function(scm, effects) {
  scm$covariates$effects <- effects
  write_scm_items(scm, covariates = covariates_as_rows(scm$covariates))
}

#' Report what an SCM editing function wrote
#'
#' @param what Description of the edit
#' @param path Path to the SCM config
#' @keywords internal
#' @noRd
scm_edit_note <- function(what, path) {
  cli::cli_alert_success("{what} in {.path {to_config_relative(path)}}")
}

SCM_SETTING_FORMS <- c(
  direction = '"forward", "backward", or both',
  forward_alpha = "a single number",
  backward_alpha = "a single number",
  max_retries = "a single whole number",
  retry_jitter = "a single number",
  num_rounds = "a single whole number",
  cov_step = "TRUE or FALSE",
  forward_final_cov_step = "TRUE or FALSE",
  final_cov_step = "TRUE or FALSE"
)

#' Validate a value for an SCM config setting
#'
#' @param key Name of the setting
#' @param value Value given for it
#' @return The value as the config stores it
#' @keywords internal
#' @noRd
scm_setting <- function(key, value) {
  one <- length(value) == 1 && !is.na(value)
  ok <- switch(
    key,
    direction = is.character(value) &&
      length(value) %in% 1:2 &&
      all(value %in% c("forward", "backward")) &&
      !anyDuplicated(value),
    max_retries = ,
    num_rounds = one &&
      is.numeric(value) &&
      value >= 0 &&
      value == trunc(value),
    cov_step = ,
    forward_final_cov_step = ,
    final_cov_step = one && is.logical(value),
    one && is.numeric(value)
  )
  if (!ok) {
    rlang::abort(paste0("`", key, "` must be ", SCM_SETTING_FORMS[[key]]))
  }
  if (key %in% c("max_retries", "num_rounds")) as.integer(value) else value
}

#' Update the settings of an SCM config
#'
#' Writes the given settings into the SCM config (`<stem>scm.toml`) and
#' leaves the other settings unchanged. Edits are written to the file; use
#' [read_scm()] to read the updated config. Candidate effects are edited
#' with [add_scm_effect()], [update_scm_effect()] and [remove_scm_effect()].
#'
#' @param path Path to the SCM config (`<stem>scm.toml`), as returned by
#'   [init_scm()].
#' @param direction Phases to run: `"forward"`, `"backward"`, or both.
#' @param forward_alpha,backward_alpha P-value threshold for adding a
#'   covariate in forward selection, and for keeping one in backward
#'   elimination.
#' @param max_retries Number of retries for a failed fit.
#' @param retry_jitter Fraction by which a retry perturbs the estimates it
#'   starts from.
#' @param num_rounds Number of rounds after which to pause. `0` runs all
#'   rounds.
#' @param cov_step Whether the generated models run `$COVARIANCE`.
#' @param forward_final_cov_step,final_cov_step Whether the
#'   forward model and the final model are re-fitted with
#'   `$COVARIANCE` on.
#' @return Invisibly returns the path to the SCM config.
#' @export
#'
#' @examples \dontrun{
#' config <- "model/nonmem/scm/run001scm.toml"
#' update_scm_settings(config, direction = "forward")
#' update_scm_settings(config, forward_alpha = 0.01, num_rounds = 2)
#' read_scm(config)
#' }
update_scm_settings <- function(
  path,
  direction = NULL,
  forward_alpha = NULL,
  backward_alpha = NULL,
  max_retries = NULL,
  retry_jitter = NULL,
  num_rounds = NULL,
  cov_step = NULL,
  forward_final_cov_step = NULL,
  final_cov_step = NULL
) {
  scm <- read_scm(check_scm_edit_path(path))
  given <- list(
    direction = direction,
    forward_alpha = forward_alpha,
    backward_alpha = backward_alpha,
    max_retries = max_retries,
    retry_jitter = retry_jitter,
    num_rounds = num_rounds,
    cov_step = cov_step,
    forward_final_cov_step = forward_final_cov_step,
    final_cov_step = final_cov_step
  )
  given <- given[!vapply(given, is.null, logical(1))]
  if (length(given) == 0) {
    rlang::abort("At least one setting must be provided")
  }
  for (key in names(given)) {
    given[[key]] <- scm_setting(key, given[[key]])
  }
  path <- write_scm_items(scm, !!!given)
  scm_edit_note(paste0("Set ", paste(names(given), collapse = ", ")), path)
  invisible(path)
}

#' Initialize an SCM process for a model
#'
#' Writes a starter SCM config (`<stem>scm.toml`) and creates the SCM
#' directory beside the model, as `pharos nonmem scm init` does, with the
#' candidate effects filled in. If the config already exists, it is left
#' unchanged and its path is returned, so the same call can be run again.
#' Use `overwrite = TRUE` to replace it.
#'
#' @param model Path to the initial model (`.mod`/`.ctl`), or a
#'   `hyperion_nonmem_model` object.
#' @param effects Candidate effects, each named by its `$THETA` comment in
#'   the model. Either a character vector of names, or a list whose elements
#'   are a name, an [ScmEffect()], or a list of the form `list(name = , type
#'   = , initial = , fixed = , lower = , upper = )`, where `type` is
#'   `"continuous"` (the default) or `"categorical"` and the numeric fields
#'   are optional. `NULL` writes a config with no effects, to be added with
#'   [add_scm_effect()] or by hand. If the config already exists, the
#'   effects are not written, and a warning is raised when their names
#'   differ from those in the file.
#' @param overwrite Whether to replace an existing config. Default `FALSE`.
#' @return Invisibly returns the path to the SCM config. Use [read_scm()] to
#'   read it.
#' @seealso [read_scm()], [add_scm_effect()], [remove_scm_effect()]
#' @export
#'
#' @examples \dontrun{
#' config <- init_scm("model/nonmem/run001.mod", effects = c("WT_CL", "CRCL_CL"))
#' scm <- read_scm(config)
#' check_scm(scm)
#'
#' init_scm(
#'   "model/nonmem/run001.mod",
#'   effects = list(
#'     "WT_CL",
#'     list(name = "SEX_V", type = "categorical", initial = 1, lower = 0, upper = 5)
#'   ),
#'   overwrite = TRUE
#' )
#' }
init_scm <- function(model, effects = NULL, overwrite = FALSE) {
  effects <- normalize_effects(effects)
  res <- init_scm_impl(model, overwrite)
  config <- res$config_path
  config_shown <- to_config_relative(config)
  if (isTRUE(res$existed)) {
    cli::cli_alert_info("SCM config already exists: {.path {config_shown}}")
    cli::cli_text(
      "{.fn read_scm} reads it; {.code overwrite = TRUE} replaces it."
    )
    if (length(effects)) {
      warn_scm_effects_differ(read_scm(config)$covariates$effects, effects)
    }
    return(invisible(config))
  }
  if (length(effects)) {
    write_scm_effects(read_scm(config), effects)
  }
  cli::cli_alert_success("SCM config written: {.path {config_shown}}")
  cli::cli_alert_info(
    "SCM directory: {.path {to_config_relative(res$out_dir)}}"
  )
  if (length(effects) == 0) {
    cli::cli_alert_warning(SCM_NO_EFFECTS)
  }
  cli::cli_text("Read it with {.fn read_scm}.")
  cli::cli_text(
    "{.fn check_scm} shows what a submit would do; {.fn submit_scm_to_slurm} runs it."
  )
  invisible(config)
}

#' Warn when the effects given to init_scm() differ from an existing config
#'
#' @param current List of ScmEffect objects in the config
#' @param given List of ScmEffect objects passed to init_scm()
#' @keywords internal
#' @noRd
warn_scm_effects_differ <- function(current, given) {
  have <- scm_effect_names(current)
  want <- scm_effect_names(given)
  if (setequal(have, want)) {
    return(invisible())
  }
  show <- function(x) if (length(x)) paste(x, collapse = ", ") else "(none)"
  rlang::warn(c(
    "`effects` differ from the effects in the existing config, which was left unchanged",
    i = paste0("In the config: ", show(have)),
    i = paste0("Given: ", show(want)),
    i = "Edit them with add_scm_effect(), update_scm_effect() and remove_scm_effect(), or pass overwrite = TRUE."
  ))
}

#' Check an SCM config
#'
#' Resolves an SCM config to its plan without writing or running anything.
#' The plan prints its facts (the model, the direction and alphas, the
#' retries a failed fit gets, where the covariance step runs, and the
#' maximum number of models it can fit), a table of its candidate effects,
#' the SCM process already in its output directory with what the plan
#' changes about it, and its warnings.
#'
#' @param scm A `hyperion_nonmem_scm` object from [read_scm()], or the path
#'   to the SCM config (`<stem>scm.toml`).
#' @param num_rounds Number of rounds after which to pause. `NULL` uses the
#'   config's `num_rounds`; `0L` runs all rounds.
#'
#' @return A `hyperion_nonmem_scm_plan` S3 object: the plan (`model`,
#'   `out_dir`, `candidates`, `options`, ...) with attributes `text` (the plan
#'   as rendered by pharos), `warnings`, `resumable`, `out_dir` and
#'   `project_root`.
#' @export
#'
#' @examples \dontrun{
#' scm <- read_scm("model/nonmem/scm/run001scm.toml")
#' check_scm(scm)
#' check_scm(scm, num_rounds = 2)
#' }
check_scm <- function(scm, num_rounds = NULL) {
  check_scm_impl(scm, num_rounds)
}

#' Submit an SCM process to SLURM
#'
#' Queues the SCM driver as a SLURM job, which submits the model fits and
#' waits for them. Submitting the config of a paused or stopped process
#' resumes it.
#'
#' @param scm A `hyperion_nonmem_scm` object from [read_scm()], or the path
#'   to the SCM config (`<stem>scm.toml`).
#' @param num_rounds Number of rounds after which to pause. `NULL` uses the
#'   config's `num_rounds`.
#' @param partition SLURM partition for the model fits.
#' @param driver_partition SLURM partition for the driver job.
#' @param account SLURM account for the driver job and the model fits.
#' @param max_concurrent Maximum number of fits running at once.
#' @param shared_node Whether to run every fit on one whole node instead of
#'   one SLURM job per fit. Default `FALSE`.
#' @param overwrite Whether to discard the SCM process already in the output
#'   directory and start fresh. Default `FALSE`.
#' @param verbose Whether to include DEBUG logs in the driver's SLURM log.
#'   Default `FALSE`.
#' @param allow_version_mismatch Whether to submit even when the `pharos` on
#'   `PATH` is not the version hyperion expects. Default `FALSE`.
#' @return `NULL`, after printing the driver's job id and log path.
#' @export
#'
#' @examples \dontrun{
#' scm <- read_scm("model/nonmem/scm/run001scm.toml")
#' check_scm(scm)
#' submit_scm_to_slurm(scm, partition = "cpu")
#' submit_scm_to_slurm(scm, num_rounds = 2)
#' submit_scm_to_slurm(scm, shared_node = TRUE, partition = "cpu32")
#' }
submit_scm_to_slurm <- function(
  scm,
  num_rounds = NULL,
  partition = NULL,
  driver_partition = NULL,
  account = NULL,
  max_concurrent = NULL,
  shared_node = FALSE,
  overwrite = FALSE,
  verbose = FALSE,
  allow_version_mismatch = FALSE
) {
  if (isTRUE(shared_node) && !is.null(driver_partition)) {
    rlang::abort(
      "`driver_partition` cannot be used with `shared_node = TRUE`: the driver and the fits share the node `partition` picks"
    )
  }
  check_pharos_cli_version(allow_version_mismatch)
  res <- without_slurm_env(submit_scm_to_slurm_impl(
    scm,
    num_rounds,
    partition,
    driver_partition,
    account,
    max_concurrent,
    shared_node,
    overwrite,
    verbose
  ))

  for (w in res$warnings) {
    rlang::warn(w)
  }
  if (!is.null(res$discard_note)) {
    cli::cli_alert_info(res$discard_note)
  }
  cli::cli_alert_success(
    "Submitted the SCM process as slurm job {res$job_id} ({res$job_name})"
  )
  cli::cli_alert_info("Log: {.path {res$log}}")
  cli::cli_text(
    "Follow it with {.code get_scm_status(\"{res$config}\")}; stop it with {.code stop_scm(\"{res$config}\")}."
  )
  NULL
}

#' Check the pharos CLI on PATH against the version hyperion expects
#'
#' @param allow_mismatch Whether to accept a version other than the expected
#'   one
#' @keywords internal
#' @noRd
check_pharos_cli_version <- function(allow_mismatch = FALSE) {
  cli_info <- detect_pharos()
  if (is.na(cli_info$path)) {
    rlang::abort(
      "pharos CLI not found on PATH. The SCM driver job runs `pharos nonmem scm run`."
    )
  }
  if (isTRUE(allow_mismatch)) {
    return(invisible())
  }
  expected <- utils::packageDescription(
    "hyperion",
    fields = "Config/PharosVersion"
  )
  if (is.na(cli_info$version) || !identical(cli_info$version, expected)) {
    rlang::abort(c(
      paste0(
        "pharos CLI version mismatch: installed ",
        cli_info$version,
        " (",
        cli_info$path,
        "), hyperion expects ",
        expected
      ),
      i = paste0(
        "Install pharos ",
        expected,
        ", or pass allow_version_mismatch = TRUE to submit anyway."
      )
    ))
  }
  invisible()
}

#' Evaluate an expression with the inherited SLURM variables unset
#'
#' @param expr Expression to evaluate
#' @return The value of `expr`; the variables are restored on exit
#' @keywords internal
#' @noRd
without_slurm_env <- function(expr) {
  old <- Sys.getenv(SLURM_INHERITED_VARS, unset = NA, names = TRUE)
  on.exit(
    if (any(set <- !is.na(old))) do.call(Sys.setenv, as.list(old[set])),
    add = TRUE
  )
  Sys.unsetenv(SLURM_INHERITED_VARS)
  expr
}

#' Stop an SCM process
#'
#' Stops the driver of an SCM process and cancels its fits that are still in
#' the SLURM queue. The process is recorded as paused; submit its config
#' again with [submit_scm_to_slurm()] to resume it.
#'
#' @param scm A `hyperion_nonmem_scm` object from [read_scm()], or the path
#'   to the SCM config (`<stem>scm.toml`).
#' @return `NULL`, after printing what was stopped and cancelled.
#' @export
#'
#' @examples \dontrun{
#' stop_scm("model/nonmem/scm/run001scm.toml")
#' }
stop_scm <- function(scm) {
  res <- stop_scm_impl(scm)
  cli::cli_h1("SCM Stop: {res$out_dir}")
  process <- if (isTRUE(res$was_alive)) {
    "told to stop; it is recorded as paused once its running fits are handed back"
  } else {
    "already stopped"
  }
  cli::cli_text("{.strong Process:} {process}")
  fits <- if (length(res$fits_cancelled)) {
    paste0(
      "cancelled slurm job(s) ",
      paste(format(res$fits_cancelled, scientific = FALSE), collapse = ", ")
    )
  } else {
    "none in the slurm queue"
  }
  cli::cli_text("{.strong Fits:} {fits}")
  if (!is.null(res$allocation)) {
    cli::cli_text("{.strong Allocation:} {res$allocation}")
  }
  cli::cli_text(
    "Follow it with {.code get_scm_status(\"{res$config}\")}; resume it with {.code submit_scm_to_slurm(\"{res$config}\")}."
  )
  NULL
}

#' Get the status of an SCM process
#'
#' Reads where an SCM process stands now: the process facts; the base,
#' forward and final models fitted so far; each decided
#' round with its decision and a table of its candidates; the open round
#' fit by fit (its SLURM job, how long it has run, and the latest iteration
#' and OFV from its `.ext` file); and the state of the driver (running,
#' exited, or gone while the process is still recorded as running, with its
#' log). It holds what `pharos nonmem scm status` shows.
#'
#' @param scm A `hyperion_nonmem_scm` object from [read_scm()], or the path
#'   to the SCM config (`<stem>scm.toml`).
#'
#' @return A `hyperion_nonmem_scm_status` S3 object (also of class
#'   `hyperion_nonmem_scm_record`): the process record (`status`, `phase`,
#'   `candidates`, `retained`, `rounds`, `totals`, `models_running`, ...) as
#'   pharos writes it to `scm_summary.json`, plus a `driver` element, with
#'   attributes `text` (the status as rendered by pharos), `out_dir` and
#'   `project_root`. Every path in the record is relative to `out_dir`. An
#'   unreadable `scm_driver.json` raises a warning, and the driver is
#'   treated as never started.
#' @export
#'
#' @examples \dontrun{
#' scm <- read_scm("model/nonmem/scm/run001scm.toml")
#' get_scm_status(scm)
#' get_scm_status("model/nonmem/scm/run001scm.toml")
#' }
get_scm_status <- function(scm) {
  res <- get_scm_status_impl(scm)
  for (w in res$warnings) {
    rlang::warn(w)
  }
  res$status
}
