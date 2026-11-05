SCM_CONFIG_SUFFIX <- "scm.toml"
SCM_NO_EFFECTS <- "No effects yet: pass {.arg effects} to {.fn init_scm} or add them with {.fn add_scm_effect}."

# Slurm job variables a nested `sbatch` would read back as defaults.
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
#' @param path Path to the SCM config (`<stem>scm.toml`) written by [init_scm()].
#' @return A `hyperion_nonmem_scm` object: the config's keys as a list, with
#'   the effects as `ScmEffect` objects under `covariates$effects`.
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

check_scm_config_path <- function(path, must_exist = FALSE) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    rlang::abort("`path` must be a single path to an SCM config (<stem>scm.toml)")
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

# The config path an editor takes: the file itself, never the object, so
# that every edit lands in the file.
check_scm_edit_path <- function(path) {
  if (inherits(path, "hyperion_nonmem_scm")) {
    rlang::abort(c(
      "`path` must be the SCM config (<stem>scm.toml), not the object",
      i = "the edits go to the file; `read_scm()` reads them back"
    ))
  }
  check_scm_config_path(path, must_exist = TRUE)
}

# Write the items into the object's config; returns the config path.
write_scm_items <- function(scm, ...) {
  path <- from_config_relative(attr(scm, "config_source"))
  toml <- tomledit::insert_items(tomledit::read_toml(path), ...)
  tomledit::write_toml(toml, path)
  # tomledit writes a one-element `direction` as a string; pharos reads an array.
  lines <- readLines(path)
  scalar <- grepl('^direction = "', lines)
  if (any(scalar)) {
    lines[scalar] <- sub('^direction = "(.*)"$', 'direction = ["\\1"]', lines[scalar])
    writeLines(lines, path)
  }
  invisible(path)
}

write_scm_effects <- function(scm, effects) {
  scm$covariates$effects <- effects
  write_scm_items(scm, covariates = covariates_as_rows(scm$covariates))
}

# One line on what an editor wrote, with the config as the object shows it.
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

# `value` as the config takes `key`, or an error.
scm_setting <- function(key, value) {
  one <- length(value) == 1 && !is.na(value)
  ok <- switch(
    key,
    direction = is.character(value) && length(value) %in% 1:2 && all(value %in% c("forward", "backward")) && !anyDuplicated(value),
    max_retries = ,
    num_rounds = one && is.numeric(value) && value >= 0 && value == trunc(value),
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
#' Writes the settings given into the config (`<stem>scm.toml`); the others
#' keep their values. The candidate effects are edited with
#' [add_scm_effect()], [update_scm_effect()] and [remove_scm_effect()].
#' Every edit goes to the file: [read_scm()] reads the result back.
#'
#' @param path The SCM config (`<stem>scm.toml`), as [init_scm()] returns it.
#' @param direction The phases to run: `"forward"`, `"backward"`, or both.
#' @param forward_alpha,backward_alpha The p-value to add a covariate in
#'   forward selection, and to keep one in backward elimination.
#' @param max_retries Retries per failed fit.
#' @param retry_jitter How far a retry perturbs the estimates it starts from,
#'   as a fraction.
#' @param num_rounds Pause after this many rounds; `0` runs all rounds.
#' @param cov_step Whether the generated models run `$COVARIANCE`.
#' @param forward_final_cov_step,final_cov_step Whether the forward model and
#'   the final model are re-fitted with `$COVARIANCE` on.
#' @return The config path, invisibly.
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
    rlang::abort("give at least one setting to update")
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
#' Writes the starter config (`<stem>scm.toml`) and the SCM directory beside
#' the model, as `pharos nonmem scm init` does, with the candidate effects
#' filled in. A config already there is left as it is and its path returned,
#' so the call is the same the first time and every time after; `overwrite =
#' TRUE` replaces it.
#'
#' @param model Path to the initial model (`.mod`/`.ctl`), or a
#'   `hyperion_nonmem_model` object.
#' @param effects The candidate effects, each named by its `$THETA` comment in
#'   the model: a character vector of names, or a list whose elements are a
#'   name or a row `list(name = , type = , initial = ,
#'   fixed = , lower = , upper = )` (`type` is `"continuous"`, the default, or
#'   `"categorical"`; the numbers are optional). `NULL` writes the config with
#'   no effects, to be added with [add_scm_effect()] or by hand. When the
#'   config already exists the effects are not written; names that differ
#'   from the file's are a warning.
#' @param overwrite Replace an existing config. Default `FALSE`.
#' @return The path to the config, invisibly. [read_scm()] reads it.
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
    cli::cli_text("{.fn read_scm} reads it; {.code overwrite = TRUE} replaces it.")
    if (length(effects)) {
      warn_scm_effects_differ(read_scm(config)$covariates$effects, effects)
    }
    return(invisible(config))
  }
  if (length(effects)) {
    write_scm_effects(read_scm(config), effects)
  }
  cli::cli_alert_success("SCM config written: {.path {config_shown}}")
  cli::cli_alert_info("SCM directory: {.path {to_config_relative(res$out_dir)}}")
  if (length(effects) == 0) {
    cli::cli_alert_warning(SCM_NO_EFFECTS)
  }
  cli::cli_text("Read it with {.fn read_scm}.")
  cli::cli_text(
    "{.fn check_scm} shows what a submit would do; {.fn submit_scm_to_slurm} runs it."
  )
  invisible(config)
}

# A warning when the effect names given to init_scm() are not the config's.
warn_scm_effects_differ <- function(current, given) {
  have <- scm_effect_names(current)
  want <- scm_effect_names(given)
  if (setequal(have, want)) {
    return(invisible())
  }
  show <- function(x) if (length(x)) paste(x, collapse = ", ") else "(none)"
  cli::cli_warn(c(
    "`effects` differ from the config's, which was left as it is",
    i = "in the config: {show(have)}",
    i = "given: {show(want)}",
    i = "edit them with {.fn add_scm_effect}, {.fn update_scm_effect} and {.fn remove_scm_effect}, or pass {.code overwrite = TRUE}"
  ))
}

#' Check an SCM config: the plan it resolves to and its warnings
#'
#'
#' @param scm A `hyperion_nonmem_scm` object from [read_scm()], or the path
#'   to the SCM config (`<stem>scm.toml`).
#' @param num_rounds Pause after this many rounds; `NULL` uses the config's
#'   `num_rounds`, `0L` runs all rounds.
#' @param overwrite Show the plan as a submit with `overwrite = TRUE` would run it.
#'
#' @return A `hyperion_nonmem_scm_plan` S3 object: the plan (`model`,
#'   `out_dir`, `candidates`, `options`, ...) with attributes `text` (the plan
#'   as pharos renders it), `warnings`, `resumable`, `out_dir` and
#'   `project_root`.
#' @export
#'
#' @examples \dontrun{
#' scm <- read_scm("model/nonmem/scm/run001scm.toml")
#' check_scm(scm)
#' check_scm(scm, num_rounds = 2)
#' }
check_scm <- function(scm, num_rounds = NULL, overwrite = FALSE) {
  check_scm_impl(scm, num_rounds, overwrite)
}

#' Submit an SCM process to slurm
#'
#' @param scm A `hyperion_nonmem_scm` object or the SCM config (`<stem>scm.toml`).
#' @param num_rounds Pause after this many rounds; `NULL` uses the config.
#' @param partition Slurm partition for the model fits.
#' @param driver_partition Slurm partition for the driver job.
#' @param account Slurm account for the driver and the fits.
#' @param max_concurrent Cap on fits running at once.
#' @param shared_node Run every fit on one whole node instead of one slurm
#'   job per fit. Default `FALSE`.
#' @param overwrite Discard the SCM process already in the output directory
#'   and start fresh. Default `FALSE`.
#' @param verbose Include DEBUG logs in the driver's slurm log. Default `FALSE`.
#' @param allow_version_mismatch Submit even when the `pharos` on `PATH` is
#'   not the version hyperion expects. Default `FALSE`.
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

check_pharos_cli_version <- function(allow_mismatch = FALSE) {
  cli_info <- detect_pharos()
  if (is.na(cli_info$path)) {
    rlang::abort(
      "pharos CLI not found on PATH; the SCM driver job runs `pharos nonmem scm run`"
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

# Run `expr` with the inherited slurm variables unset, then restore them.
without_slurm_env <- function(expr) {
  old <- Sys.getenv(SLURM_INHERITED_VARS, unset = NA, names = TRUE)
  on.exit(if (any(set <- !is.na(old))) do.call(Sys.setenv, as.list(old[set])), add = TRUE)
  Sys.unsetenv(SLURM_INHERITED_VARS)
  expr
}

#' Stop an SCM process
#'
#' Stops the driver of a stepwise covariate modeling process
#'
#' @param scm A `hyperion_nonmem_scm` object or the SCM config (`<stem>scm.toml`).
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
#' Where the SCM process stands, as its driver's terminal would show it now:
#' the process facts, each decided round's decision and table, the open
#' round fit by fit (its slurm job, how long it has run, the latest
#' iteration and OFV read off its `.ext` file), and the driver's own state
#' (running, exited, or gone while the process still says running, with its
#' log). Mirrors `pharos nonmem scm status`.
#'
#' @param scm A `hyperion_nonmem_scm` object or the SCM config (`<stem>scm.toml`).
#'
#' @return A `hyperion_nonmem_scm_status` S3 object (also of class
#'   `hyperion_nonmem_scm_record`): the process record (`status`, `phase`,
#'   `candidates`, `retained`, `rounds`, `totals`, `models_running`, ...) as
#'   pharos writes it to `scm_summary.json`, plus a `driver` element, with
#'   attributes `text` (the status as pharos renders it), `out_dir` and
#'   `project_root`. Every path in the record is relative to `out_dir`.
#'   An unreadable `scm_driver.json` is a warning: the driver is read as
#'   never started.
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
