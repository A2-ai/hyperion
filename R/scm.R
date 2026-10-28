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
#'   the effects as [ScmEffect] objects under `covariates$effects`.
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
  new_scm(x, path)
}

new_scm <- function(x, path) {
  path <- normalizePath(path, mustWork = FALSE)
  attr(x, "config_source") <- to_config_relative(path)
  attr(x, "filename") <- scm_stem(path)
  class(x) <- "hyperion_nonmem_scm"
  x
}

scm_stem <- function(path) {
  sub(paste0(SCM_CONFIG_SUFFIX, "$"), "", basename(path))
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

check_scm_object <- function(scm) {
  if (!inherits(scm, "hyperion_nonmem_scm")) {
    rlang::abort("`scm` must be a hyperion_nonmem_scm object from read_scm() or init_scm()")
  }
  invisible(scm)
}

#' Write an SCM config
#'
#' @param scm A `hyperion_nonmem_scm` object.
#' @param path Where to write; defaults to the config the object was read from.
#' @return The object, invisibly, with `config_source` pointing at `path`.
#' @export
#'
#' @examples \dontrun{
#' scm <- read_scm("model/nonmem/scm/run001scm.toml")
#' scm$forward_alpha <- 0.01
#' scm <- add_scm_effect(scm, "AGE_CL")
#' write_scm(scm)
#' }
write_scm <- function(scm, path = NULL) {
  check_scm_object(scm)
  if (is.null(path)) {
    path <- from_config_relative(attr(scm, "config_source"))
  }
  check_scm_config_path(path)
  scm$covariates <- normalize_covariates(scm$covariates)
  exists <- file.exists(path)
  toml <- if (exists) tomledit::read_toml(path) else tomledit::toml()
  # tomledit drops a key's comments when it replaces the key: rewrite changed keys only.
  fields <- scm_fields_as_rows(scm)
  current <- if (exists) scm_fields_as_rows(read_scm(path)) else list()
  changed <- intersect(scm_changed_keys(current, fields), names(fields))
  if (length(changed)) {
    toml <- do.call(tomledit::insert_items, c(list(toml), fields[changed]))
  }
  tomledit::write_toml(toml, path)
  invisible(new_scm(scm, path))
}

# Equal as TOML values: an integer and the same double are one value.
scm_value_equal <- function(a, b) {
  if (is.null(a) || is.null(b)) {
    return(is.null(a) && is.null(b))
  }
  isTRUE(all.equal(a, b, check.attributes = FALSE))
}

# The keys whose value differs between two configs.
scm_changed_keys <- function(from, to) {
  keys <- union(names(from), names(to))
  keys[!vapply(keys, function(k) scm_value_equal(from[[k]], to[[k]]), logical(1))]
}

#' Initialize an SCM process for a model
#'
#' Writes the starter config (`<stem>scm.toml`) and the SCM directory beside
#' the model, as `pharos nonmem scm init` does, with the candidate effects
#' filled in.
#'
#' @param model Path to the initial model (`.mod`/`.ctl`), or a
#'   `hyperion_nonmem_model` object.
#' @param effects The candidate effects, each named by its `$THETA` comment in
#'   the model: a character vector of names, or a list whose elements are a
#'   name, an [ScmEffect], or a row `list(name = , type = , initial = ,
#'   fixed = , lower = , upper = )` (`type` is `"continuous"`, the default, or
#'   `"categorical"`; the numbers are optional). `NULL` writes the config with
#'   no effects, to be added with [add_scm_effect()] and [write_scm()] or by
#'   hand.
#' @param overwrite Replace an existing config. Default `FALSE`.
#' @return The new `hyperion_nonmem_scm` object, invisibly.
#' @seealso [add_scm_effect()], [remove_scm_effect()], [ScmEffect]
#' @export
#'
#' @examples \dontrun{
#' scm <- init_scm("model/nonmem/run001.mod", effects = c("WT_CL", "CRCL_CL"))
#' check_scm(scm)
#'
#' init_scm(
#'   "model/nonmem/run001.mod",
#'   effects = list(
#'     "WT_CL",
#'     ScmEffect("SEX_V", type = "categorical", initial = 1, lower = 0, upper = 5)
#'   ),
#'   overwrite = TRUE
#' )
#' }
init_scm <- function(model, effects = NULL, overwrite = FALSE) {
  effects <- normalize_effects(effects)
  res <- init_scm_impl(model, overwrite)
  scm <- read_scm(res$config_path)
  if (length(effects)) {
    scm$covariates$effects <- effects
    scm <- write_scm(scm)
  }
  config_shown <- attr(scm, "config_source")
  out_dir_shown <- to_config_relative(res$out_dir)
  cli::cli_alert_success("SCM config written: {.path {config_shown}}")
  cli::cli_alert_info("SCM directory: {.path {out_dir_shown}}")
  if (length(effects) == 0) {
    cli::cli_alert_warning(SCM_NO_EFFECTS)
  }
  cli::cli_text(
    "{.fn check_scm} shows what a submit would do; {.fn submit_scm_to_slurm} runs it."
  )
  invisible(scm)
}

check_scm_written <- function(scm) {
  if (!inherits(scm, "hyperion_nonmem_scm")) {
    return(invisible(scm))
  }
  path <- from_config_relative(attr(scm, "config_source"))
  if (!file.exists(path)) {
    rlang::abort(paste0(
      "the SCM config ",
      attr(scm, "config_source"),
      " does not exist; call write_scm() first"
    ))
  }
  if (length(scm_changed_keys(scm_fields_as_rows(read_scm(path)), scm_fields_as_rows(scm)))) {
    rlang::abort(paste0(
      "`scm` has changes that are not written to ",
      attr(scm, "config_source"),
      "; call write_scm() first"
    ))
  }
  invisible(scm)
}

# The config as a plain list with the effects as rows, for comparison.
scm_fields_as_rows <- function(scm) {
  fields <- unclass(scm)
  attributes(fields) <- list(names = names(fields))
  fields$covariates <- covariates_as_rows(fields$covariates)
  fields
}

#' Check an SCM config: the plan it resolves to and its warnings
#'
#'
#' @param scm A `hyperion_nonmem_scm` object from [read_scm()] or [init_scm()],
#'   the path to the SCM config (`<stem>scm.toml`), or the SCM output directory.
#' @param num_rounds Pause after this many rounds; `NULL` uses the config's
#'   `num_rounds`, `0L` runs all rounds.
#' @param overwrite Show the plan as a submit with `overwrite = TRUE` would run it.
#'
#' @return A `hyperion_nonmem_scm_plan` S3 object: the plan (`model`,
#'   `out_dir`, `candidates`, `options`, ...) with attributes `text` (the plan
#'   as pharos renders it), `warnings`, `resumable`, `changes`, `out_dir` and
#'   `project_root`.
#' @export
#'
#' @examples \dontrun{
#' scm <- read_scm("model/nonmem/scm/run001scm.toml")
#' check_scm(scm)
#' check_scm(scm, num_rounds = 2)
#' }
check_scm <- function(scm, num_rounds = NULL, overwrite = FALSE) {
  check_scm_written(scm)
  check_scm_impl(scm, num_rounds, overwrite)
}

#' Submit an SCM process to slurm
#'
#' @param scm A `hyperion_nonmem_scm` object, the SCM config
#'   (`<stem>scm.toml`), or the SCM output directory.
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
  check_scm_written(scm)
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
    "Follow it with {.code get_scm_status(\"{res$out_dir}\")}; stop it with {.code stop_scm(\"{res$out_dir}\")}."
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
#' @param scm A `hyperion_nonmem_scm` object, the SCM config
#'   (`<stem>scm.toml`), or the SCM output directory.
#' @return `NULL`, after printing what was stopped and cancelled.
#' @export
#'
#' @examples \dontrun{
#' stop_scm("model/nonmem/scm/run001")
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
  resume <- res$config %||% paste0("<", res$out_dir, "'s scm config>")
  cli::cli_text(
    "Follow it with {.code get_scm_status(\"{res$out_dir}\")}; resume it with {.code submit_scm_to_slurm(\"{resume}\")}."
  )
  NULL
}
