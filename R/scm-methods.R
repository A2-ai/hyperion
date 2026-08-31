# Stepwise covariate modeling (SCM) --------------------------------------
#
# hyperion plans and inspects; pharos executes. `scm_plan()` builds the plan
# (an S3 object wrapping the pharos ScmPlan struct) and writes the plan.json
# to out_dir, `scm_run()` hands it to the pharos CLI in the
# background, and `scm_status()` checks on the search in its entirety while
# it runs. pharos rewrites the decision log after every round (and leaves a
# round_summary.json/.md in each round directory as it concludes);
# `summary()` on a status reads the decision log into R as a data.frame.

#' Plan a stepwise covariate modeling (SCM) search
#'
#' Validates the covariate candidates against a user-authored
#' template control stream, returns the plan as a `hyperion_scm_plan`
#' object, and writes it to `<out_dir>/plan.json` —
#' [scm_run()] and `pharos nonmem scm run` execute. Nothing is fitted. The
#' template carries the candidate effects already written into `$PK` and
#' fixed to zero in `$THETA` (`(0 FIX)`); the tool releases and tests them.
#'
#' @param mod path to the template control stream (.mod/.ctl)
#' @param covariates 1-based THETA numbers of the candidate covariate
#'   effects, as numbers only — e.g. `c(6, 7, 11)` or `6:11`. Candidate names
#'   are read from each theta's comment in whatever form it takes (`; WT_CL`,
#'   `; WT_CL cov`, `; WT_CL (L/h)`, and the numbered style
#'   `; 6 WT_CL WT on clearance` all name `WT_CL`), and fall back to
#'   `THETA<n>` for a theta with no comment.
#' @param direction which phases to run: `"forward"`, `"backward"`, or
#'   `c("forward", "backward")` (forward runs first). Required — there is
#'   no default, matching `pharos nonmem scm plan`.
#' @param out_dir output directory for the search. `NULL` (default) uses
#'   `scm/<model name>` beside the model.
#' @param forward_alpha significance level for adding a covariate in forward
#'   selection (default 0.05)
#' @param backward_alpha significance level for keeping a covariate in
#'   backward elimination (default 0.001)
#' @param num_rounds pause the search after this many rounds per
#'   [scm_run()] invocation; the search is resumable. `NULL` = no cap.
#' @param max_retries retries per failed fit (default 3). Retries are never
#'   jittered: each retry starts from wherever the previous attempt left off
#'   (its final estimates, or its last iteration if it never finished).
#' @param release_init initial estimate a newly released covariate theta
#'   starts at (default 0.1); parameters already free in the round's
#'   reference fit continue from its estimates, and covariate thetas not in
#'   a given model stay `0 FIX`.
#' @param cov_step whether generated models run the covariance step
#'   (`$COVARIANCE`, default `TRUE`)
#' @param overwrite replace existing SCM output from a *different* plan in
#'   `out_dir` (re-running the same plan resumes and needs no overwrite)
#'
#' @return A `hyperion_scm_plan` object; `plan.json` is already on disk in
#'   `out_dir` (its path is the `plan_path` attribute). Run it with
#'   [scm_run()].
#' @export
#'
#' @examples \dontrun{
#' plan <- scm_plan(
#'   mod        = "model/nonmem/1001.mod",
#'   covariates = c(6, 7, 8, 9, 10, 11),
#'   direction  = c("forward", "backward")
#' )
#' plan
#' scm_run(plan)
#' }
scm_plan <- function(mod,
                     covariates,
                     direction,
                     out_dir = NULL,
                     forward_alpha = 0.05,
                     backward_alpha = 0.001,
                     num_rounds = NULL,
                     max_retries = 3,
                     cov_step = TRUE,
                     overwrite = FALSE,
                     release_init = 0.1) {
  if (!is.character(mod) || length(mod) != 1L) {
    rlang::abort("`mod` must be a single path to a control stream")
  }
  if (!is.numeric(covariates)) {
    rlang::abort(
      "`covariates` must be THETA numbers, e.g. `c(6, 7, 11)` or `6:11`"
    )
  }
  if (any(covariates != as.integer(covariates))) {
    rlang::abort("`covariates` must be whole THETA numbers")
  }
  if (missing(direction)) {
    rlang::abort(
      "`direction` is required: \"forward\", \"backward\", or c(\"forward\", \"backward\")"
    )
  }
  direction <- match.arg(direction, c("forward", "backward"), several.ok = TRUE)

  plan <- scm_plan_impl(
    model = mod,
    covariates = as.integer(covariates),
    direction = direction,
    out_dir = out_dir,
    forward_alpha = forward_alpha,
    backward_alpha = backward_alpha,
    num_rounds = if (is.null(num_rounds)) NULL else as.integer(num_rounds),
    max_retries = as.integer(max_retries),
    release_init = release_init,
    cov_step = isTRUE(cov_step),
    overwrite = isTRUE(overwrite)
  )

  for (w in attr(plan, "warnings")) {
    rlang::warn(w)
  }

  # The plan is on disk the moment it exists --
  # <out_dir>/plan.json, ready for scm_run() or `pharos nonmem scm run`.
  cli::cli_inform("plan written to {.file {attr(plan, 'plan_path')}}")

  plan
}

#' Resolve a plan object / out_dir / plan.json path to the SCM out_dir
#' @noRd
scm_out_dir <- function(x) {
  if (inherits(x, "hyperion_scm_plan")) {
    return(x$out_dir)
  }
  if (is.character(x) && length(x) == 1L) {
    if (dir.exists(x)) {
      return(x)
    }
    if (file.exists(x)) {
      return(dirname(x))
    }
    rlang::abort(paste0("no SCM output found at ", x))
  }
  rlang::abort(
    "expected a hyperion_scm_plan, an SCM out_dir, or a plan.json path"
  )
}

#' Run (or resume) an SCM search
#'
#' Launches `pharos nonmem scm run` in the background so pharos drives the whole
#' search — building each round from the template, submitting the fits,
#' retrying failures from where they left off, scoring, and persisting
#' resumable state. The R session stays free; check on the search with
#' [scm_status()].
#'
#' Everything that defines the search lives in the plan (see [scm_plan()]).
#' `scm_run()` takes only run control: where the fits run, and how many
#' rounds to run this invocation. Everything else (NONMEM version, account,
#' concurrency caps) comes from pharos.toml and the pharos CLI defaults; the pharos executable itself is found on the
#' PATH, or set `options(hyperion.pharos_exe = "/path/to/pharos")` to use
#' another build.
#'
#' @param plan a `hyperion_scm_plan` from [scm_plan()], or a path to a
#'   plan.json
#' @param slurm fit rounds on the cluster (default `TRUE`); `FALSE` runs the
#'   fits locally on this machine
#' @param partition Slurm partition; `NULL` uses the pharos.toml / cluster
#'   default
#' @param num_rounds override the plan's `num_rounds` for this invocation
#'   only — the plan file is not modified and resumability is unaffected. A
#'   number pauses after that many rounds now; `Inf` runs to completion
#'   regardless of the plan's cap. `NULL` (default) honors the plan.
#'
#' @return invisibly, a list with `out_dir`, `plan_path`, and `log` (the
#'   file the background run streams into)
#' @export
#'
#' @examples \dontrun{
#' scm_run(plan)
#' scm_run("model/nonmem/scm/1001/plan.json", slurm = FALSE)
#' scm_run(plan, num_rounds = 1)    # just one more round, then pause
#' scm_run(plan, num_rounds = Inf)  # ignore the plan's cap, run to the end
#' }
scm_run <- function(plan,
                    slurm = TRUE,
                    partition = NULL,
                    num_rounds = NULL) {
  if (!is.null(num_rounds)) {
    ok <- is.numeric(num_rounds) && length(num_rounds) == 1L && !is.na(num_rounds) &&
      (is.infinite(num_rounds) && num_rounds > 0 ||
        (is.finite(num_rounds) && num_rounds %% 1 == 0 && num_rounds >= 1))
    if (!ok) {
      rlang::abort(
        "`num_rounds` must be a whole number >= 1, `Inf` to run to completion, or NULL to honor the plan"
      )
    }
  }
  if (inherits(plan, "hyperion_scm_plan")) {
    plan_path <- attr(plan, "plan_path")
    if (is.null(plan_path) || !file.exists(plan_path)) {
      rlang::abort(c(
        "this plan's plan.json is missing on disk",
        "i" = "rebuild it with `scm_plan()` before running"
      ))
    }
    out_dir <- plan$out_dir
  } else if (is.character(plan) && length(plan) == 1L && file.exists(plan)) {
    plan_path <- plan
    out_dir <- dirname(plan)
  } else {
    rlang::abort("`plan` must be a hyperion_scm_plan or a path to plan.json")
  }

  pharos_exe <- getOption("hyperion.pharos_exe", NULL)
  pharos_path <- if (!is.null(pharos_exe)) {
    if (!file.exists(pharos_exe)) {
      rlang::abort(paste0(
        "pharos executable not found at ", pharos_exe,
        " (from options(hyperion.pharos_exe))"
      ))
    }
    pharos_exe
  } else {
    found <- detect_pharos()
    if (is.na(found$path)) {
      rlang::abort(
        "pharos executable not found on PATH; install pharos to run an SCM search"
      )
    }
    found$path
  }

  args <- c("nonmem", "scm", "run", "--plan", plan_path)
  if (isTRUE(slurm)) {
    args <- c(args, "--slurm")
  }
  if (!is.null(partition)) {
    args <- c(args, "--partition", partition)
  }
  if (!is.null(num_rounds)) {
    args <- c(
      args, "--num-rounds",
      if (is.infinite(num_rounds)) "all" else format(as.integer(num_rounds))
    )
  }

  log_file <- file.path(out_dir, "scm_run.log")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  system2(
    pharos_path, args,
    stdout = log_file, stderr = log_file, wait = FALSE
  )
  cli::cli_inform(c(
    "v" = "SCM search launched in the background",
    "i" = "log: {.file {log_file}}",
    "i" = "check on it with {.code scm_status(\"{out_dir}\")}"
  ))

  invisible(list(out_dir = out_dir, plan_path = plan_path, log = log_file))
}

#' Check on an SCM search in its entirety
#'
#' Reads the plan and the persistent search state — rounds completed, the
#' decision each round made, retries used, models running right now, the
#' current reference model and OFV — wherever the search currently stands
#' (planned, running, paused, completed, or failed).
#'
#' @param x a `hyperion_scm_plan`, an SCM output directory, or a plan.json
#'   path
#'
#' @return a `hyperion_scm_status` object; its [summary()] method returns the
#'   decision log as a data.frame
#' @export
#'
#' @examples \dontrun{
#' st <- scm_status(plan)
#' st
#' summary(st)   # the decision log as a data.frame
#' }
scm_status <- function(x) {
  scm_status_impl(scm_out_dir(x))
}

# Display methods ---------------------------------------------------------

#' @noRd
scm_plan_display_parts <- function(x) {
  candidates <- data.frame(
    name = vapply(x$candidates, function(c) c$name, character(1)),
    theta = vapply(x$candidates, function(c) as.integer(c$theta), integer(1)),
    stringsAsFactors = FALSE
  )
  direction <- unlist(x$options$direction)
  n <- nrow(candidates)
  worst_case_per_phase <- n * (n + 1) / 2
  list(
    model = x$model,
    out_dir = x$out_dir,
    direction = paste(direction, collapse = " -> "),
    runs_forward = "forward" %in% direction,
    runs_backward = "backward" %in% direction,
    forward_alpha = x$options$forward_alpha,
    backward_alpha = x$options$backward_alpha,
    num_rounds = x$options$num_rounds,
    max_retries = x$options$max_retries,
    release_init = x$options$release_init,
    cov_step = isTRUE(x$options$cov_step),
    candidates = candidates,
    n_candidates = n,
    worst_case_fits = 1L +
      (if ("forward" %in% direction) worst_case_per_phase else 0L) +
      (if ("backward" %in% direction) worst_case_per_phase else 0L)
  )
}

#' Print method for hyperion_scm_plan objects
#'
#' The canonical, complete display of a plan — candidates, alphas, retry
#' policy, and the worst-case search size. There is deliberately no
#' `summary()` for a plan: a plan is a static declaration with nothing to
#' compute beyond what printing shows. (`summary()` on a *status* is
#' different — it returns the decision log; see
#' [summary.hyperion_scm_status()].)
#'
#' @param x a `hyperion_scm_plan`
#' @param ... ignored
#' @return invisible copy of x
#' @exportS3Method base::print hyperion_scm_plan
print.hyperion_scm_plan <- function(x, ...) {
  parts <- scm_plan_display_parts(x)

  cli::cli_h1("SCM plan")
  cli::cli_text("{.strong model:} {.file {parts$model}}")
  cli::cli_text("{.strong out dir:} {.file {parts$out_dir}}")
  cli::cli_text("{.strong direction:} {parts$direction}")
  if (parts$runs_forward) {
    cli::cli_text("{.strong forward:} alpha {parts$forward_alpha}")
  }
  if (parts$runs_backward) {
    cli::cli_text("{.strong backward:} alpha {parts$backward_alpha}")
  }
  cli::cli_text(
    "{.strong on failure:} retry up to {parts$max_retries}x from the previous attempt's estimates"
  )
  cli::cli_text(
    "{.strong cov step:} {if (parts$cov_step) 'on' else 'off'}"
  )
  if (!is.null(parts$num_rounds)) {
    cli::cli_text(
      "{.strong num rounds:} pause after {parts$num_rounds} (resumable)"
    )
  }
  cli::cli_h2("Candidates")
  for (i in seq_len(nrow(parts$candidates))) {
    cli::cli_text(
      "{.strong {parts$candidates$name[i]}} THETA({parts$candidates$theta[i]}) -> release {parts$release_init}"
    )
  }
  cli::cli_h2("Search size")
  cli::cli_text(
    "{parts$n_candidates} candidate{?s}; worst case {parts$worst_case_fits} fits (incl. reference, excl. retries)"
  )
  invisible(x)
}

#' Knit print method for hyperion_scm_plan objects
#'
#' @param x a `hyperion_scm_plan`
#' @param ... ignored
#' @return knitr asis output
#' @exportS3Method knitr::knit_print hyperion_scm_plan
knit_print.hyperion_scm_plan <- function(x, ...) {
  parts <- scm_plan_display_parts(x)

  output <- c(
    "### SCM plan",
    "",
    paste0("- **model:** `", parts$model, "`"),
    paste0("- **out dir:** `", parts$out_dir, "`"),
    paste0("- **direction:** ", parts$direction),
    if (parts$runs_forward) {
      paste0("- **forward:** alpha ", parts$forward_alpha)
    },
    if (parts$runs_backward) {
      paste0("- **backward:** alpha ", parts$backward_alpha)
    },
    paste0(
      "- **on failure:** retry up to ", parts$max_retries,
      "x from the previous attempt's estimates"
    ),
    paste0("- **cov step:** ", if (parts$cov_step) "on" else "off"),
    if (!is.null(parts$num_rounds)) {
      paste0("- **num rounds:** pause after ", parts$num_rounds, " (resumable)")
    },
    "",
    "| candidate | theta | release |",
    "|---|---|---|",
    sprintf(
      "| %s | THETA(%d) | %s |",
      parts$candidates$name,
      parts$candidates$theta,
      format(parts$release_init)
    ),
    "",
    sprintf(
      "%d candidate%s; worst case %d fits (incl. reference, excl. retries)",
      parts$n_candidates,
      if (parts$n_candidates == 1) "" else "s",
      parts$worst_case_fits
    ),
    ""
  )
  knitr::asis_output(paste(output, collapse = "\n"))
}

#' Print method for hyperion_scm_status objects
#'
#' @param x a `hyperion_scm_status`
#' @param ... ignored
#' @return invisible copy of x
#' @exportS3Method base::print hyperion_scm_status
print.hyperion_scm_status <- function(x, ...) {
  # pharos renders the status; printing its text verbatim keeps hyperion and
  # `pharos nonmem scm status` from ever drifting apart.
  cat(attr(x, "rendered"), "\n")
  invisible(x)
}

#' Knit print method for hyperion_scm_status objects
#'
#' @param x a `hyperion_scm_status`
#' @param ... ignored
#' @return knitr asis output
#' @exportS3Method knitr::knit_print hyperion_scm_status
knit_print.hyperion_scm_status <- function(x, ...) {
  rendered <- attr(x, "rendered")
  output <- c("```", strsplit(rendered, "\n")[[1]], "```", "")
  knitr::asis_output(paste(output, collapse = "\n"))
}

#' Summarize an SCM search: the decision log as a data.frame
#'
#' Returns the decision log — every model fitted, every attempt, ΔOFV
#' (candidate − reference, negative when the candidate improves), degrees of
#' freedom, p-values, heuristic checks that fired, and each round's decision.
#' pharos rewrites `scm_decision_log.csv` and `scm_decision_log.md` in the
#' search's output directory after every round; this is how you get the same
#' record into R (re-writing the files by default, so they always match the
#' current state — useful mid-search too).
#'
#' @param object a `hyperion_scm_status` from [scm_status()]
#' @param write whether to (re)write the decision log files (default `TRUE`)
#' @param ... ignored
#'
#' @return the decision log as a data.frame, with attributes
#'   `files_written`, `retained`, and (when the search completed)
#'   `final_model`
#' @exportS3Method base::summary hyperion_scm_status
summary.hyperion_scm_status <- function(object, write = TRUE, ...) {
  log <- scm_decision_log_impl(object$out_dir, isTRUE(write))
  files <- attr(log, "files_written")
  if (length(files)) {
    cli::cli_inform("decision log written to {.file {files}}")
  }
  log
}
