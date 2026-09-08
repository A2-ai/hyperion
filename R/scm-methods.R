# Stepwise covariate modeling (SCM) --------------------------------------
#
# hyperion sets up, plans and inspects; pharos executes. `scm_init()` writes
# the config file beside a model and creates the SCM output directory,
# `scm_plan()` builds the plan
# (an S3 object wrapping the pharos ScmPlan struct) and writes the plan.json
# to out_dir, `scm_run()` hands it to the pharos CLI in the
# background, and `scm_status()` checks on the SCM process in its entirety while
# it runs. pharos rewrites the decision log after every round (and leaves a
# round_summary.json/.md in each round directory as it concludes);
# `summary()` on a status reads the decision log into R as a data.frame.

#' Set up an SCM process for a model
#'
#' Writes the SCM config file beside `model` as `<model>-scm.toml` and
#' creates the `scm/<model>` directory the SCM process writes into. The
#' config comes with `direction` and every optional setting already filled
#' in at its default and `covariates` left empty: name the covariate effects
#' to be tested — by their `$PK` term, e.g. `["WT_CL", "CRCL_CL"]` — then
#' [scm_plan()] the file. Nothing is planned and nothing is fitted — the
#' candidates are yours to choose.
#'
#' The model is the *template* control stream: it carries the candidate
#' effects already written into `$PK`, each its own term over a single
#' THETA. How that theta is written is up to you — `(0 FIX)` for an effect
#' the template leaves out, or an ordinary theta carrying an initial guess.
#' `covariates` alone decides what is tested: pharos fixes every candidate
#' it is not testing at zero in the models it generates.
#'
#' @param model path to the template control stream (`.mod` / `.ctl`). The
#'   config file and the output directory are created beside it.
#' @param overwrite replace an existing `<model>-scm.toml` (default `FALSE`,
#'   so a config you have already filled in is never clobbered)
#'
#' @return invisibly, a list with `config` (the config file written) and
#'   `out_dir` (the output directory created)
#' @export
#'
#' @examples \dontrun{
#' setup <- scm_init("model/nonmem/PK/scm-demo.mod")
#' # fill in `covariates` in setup$config, then:
#' plan <- scm_plan(setup$config)
#' }
scm_init <- function(model, overwrite = FALSE) {
  if (!is.character(model) || length(model) != 1L || is.na(model)) {
    rlang::abort("`model` must be a single path to a control stream (.mod / .ctl)")
  }
  if (!file.exists(model)) {
    rlang::abort(paste0("model file not found: ", model))
  }
  if (!(isTRUE(overwrite) || isFALSE(overwrite))) {
    rlang::abort("`overwrite` must be TRUE or FALSE")
  }

  setup <- scm_init_impl(model = model, overwrite = isTRUE(overwrite))

  cli::cli_inform(c(
    "v" = "SCM setup created for {.file {model}}",
    "i" = "config: {.file {setup$config}}",
    "i" = "output directory: {.file {setup$out_dir}}",
    "*" = "fill in {.code covariates}, then {.code scm_plan(\"{setup$config}\")}"
  ))

  invisible(setup)
}

#' Plan a stepwise covariate modeling (SCM) process
#'
#' Reads the SCM process setup from an SCM config file (TOML) — the one
#' [scm_init()] wrote — validates the covariate candidates against the
#' user-authored template control stream it names, returns the plan as a
#' `hyperion_scm_plan` object, and writes it to `<out_dir>/plan.json` —
#' [scm_run()] and `pharos scm run` execute. Nothing is fitted. The template
#' carries the candidate effects already written into `$PK`; `covariates`
#' names the ones to test, and every model generated holds the rest out by
#' fixing their thetas at zero.
#'
#' The config file defines the SCM process; the `scm_plan()` call carries only
#' per-invocation control (`num_rounds`, overrides, `overwrite`):
#'
#' ```toml
#' model = "scm-demo.mod"
#' covariates = ["WT_CL", "CRCL_CL", "AGE_CL"]  # $PK term names
#' direction = ["forward", "backward"]
#'
#' # optional, at their defaults:
#' forward_alpha = 0.05
#' backward_alpha = 0.001
#' max_retries = 3
#' cov_step = false
#' release_init = 0.1
#' ```
#'
#' Relative paths in the config resolve against the config file's own
#' directory. The SCM process always writes into `scm/<model stem>` beside the
#' model — the directory [scm_init()] creates; that location is not
#' configurable. `covariates` names the candidate effects by their `$PK`
#' term: the names of the `$PK` assignments holding them, exactly as the
#' template's author wrote them (matched case-insensitively), each
#' referencing exactly one THETA. That name is the candidate's name
#' throughout the SCM process, and a theta comment that disagrees with it only
#' warns. THETA numbers are not accepted.
#'
#' @param config path to the SCM config file (TOML), as above
#' @param num_rounds pause the SCM process after this many rounds per
#'   [scm_run()] invocation; the SCM process is resumable. `NULL` = no cap.
#' @param max_retries override the config's retries per failed fit
#'   (config default 3). Retries are never jittered: each retry starts from
#'   wherever the previous attempt left off (its final estimates, or its
#'   last iteration if it never finished). `NULL` = use the config.
#' @param cov_step override whether generated models run the covariance step
#'   (`$COVARIANCE`, config default `FALSE`). `NULL` = use the config.
#' @param release_init override the initial estimate a released covariate
#'   theta starts at when the template gives it none (config default 0.1);
#'   a theta the template already carries an initial guess for starts from
#'   that instead, parameters already free in the round's reference fit
#'   continue from its estimates, and candidate thetas held out of a given
#'   model are fixed at zero. `NULL` = use the config.
#' @param overwrite replace existing SCM output from a *different* plan in
#'   `out_dir` (re-running the same plan resumes and needs no overwrite)
#'
#' @return A `hyperion_scm_plan` object; `plan.json` is already on disk in
#'   the output directory (its path is the `plan_path` attribute). Run it with
#'   [scm_run()]. When the out_dir already holds an SCM process, printing the plan
#'   also says where that SCM process got to and what this plan changed about the
#'   plan it replaced (the `context` attribute).
#' @export
#'
#' @examples \dontrun{
#' plan <- scm_plan("model/nonmem/PK/scm-demo-scm.toml")
#'
#' # pause after 2 rounds, and retry harder than the config says
#' plan <- scm_plan("model/nonmem/PK/scm-demo-scm.toml",
#'                  num_rounds = 2, max_retries = 5)
#' plan
#' scm_run(plan)
#' }
scm_plan <- function(config,
                     num_rounds = NULL,
                     max_retries = NULL,
                     cov_step = NULL,
                     release_init = NULL,
                     overwrite = FALSE) {
  if (!is.character(config) || length(config) != 1L || is.na(config)) {
    rlang::abort("`config` must be a single path to an SCM config file (TOML)")
  }
  if (!file.exists(config)) {
    rlang::abort(paste0("SCM config file not found: ", config))
  }
  if (!is.null(num_rounds)) {
    ok <- is.numeric(num_rounds) && length(num_rounds) == 1L &&
      !is.na(num_rounds) && is.finite(num_rounds) &&
      num_rounds %% 1 == 0 && num_rounds >= 1
    if (!ok) {
      rlang::abort("`num_rounds` must be a whole number >= 1, or NULL for no cap")
    }
  }
  if (!is.null(max_retries)) {
    ok <- is.numeric(max_retries) && length(max_retries) == 1L &&
      !is.na(max_retries) && is.finite(max_retries) &&
      max_retries %% 1 == 0 && max_retries >= 0
    if (!ok) {
      rlang::abort("`max_retries` must be a whole number >= 0, or NULL to use the config")
    }
  }
  if (!is.null(cov_step) && !(isTRUE(cov_step) || isFALSE(cov_step))) {
    rlang::abort("`cov_step` must be TRUE, FALSE, or NULL to use the config")
  }
  if (!is.null(release_init)) {
    ok <- is.numeric(release_init) && length(release_init) == 1L &&
      !is.na(release_init) && is.finite(release_init) && release_init != 0
    if (!ok) {
      rlang::abort("`release_init` must be a non-zero number, or NULL to use the config")
    }
  }

  plan <- scm_plan_impl(
    config = config,
    num_rounds = if (is.null(num_rounds)) NULL else as.integer(num_rounds),
    max_retries = if (is.null(max_retries)) NULL else as.integer(max_retries),
    cov_step = cov_step,
    release_init = release_init,
    overwrite = isTRUE(overwrite)
  )

  for (w in attr(plan, "warnings")) {
    rlang::warn(w)
  }

  # The plan is on disk the moment it exists --
  # <out_dir>/plan.json, ready for scm_run() or `pharos scm run`.
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

#' Run (or resume) an SCM process
#'
#' Launches `pharos nonmem scm run` in the background so pharos drives the whole
#' SCM process — building each round from the template, submitting the fits,
#' retrying failures from where they left off, scoring, and persisting
#' resumable state. The R session stays free; check on the SCM process with
#' [scm_status()].
#'
#' Everything that defines the SCM process lives in the plan (see [scm_plan()]),
#' including `num_rounds`, which paces how many rounds run before the SCM process
#' pauses. `scm_run()` takes only run control: where the fits run, how many
#' at a time, and which candidate breaks a tie the SCM process paused on.
#' Everything else (NONMEM version, account) comes from
#' pharos.toml and the pharos CLI defaults; the pharos executable itself is
#' found on the PATH, or set
#' `options(hyperion.pharos_exe = "/path/to/pharos")` to use another build.
#'
#' @param plan a `hyperion_scm_plan` from [scm_plan()], or a path to a
#'   plan.json
#' @param slurm fit rounds on the cluster (default `TRUE`); `FALSE` runs the
#'   fits locally on this machine
#' @param partition Slurm partition; `NULL` uses the pharos.toml / cluster
#'   default
#' @param max_concurrent how many of a round's fits run at once — the cap on
#'   Slurm jobs in flight (`0` = submit them all, no cap), or the number of
#'   fits run in parallel when `slurm = FALSE`. Further models start as
#'   earlier ones finish. `NULL` uses the pharos default (4 concurrent Slurm
#'   jobs; one local fit per core).
#' @param choose which tied candidate wins, when the SCM process has paused on a
#'   tie the scores could not break — [scm_status()] names the candidates and
#'   the round. Must be one of those candidates; the SCM process resumes from the
#'   still-open round, re-scoring it without refitting anything. The choice
#'   settles that one tie and is spent there: a later tie pauses for its own
#'   decision. `NULL` (the default) leaves a tie unresolved, so an SCM process
#'   paused on one pauses again at the same place.
#'
#' @return invisibly, a list with `out_dir`, `plan_path`, and `log` (the
#'   file the background run streams into)
#' @export
#'
#' @examples \dontrun{
#' scm_run(plan)
#' scm_run(plan, max_concurrent = 12)
#' scm_run(plan, choose = "CRCL_CL") # break a tie the SCM process paused on
#' scm_run("model/nonmem/scm/1001/plan.json", slurm = FALSE, max_concurrent = 4)
#' }
scm_run <- function(plan,
                    slurm = TRUE,
                    partition = NULL,
                    max_concurrent = NULL,
                    choose = NULL) {
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

  if (!is.null(max_concurrent)) {
    floor_ <- if (isTRUE(slurm)) 0 else 1
    ok <- is.numeric(max_concurrent) && length(max_concurrent) == 1L &&
      !is.na(max_concurrent) && is.finite(max_concurrent) &&
      max_concurrent %% 1 == 0 && max_concurrent >= floor_
    if (!ok) {
      # 0 means "no cap" only on slurm; locally it would mean no fits at all
      rlang::abort(paste0(
        "`max_concurrent` must be a whole number >= ", floor_,
        if (isTRUE(slurm)) " (0 = no cap)" else " when `slurm = FALSE`",
        ", or NULL for the pharos default"
      ))
    }
  }

  if (!is.null(choose)) {
    ok <- is.character(choose) && length(choose) == 1L &&
      !is.na(choose) && nzchar(trimws(choose))
    if (!ok) {
      rlang::abort(paste(
        "`choose` must be a single candidate name -- the one to win the tie",
        "the SCM process paused on -- or NULL"
      ))
    }
    choose <- trimws(choose)
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
        "pharos executable not found on PATH; install pharos to run an SCM process"
      )
    }
    found$path
  }

  # slurm is the pharos default; only opting out needs a flag. `nonmem scm`
  # is the long spelling of `pharos scm` -- both work, and the long one also
  # works against a pharos predating the top-level `scm`.
  args <- c("nonmem", "scm", "run", "--plan", plan_path)
  if (!isTRUE(slurm)) {
    args <- c(args, "--local")
  }
  if (!is.null(partition)) {
    args <- c(args, "--partition", partition)
  }
  if (!is.null(max_concurrent)) {
    # the cluster caps jobs in flight; locally it is the parallel fit count
    flag <- if (isTRUE(slurm)) "--max-concurrent" else "--num-parallel"
    args <- c(args, flag, format(as.integer(max_concurrent)))
  }
  if (!is.null(choose)) {
    # pharos rejects a name that is not one of the tied candidates; the run
    # is backgrounded, so that refusal lands in the log rather than here
    args <- c(args, "--choose", choose)
  }
  log_file <- file.path(out_dir, "scm_run.log")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  system2(
    pharos_path, args,
    stdout = log_file, stderr = log_file, wait = FALSE
  )
  cli::cli_inform(c(
    "v" = "SCM process launched in the background",
    "i" = "log: {.file {log_file}}",
    "i" = "check on it with {.code scm_status(\"{out_dir}\")}"
  ))

  invisible(list(out_dir = out_dir, plan_path = plan_path, log = log_file))
}

#' Check on an SCM process in its entirety
#'
#' Reads the plan and the persistent SCM process state — rounds completed, the
#' decision each round made, retries used, models running right now, the
#' current reference model and OFV — wherever the SCM process currently stands
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
    # Where pharos releases the effect the first time it is tested: the
    # template's own initial estimate for the theta when it has one, else
    # `release_init`. Plans from an older pharos carry no per-candidate
    # value, so fall back to the option.
    init = vapply(
      x$candidates,
      function(c) if (is.null(c$init)) NA_real_ else as.numeric(c$init),
      numeric(1)
    ),
    stringsAsFactors = FALSE
  )
  candidates$init[is.na(candidates$init)] <- as.numeric(x$options$release_init)
  direction <- unlist(x$options$direction)
  n <- nrow(candidates)
  # pharos computes max_models into the plan; recompute only for a plan
  # object predating the field
  max_models <- x$max_models
  if (is.null(max_models) || as.integer(max_models) == 0L) {
    max_models <- 1L + length(direction) * n * (n + 1) / 2
  }
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
    max_models = as.integer(max_models),
    context = scm_plan_context_parts(x)
  )
}

#' Tidy the `context` attribute a freshly built plan carries: where the
#' SCM process in its out_dir already stands, and what the plan changed about the
#' plan.json it replaced. `NULL` for a plan with nothing behind it -- a fresh
#' out_dir, or a plan object from an older hyperion.
#' @noRd
scm_plan_context_parts <- function(x) {
  ctx <- attr(x, "context")
  if (is.null(ctx)) {
    return(NULL)
  }
  had_previous <- isTRUE(unlist(ctx$had_previous_plan))
  progress <- ctx$progress
  if (!had_previous && is.null(progress)) {
    # A fresh out_dir: the plan is the whole story.
    return(NULL)
  }

  if (!is.null(progress)) {
    tie <- progress$pending_tie
    current <- progress$current_round
    progress <- list(
      status = unlist(progress$status),
      phase = unlist(progress$phase),
      rounds_complete = as.integer(unlist(progress$rounds_complete)),
      current_round = if (is.null(current)) {
        NULL
      } else {
        list(
          name = unlist(current$name),
          concluded = as.integer(unlist(current$concluded)),
          total = as.integer(unlist(current$total))
        )
      },
      retained = unlist(progress$retained) %||% character(),
      final_model = unlist(progress$final_model),
      models_running = as.integer(unlist(progress$models_running)),
      updated = unlist(progress$updated),
      tie_candidates = if (is.null(tie)) NULL else unlist(tie$candidates),
      tie_round = if (is.null(tie)) NULL else unlist(tie$round)
    )
  }

  changes <- lapply(ctx$changes, function(c) {
    list(
      field = unlist(c$field),
      detail = unlist(c$detail),
      scm_defining = isTRUE(unlist(c$scm_defining))
    )
  })

  list(
    had_previous_plan = had_previous,
    progress = progress,
    changes = changes,
    state_is_stale = isTRUE(unlist(ctx$state_is_stale))
  )
}

#' The progress half of the context as display lines, in the order both the
#' print and the knit_print method show them.
#' @noRd
scm_context_progress_lines <- function(ctx) {
  p <- ctx$progress
  if (is.null(p)) {
    return(c(progress = paste(
      "not started -- the out_dir holds a plan but no SCM state"
    )))
  }

  rounds <- switch(as.character(min(p$rounds_complete, 2L)),
    "0" = "no rounds complete yet",
    "1" = "1 round complete",
    paste0(p$rounds_complete, " rounds complete")
  )
  phase <- if (is.null(p$phase)) "" else paste0(", ", p$phase, " phase")

  lines <- c(
    progress = paste0(
      p$status, " -- ", rounds, phase, " (updated ", p$updated, ")"
    ),
    selected = if (length(p$retained)) {
      paste(p$retained, collapse = ", ")
    } else {
      "none"
    }
  )
  if (!is.null(p$current_round)) {
    lines["in round"] <- paste0(
      p$current_round$name, " -- ", p$current_round$concluded, "/",
      p$current_round$total, " concluded"
    )
  }
  if (isTRUE(p$models_running > 0)) {
    lines["running"] <- paste0(p$models_running, " model(s)")
  }
  if (!is.null(p$tie_candidates)) {
    # the same note pharos prints, in the call form an R user can act on
    lines["awaiting"] <- paste0(
      "your decision on ", paste(p$tie_candidates, collapse = " / "),
      " -- they tied in ", p$tie_round, "; re-run with ",
      "scm_run(choose = \"", p$tie_candidates[1], "\")"
    )
  }
  if (!is.null(p$final_model)) {
    lines["final model"] <- p$final_model
  }
  lines
}

#' The one consequence of a changed plan the user has to act on.
#' @noRd
scm_context_stale_note <- function(ctx) {
  if (!isTRUE(ctx$state_is_stale)) {
    return(NULL)
  }
  paste(
    "The SCM process in out_dir belongs to the previous plan and cannot resume",
    "under this one; re-plan with `overwrite = TRUE` to discard it and start",
    "the SCM process fresh."
  )
}

#' Print method for hyperion_scm_plan objects
#'
#' The canonical, complete display of a plan — candidates, alphas, retry
#' policy, and the worst-case size. A plan built over an out_dir that
#' already holds an SCM process says so too: how far it got, which
#' covariates it has selected, and what this plan changed about the plan it
#' replaced — a sketch, where [scm_status()] and [scm_summary()] are the
#' detailed views. There is deliberately no
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
      "{.strong {parts$candidates$name[i]}} THETA({parts$candidates$theta[i]}) -> released at {parts$candidates$init[i]} when first tested"
    )
  }
  cli::cli_h2("SCM size")
  cli::cli_text(
    "{parts$n_candidates} candidate{?s}; max models {parts$max_models} (incl. reference fit, excl. retries)"
  )

  ctx <- parts$context
  if (!is.null(ctx)) {
    cli::cli_h2("Where the SCM process stands")
    lines <- scm_context_progress_lines(ctx)
    for (label in names(lines)) {
      cli::cli_text("{.strong {label}:} {lines[[label]]}")
    }

    if (isTRUE(ctx$had_previous_plan)) {
      cli::cli_h2("Changes from the previous plan")
      if (!length(ctx$changes)) {
        cli::cli_text("none - identical to the previous plan")
      } else {
        cli::cli_ul(vapply(ctx$changes, scm_change_line, character(1)))
      }
    }
    note <- scm_context_stale_note(ctx)
    if (!is.null(note)) {
      cli::cli_alert_warning(note)
    }
  }
  invisible(x)
}

#' One line for one plan change, flagging the ones that cost the SCM process its
#' resumable state.
#' @noRd
scm_change_line <- function(c) {
  paste0(
    c$field, ": ", c$detail,
    if (isTRUE(c$scm_defining)) " (SCM-defining)" else ""
  )
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
    "| candidate | theta | first release |",
    "|---|---|---|",
    sprintf(
      "| %s | THETA(%d) | %s |",
      parts$candidates$name,
      parts$candidates$theta,
      format(parts$candidates$init)
    ),
    "",
    sprintf(
      "%d candidate%s; max models %d (incl. reference fit, excl. retries)",
      parts$n_candidates,
      if (parts$n_candidates == 1) "" else "s",
      parts$max_models
    ),
    ""
  )

  ctx <- parts$context
  if (!is.null(ctx)) {
    lines <- scm_context_progress_lines(ctx)
    output <- c(
      output,
      "#### Where the SCM process stands",
      "",
      paste0("- **", names(lines), ":** ", unname(lines)),
      ""
    )
    if (isTRUE(ctx$had_previous_plan)) {
      output <- c(
        output,
        "#### Changes from the previous plan",
        "",
        if (!length(ctx$changes)) {
          "none - identical to the previous plan"
        } else {
          paste0("- ", vapply(ctx$changes, scm_change_line, character(1)))
        },
        ""
      )
    }
    note <- scm_context_stale_note(ctx)
    if (!is.null(note)) {
      output <- c(output, paste0("**Note:** ", note), "")
    }
  }
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

#' Drill into one round of an SCM process
#'
#' Where [scm_status()] shows the whole SCM process one line per round,
#' `scm_summary()` shows everything about a single round: every model file
#' run in it — every retry included — with its outcome, the round's
#' reference model and OFV, per-candidate scoring (ΔOFV, p-value,
#' significance, selection), the heuristic checks that fired, and where the
#' round's own `round_summary.md` lives (the full per-run record, heuristics
#' included).
#'
#' @param x a `hyperion_scm_plan`, an SCM output directory, or a plan.json
#'   path
#' @param round which round: the Nth SCM round (`2` or `"round 2"` — the
#'   reference fit is not a round), a round name (`"forward_round1"`,
#'   `"backward_round1"`), or `"reference"`
#'
#' @return a `hyperion_scm_round` object; print it for the rendered view
#' @export
#'
#' @examples \dontrun{
#' scm_summary(plan, 2)
#' scm_summary("model/nonmem/PK/scm/scm-demo", "round 2")
#' scm_summary(plan, "backward_round1")
#' }
scm_summary <- function(x, round) {
  out_dir <- scm_out_dir(x)
  if (missing(round)) {
    rlang::abort(
      "`round` is required: a number (2), \"round 2\", a round name (\"forward_round1\"), or \"reference\""
    )
  }
  if (is.numeric(round)) {
    ok <- length(round) == 1L && !is.na(round) && is.finite(round) &&
      round %% 1 == 0 && round >= 1
    if (!ok) {
      rlang::abort("`round` must be a whole number >= 1, or a round name")
    }
    round <- as.character(as.integer(round))
  }
  if (!is.character(round) || length(round) != 1L || is.na(round) ||
        !nzchar(trimws(round))) {
    rlang::abort(
      "`round` must be a round number or name, e.g. 2, \"round 2\", or \"forward_round1\""
    )
  }
  scm_summary_impl(path = out_dir, round = round)
}

#' Print method for hyperion_scm_round objects
#'
#' @param x a `hyperion_scm_round`
#' @param ... ignored
#' @return invisible copy of x
#' @exportS3Method base::print hyperion_scm_round
print.hyperion_scm_round <- function(x, ...) {
  # pharos renders the round; printing its text verbatim keeps hyperion and
  # `pharos nonmem scm summary` from ever drifting apart.
  cat(attr(x, "rendered"), "\n")
  invisible(x)
}

#' Knit print method for hyperion_scm_round objects
#'
#' @param x a `hyperion_scm_round`
#' @param ... ignored
#' @return knitr asis output
#' @exportS3Method knitr::knit_print hyperion_scm_round
knit_print.hyperion_scm_round <- function(x, ...) {
  rendered <- attr(x, "rendered")
  output <- c("```", strsplit(rendered, "\n")[[1]], "```", "")
  knitr::asis_output(paste(output, collapse = "\n"))
}

#' Summarize an SCM process: the decision log as a data.frame
#'
#' Returns the decision log — every model fitted, every attempt, ΔOFV
#' (candidate − reference, negative when the candidate improves), degrees of
#' freedom, p-values, heuristic checks that fired, and each round's decision.
#' pharos rewrites `scm_decision_log.csv` and `scm_decision_log.md` in the
#' SCM output directory after every round; this is how you get the same
#' record into R (re-writing the files by default, so they always match the
#' current state — useful mid-run too).
#'
#' @param object a `hyperion_scm_status` from [scm_status()]
#' @param write whether to (re)write the decision log files (default `TRUE`)
#' @param ... ignored
#'
#' @return the decision log as a data.frame, with attributes
#'   `files_written`, `retained`, and (when the SCM process completed)
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
