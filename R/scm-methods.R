# Stepwise covariate modeling (SCM) --------------------------------------
#
# hyperion sets up, plans and inspects; pharos executes. `scm_init()` writes
# the config file beside a model and creates the SCM output directory,
# `scm_plan()` builds the plan
# (an S3 object wrapping the pharos ScmPlan struct) and writes the plan.json
# to out_dir, `scm_run()` hands it to the pharos CLI in the
# background, and `scm_status()` checks on the SCM process in its entirety while
# it runs -- the driver (recorded in scm_driver.json), each decided round's
# decision and table, and the open round fit by fit. pharos rewrites
# scm_summary.json after every round (and leaves a round_summary.json/.md in
# each round directory as it concludes); `scm_summary()` renders that record
# for every round to date or a selection of it, and `summary()` on a status
# reads it into R as a data.frame.
#
# Every rendering is pharos's own: the Rust side calls the same code
# `pharos nonmem scm plan / status / summary` runs, so hyperion and the CLI
# never disagree about an SCM process.

#' Set up an SCM process for a model
#'
#' Creates the directory the SCM process writes into — `scm/<model stem>`
#' beside the model, or wherever the project's `[nonmem.scm] out_dir`
#' template puts it — and puts the SCM config file inside it as
#' `<model stem>scm.toml`. The
#' config comes with `direction` and every optional setting already filled
#' in at its default and the `[covariates]` section's `effects` left empty:
#' name the covariate effects to be tested — by the name the initial model's
#' `$THETA` record gives each one, e.g. `effects = ["WT_CL", "CRCL_CL"]` —
#' then [scm_plan()] the file. Nothing is planned and nothing is fitted —
#' the candidates are yours to choose. The directory also gets a `.gitignore`
#' that tracks what the project's `[nonmem.scm] track_in_git` setting asks
#' for (`"milestones"` unless `pharos.toml` says otherwise: the final model,
#' the reference fit and the forward model's fit; `"final"` or `"all"` for
#' less or more), rewritten by every [scm_plan()] so a changed setting takes
#' effect at the next plan.
#'
#' The model is the *initial model*: it carries a theta for each candidate
#' effect, named either by a `$THETA` label (`$THETA WT_CL=(0, 0.4)`, or
#' `$THETA NAMES(WT_CL, CRCL_CL) ...`) or by the theta's comment
#' (`; WT_CL cov`, `; WT_CL`, `; 4 WT_CL WT on clearance`). Nothing else in
#' the model is read — `$PK` is not consulted, so an effect folded into a
#' structural expression is named the same way as one standing on its own,
#' and a candidate theta need not be written `(0 FIX)`: an ordinary theta
#' carrying a guess of its own is fine. `effects` alone decides what is
#' tested: pharos fixes every candidate it is not testing at its `fixed`
#' value (0 unless the config says otherwise) in the models it generates.
#'
#' @param model path to the initial model (`.mod` / `.ctl`). The output
#'   directory is created beside it, and the config file inside that.
#' @param overwrite replace an existing `<model stem>scm.toml` (default `FALSE`,
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
    "*" = "fill in {.code effects} under {.code [covariates]}, then {.code scm_plan(\"{setup$config}\")}"
  ))

  invisible(setup)
}

#' Plan a stepwise covariate modeling (SCM) process
#'
#' Reads the SCM process setup from an SCM config file (TOML) — the one
#' [scm_init()] wrote — validates the covariate candidates against the
#' user-authored initial model it names, returns the plan as a
#' `hyperion_scm_plan` object, and writes it to `<out_dir>/plan.json` —
#' [scm_run()] and `pharos nonmem scm submit` execute. Nothing is fitted. The initial model
#' names a theta for each candidate effect in its `$THETA` records; the
#' `[covariates]` section names the ones to test, and every model generated
#' holds the rest out by fixing their thetas at their `fixed` value.
#'
#' The config file defines the SCM process; the `scm_plan()` call carries only
#' per-invocation control (`num_rounds`, and `overwrite` to start over):
#'
#' ```toml
#' model = "../../scm-demo.mod"
#' direction = ["forward", "backward"]
#'
#' # optional, at their defaults:
#' forward_alpha = 0.05
#' backward_alpha = 0.001
#' max_retries = 3
#' cov_step = false
#' final_cov_step = true
#' forward_final_cov_step = true
#'
#' [covariates]
#' fixed = 0                     # default: what a held-out effect's theta is fixed at
#' continuous  = { initial = 0.1 } # default initial estimate for a continuous effect
#' categorical = { initial = 1 }   # default initial estimate for a categorical effect
#' effects = [
#'   "WT_CL", "CRCL_CL", "AGE_CL",                             # theta names, continuous, at the defaults
#'   { name = "SEXEFF_CL", type = "categorical", fixed = 1 },  # a fold-change effect: 1 = no effect
#'   { name = "WT_V", initial = 0.3, lower = 0, upper = 5 },   # its own estimate, bounded while it is in the model
#' ]
#' ```
#'
#' `final_cov_step` (default `TRUE`) re-fits the final model with
#' `$COVARIANCE` on once the SCM process finishes, whatever the rounds
#' themselves ran with: the SCM process chooses the covariates, and that one
#' fit is what reports their estimates with standard errors.
#' `forward_final_cov_step` (default `TRUE`) does the same for the forward
#' phase's model the moment forward selection ends, as a fit that runs
#' alongside backward elimination rather than holding it up; it only applies
#' when both phases run, and when `cov_step` is on the forward model has
#' already run the step and is used as it is. Backward elimination that drops
#' nothing leaves the forward model as the final model, and that fit of it
#' is copied into `final/` rather than fitted again. Neither `num_rounds` nor
#' `forward_final_cov_step` is SCM-defining: an SCM process resumes across a
#' change to either.
#'
#' Relative paths in the config resolve against the config file's own
#' directory. The config lives in the SCM process's own directory,
#' `scm/<model stem>` beside the model — the directory [scm_init()] creates —
#' which is why the generated `model` key reads `../../<model>`. Where that
#' directory lands is the project's call: `[nonmem.scm] out_dir` in
#' `pharos.toml` is a template rendered against the model directory
#' (`scm/{{name}}` unless the project says otherwise), and [scm_init()]
#' points the `model` key back up at the model from wherever it lands. The
#' template may not carry a timestamp — the SCM process has to be findable
#' again. `effects` names the candidate effects the way the initial
#' model's `$THETA` records name them, matched case-insensitively: a `$THETA`
#' label (`$THETA WT_CL=(0, 0.4)`, `$THETA NAMES(WT_CL, CRCL_CL) ...`), or
#' the name the theta's comment carries — `; WT_CL cov`, `; WT_CL`,
#' `; THETA4: WT_CL` and `; 4 WT_CL WT on clearance` all name `WT_CL`. `$PK`
#' is not read, so an effect written into a structural expression is named
#' exactly like one standing on its own. A name must pick out one theta: if
#' two thetas answer to it, planning stops and says which — rename one. Note
#' that several thetas on one `$THETA` record sharing a single comment all
#' take that comment's name, so write one theta per record or use `NAMES()`.
#' The name the model uses is the candidate's name throughout the SCM
#' process, whichever of its names the config asked for. THETA numbers are
#' not accepted. A bare name is a continuous effect, first tested at the
#' initial estimate its type's table gives it (`continuous = { initial = 0.1 }`,
#' `categorical = { initial = 1 }`, those two figures unless the config says
#' otherwise), and held out at the section's `fixed`. A row overrides both for
#' one effect and declares its `type`, which a fold-change form such as
#' `SEXEFF_CL = THETA(n)**SEX` needs (`type = "categorical"`, `fixed = 1`).
#' A per-type `initial` only reaches an effect whose `$THETA` is authored at
#' the held-out value (`(0 FIX)`): a theta the initial model already inits at
#' something else keeps that value. `initial` is a row-only key — the section
#' carries it per type, never flat. `fixed` was spelled `off` before; that
#' spelling is still accepted.
#'
#' `lower` and `upper`, on an effect's row, are the `$THETA` bounds that
#' effect is estimated under in every model that has it in — the usual
#' answer to a covariate whose estimation runs off somewhere absurd. Each
#' bound comes from the row, else the bound the initial model's
#' own `$THETA` spec carries, so a config that says nothing about bounds
#' leaves every candidate exactly as the initial model authored it. A held-out
#' effect is written `(fixed FIX)` and needs no bounds; `initial` must sit
#' strictly inside them, which the plan checks.
#'
#' Re-planning without a candidate that has never won a round is not a new
#' SCM process: [scm_run()] carries on without it, keeping every round already
#' fitted. Neither is a new `initial` or new bounds on a candidate — the
#' usual fix when a candidate fails on either. Re-plan with the edited config
#' and [scm_run()] picks up where it left off: the new values apply to every
#' model written from then on, and a candidate still in the open round is
#' refitted under them (its earlier attempts kept on record and on disk, but
#' never scored). Rounds already concluded keep the results they ran under.
#' Removing a candidate that has won a round, adding one, or changing any
#' other SCM-defining setting is a different SCM process: the plan is not
#' written and `scm_plan()` errors, saying why, until re-planned with
#' `overwrite = TRUE` — which discards the SCM process in the out_dir (its
#' fits, state and summaries; the config, `plan.json` and anything else in
#' the directory stay) once the new plan has validated. It is refused while
#' that process's driver is still running (see [scm_status()]), so a running
#' SCM process is never pulled out from under its fits.
#'
#' @param config path to the SCM config file (TOML), as above
#' @param num_rounds pause the SCM process after this many rounds per
#'   [scm_run()] invocation; the SCM process is resumable. `NULL` = no cap.
#' @param overwrite discard the SCM process already in `out_dir` and start
#'   fresh under this plan. Re-running the same plan, or one the process can
#'   resume under, needs no overwrite.
#'
#' @return A `hyperion_scm_plan` object; `plan.json` is already on disk in
#'   the output directory (its path is the `plan_path` attribute). Run it with
#'   [scm_run()]. When the out_dir already holds an SCM process, printing the plan
#'   also says where that SCM process got to and what this plan changed about the
#'   plan it replaced (the `context` attribute); what `overwrite` discarded is
#'   the `discarded` attribute.
#' @export
#'
#' @examples \dontrun{
#' plan <- scm_plan("model/nonmem/PK/scm/scm-demo/scm-demoscm.toml")
#'
#' # pause after 2 rounds, so the SCM process can be looked at as it goes
#' plan <- scm_plan("model/nonmem/PK/scm/scm-demo/scm-demoscm.toml",
#'                  num_rounds = 2)
#' plan
#' scm_run(plan)
#'
#' # start over under a changed config, discarding the fits so far
#' plan <- scm_plan("model/nonmem/PK/scm/scm-demo/scm-demoscm.toml",
#'                  overwrite = TRUE)
#' }
scm_plan <- function(config,
                     num_rounds = NULL,
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

  plan <- scm_plan_impl(
    config = config,
    num_rounds = if (is.null(num_rounds)) NULL else as.integer(num_rounds),
    overwrite = isTRUE(overwrite)
  )

  # `overwrite` acted on the out_dir once the plan had validated: say what
  # it threw away, as `pharos nonmem scm plan --overwrite` does.
  discarded <- attr(plan, "discarded")
  if (!is.null(discarded)) {
    cli::cli_inform(c(
      "i" = "discarded the SCM process in {.file {dirname(attr(plan, 'plan_path'))}} ({discarded})"
    ))
  }

  for (w in attr(plan, "warnings")) {
    rlang::warn(w)
  }

  # The plan is on disk the moment it exists --
  # <out_dir>/plan.json, ready for scm_run() or `pharos nonmem scm submit`.
  cli::cli_inform("plan written to {.file {attr(plan, 'plan_path')}}")

  plan
}

#' Resolve a plan object / out_dir / plan.json path to the SCM out_dir
#' @noRd
scm_out_dir <- function(x) {
  if (inherits(x, "hyperion_scm_plan")) {
    # a plan writes its paths relative to the pharos project root, so the
    # plan.json it was just saved to is what locates the out_dir on disk
    return(dirname(attr(x, "plan_path")))
  }
  if (inherits(x, "hyperion_scm_status")) {
    return(attr(x, "out_dir"))
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
#' Hands the plan to the pharos CLI, which drives the whole SCM process —
#' building each round from the initial model, submitting the fits, retrying
#' failures from where they left off, scoring, and persisting resumable
#' state. Fits always run on Slurm; `driver` picks where the process that
#' submits and scores them runs:
#'
#' * `"slurm"` (default) queues the driver as its own Slurm job
#'   (`pharos nonmem scm slurm submit`) and returns once it is queued.
#' * `"login"` runs the driver here, on the login node
#'   (`pharos nonmem scm submit`), in the background until the SCM process
#'   finishes or pauses. The R session stays free.
#'
#' Either way, check on the SCM process with [scm_status()]. Whatever the
#' mode, the driver records where it runs in `scm_driver.json` in the out_dir
#' (its Slurm job, or its pid and host) and writes a timestamped record of
#' what happens — each fit as it is submitted and as it ends, each round's
#' decision and the round at a glance — to the driver job's Slurm log, or
#' with `driver = "login"` to `scm_driver.log` in the out_dir. pharos refuses
#' to start a driver while another one for the same SCM process is queued or
#' alive, and `scm_run()` reports that refusal. With `shared_node`, the fits
#' run on one whole node: the driver's own with `driver = "slurm"`, or one
#' allocated for them with `driver = "login"` and released when the process
#' ends or is stopped.
#'
#' Fits submitted as Slurm jobs are independent of the driver: a driver that
#' stops (Ctrl-C, `scancel`, a time limit) leaves them running, and running
#' the same plan again resumes — a fit whose job is still in the queue is
#' waited for, not resubmitted. Fits on a shared node die with it and are
#' refitted on resume.
#'
#' Everything that defines the SCM process lives in the plan (see [scm_plan()]),
#' including `num_rounds`, which paces how many rounds run before the SCM process
#' pauses. `scm_run()` takes only run control: where the driver and the fits
#' run and how many fits run at once.
#' Everything else (NONMEM version) comes from
#' pharos.toml — where `[nonmem.scm]` also holds this project's own defaults
#' for `max_concurrent`, `partition`, `driver_partition` and `account`,
#' each overridden by the matching argument here, and `poll_interval`, the
#' seconds between the driver's checks on Slurm fits (30 unless set) — and
#' the pharos CLI defaults; the pharos executable itself is
#' found on the PATH, or set
#' `options(hyperion.pharos_exe = "/path/to/pharos")` to use another build.
#'
#' @param plan a `hyperion_scm_plan` from [scm_plan()], or a path to a
#'   plan.json
#' @param driver where the SCM driver runs: `"slurm"` (its own Slurm job) or
#'   `"login"` (this machine, in the background)
#' @param partition Slurm partition for the fits (with `shared_node`, the
#'   partition of the one node they all run on); `NULL` uses the project's
#'   `[nonmem.scm] partition`, else the cluster default
#' @param driver_partition Slurm partition for the driver job when
#'   `driver = "slurm"`; `NULL` uses the project's
#'   `[nonmem.scm] driver_partition`, else the cluster default. Not used with
#'   `shared_node`, where the driver runs on the fits' node
#' @param account Slurm account; `NULL` uses the project's
#'   `[nonmem.scm] account`
#' @param max_concurrent how many of a round's fits run at once (`0` = no
#'   cap). Further models start as earlier ones finish. `NULL` uses the
#'   project's `[nonmem.scm] max_concurrent`, else no cap: every ready fit is
#'   submitted at once (with `shared_node`, as many as the node's CPUs hold).
#' @param shared_node run every fit on one whole node instead of one Slurm
#'   job per fit
#' @param overwrite discard the SCM process already in the out_dir and run
#'   this plan from scratch (resuming needs no overwrite)
#'
#' @return invisibly, a list with `out_dir`, `plan_path`, and `log` (the
#'   file the pharos output went to; with `driver = "login"` the driver's own
#'   record is `scm_driver.log` in the out_dir)
#' @export
#'
#' @examples \dontrun{
#' scm_run(plan)
#' scm_run(plan, max_concurrent = 12)
#' scm_run(plan, shared_node = TRUE, partition = "big")
#' scm_run("model/nonmem/scm/1001/plan.json", driver = "login")
#' }
scm_run <- function(plan,
                    driver = c("slurm", "login"),
                    partition = NULL,
                    driver_partition = NULL,
                    account = NULL,
                    max_concurrent = NULL,
                    shared_node = FALSE,
                    overwrite = FALSE) {
  if (inherits(plan, "hyperion_scm_plan")) {
    plan_path <- attr(plan, "plan_path")
    if (is.null(plan_path) || !file.exists(plan_path)) {
      rlang::abort(c(
        "this plan's plan.json is missing on disk",
        "i" = "rebuild it with `scm_plan()` before running"
      ))
    }
    out_dir <- dirname(plan_path)
  } else if (is.character(plan) && length(plan) == 1L && file.exists(plan)) {
    plan_path <- plan
    out_dir <- dirname(plan)
  } else {
    rlang::abort("`plan` must be a hyperion_scm_plan or a path to plan.json")
  }

  driver <- rlang::arg_match(driver)

  for (arg in c("partition", "driver_partition", "account")) {
    value <- get(arg)
    if (!is.null(value) &&
      (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value))) {
      rlang::abort(paste0("`", arg, "` must be a single string, or NULL"))
    }
  }
  if (!(isTRUE(shared_node) || isFALSE(shared_node))) {
    rlang::abort("`shared_node` must be TRUE or FALSE")
  }
  if (!(isTRUE(overwrite) || isFALSE(overwrite))) {
    rlang::abort("`overwrite` must be TRUE or FALSE")
  }
  if (!is.null(driver_partition) && (driver != "slurm" || isTRUE(shared_node))) {
    rlang::abort(
      "`driver_partition` only applies to `driver = \"slurm\"` without `shared_node`"
    )
  }

  if (!is.null(max_concurrent)) {
    ok <- is.numeric(max_concurrent) && length(max_concurrent) == 1L &&
      !is.na(max_concurrent) && is.finite(max_concurrent) &&
      max_concurrent %% 1 == 0 && max_concurrent >= 0
    if (!ok) {
      rlang::abort(
        "`max_concurrent` must be a whole number >= 0 (0 = no cap), or NULL for the pharos default"
      )
    }
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

  args <- c(scm_run_subcommand(driver), plan_path)
  if (!is.null(driver_partition)) {
    args <- c(args, "--driver-partition", driver_partition)
  }
  if (!is.null(partition)) {
    args <- c(args, "--partition", partition)
  }
  if (!is.null(account)) {
    args <- c(args, "--account", account)
  }
  if (!is.null(max_concurrent)) {
    args <- c(args, "--max-concurrent", format(as.integer(max_concurrent)))
  }
  if (isTRUE(shared_node)) {
    args <- c(args, "--shared-node")
  }
  if (isTRUE(overwrite)) {
    args <- c(args, "--overwrite")
  }
  log_file <- file.path(out_dir, "scm_run.log")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  if (driver == "slurm") {
    # Queuing the driver returns at once, so wait for it and surface the
    # job id -- or the reason pharos refused.
    status <- system2(
      pharos_path, args,
      stdout = log_file, stderr = log_file, wait = TRUE
    )
    output <- readLines(log_file, warn = FALSE)
    if (!identical(status, 0L)) {
      # pharos output is passed through as-is, never interpolated
      rlang::abort(c(
        "pharos failed to submit the SCM driver",
        stats::setNames(output, rep("x", length(output))),
        "i" = paste0("log: ", log_file)
      ))
    }
    output <- output[nzchar(output)]
    rlang::inform(c(
      "v" = "SCM driver submitted to Slurm",
      stats::setNames(output, rep(" ", length(output)))
    ))
    cli::cli_inform(c(
      "i" = "check on it with {.code scm_status(\"{out_dir}\")}"
    ))
  } else {
    # The driver outlives this call, so it runs under a shell that leaves
    # its exit status behind: pharos refuses to drive an SCM process that
    # already has a driver, and says so at once rather than in a log nobody
    # reads.
    exit_file <- tempfile("scm_run_exit")
    command <- paste0(
      paste(shQuote(c(pharos_path, args)), collapse = " "),
      " > ", shQuote(log_file), " 2>&1; echo $? > ", shQuote(exit_file)
    )
    system2("sh", c("-c", shQuote(command)), wait = FALSE)
    for (i in seq_len(12)) {
      if (file.exists(exit_file)) break
      Sys.sleep(0.25)
    }
    if (file.exists(exit_file)) {
      status <- as.integer(readLines(exit_file, warn = FALSE)[1])
      unlink(exit_file)
      if (!identical(status, 0L)) {
        output <- readLines(log_file, warn = FALSE)
        rlang::abort(c(
          "pharos refused to start the SCM driver",
          stats::setNames(output, rep("x", length(output))),
          "i" = paste0("log: ", log_file)
        ))
      }
    }
    cli::cli_inform(c(
      "v" = "SCM driver launched in the background",
      "i" = "log: {.file {log_file}}",
      "i" = "check on it with {.code scm_status(\"{out_dir}\")}"
    ))
  }

  invisible(list(out_dir = out_dir, plan_path = plan_path, log = log_file))
}

#' The pharos subcommand that runs an SCM driver of the given kind
#' @noRd
scm_run_subcommand <- function(driver) {
  switch(driver,
    slurm = c("nonmem", "scm", "slurm", "submit"),
    login = c("nonmem", "scm", "submit")
  )
}

#' Check on an SCM process in its entirety
#'
#' Where the SCM process stands, as its driver's terminal would show it now:
#' the process facts (status, phase, what is running, the forward and final
#' models once fitted), the driver — running, exited, or gone while the
#' process still says it is running, in which case re-running the plan
#' resumes it — then each decided round's decision line and the round at a
#' glance (every candidate best first, with its OFV, ΔOFV, p and what became
#' of it), and the open round fit by fit: fitted, unusable, running (its
#' Slurm job, for how long, and the latest iteration and OFV read off its
#' `.ext` file), queued or pending. Read off disk now, wherever the process
#' stands (planned, running, paused, completed, or failed): fits that
#' finished since the driver last wrote its state are picked up and scored
#' the same way the driver will.
#'
#' @param x a `hyperion_scm_plan`, an SCM output directory, or a plan.json
#'   path
#'
#' @return a `hyperion_scm_status` object: the summary record (see
#'   [scm_summary()]), with the text `pharos nonmem scm status` prints as its
#'   `rendered` attribute and the driver record — `scm_driver.json`, plus
#'   whether that driver is `"alive"`, `"gone"` or `"unknown"` from here — as
#'   its `driver` attribute (`NULL` before a driver was ever started). Its
#'   [summary()] method returns the record as a data.frame, one row per
#'   candidate per round
#' @export
#'
#' @examples \dontrun{
#' st <- scm_status(plan)
#' st
#' attr(st, "driver")$liveness
#' summary(st)   # one row per candidate per round
#' }
scm_status <- function(x) {
  scm_status_impl(scm_out_dir(x))
}

# Display methods ---------------------------------------------------------

#' The bounds of one plan candidate, spelled the way pharos writes them into
#' `$THETA`: `(0, INF)`, `(-INF, 2)`, `(0.01, 10)`; `""` when unbounded.
#' @noRd
scm_bounds_label <- function(candidate) {
  lower <- candidate$lower
  upper <- candidate$upper
  if (is.null(lower) && is.null(upper)) {
    return("")
  }
  num <- function(v, infinite) {
    if (is.null(v)) infinite else format(as.numeric(v))
  }
  paste0("(", num(lower, "-INF"), ", ", num(upper, "INF"), ")")
}

#' @noRd
scm_plan_display_parts <- function(x) {
  candidates <- data.frame(
    name = vapply(x$candidates, function(c) c$name, character(1)),
    theta = vapply(x$candidates, function(c) as.integer(c$theta), integer(1)),
    # What the effect measures; it picked the default initial estimate below.
    # A plan from a pharos predating typed effects says nothing, i.e.
    # continuous.
    type = vapply(
      x$candidates,
      function(c) unlist(c$kind) %||% "continuous",
      character(1)
    ),
    # The effect's initial estimate the first time it is tested, and what
    # pharos fixes the theta at when the effect is held out.
    initial = vapply(x$candidates, function(c) as.numeric(c$initial), numeric(1)),
    fixed = vapply(x$candidates, function(c) as.numeric(c$fixed), numeric(1)),
    # The $THETA bounds the effect is estimated under while it is in the
    # model; a candidate the config and initial model both leave unbounded
    # has none.
    bounds = vapply(x$candidates, scm_bounds_label, character(1)),
    stringsAsFactors = FALSE
  )
  direction <- unlist(x$options$direction)
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
    # retries restart from the previous attempt's estimates, jittered
    retry_jitter = paste0(
      format(as.numeric(attr(x, "retry_jitter")) * 100), "%"
    ),
    cov_step = isTRUE(x$options$cov_step),
    final_cov_step = isTRUE(x$options$final_cov_step),
    # The forward model's own cov-step fit: only when both phases run and
    # the option is on. A plan from a pharos predating it says nothing.
    forward_fit = if (
      "forward" %in% direction && "backward" %in% direction &&
        isTRUE(x$options$forward_final_cov_step %||% TRUE)
    ) {
      if (isTRUE(x$options$cov_step)) {
        "the forward model already runs the cov step; it is used as it is"
      } else {
        "re-fit the forward model with the cov step on, alongside backward elimination"
      }
    },
    candidates = candidates,
    n_candidates = nrow(candidates),
    max_models = as.integer(attr(x, "max_models")),
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
          total = as.integer(unlist(current$total)),
          # the candidates the open round holds, so a retune can say which
          # of them that round refits
          candidates = unlist(current$candidates) %||% character()
        )
      },
      retained = unlist(progress$retained) %||% character(),
      removed = unlist(progress$removed) %||% character(),
      final_model = unlist(progress$final_model),
      models_running = as.integer(unlist(progress$models_running)),
      updated = unlist(progress$updated)
    )
  }

  changes <- lapply(ctx$changes, function(c) {
    list(
      field = unlist(c$field),
      detail = unlist(c$detail),
      scm_defining = isTRUE(unlist(c$scm_defining))
    )
  })

  # Candidates whose initial estimate or bounds this plan moves. pharos
  # builds the label off the same two pieces, so keep them side by side.
  retunes <- lapply(ctx$retunes, function(r) {
    changes <- unlist(r$changes) %||% character()
    name <- unlist(r$candidate$name)
    list(
      name = name,
      changes = changes,
      label = paste0(name, ": ", paste(changes, collapse = "; "))
    )
  })

  list(
    had_previous_plan = had_previous,
    progress = progress,
    changes = changes,
    removals = unlist(ctx$removals) %||% character(),
    retunes = retunes,
    state_is_stale = isTRUE(unlist(ctx$state_is_stale)),
    stale_reasons = unlist(ctx$stale_reasons) %||% character()
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
  if (length(p$removed)) {
    lines["removed"] <- paste(p$removed, collapse = ", ")
  }
  if (!is.null(p$current_round)) {
    lines["in round"] <- paste0(
      p$current_round$name, " -- ", p$current_round$concluded, "/",
      p$current_round$total, " concluded"
    )
  }
  if (isTRUE(p$models_running > 0)) {
    lines["running"] <- paste0(p$models_running, " model(s)")
  }
  if (!is.null(p$final_model)) {
    lines["final model"] <- p$final_model
  }
  lines
}

#' Removals of never-selected candidates take effect on the next run without
#' disturbing anything already fitted.
#' @noRd
scm_context_removal_note <- function(ctx) {
  if (!length(ctx$removals) || is.null(ctx$progress)) {
    return(NULL)
  }
  paste0(
    "Removing ", paste(ctx$removals, collapse = ", "),
    " -- never selected; takes effect from the next round, earlier rounds",
    " keep their results."
  )
}

#' A candidate's initial estimate or bounds moved: not a different SCM
#' process, so the SCM process resumes under the new values. One note per
#' retuned candidate -- whether the open round refits it or the values only
#' bear on models still to be written -- plus, when a retuned candidate is
#' already in the model, the reminder that the rounds it was fitted in keep
#' the values they ran under. The same notes `pharos nonmem scm plan` prints.
#' @noRd
scm_context_retune_notes <- function(ctx) {
  if (!length(ctx$retunes) || is.null(ctx$progress)) {
    return(character())
  }
  open_round <- ctx$progress$current_round
  notes <- vapply(ctx$retunes, function(r) {
    effect <- if (!is.null(open_round) && r$name %in% open_round$candidates) {
      paste0("refitted in ", open_round$name, " under the new values")
    } else {
      "takes effect from the next model written"
    }
    paste0("Retuning ", r$label, " -- ", effect, ".")
  }, character(1))

  # A retune bears only on models still to be written; a candidate already in
  # the model was fitted in earlier rounds under the old values, and those
  # rounds stand.
  retained <- ctx$progress$retained
  in_model <- vapply(ctx$retunes, function(r) r$name, character(1))
  in_model <- in_model[in_model %in% retained]
  if (length(in_model)) {
    notes <- c(notes, paste0(
      paste(in_model, collapse = ", "), " is already in the model: the rounds",
      " it was fitted in keep the values they ran under."
    ))
  }
  notes
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
#' different — it returns the record as a data.frame; see
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
    "{.strong on failure:} retry up to {parts$max_retries}x from the previous attempt's estimates, jittered {parts$retry_jitter}"
  )
  cli::cli_text(
    "{.strong cov step:} {if (parts$cov_step) 'on' else 'off'}"
  )
  if (!is.null(parts$forward_fit)) {
    cli::cli_text("{.strong forward fit:} {parts$forward_fit}")
  }
  cli::cli_text(
    "{.strong final fit:} {if (parts$final_cov_step) 're-fit the final model with the cov step on' else 'final model written, not fitted'}"
  )
  if (!is.null(parts$num_rounds)) {
    cli::cli_text(
      "{.strong num rounds:} pause after {parts$num_rounds} (resumable)"
    )
  }
  cli::cli_h2("Candidates")
  for (i in seq_len(nrow(parts$candidates))) {
    bounds <- parts$candidates$bounds[i]
    bounds <- if (nzchar(bounds)) paste0(", bounded to ", bounds) else ""
    cli::cli_text(
      "{.strong {parts$candidates$name[i]}} THETA({parts$candidates$theta[i]}), {parts$candidates$type[i]} -> initial estimate {parts$candidates$initial[i]} when first tested, FIXED at {parts$candidates$fixed[i]} when held out{bounds}"
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
    removal <- scm_context_removal_note(ctx)
    if (!is.null(removal)) {
      cli::cli_alert_info(removal)
    }
    for (note in scm_context_retune_notes(ctx)) {
      cli::cli_alert_info(note)
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
  # The bounds column only earns its width when something is bounded.
  bounded <- any(nzchar(parts$candidates$bounds))
  candidate_rows <- if (bounded) {
    sprintf(
      "| %s | THETA(%d) | %s | %s | %s | %s |",
      parts$candidates$name,
      parts$candidates$theta,
      parts$candidates$type,
      format(parts$candidates$initial),
      format(parts$candidates$fixed),
      ifelse(nzchar(parts$candidates$bounds), parts$candidates$bounds, "-")
    )
  } else {
    sprintf(
      "| %s | THETA(%d) | %s | %s | %s |",
      parts$candidates$name,
      parts$candidates$theta,
      parts$candidates$type,
      format(parts$candidates$initial),
      format(parts$candidates$fixed)
    )
  }

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
      "x from the previous attempt's estimates, jittered ", parts$retry_jitter
    ),
    paste0("- **cov step:** ", if (parts$cov_step) "on" else "off"),
    if (!is.null(parts$forward_fit)) {
      paste0("- **forward fit:** ", parts$forward_fit)
    },
    paste0(
      "- **final fit:** ",
      if (parts$final_cov_step) {
        "re-fit the final model with the cov step on"
      } else {
        "final model written, not fitted"
      }
    ),
    if (!is.null(parts$num_rounds)) {
      paste0("- **num rounds:** pause after ", parts$num_rounds, " (resumable)")
    },
    "",
    if (bounded) {
      "| candidate | theta | type | initial estimate | FIXED (held out) | bounds |"
    } else {
      "| candidate | theta | type | initial estimate | FIXED (held out) |"
    },
    if (bounded) "|---|---|---|---|---|---|" else "|---|---|---|---|---|",
    candidate_rows,
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
    removal <- scm_context_removal_note(ctx)
    if (!is.null(removal)) {
      output <- c(output, removal, "")
    }
    for (note in scm_context_retune_notes(ctx)) {
      output <- c(output, note, "")
    }
  }
  knitr::asis_output(paste(output, collapse = "\n"))
}

#' Print method for hyperion_scm_status objects
#'
#' Prints the text `pharos nonmem scm status` prints: the process facts and
#' the driver, each decided round's decision and table, and the open round
#' fit by fit.
#'
#' @param x a `hyperion_scm_status`
#' @param ... ignored
#' @return invisible copy of x
#' @exportS3Method base::print hyperion_scm_status
print.hyperion_scm_status <- function(x, ...) {
  # pharos renders the status; printing its text verbatim keeps hyperion and
  # `pharos nonmem scm status` from ever drifting apart.
  cat(attr(x, "rendered"))
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

#' The SCM summary: every round to date, with what each candidate scored
#'
#' Where [scm_status()] shows where the SCM process stands (what is running,
#' what to do next), `scm_summary()` is the scientific record: every round
#' to date, each candidate's ΔOFV and p-value against the round's critical
#' value, sorted winner-first, plus the reference fit, the path the rounds
#' took (`+WT_CL (forward 1) -> -WT_CL (backward 1) => none`) and the
#' forward and final models once fitted. The same record pharos writes to
#' `scm_summary.json` and to each round's `round_summary.json`, so the file
#' and the screen never disagree.
#'
#' Flags stack detail onto the default view, in the spirit of `ls -la -t`:
#'
#' - `long`: absolute OFV, the effect's estimate with RSE and 95% CI, df,
#'   attempts, condition number and heuristics on every candidate line, plus
#'   what the default hides — the reference fit's attempts, every attempt of
#'   every candidate (retries included, and any marked `(superseded)`:
#'   fitted before the candidate's initial estimate or bounds were retuned,
#'   so kept on disk but never scored) with its model path
#' - `timing`: start, end and wall time per fit and per round, estimation
#'   time, totals
#' - `candidate`: one candidate traced through every round it was tested in
#' - `files`: run directory, `.lst`, `.ext` and summary JSON per candidate
#'
#' @param x a `hyperion_scm_plan`, a `hyperion_scm_status`, an SCM output
#'   directory, or a plan.json path
#' @param round only this round: the Nth SCM round (`2` or `"round 2"` — the
#'   reference fit is not a round), a round name (`"forward_round1"`,
#'   `"backward_round1"`), or `"reference"`. A single round always lists
#'   its attempts. `NULL` (the default) shows every round to date.
#' @param candidate trace one candidate, by name, through every round
#' @param long,timing,files the detail flags described above
#'
#' @return a `hyperion_scm_summary` object: the summary record restricted to
#'   the rounds selected (`x$rounds[[i]]$candidates[[j]]` holds every
#'   number), with the rendered text as its `rendered` attribute. Print it
#'   for the rendered view; [as.data.frame()] gives one row per candidate
#'   per round.
#' @export
#'
#' @examples \dontrun{
#' scm_summary(plan)                        # every round to date
#' scm_summary(plan, long = TRUE)
#' scm_summary(plan, candidate = "AGE_CL")  # why did it never get in?
#' scm_summary(plan, 2)                     # one round, every attempt
#' scm_summary("model/nonmem/PK/scm/scm-demo", "backward_round1", timing = TRUE)
#' as.data.frame(scm_summary(plan))
#' }
scm_summary <- function(x,
                        round = NULL,
                        candidate = NULL,
                        long = FALSE,
                        timing = FALSE,
                        files = FALSE) {
  out_dir <- scm_out_dir(x)
  if (!is.null(round)) {
    if (is.numeric(round)) {
      ok <- length(round) == 1L && !is.na(round) && is.finite(round) &&
        round %% 1 == 0 && round >= 1
      if (!ok) {
        rlang::abort("`round` must be a whole number >= 1, a round name, or NULL for every round")
      }
      round <- as.character(as.integer(round))
    }
    if (!is.character(round) || length(round) != 1L || is.na(round) ||
          !nzchar(trimws(round))) {
      rlang::abort(
        "`round` must be a round number or name, e.g. 2, \"round 2\", or \"forward_round1\""
      )
    }
  }
  if (!is.null(candidate)) {
    ok <- is.character(candidate) && length(candidate) == 1L &&
      !is.na(candidate) && nzchar(trimws(candidate))
    if (!ok) {
      rlang::abort("`candidate` must be a single candidate name, or NULL")
    }
    candidate <- trimws(candidate)
  }
  for (flag in c("long", "timing", "files")) {
    value <- get(flag)
    if (!(isTRUE(value) || isFALSE(value))) {
      rlang::abort(paste0("`", flag, "` must be TRUE or FALSE"))
    }
  }

  scm_summary_impl(
    path = out_dir,
    round = round,
    candidate = candidate,
    long = long,
    timing = timing,
    files = files
  )
}

#' Print method for hyperion_scm_summary objects
#'
#' @param x a `hyperion_scm_summary`
#' @param ... ignored
#' @return invisible copy of x
#' @exportS3Method base::print hyperion_scm_summary
print.hyperion_scm_summary <- function(x, ...) {
  # pharos renders the summary; printing its text verbatim keeps hyperion and
  # `pharos nonmem scm summary` from ever drifting apart.
  cat(attr(x, "rendered"))
  invisible(x)
}

#' Knit print method for hyperion_scm_summary objects
#'
#' Emits pharos's markdown rendering — real tables, one per round — rather
#' than the terminal text in a fenced block.
#'
#' @param x a `hyperion_scm_summary`
#' @param ... ignored
#' @return knitr asis output
#' @exportS3Method knitr::knit_print hyperion_scm_summary
knit_print.hyperion_scm_summary <- function(x, ...) {
  knitr::asis_output(paste0(attr(x, "markdown"), "\n"))
}

#' One row per candidate per round of an SCM summary
#'
#' @param x a `hyperion_scm_summary`
#' @param ... ignored
#' @return a data.frame with the round and its direction, reference OFV and
#'   alpha, and per candidate its action, status, model, attempts (and
#'   `superseded`, attempts made under an initial estimate or bounds since
#'   retuned away), scoring (OFV, ΔOFV, statistic, df, p, critical ΔOFV,
#'   significance, selection, rank), the effect's theta (its initial and
#'   held-out values are on the summary's `roster`), and the heuristics that
#'   fired
#' @exportS3Method base::as.data.frame hyperion_scm_summary
as.data.frame.hyperion_scm_summary <- function(x, ...) {
  num <- function(v) if (is.null(v)) NA_real_ else as.numeric(v)
  int <- function(v) if (is.null(v)) NA_integer_ else as.integer(v)
  lgl <- function(v) if (is.null(v)) NA else isTRUE(v)
  rows <- list()
  for (r in x$rounds) {
    for (c in r$candidates) {
      rows[[length(rows) + 1L]] <- data.frame(
        round = unlist(r$round),
        direction = unlist(r$direction),
        candidate = unlist(c$candidate),
        action = unlist(c$action),
        status = unlist(c$status),
        model = unlist(c$model),
        attempts = length(c$attempts),
        # Attempts made before the candidate's initial estimate or bounds
        # were retuned: fitted and kept on disk, never scored.
        superseded = length(c$superseded),
        ofv = num(c$ofv),
        # the round's reference fit is what every candidate in it is scored
        # against, so it belongs on each of its rows
        reference_ofv = num(r$reference_ofv),
        delta_ofv = num(c$delta_ofv),
        statistic = num(c$statistic),
        df = int(c$df),
        p_value = num(c$p_value),
        alpha = num(r$alpha),
        critical_delta_ofv = num(c$critical_delta_ofv),
        significant = lgl(c$significant),
        selected = isTRUE(c$selected),
        rank = int(c$rank),
        theta = int(c$theta),
        heuristics = paste(unlist(c$heuristics), collapse = "; "),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) {
    return(data.frame(
      round = character(), direction = character(), candidate = character(),
      action = character(), status = character(), model = character(),
      attempts = integer(), superseded = integer(), ofv = numeric(),
      reference_ofv = numeric(), delta_ofv = numeric(),
      statistic = numeric(), df = integer(), p_value = numeric(),
      alpha = numeric(), critical_delta_ofv = numeric(),
      significant = logical(), selected = logical(), rank = integer(),
      theta = integer(), heuristics = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

#' Summarize an SCM process: its record as a data.frame
#'
#' Returns the whole record behind [scm_status()] one row at a time — every
#' candidate of every round, with the model fitted, the attempts it took,
#' ΔOFV (candidate − reference, negative when the candidate improves),
#' degrees of freedom, p-values against the round's alpha, the heuristic
#' checks that fired, and whether the round selected it. The same numbers
#' [scm_summary()] renders, and the same ones pharos writes to
#' `scm_summary.json` after every round.
#'
#' @param object a `hyperion_scm_status` from [scm_status()]
#' @param ... ignored
#'
#' @return a data.frame, one row per candidate per round; see
#'   [as.data.frame.hyperion_scm_summary()] for the columns
#' @exportS3Method base::summary hyperion_scm_status
summary.hyperion_scm_status <- function(object, ...) {
  as.data.frame(scm_summary(object))
}
