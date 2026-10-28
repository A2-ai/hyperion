# Print, knit and helper methods for the SCM objects.

# ---- the config object -------------------------------------------------------

#' @noRd
build_scm_display_parts <- function(x) {
  effects <- normalize_effects(x$covariates$effects)
  effects_df <- NULL
  if (length(effects)) {
    pick <- function(field, default) {
      vapply(effects, function(e) as.character(S7::prop(e, field) %||% default), character(1))
    }
    number <- function(field) {
      values <- vapply(effects, function(e) as.numeric(S7::prop(e, field) %||% NA), numeric(1))
      out <- format_hyperion_sigfig_string(values)
      out[is.na(out)] <- ""
      out
    }
    effects_df <- data.frame(
      Effect = pick("name", ""),
      Type = pick("type", "continuous"),
      Initial = number("initial"),
      Fixed = number("fixed"),
      Lower = number("lower"),
      Upper = number("upper"),
      stringsAsFactors = FALSE
    )
    effects_df <- format_display_data(effects_df)
  }
  on_off <- function(flag) if (isTRUE(flag)) "on" else "off"
  rounds <- x$num_rounds %||% 0
  list(
    title = paste0("SCM: ", attr(x, "filename") %||% ""),
    model = x$model %||% "",
    config = attr(x, "config_source") %||% "",
    direction = paste(unlist(x$direction), collapse = " -> "),
    alphas = paste0(
      "forward = ",
      x$forward_alpha %||% 0.05,
      ", backward = ",
      x$backward_alpha %||% 0.001
    ),
    retries = paste0(
      x$max_retries %||% 3,
      " per failed fit, jittered ",
      100 * (x$retry_jitter %||% 0.05),
      "%"
    ),
    cov = paste0(
      "rounds ",
      on_off(x$cov_step),
      ", forward model ",
      on_off(x$forward_final_cov_step %||% TRUE),
      ", final model ",
      on_off(x$final_cov_step %||% TRUE)
    ),
    rounds = if (rounds > 0) paste0("pause after ", rounds) else "all",
    effects = effects_df
  )
}

#' Print method for hyperion_nonmem_scm objects
#'
#' @param x A hyperion_nonmem_scm object
#' @param ... Additional arguments (ignored)
#' @return Invisible copy of x
#' @exportS3Method base::print hyperion_nonmem_scm
print.hyperion_nonmem_scm <- function(x, ...) {
  parts <- build_scm_display_parts(x)
  cli::cli_text("")
  cli::cli_h1(parts$title)
  cli::cli_text("{.strong Model:} {parts$model}")
  cli::cli_text("{.strong Config:} {parts$config}")
  cli::cli_text("{.strong Direction:} {parts$direction}")
  cli::cli_text("{.strong Alphas:} {parts$alphas}")
  cli::cli_text("{.strong Retries:} {parts$retries}")
  cli::cli_text("{.strong Covariance step:} {parts$cov}")
  cli::cli_text("{.strong Rounds:} {parts$rounds}")
  if (is.null(parts$effects)) {
    cli::cli_alert_warning(
      "No effects yet: pass {.arg effects} to {.fn init_scm} or add them with {.fn add_scm_effect}"
    )
  } else {
    print_data_table_console(parts$effects, "Effects")
  }
  invisible(x)
}

#' Knit print method for hyperion_nonmem_scm objects
#'
#' @param x A hyperion_nonmem_scm object
#' @param ... Additional arguments (ignored)
#' @return HTML output for rendered documents
#' @exportS3Method knitr::knit_print
knit_print.hyperion_nonmem_scm <- function(x, ...) {
  parts <- build_scm_display_parts(x)
  output <- c(
    "",
    paste0("<strong>", parts$title, "</strong>"),
    "",
    paste0("<strong>Model:</strong> ", parts$model),
    "",
    paste0("<strong>Config:</strong> ", parts$config),
    "",
    paste0("<strong>Direction:</strong> ", parts$direction),
    "",
    paste0("<strong>Alphas:</strong> ", parts$alphas),
    "",
    paste0("<strong>Retries:</strong> ", parts$retries),
    "",
    paste0("<strong>Covariance step:</strong> ", parts$cov),
    "",
    paste0("<strong>Rounds:</strong> ", parts$rounds),
    ""
  )
  if (is.null(parts$effects)) {
    output <- c(output, "No effects yet.", "")
  } else {
    output <- c(output, print_data_table_knit(parts$effects, "Effects"))
  }
  knitr::asis_output(paste(output, collapse = "\n"))
}

# ---- the records: plan, status, summary ---------------------------------------

# pharos' rendering without its own `<scm ...>` headline, which the cli title
# replaces.
scm_text_lines <- function(x) {
  lines <- strsplit(attr(x, "text") %||% "", "\n", fixed = TRUE)[[1]]
  if (length(lines) && startsWith(lines[1], "<scm ")) {
    lines <- lines[-1]
  }
  lines
}

scm_title <- function(x, what) {
  paste0("SCM ", what, ": ", x$out_dir %||% "")
}

print_scm_text <- function(x, what) {
  cli::cli_text("")
  cli::cli_h1(scm_title(x, what))
  cat(scm_text_lines(x), sep = "\n")
}

html_escape <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  gsub(">", "&gt;", s, fixed = TRUE)
}

knit_scm_text <- function(x, what, extra = character()) {
  output <- c(
    "",
    paste0("<strong>", scm_title(x, what), "</strong>"),
    "",
    '<pre class="hyperion-scm" style="white-space: pre; overflow-x: auto;">',
    html_escape(scm_text_lines(x)),
    "</pre>",
    extra,
    ""
  )
  knitr::asis_output(paste(output, collapse = "\n"))
}

#' Print method for hyperion_nonmem_scm_plan objects
#'
#' @param x A hyperion_nonmem_scm_plan object
#' @param ... Additional arguments (ignored)
#' @return Invisible copy of x
#' @exportS3Method base::print hyperion_nonmem_scm_plan
print.hyperion_nonmem_scm_plan <- function(x, ...) {
  print_scm_text(x, "Plan")
  for (w in attr(x, "warnings") %||% character()) {
    cli::cli_alert_warning(w)
  }
  if (isFALSE(attr(x, "resumable"))) {
    cli::cli_alert_danger(
      "The SCM process in {.path {x$out_dir}} cannot resume under this plan; submit with {.code overwrite = TRUE} to discard it."
    )
  }
  invisible(x)
}

#' Print method for hyperion_nonmem_scm_status objects
#'
#' @param x A hyperion_nonmem_scm_status object
#' @param ... Additional arguments (ignored)
#' @return Invisible copy of x
#' @exportS3Method base::print hyperion_nonmem_scm_status
print.hyperion_nonmem_scm_status <- function(x, ...) {
  print_scm_text(x, "Status")
  invisible(x)
}

#' Print method for hyperion_nonmem_scm_summary objects
#'
#' @param x A hyperion_nonmem_scm_summary object
#' @param ... Additional arguments (ignored)
#' @return Invisible copy of x
#' @exportS3Method base::print hyperion_nonmem_scm_summary
print.hyperion_nonmem_scm_summary <- function(x, ...) {
  print_scm_text(x, "Summary")
  n <- x$totals$unusable %||% 0
  if (n > 0) {
    cli::cli_alert_warning(
      "{n} candidate fit{?s} {?was/were} unusable; see the round tables above."
    )
  }
  invisible(x)
}

#' Knit print methods for SCM records
#'
#' @param x A hyperion_nonmem_scm_plan, _status or _summary object
#' @param ... Additional arguments (ignored)
#' @return HTML output for rendered documents
#' @exportS3Method knitr::knit_print
knit_print.hyperion_nonmem_scm_plan <- function(x, ...) {
  w <- attr(x, "warnings") %||% character()
  extra <- if (length(w)) {
    c("<ul>", paste0("<li>&#x26A0; ", html_escape(w), "</li>"), "</ul>")
  } else {
    character()
  }
  knit_scm_text(x, "Plan", extra)
}

#' @rdname knit_print.hyperion_nonmem_scm_plan
#' @exportS3Method knitr::knit_print
knit_print.hyperion_nonmem_scm_status <- function(x, ...) {
  knit_scm_text(x, "Status")
}

#' @rdname knit_print.hyperion_nonmem_scm_plan
#' @exportS3Method knitr::knit_print
knit_print.hyperion_nonmem_scm_summary <- function(x, ...) {
  knit_scm_text(x, "Summary")
}

#' Structure method for SCM records
#'
#' Displays the record without the rendered text attribute.
#'
#' @param object A hyperion_nonmem_scm_record object
#' @param ... Additional arguments passed to str
#' @return Invisible NULL (called for side effects)
#' @exportS3Method utils::str hyperion_nonmem_scm_record
str.hyperion_nonmem_scm_record <- function(object, ...) {
  attr(object, "text") <- NULL
  class(object) <- "list"
  utils::str(object, ...)
}

# ---- helpers over the record ---------------------------------------------------

as_scm_record <- function(x, arg = "x") {
  if (inherits(x, "hyperion_nonmem_scm_record")) {
    return(x)
  }
  if (inherits(x, "hyperion_nonmem_scm")) {
    return(summary(x))
  }
  if (is.character(x) && length(x) == 1) {
    return(get_scm_summary(x))
  }
  rlang::abort(paste0(
    "`",
    arg,
    "` must be an SCM summary or status, a hyperion_nonmem_scm object, or the SCM config or output directory"
  ))
}

#' Get the candidates of an SCM process as a data frame
#'
#' @param x A `hyperion_nonmem_scm_summary` or `_status` object, a
#'   `hyperion_nonmem_scm` object, or the SCM config or output directory.
#' @param timing Add the candidate's `started`, `ended`, `wall_seconds` and
#'   `estimation_seconds`. Default `FALSE`.
#' @return A data frame with columns `round`, `direction`, `index`,
#'   `complete`, `reference_model`, `reference_ofv`, `alpha`, `candidate`,
#'   `action`, `status`, `model`, `theta`, `ofv`, `delta_ofv`, `statistic`,
#'   `df`, `p_value`, `critical_delta_ofv`, `significant`, `selected`, `rank`,
#'   `attempts`, `heuristics` and `run_dir`. `model` and `run_dir` are
#'   relative to `attr(x, "out_dir")`.
#' @export
#'
#' @examples \dontrun{
#' s <- summary(read_scm("model/nonmem/scm/run001scm.toml"))
#' get_scm_candidates(s)
#' }
get_scm_candidates <- function(x, timing = FALSE) {
  x <- as_scm_record(x)
  rows <- list()
  for (round in x$rounds %||% list()) {
    for (cand in round$candidates %||% list()) {
      rows[[length(rows) + 1]] <- scm_candidate_row(round, cand, timing)
    }
  }
  if (length(rows) == 0) {
    return(scm_candidate_row(list(), list(), timing)[0, ])
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# One candidate of one round as a one-row data frame; empty `round` and
# `cand` give the column template.
scm_candidate_row <- function(round, cand, timing) {
  row <- data.frame(
    round = round$round %||% NA_character_,
    direction = round$direction %||% NA_character_,
    index = as.integer(round$index %||% NA),
    complete = isTRUE(round$complete),
    reference_model = round$reference_model %||% NA_character_,
    reference_ofv = as.numeric(round$reference_ofv %||% NA),
    alpha = as.numeric(round$alpha %||% NA),
    candidate = cand$candidate %||% NA_character_,
    action = cand$action %||% NA_character_,
    status = cand$status %||% NA_character_,
    model = cand$model %||% NA_character_,
    theta = as.integer(cand$theta %||% NA),
    ofv = as.numeric(cand$ofv %||% NA),
    delta_ofv = as.numeric(cand$delta_ofv %||% NA),
    statistic = as.numeric(cand$statistic %||% NA),
    df = as.integer(cand$df %||% NA),
    p_value = as.numeric(cand$p_value %||% NA),
    critical_delta_ofv = as.numeric(cand$critical_delta_ofv %||% NA),
    significant = as.logical(cand$significant %||% NA),
    selected = isTRUE(cand$selected),
    rank = as.integer(cand$rank %||% NA),
    attempts = length(cand$attempts %||% list()),
    heuristics = paste(unlist(cand$heuristics), collapse = ", "),
    run_dir = cand$files$run_dir %||% NA_character_,
    stringsAsFactors = FALSE
  )
  if (isTRUE(timing)) {
    row$started <- cand$timing$started %||% NA_character_
    row$ended <- cand$timing$ended %||% NA_character_
    row$wall_seconds <- as.numeric(cand$timing$wall_seconds %||% NA)
    row$estimation_seconds <- as.numeric(cand$timing$estimation_seconds %||% NA)
  }
  row
}

find_scm_round <- function(x, round) {
  rounds <- x$rounds %||% list()
  if (length(rounds) == 0) {
    rlang::abort("the SCM process has no rounds yet")
  }
  selector <- trimws(as.character(round))
  names <- vapply(rounds, function(r) r$round %||% "", character(1))
  hit <- which(tolower(names) == tolower(selector))
  if (length(hit) == 0) {
    number <- suppressWarnings(as.integer(sub("^round\\s*", "", tolower(selector))))
    if (!is.na(number)) {
      indices <- vapply(rounds, function(r) as.integer(r$index %||% NA), integer(1))
      hit <- which(indices == number)
    }
  }
  if (length(hit) == 0) {
    rlang::abort(paste0(
      "no round matching '",
      round,
      "'; rounds so far: ",
      paste(names, collapse = ", ")
    ))
  }
  rounds[[hit[1]]]
}

#' Read a model of an SCM process
#'
#' @param x A `hyperion_nonmem_scm_summary` or `_status` object, a
#'   `hyperion_nonmem_scm` object, or the SCM config or output directory.
#' @param which Which model: `"final"`, `"forward"`, `"initial"` or `"reference"`.
#' @param round A round, as `summary()` names it (`"2"`, `"forward_round1"`,
#'   `"reference"`); overrides `which`.
#' @param candidate A candidate in `round`; defaults to the round's winner.
#' @return A `hyperion_nonmem_model` object.
#' @export
#'
#' @examples \dontrun{
#' s <- summary(read_scm("model/nonmem/scm/run001scm.toml"))
#' final <- read_scm_model(s)
#' summary(final)
#' read_scm_model(s, round = "forward_round1", candidate = "WT_CL")
#' }
read_scm_model <- function(
  x,
  which = c("final", "forward", "initial", "reference"),
  round = NULL,
  candidate = NULL
) {
  x <- as_scm_record(x)
  which <- match.arg(which)
  out_dir <- attr(x, "out_dir")
  if (!is.null(round) || !is.null(candidate)) {
    if (is.null(round)) {
      rlang::abort("`candidate` needs `round`")
    }
    r <- find_scm_round(x, round)
    name <- candidate %||% r$winner
    if (is.null(name)) {
      rlang::abort(paste0(
        "round ",
        r$round,
        " has no winner; pass `candidate`"
      ))
    }
    cands <- r$candidates %||% list()
    cand_names <- vapply(cands, function(c) c$candidate %||% "", character(1))
    hit <- which(tolower(cand_names) == tolower(name))
    if (length(hit) == 0) {
      rlang::abort(paste0(
        "no candidate named ",
        name,
        " in ",
        r$round,
        "; this round knows: ",
        paste(cand_names, collapse = ", ")
      ))
    }
    rel <- cands[[hit[1]]]$model
    return(read_model(file.path(out_dir, rel)))
  }
  if (which == "initial") {
    root <- attr(x, "project_root")
    if (is.null(root)) {
      rlang::abort("the record has no project_root; cannot locate the initial model")
    }
    return(read_model(file.path(root, x$initial_model)))
  }
  rel <- switch(
    which,
    final = x$final_model,
    forward = x$forward_final$model,
    reference = {
      ref <- Filter(function(r) identical(r$round, "reference"), x$rounds %||% list())
      if (length(ref)) ref[[1]]$candidates[[1]]$model else NULL
    }
  )
  if (is.null(rel) || !nzchar(rel)) {
    rlang::abort(paste0(
      "no ",
      which,
      " model yet (status: ",
      x$status %||% "unknown",
      ")"
    ))
  }
  read_model(file.path(out_dir, rel))
}
