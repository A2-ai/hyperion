#' @noRd
build_scm_display_parts <- function(x) {
  effects <- normalize_effects(x$covariates$effects)
  effects_df <- NULL
  if (length(effects)) {
    pick <- function(field, default) {
      vapply(effects, function(e) as.character(S7::prop(e, field) %||% default), character(1))
    }
    number <- function(field) {
      vapply(effects, function(e) as.numeric(S7::prop(e, field) %||% NA), numeric(1))
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
    cli::cli_alert_warning(SCM_NO_EFFECTS)
  } else {
    print_data_table_console(parts$effects, "Effects")
  }
  invisible(x)
}

#' @rdname print.hyperion_nonmem_scm
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

# pharos' text without its own headline; the cli title replaces it.
scm_text_lines <- function(x) {
  lines <- strsplit(attr(x, "text") %||% "", "\n", fixed = TRUE)[[1]]
  if (length(lines) && startsWith(lines[1], "<scm ")) {
    lines <- lines[-1]
  }
  lines
}

# The record's kind, from its class: "Plan", "Status" or "Summary".
scm_title <- function(x) {
  what <- sub("^hyperion_nonmem_scm_", "", class(x)[1])
  paste0("SCM ", tools::toTitleCase(what), ": ", x$out_dir %||% "")
}

html_escape <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  gsub(">", "&gt;", s, fixed = TRUE)
}

#' Print method for SCM records
#'
#' Prints pharos' rendering of a plan, status or summary under a title, then
#' a plan's warnings and whether the process can resume under it, or a
#' summary's count of unusable fits.
#'
#' @param x A hyperion_nonmem_scm_plan, _status or _summary object
#' @param ... Additional arguments (ignored)
#' @return Invisible copy of x
#' @exportS3Method base::print hyperion_nonmem_scm_record
print.hyperion_nonmem_scm_record <- function(x, ...) {
  cli::cli_text("")
  cli::cli_h1(scm_title(x))
  cat(scm_text_lines(x), sep = "\n")
  for (w in attr(x, "warnings") %||% character()) {
    cli::cli_alert_warning(w)
  }
  if (isFALSE(attr(x, "resumable"))) {
    cli::cli_alert_danger(
      "The SCM process in {.path {x$out_dir}} cannot resume under this plan; submit with {.code overwrite = TRUE} to discard it."
    )
  }
  n <- x$totals$unusable %||% 0
  if (n > 0) {
    cli::cli_alert_warning(
      "{n} candidate fit{?s} {?was/were} unusable; see the round tables above."
    )
  }
  invisible(x)
}

#' @rdname print.hyperion_nonmem_scm_record
#' @exportS3Method knitr::knit_print
knit_print.hyperion_nonmem_scm_record <- function(x, ...) {
  w <- attr(x, "warnings") %||% character()
  output <- c(
    "",
    paste0("<strong>", scm_title(x), "</strong>"),
    "",
    '<pre class="hyperion-scm" style="white-space: pre; overflow-x: auto;">',
    html_escape(scm_text_lines(x)),
    "</pre>",
    if (length(w)) c("<ul>", paste0("<li>&#x26A0; ", html_escape(w), "</li>"), "</ul>"),
    ""
  )
  knitr::asis_output(paste(output, collapse = "\n"))
}

#' @rdname print.hyperion_nonmem_scm_record
#' @param object A hyperion_nonmem_scm_record object; `str()` shows it
#'   without the rendered `text` attribute.
#' @exportS3Method utils::str hyperion_nonmem_scm_record
str.hyperion_nonmem_scm_record <- function(object, ...) {
  attr(object, "text") <- NULL
  class(object) <- "list"
  utils::str(object, ...)
}

#' Summarize an SCM process
#'
#' @param object A `hyperion_nonmem_scm` object.
#' @param round Show one round only.
#' @param candidate Show one candidate only.
#' @param long,timing,files Add the corresponding detail to the printed text.
#' @param ... Unused.
#' @return A `hyperion_nonmem_scm_summary` S3 object (also of class
#'   `hyperion_nonmem_scm_record`) with attributes `text`, `out_dir` and
#'   `project_root`.
#' @exportS3Method base::summary hyperion_nonmem_scm
#'
#' @examples \dontrun{
#' scm <- read_scm("model/nonmem/scm/run001scm.toml")
#' summary(scm)
#' summary(scm, round = "forward_round1", long = TRUE)
#' }
summary.hyperion_nonmem_scm <- function(
  object,
  round = NULL,
  candidate = NULL,
  long = FALSE,
  timing = FALSE,
  files = FALSE,
  ...
) {
  get_scm_summary(object, round, candidate, long, timing, files)
}
