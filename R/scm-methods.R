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
      on_off(x$cov_step),
      " in the round fits, ",
      on_off(x$working_full_cov_step %||% TRUE),
      " in the working full model's re-fit, ",
      on_off(x$reduced_covariate_cov_step %||% TRUE),
      " in the reduced covariate model's re-fit"
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
    print_data_table_console(parts$effects, "Candidates")
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
    output <- c(output, print_data_table_knit(parts$effects, "Candidates"))
  }
  knitr::asis_output(paste(output, collapse = "\n"))
}

# pharos' text without its own headline (`<scm plan> ...`); the cli title
# replaces it.
scm_text_lines <- function(x) {
  lines <- strsplit(attr(x, "text") %||% "", "\n", fixed = TRUE)[[1]]
  lines[!startsWith(lines, "<scm ")]
}

html_escape <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  gsub(">", "&gt;", s, fixed = TRUE)
}

# A fact's value with its opening word in the fact's color.
scm_toned_value <- function(value, tone, colorize) {
  if (is.na(tone)) {
    return(value)
  }
  word <- regmatches(value, regexpr("^[A-Za-z]+", value))
  if (length(word) == 0) {
    return(value)
  }
  paste0(colorize(word, tone), substring(value, nchar(word) + 1))
}

print_scm_facts <- function(facts) {
  for (i in seq_len(nrow(facts %||% data.frame()))) {
    label <- facts$label[i]
    value <- scm_toned_value(facts$value[i], facts$tone[i], display_color_console)
    if (nzchar(label)) {
      cli::cli_text("{.strong {label}:} {value}")
    } else {
      cli::cli_text("{value}")
    }
  }
}

print_scm_alert <- function(alert) {
  text <- alert$text
  switch(
    alert$type,
    success = cli::cli_alert_success("{text}"),
    warning = cli::cli_alert_warning("{text}"),
    danger = cli::cli_alert_danger("{text}"),
    cli::cli_alert_info("{text}")
  )
}

#' Print method for SCM records
#'
#' Prints a plan, status or summary the way a model summary prints: the
#' process facts under a title, then sections with tables. A summary and a
#' status show the base, working full and reduced covariate models, then each round with its
#' decision and its candidates, the one the round added or dropped in
#' green and an unusable fit in red, with a rule between the rounds; a status shows its open round fit by
#' fit and closes with the driver's state. A plan shows its candidate
#' effects, the process already in its output directory, its warnings, and
#' whether that process can resume under it.
#'
#' @param x A hyperion_nonmem_scm_plan, _status or _summary object
#' @param ... Additional arguments (ignored)
#' @return Invisible copy of x
#' @exportS3Method base::print hyperion_nonmem_scm_record
print.hyperion_nonmem_scm_record <- function(x, ...) {
  parts <- build_scm_record_parts(x)
  cli::cli_text("")
  cli::cli_h1(parts$title)
  for (alert in parts$top) {
    print_scm_alert(alert)
  }
  print_scm_facts(parts$facts)
  for (section in parts$sections) {
    if (isTRUE(section$rule)) {
      cli::cli_verbatim("")
      cli::cli_rule()
    }
    cli::cli_h2(section$title)
    for (alert in section$alerts) {
      print_scm_alert(alert)
    }
    print_scm_facts(section$facts)
    # A table goes through cli, like the lines around it, so the headings'
    # margins fall where they should.
    above <- length(section$alerts) > 0 || nrow(section$facts %||% data.frame()) > 0
    for (table in section$tables) {
      if (!is.null(table$title)) {
        cli::cli_h3(table$title)
      }
      if (above || !is.null(table$title)) {
        cli::cli_verbatim("")
      }
      cli::cli_verbatim(data_table_console_lines(table$data))
      above <- TRUE
    }
    if (length(section$notes)) {
      cli::cli_verbatim("")
    }
    for (note in section$notes) {
      cli::cli_text("{.emph {note}}")
    }
  }
  if (length(parts$alerts)) {
    cli::cli_verbatim("")
  }
  for (alert in parts$alerts) {
    print_scm_alert(alert)
  }
  invisible(x)
}

knit_scm_facts <- function(facts) {
  output <- character()
  for (i in seq_len(nrow(facts %||% data.frame()))) {
    label <- facts$label[i]
    value <- scm_toned_value(html_escape(facts$value[i]), facts$tone[i], display_color_html)
    output <- c(
      output,
      if (nzchar(label)) paste0("<strong>", label, ":</strong> ", value) else value,
      ""
    )
  }
  output
}

knit_scm_alerts <- function(alerts) {
  output <- character()
  for (alert in alerts) {
    mark <- switch(
      alert$type,
      success = display_color_html("&#x2714;", "green"),
      warning = display_color_html("&#x26A0;", "yellow"),
      danger = display_color_html("&#x2716;", "red"),
      display_color_html("&#x2139;", "blue")
    )
    output <- c(output, paste0(mark, " ", html_escape(alert$text)), "")
  }
  output
}

# The table with its text escaped for HTML, its colors and alignment kept.
knit_scm_table <- function(data) {
  escaped <- data
  escaped[] <- lapply(data, html_escape)
  attr(escaped, "cell_colors") <- attr(data, "cell_colors")
  attr(escaped, "numeric_cols") <- attr(data, "numeric_cols")
  escaped
}

#' @rdname print.hyperion_nonmem_scm_record
#' @exportS3Method knitr::knit_print
knit_print.hyperion_nonmem_scm_record <- function(x, ...) {
  parts <- build_scm_record_parts(x)
  output <- c(
    "",
    paste0("<strong>", html_escape(parts$title), "</strong>"),
    "",
    knit_scm_alerts(parts$top),
    knit_scm_facts(parts$facts)
  )
  for (section in parts$sections) {
    output <- c(
      output,
      if (isTRUE(section$rule)) c("", "<hr>"),
      "",
      paste0("<strong>", section$title, "</strong>"),
      "",
      knit_scm_alerts(section$alerts),
      knit_scm_facts(section$facts)
    )
    for (table in section$tables) {
      output <- c(output, print_data_table_knit(knit_scm_table(table$data), table$title))
    }
    for (note in section$notes) {
      output <- c(output, paste0("<em>", html_escape(note), "</em>"), "")
    }
  }
  output <- c(output, knit_scm_alerts(parts$alerts))
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
#' The summary prints the process facts with the covariates' path, the
#' base, working full and reduced covariate models, and each round: its decision, its
#' reference, and a table of its candidates with their OFV, dOFV against
#' the reference, p-value, and which one the round added or dropped.
#'
#' @param object A `hyperion_nonmem_scm` object.
#' @param round Show one round only.
#' @param candidate Show one candidate only.
#' @param long,timing,files Add the corresponding detail to what prints:
#'   each candidate's estimate, degrees of freedom and attempts; the time
#'   the fits took; the files each run left.
#' @param ... Unused.
#' @return A `hyperion_nonmem_scm_summary` S3 object (also of class
#'   `hyperion_nonmem_scm_record`) with attributes `text` (the summary as
#'   pharos renders it), `view` (the arguments above, which its print
#'   method follows), `out_dir` and `project_root`.
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
  res <- get_scm_summary(object, round, candidate, long, timing, files)
  attr(res, "view") <- list(
    round = round,
    candidate = candidate,
    long = isTRUE(long),
    timing = isTRUE(timing),
    files = isTRUE(files)
  )
  res
}
