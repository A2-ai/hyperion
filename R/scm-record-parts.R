# How the SCM records (plan, status, summary) are laid out for printing.
#
# A record prints the way a model summary does: a title, the process facts
# as `Label: value` lines, then sections with tables of hyperion's own. The
# tables are built from the record itself. The facts are read off pharos'
# rendering of the record (its `text` attribute), which spells out what the
# record only implies (the status line, the covariates' path, what a plan
# changes about a started process); so are the lines only a live read of the
# process can give, a running fit's latest iteration and the driver's state.

# ---- pharos' text, in pieces ------------------------------------------------

# The text's lines, split at its blank lines.
scm_text_blocks <- function(lines) {
  if (!length(lines)) {
    return(list())
  }
  blank <- !nzchar(trimws(lines))
  unname(split(lines[!blank], cumsum(blank)[!blank]))
}

# `label : value` lines as a data frame of label and value. An indented line
# continues the fact above it; any other line is a fact with no label.
scm_parse_facts <- function(lines) {
  label <- character()
  value <- character()
  for (line in lines) {
    m <- regmatches(line, regexec("^([A-Za-z][A-Za-z ]*?) *: ?(.*)$", line))[[1]]
    if (length(m) == 3) {
      label <- c(label, m[2])
      value <- c(value, trimws(m[3]))
    } else if (length(label) && grepl("^\\s", line)) {
      value[length(value)] <- trimws(paste(value[length(value)], trimws(line)))
    } else {
      label <- c(label, "")
      value <- c(value, trimws(line))
    }
  }
  data.frame(label = label, value = value, stringsAsFactors = FALSE)
}

# The driver's lines `get_scm_status()` appends to the text, apart from the
# rest: `driver : ...` and `allocation : ...`, with the lines continuing them.
scm_split_driver_lines <- function(lines) {
  starts <- grepl("^(driver|allocation) *:", lines)
  driver <- starts
  for (i in seq_along(lines)[-1]) {
    if (!starts[i] && driver[i - 1] && grepl("^\\s+\\S", lines[i])) {
      driver[i] <- TRUE
    }
  }
  list(lines = lines[!driver], driver = scm_parse_facts(lines[driver]))
}

SCM_FACT_LABELS <- c(
  "model" = "Model",
  "status" = "Status",
  "direction" = "Direction",
  "alphas" = "Alphas",
  "cov step" = "Covariance step",
  "candidates" = "Candidates",
  "phase" = "Phase",
  "note" = "Note",
  "running" = "Running",
  "queued" = "Queued",
  "progress" = "Progress",
  "path" = "Path",
  "removed" = "Removed",
  "retuned" = "Retuned",
  "working full model" = "Working full model",
  "working full fit" = "Working full fit",
  "reduced covariate model" = "Reduced covariate model",
  "reduced covariate fit" = "Reduced covariate fit",
  "time" = "Time",
  "selected" = "Selected",
  "changes" = "Changes",
  "max models" = "Max models",
  "num rounds" = "Rounds",
  "on failure" = "On failure",
  "driver" = "Driver",
  "allocation" = "Allocation"
)

scm_fact_label <- function(label) {
  known <- SCM_FACT_LABELS[label]
  fallback <- paste0(toupper(substr(label, 1, 1)), substring(label, 2))
  unname(ifelse(is.na(known), fallback, known))
}

# The color a process status reads in, from the word its value opens with.
scm_status_tone <- function(value) {
  word <- tolower(sub("[^A-Za-z].*$", "", value))
  switch(
    word,
    completed = "green",
    running = "blue",
    planned = ,
    paused = ,
    stopped = "yellow",
    failed = "red",
    NA_character_
  )
}

# ---- formatting ---------------------------------------------------------------

scm_none_or_list <- function(x) {
  x <- as.character(unlist(x))
  if (length(x) == 0) "none" else paste(x, collapse = ", ")
}

scm_is_missing <- function(x) {
  is.null(x) || length(x) == 0 || is.na(x)
}

scm_fmt_ofv <- function(x) {
  if (scm_is_missing(x)) "" else sprintf("%.3f", x)
}

scm_fmt_delta <- function(x) {
  if (scm_is_missing(x)) "" else sprintf("%+.3f", x)
}

scm_fmt_p <- function(p) {
  if (scm_is_missing(p)) {
    return("")
  }
  if (p < 0.001) formatC(p, format = "e", digits = 2) else formatC(p, format = "f", digits = 3)
}

scm_fmt_number <- function(x) {
  if (scm_is_missing(x)) "" else format(x, trim = TRUE)
}

scm_fmt_duration <- function(seconds) {
  if (scm_is_missing(seconds)) {
    return("")
  }
  s <- round(seconds)
  if (s < 60) {
    sprintf("%ds", s)
  } else if (s < 3600) {
    sprintf("%dm %02ds", s %/% 60, s %% 60)
  } else {
    sprintf("%dh %02dm", s %/% 3600, (s %% 3600) %/% 60)
  }
}

# "forward_round2" -> "Forward round 2"; a stage keeps its name.
scm_round_title <- function(round) {
  m <- regmatches(round, regexec("^(forward|backward)_round([0-9]+)$", round))[[1]]
  if (length(m) == 3) {
    return(paste0(toupper(substr(m[2], 1, 1)), substring(m[2], 2), " round ", m[3]))
  }
  paste0(toupper(substr(round, 1, 1)), substring(round, 2))
}

scm_on_off <- function(flag) {
  if (isTRUE(flag)) "on" else "off"
}

# A table for the printers: text cells, the columns nothing fills dropped,
# and the cells to color (see `display_cell_colors()`).
scm_display_table <- function(rows, colors = NULL, numeric = character()) {
  df <- as.data.frame(
    do.call(rbind, lapply(rows, function(r) unlist(r))),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(df) <- names(rows[[1]])
  df[is.na(df)] <- ""
  color_matrix <- matrix(NA_character_, nrow = nrow(df), ncol = ncol(df))
  for (cell in colors) {
    color_matrix[cell$row, match(cell$column, names(df))] <- cell$color
  }
  keep <- vapply(df, function(col) any(nzchar(col)), logical(1))
  keep[1] <- TRUE
  df <- df[, keep, drop = FALSE]
  rownames(df) <- NULL
  attr(df, "cell_colors") <- color_matrix[, keep, drop = FALSE]
  attr(df, "numeric_cols") <- intersect(numeric, names(df))
  df
}

scm_section <- function(title, alerts = list(), facts = NULL, tables = list(), notes = character()) {
  list(title = title, alerts = alerts, facts = facts, tables = tables, notes = notes)
}

scm_alert <- function(type, text) {
  list(type = type, text = text)
}

scm_facts <- function(label = character(), value = character(), tone = NA_character_) {
  data.frame(
    label = label,
    value = value,
    tone = rep_len(tone, length(label)),
    stringsAsFactors = FALSE
  )
}

# ---- the process facts ----------------------------------------------------------

# The record's header facts, from pharos' text: labeled the hyperion way,
# the status in its color, and the covariance step spelled out.
scm_record_facts <- function(x, header, drop = character()) {
  parsed <- scm_parse_facts(header)
  parsed <- parsed[!parsed$label %in% drop, , drop = FALSE]
  facts <- scm_facts(scm_fact_label(parsed$label), parsed$value)
  status <- parsed$label %in% c("status", "progress")
  facts$tone[status] <- vapply(parsed$value[status], scm_status_tone, character(1))
  cov <- parsed$label == "cov step"
  facts$value[cov] <- paste0(scm_on_off(x$options$cov_step), " in the round fits")
  facts
}

# ---- the models table -------------------------------------------------------------

scm_reference_round <- function(x) {
  for (r in x$rounds %||% list()) {
    if (identical(r$reference_model, "-") || identical(r$round, "reference")) {
      return(r)
    }
  }
  NULL
}

# The base (or full) model, the working full model and the reduced covariate model, with
# their OFV and the covariates each holds.
scm_models_section <- function(x) {
  rows <- list()
  colors <- list()
  add <- function(label, file, ofv, covariates, status) {
    status <- status %||% ""
    shown <- switch(status, succeeded = "", reused = "cov step already on", status)
    rows[[length(rows) + 1]] <<- list(
      Model = label,
      File = file %||% "",
      OFV = scm_fmt_ofv(ofv),
      Covariates = scm_none_or_list(covariates),
      Status = shown
    )
    tone <- switch(status, unusable = "red", running = "blue", NA_character_)
    if (!is.na(tone)) {
      colors[[length(colors) + 1]] <<- list(row = length(rows), column = "Status", color = tone)
    }
  }
  reference <- scm_reference_round(x)
  if (!is.null(reference) && length(reference$candidates)) {
    fit <- reference$candidates[[1]]
    name <- fit$candidate %||% "base"
    add(
      paste0(toupper(substr(name, 1, 1)), substring(name, 2), " model"),
      fit$model,
      fit$ofv,
      reference$retained_after,
      fit$status
    )
  }
  forward <- x$forward_final
  if (!is.null(forward)) {
    add("Working full model", forward$model, forward$ofv, forward$retained, forward$status)
  }
  if (!is.null(x$final_model)) {
    add("Reduced covariate model", x$final_model, x$final_ofv, x$retained, "")
  }
  if (length(rows) == 0) {
    return(NULL)
  }
  scm_section(
    "Models",
    tables = list(list(title = NULL, data = scm_display_table(rows, colors, numeric = "OFV")))
  )
}

# ---- a round ------------------------------------------------------------------------

SCM_DEFAULT_VIEW <- list(round = NULL, candidate = NULL, long = FALSE, timing = FALSE, files = FALSE)

# A round's candidates as pharos lists them, the winner first, then by rank
# or p-value; only `candidate` when one is asked for.
scm_round_candidates <- function(round, candidate = NULL) {
  cands <- round$candidates %||% list()
  key <- vapply(cands, function(c) {
    as.numeric(c$rank %||% c$p_value %||% Inf)
  }, numeric(1))
  cands <- cands[order(key)]
  if (!is.null(candidate)) {
    keep <- vapply(cands, function(c) identical(tolower(c$candidate), tolower(candidate)), logical(1))
    cands <- cands[keep]
  }
  cands
}

# pharos' `--long` line under each candidate of a round (`est 0.05 (0.01) ·
# CI95 ... · df 1 · tries 2`), keyed by candidate, read off the round's text.
scm_round_details <- function(block, names) {
  details <- list()
  current <- NULL
  for (line in block) {
    row <- regmatches(line, regexec("^  (\\S+)\\s", line))[[1]]
    if (length(row) == 2 && row[2] %in% names) {
      current <- row[2]
      next
    }
    if (!is.null(current) && grepl("^ {6}(est|CI95|df|tries) ", line)) {
      items <- strsplit(trimws(line), " · ", fixed = TRUE)[[1]]
      label <- sub(" .*$", "", items)
      details[[current]] <- stats::setNames(as.list(sub("^\\S+ ", "", items)), label)
      current <- NULL
    }
  }
  details
}

# One round as a section: where it got to, its reference, its candidates
# as a table, and under `long`, `timing` and `files` the detail they add.
scm_round_section <- function(round, view = SCM_DEFAULT_VIEW, block = character()) {
  backward <- identical(round$direction, "backward")
  complete <- isTRUE(round$complete)
  counts <- round$counts %||% list()
  cands <- scm_round_candidates(round, view$candidate)
  detail <- isTRUE(view$long) || !is.null(view$round)

  decision <- round$decision %||% ""
  text <- if (complete) {
    decision
  } else {
    concluded <- sum(unlist(counts[c("succeeded", "unusable", "withdrawn")]))
    paste0(
      "in progress: ", concluded, "/", counts$candidates %||% length(cands), " concluded",
      if (nzchar(decision)) paste0(" (", decision, ")")
    )
  }
  extra <- c(
    if ((counts$retries %||% 0) > 0) {
      paste0(counts$retries, if (counts$retries == 1) " retry" else " retries")
    },
    if ((counts$withdrawn %||% 0) > 0) paste0(counts$withdrawn, " withdrawn")
  )
  if (length(extra)) {
    text <- paste0(text, " [", paste(extra, collapse = ", "), "]")
  }
  alert <- scm_alert(if (complete && !is.null(round$winner)) "success" else "info", text)

  facts <- scm_facts(
    "Reference",
    paste0(
      round$reference_model,
      if (!scm_is_missing(round$reference_ofv)) paste0(" (OFV ", scm_fmt_ofv(round$reference_ofv), ")")
    )
  )
  add_fact <- function(label, value) {
    facts <<- rbind(facts, scm_facts(label, value))
  }
  if (detail) {
    add_fact("Retained before", scm_none_or_list(round$retained_before))
  }
  if (complete) {
    add_fact(if (detail) "Retained after" else "Retained", scm_none_or_list(round$retained_after))
  }
  if (detail && length(round$removed_before)) {
    add_fact("Removed before", scm_none_or_list(round$removed_before))
  }
  if (isTRUE(view$timing)) {
    fits <- sum(vapply(round$candidates %||% list(), function(c) length(c$attempts), integer(1)))
    add_fact(
      "Time",
      paste0(fits, if (fits == 1) " fit" else " fits", ", wall ", scm_fmt_duration(round$timing$wall_seconds))
    )
  }

  if (length(cands) == 0) {
    return(scm_section(scm_round_title(round$round), list(alert), facts))
  }

  details <- if (isTRUE(view$long)) {
    scm_round_details(block, vapply(cands, function(c) c$candidate, character(1)))
  } else {
    list()
  }
  rows <- list()
  colors <- list()
  for (c in cands) {
    selected <- isTRUE(c$selected)
    status <- c$status %||% ""
    more <- details[[c$candidate]] %||% list()
    rows[[length(rows) + 1]] <- list(
      Candidate = c$candidate,
      OFV = scm_fmt_ofv(c$ofv),
      dOFV = scm_fmt_delta(c$delta_ofv),
      `p-value` = scm_fmt_p(c$p_value),
      Significant = if (is.null(c$significant)) "" else if (isTRUE(c$significant)) "Yes" else "No",
      Result = if (selected) {
        if (backward) "dropped" else "added"
      } else if (backward && isTRUE(c$significant)) {
        "kept"
      } else {
        ""
      },
      Estimate = more$est %||% "",
      `95% CI` = more$CI95 %||% "",
      df = if (isTRUE(view$long)) scm_fmt_number(c$df) else "",
      Attempts = if (length(c$attempts) > 1 || isTRUE(view$long)) as.character(length(c$attempts)) else "",
      `Cond. No.` = more[["cond#"]] %||% "",
      Heuristics = if (detail) paste(unlist(c$heuristics), collapse = ", ") else "",
      `Est. time` = if (isTRUE(view$timing)) scm_fmt_duration(c$timing$estimation_seconds) else "",
      Status = if (status == "succeeded") "" else status
    )
    i <- length(rows)
    if (selected) {
      colors[[length(colors) + 1]] <- list(row = i, column = "Result", color = "green")
    }
    tone <- switch(status, unusable = "red", withdrawn = "yellow", running = "blue", pending = "grey", NA_character_)
    if (!is.na(tone)) {
      colors[[length(colors) + 1]] <- list(row = i, column = "Status", color = tone)
    }
  }
  tables <- list(list(
    title = NULL,
    data = scm_display_table(
      rows,
      colors,
      numeric = c("OFV", "dOFV", "p-value", "df", "Attempts", "Cond. No.")
    )
  ))

  # The attempts behind a candidate: every one in a detailed view, and
  # always those of a fit that did not succeed.
  attempts <- scm_attempt_rows(cands, detail, isTRUE(view$timing))
  if (length(attempts$rows)) {
    tables[[length(tables) + 1]] <- list(
      title = "Attempts",
      data = scm_display_table(attempts$rows, attempts$colors)
    )
  }
  if (isTRUE(view$files)) {
    tables[[length(tables) + 1]] <- list(title = "Files", data = scm_file_table(cands))
  }
  scm_section(scm_round_title(round$round), list(alert), facts, tables)
}

scm_attempt_rows <- function(cands, all, timing) {
  rows <- list()
  colors <- list()
  for (c in cands) {
    status <- c$status %||% ""
    if (!all && status %in% c("succeeded", "pending")) {
      next
    }
    add <- function(model, outcome, seconds = NULL) {
      rows[[length(rows) + 1]] <<- list(
        Candidate = c$candidate,
        Model = model %||% "",
        Outcome = outcome %||% "",
        `Est. time` = if (timing) scm_fmt_duration(seconds) else ""
      )
      if (!identical(outcome, "succeeded") && !identical(outcome, status)) {
        colors[[length(colors) + 1]] <<- list(row = length(rows), column = "Outcome", color = "red")
      }
    }
    for (a in c$superseded %||% list()) {
      add(a$model, paste0(a$outcome, " (superseded)"), a$timing$estimation_seconds)
    }
    models <- character()
    for (a in c$attempts %||% list()) {
      add(a$model, a$outcome, a$timing$estimation_seconds)
      models <- c(models, a$model)
    }
    # A dispatched model joins the attempts once it finishes; one still
    # running is named here.
    if (!is.null(c$model) && nzchar(c$model) && !c$model %in% models) {
      add(c$model, status)
    }
  }
  list(rows = rows, colors = colors)
}

scm_file_table <- function(cands) {
  rows <- lapply(cands, function(c) {
    f <- c$files %||% list()
    list(
      Candidate = c$candidate,
      `Run directory` = f$run_dir %||% "",
      lst = f$lst %||% "",
      ext = f$ext %||% "",
      Summary = f$summary_json %||% ""
    )
  })
  scm_display_table(rows)
}

# The reference fit has no candidates to rank: it is a row of the models
# table. It gets a section only for what that row cannot hold.
scm_reference_section <- function(round, view) {
  cands <- round$candidates %||% list()
  if (length(cands) == 0) {
    return(NULL)
  }
  fit <- cands[[1]]
  needs <- length(fit$attempts) > 1 ||
    length(fit$heuristics) > 0 ||
    isTRUE(view$files) ||
    isTRUE(view$timing) ||
    !identical(fit$status, "succeeded")
  if (!needs) {
    return(NULL)
  }
  facts <- scm_facts()
  if (length(fit$heuristics)) {
    facts <- rbind(facts, scm_facts("Heuristics", paste(unlist(fit$heuristics), collapse = ", ")))
  }
  if (isTRUE(view$timing)) {
    n <- length(fit$attempts)
    facts <- rbind(facts, scm_facts(
      "Time",
      paste0(n, if (n == 1) " fit" else " fits", ", wall ", scm_fmt_duration(round$timing$wall_seconds))
    ))
  }
  tables <- list()
  attempts <- scm_attempt_rows(cands, length(fit$attempts) > 1, isTRUE(view$timing))
  if (length(attempts$rows)) {
    tables[[length(tables) + 1]] <- list(
      title = "Attempts",
      data = scm_display_table(attempts$rows, attempts$colors)
    )
  }
  if (isTRUE(view$files)) {
    tables[[length(tables) + 1]] <- list(title = "Files", data = scm_file_table(cands))
  }
  scm_section(
    "Reference fit",
    list(scm_alert(if (identical(fit$status, "succeeded")) "success" else "info", round$decision %||% fit$status)),
    facts,
    tables
  )
}

# The text block of a round: the one whose first line names it.
scm_round_block <- function(blocks, round) {
  for (b in blocks) {
    if (grepl(paste0("^", round, "(\\s|:)"), b[1])) {
      return(b)
    }
  }
  character()
}

# An open round of a status: pharos' live line per fit (what it has done, or
# where it is, read now), as a table under the round's progress.
scm_open_round_section <- function(round, block) {
  title <- paste0(scm_round_title(round$round), " (open)")
  if (length(block) == 0) {
    return(scm_round_section(round))
  }
  alerts <- list(scm_alert("info", sub(paste0("^", round$round, ": *"), "", block[1])))
  names <- vapply(round$candidates %||% list(), function(c) c$candidate, character(1))
  rows <- list()
  colors <- list()
  for (line in block[-1]) {
    m <- regmatches(line, regexec("^\\s*(\\S)\\s+(\\S+)\\s*(.*)$", line))[[1]]
    if (length(m) == 4 && m[3] %in% names) {
      rows[[length(rows) + 1]] <- list(` ` = m[2], Candidate = m[3], Progress = trimws(m[4]))
      tone <- switch(m[2], "✓" = "green", "✗" = "red", "●" = "blue", "grey")
      colors[[length(colors) + 1]] <- list(row = length(rows), column = " ", color = tone)
    } else {
      alerts[[length(alerts) + 1]] <- scm_alert("info", trimws(line))
    }
  }
  tables <- if (length(rows)) {
    list(list(title = NULL, data = scm_display_table(rows, colors)))
  } else {
    list()
  }
  scm_section(title, alerts, NULL, tables)
}

# ---- the three records --------------------------------------------------------------

scm_record_title <- function(x) {
  what <- sub("^hyperion_nonmem_scm_", "", class(x)[1])
  paste0("SCM ", tools::toTitleCase(what), ": ", x$out_dir %||% "")
}

build_scm_record_parts <- function(x) {
  if (inherits(x, "hyperion_nonmem_scm_plan")) {
    build_scm_plan_parts(x)
  } else if (inherits(x, "hyperion_nonmem_scm_status")) {
    build_scm_status_parts(x)
  } else {
    build_scm_summary_parts(x)
  }
}

build_scm_summary_parts <- function(x) {
  view <- utils::modifyList(SCM_DEFAULT_VIEW, attr(x, "view") %||% list(), keep.null = TRUE)
  blocks <- scm_text_blocks(scm_text_lines(x))
  models <- scm_models_section(x)
  facts <- scm_record_facts(
    x,
    if (length(blocks)) blocks[[1]] else character(),
    drop = if (is.null(models)) character() else c("working full model", "reduced covariate model")
  )

  rounds <- x$rounds %||% list()
  if (!is.null(view$round) && length(rounds)) {
    rounds <- list(scm_find_round(rounds, tolower(trimws(as.character(view$round))), view$round))
  }
  sections <- list(models)
  for (r in rounds) {
    section <- if (identical(r$reference_model, "-")) {
      scm_reference_section(r, view)
    } else if (!is.null(view$candidate) && length(scm_round_candidates(r, view$candidate)) == 0) {
      NULL
    } else {
      scm_round_section(r, view, scm_round_block(blocks, r$round))
    }
    sections[[length(sections) + 1]] <- section
  }

  alerts <- list()
  n <- x$totals$unusable %||% 0
  if (n > 0) {
    alerts[[1]] <- scm_alert(
      "warning",
      paste0(
        n, if (n == 1) " candidate fit was" else " candidate fits were",
        " unusable; see the round tables above."
      )
    )
  }
  list(
    title = scm_record_title(x),
    top = list(),
    facts = facts,
    sections = Filter(Negate(is.null), sections),
    alerts = alerts
  )
}

build_scm_status_parts <- function(x) {
  split <- scm_split_driver_lines(scm_text_lines(x))
  blocks <- scm_text_blocks(split$lines)
  models <- scm_models_section(x)
  facts <- scm_record_facts(
    x,
    if (length(blocks)) blocks[[1]] else character(),
    drop = if (is.null(models)) character() else c("working full model", "reduced covariate model")
  )

  sections <- list(models)
  # A process that has not started has its plan's rounds to come, not rounds
  # to show: pharos prints its facts alone.
  started <- length(blocks) > 1
  for (r in if (started) x$rounds %||% list() else list()) {
    section <- if (identical(r$reference_model, "-")) {
      scm_reference_section(r, SCM_DEFAULT_VIEW)
    } else if (isTRUE(r$complete)) {
      scm_round_section(r)
    } else {
      scm_open_round_section(r, scm_round_block(blocks, r$round))
    }
    sections[[length(sections) + 1]] <- section
  }

  # The driver's state closes the status: an alert in the color of its news.
  alerts <- list()
  for (i in seq_len(nrow(split$driver))) {
    label <- split$driver$label[i]
    value <- split$driver$value[i]
    type <- if (label == "allocation") {
      "warning"
    } else if (startsWith(value, "NOT RUNNING")) {
      "danger"
    } else if (grepl("cannot check", value, fixed = TRUE)) {
      "warning"
    } else {
      "info"
    }
    alerts[[length(alerts) + 1]] <- scm_alert(type, paste0(scm_fact_label(label), ": ", value))
  }
  list(
    title = scm_record_title(x),
    top = list(),
    facts = facts,
    sections = Filter(Negate(is.null), sections),
    alerts = alerts
  )
}

build_scm_plan_parts <- function(x) {
  blocks <- scm_text_blocks(scm_text_lines(x))
  main <- which(vapply(blocks, function(b) grepl("^model *:", b[1]), logical(1)))[1]
  if (is.na(main)) {
    main <- length(blocks) + 1L
  }
  # What comes before the plan is a note about it: the process a submit
  # with `overwrite = TRUE` would discard.
  top <- lapply(blocks[seq_len(main - 1)], function(b) {
    scm_alert("warning", paste(trimws(b), collapse = " "))
  })

  o <- x$options %||% list()
  parsed <- if (main <= length(blocks)) scm_parse_facts(blocks[[main]]) else scm_parse_facts(character())
  from_text <- function(label) {
    value <- parsed$value[parsed$label == label]
    if (length(value)) scm_facts(scm_fact_label(label), value[1]) else scm_facts()
  }
  direction <- unlist(o$direction)
  alphas <- c(
    if ("forward" %in% direction) paste0("forward = ", o$forward_alpha),
    if ("backward" %in% direction) paste0("backward = ", o$backward_alpha)
  )
  facts <- rbind(
    scm_facts("Model", x$model %||% ""),
    scm_facts("Direction", paste(direction, collapse = " -> ")),
    scm_facts("Alphas", paste(alphas, collapse = ", ")),
    scm_facts(
      "Retries",
      paste0(o$max_retries %||% 3, " per failed fit, jittered ", 100 * (o$retry_jitter %||% 0.05), "%")
    ),
    scm_facts("Covariance step", paste0(scm_on_off(o$cov_step), " in the round fits")),
    from_text("working full fit"),
    from_text("reduced covariate fit"),
    scm_facts("Rounds", if (is.null(o$num_rounds)) "all" else paste0("pause after ", o$num_rounds, " (resumable)")),
    from_text("max models")
  )

  sections <- list()
  cands <- x$candidates %||% list()
  if (length(cands)) {
    rows <- lapply(cands, function(c) {
      list(
        Effect = c$name,
        Theta = paste0("THETA(", c$theta, ")"),
        Type = c$kind %||% "",
        Initial = scm_fmt_number(c$initial),
        Fixed = scm_fmt_number(c$fixed),
        Lower = scm_fmt_number(c$lower),
        Upper = scm_fmt_number(c$upper)
      )
    })
    table <- scm_display_table(rows, numeric = c("Initial", "Fixed", "Lower", "Upper"))
    notes <- c(
      "Initial: the effect's initial estimate the first time it is tested.",
      "Fixed: what the effect is fixed at when it is held out.",
      if (any(c("Lower", "Upper") %in% names(table))) {
        "Lower, Upper: the $THETA bounds the effect is estimated under while it is in the model."
      }
    )
    sections[[1]] <- scm_section(
      "Candidates",
      tables = list(list(title = NULL, data = table)),
      notes = notes
    )
  }

  # What follows the plan is the process already in its output directory:
  # where it got to, and what this plan changes about it.
  after <- if (main < length(blocks)) blocks[(main + 1):length(blocks)] else list()
  if (length(after)) {
    history <- scm_parse_facts(unlist(after))
    process <- scm_facts(scm_fact_label(history$label), history$value)
    progress <- history$label == "progress"
    process$tone[progress] <- vapply(history$value[progress], scm_status_tone, character(1))
    sections[[length(sections) + 1]] <- scm_section("Process in the output directory", facts = process)
  }

  alerts <- lapply(attr(x, "warnings") %||% character(), function(w) scm_alert("warning", w))
  if (isFALSE(attr(x, "resumable"))) {
    alerts[[length(alerts) + 1]] <- scm_alert(
      "danger",
      paste0(
        "The SCM process in '", x$out_dir, "' cannot resume under this plan; ",
        "submit with `overwrite = TRUE` to discard it."
      )
    )
  }
  list(
    title = scm_record_title(x),
    top = top,
    facts = facts,
    sections = sections,
    alerts = alerts
  )
}
