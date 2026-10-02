# Model editing ------------------------------------------------------------
#
# Edits happen in memory on a hyperion_nonmem_model. Each verb returns a new
# model; nothing touches disk until write_model(). The model carries:
#   - unsaved_edits: number of edits since the last write
#   - from_copy:     TRUE when the chain started from copy_model()
#   - refs:          data.frame(name, kind, index, col) of declared placeholder
#                    refs; col is 0 except for off-diagonal OMEGA elements
#   - refs_used:     ref names used by a code edit so far

check_model_object <- function(model) {
  if (!inherits(model, "hyperion_nonmem_model")) {
    rlang::abort("`model` must be a hyperion_nonmem_model.")
  }
  invisible(model)
}

edit_path <- function(model) {
  from_config_relative(attr(model, "model_source"))
}

empty_refs <- function() {
  data.frame(
    name = character(),
    kind = character(),
    index = integer(),
    col = integer(),
    stringsAsFactors = FALSE
  )
}

model_refs <- function(model) {
  attr(model, "refs", exact = TRUE) %||% empty_refs()
}

# Copy the edit-session attributes from `old` onto a freshly parsed `new`.
carry_edit_attrs <- function(new, old) {
  for (a in c("run_status", "from_copy", "refs", "refs_used")) {
    attr(new, a) <- attr(old, a, exact = TRUE)
  }
  attr(new, "unsaved_edits") <- (attr(old, "unsaved_edits") %||% 0L) + 1L
  new
}

add_ref <- function(model, ref, kind, index, col = 0L) {
  refs <- model_refs(model)
  refs <- rbind(
    refs,
    data.frame(
      name = ref,
      kind = kind,
      index = as.integer(index),
      col = as.integer(col)
    )
  )
  attr(model, "refs") <- refs
  model
}

check_new_ref <- function(model, ref) {
  if (is.null(ref)) {
    return(invisible())
  }
  if (!is.character(ref) || length(ref) != 1 || is.na(ref) || !nzchar(ref)) {
    rlang::abort("`ref` must be a single non-empty string.")
  }
  if (!grepl("^[A-Za-z][A-Za-z0-9_.]*$", ref)) {
    rlang::abort(paste0(
      "`ref` \"",
      ref,
      "\" must start with a letter and use only letters, ",
      "digits, `_` and `.`."
    ))
  }
  if (ref %in% model_refs(model)$name) {
    rlang::abort(paste0(
      "`ref` \"",
      ref,
      "\" is already declared in this chain."
    ))
  }
  invisible()
}

# Text of a bare-name argument: `TVCL` -> "TVCL", `DADT(2)` -> "DADT(2)".
# A string is accepted as-is.
name_text <- function(expr, arg) {
  if (is.character(expr) && length(expr) == 1) {
    return(expr)
  }
  if (is.symbol(expr) || is.call(expr)) {
    return(rlang::expr_text(expr))
  }
  rlang::abort(paste0("`", arg, "` must be a name, e.g. `TVCL`."))
}

check_number <- function(x, arg) {
  if (length(x) == 1 && is.na(x)) {
    rlang::abort(paste0(
      "`",
      arg,
      "` can't be NA. Leave the argument out to keep the current value."
    ))
  }
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x)) {
    rlang::abort(paste0("`", arg, "` must be a single number."))
  }
  invisible(x)
}

check_whole <- function(x, arg) {
  check_number(x, arg)
  if (x != round(x) || x < 0 || x > .Machine$integer.max) {
    rlang::abort(paste0("`", arg, "` must be a whole number."))
  }
  invisible(x)
}

# A 1-based position, e.g. THETA(index) or the index-th $EST.
check_index <- function(x, arg) {
  check_number(x, arg)
  if (x != round(x) || x < 1 || x > .Machine$integer.max) {
    rlang::abort(paste0("`", arg, "` must be a whole number, 1 or more."))
  }
  invisible(x)
}

check_string <- function(x, arg) {
  if (length(x) == 1 && is.na(x)) {
    rlang::abort(paste0(
      "`",
      arg,
      "` can't be NA. Leave the argument out to keep it, or use NULL to remove it."
    ))
  }
  if (!is.character(x) || length(x) != 1) {
    rlang::abort(paste0("`", arg, "` must be a single string."))
  }
  invisible(x)
}

check_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    rlang::abort(paste0("`", arg, "` must be TRUE or FALSE."))
  }
  invisible(x)
}

# An update argument that can be kept (left out), removed (NULL) or set.
# `present` is !missing(arg) in the caller.
arg_change <- function(present, value, arg, check) {
  if (!present) {
    return(list(action = "keep", value = NULL))
  }
  if (is.null(value)) {
    return(list(action = "remove", value = NULL))
  }
  check(value, arg)
  list(action = "set", value = value)
}

# An update argument that can be kept (left out) or set, but not removed.
arg_value <- function(present, value, arg, check) {
  if (!present) {
    return(NULL)
  }
  if (is.null(value)) {
    rlang::abort(paste0(
      "`",
      arg,
      "` can't be removed. Leave it out to keep it."
    ))
  }
  check(value, arg)
  value
}

# Run an edit that returns a model and carry the session attributes over.
apply_edit <- function(model, f, ...) {
  check_model_object(model)
  new <- f(attr(model, "model_text"), edit_path(model), ...)
  carry_edit_attrs(new, model)
}

#' Add a THETA row
#'
#' Appends a new row after the last THETA, in memory. With `index`, the new
#' row becomes `THETA(index)` instead and every later `THETA(n)` in the code
#' is renumbered. Use `ref` to give it a name that later code edits can use as
#' a placeholder, e.g. `{wt.theta}`, so pharos fills in the THETA number.
#'
#' @param model A hyperion_nonmem_model object.
#' @param init Initial estimate.
#' @param lower,upper Optional bounds.
#' @param fix Whether the THETA is fixed.
#' @param comment Optional comment, written after `;`.
#' @param ref Optional name for use in placeholders.
#' @param index Optional position for the new row. Leave out to append.
#' @return The edited model (not yet written; see [write_model()]).
#' @export
#'
#' @examples \dontrun{
#' mod |>
#'   add_theta(1.2, comment = "WT-on-Vc", ref = "wt_v") |>
#'   update_mu(V, append = "+ {wt_v.theta} * LOG(WT / 70)")
#' }
add_theta <- function(
  model,
  init,
  lower = NULL,
  upper = NULL,
  fix = FALSE,
  comment = NULL,
  ref = NULL,
  index = NULL
) {
  check_model_object(model)
  check_new_ref(model, ref)
  check_number(init, "init")
  if (!is.null(lower)) {
    check_number(lower, "lower")
  }
  if (!is.null(upper)) {
    check_number(upper, "upper")
  }
  check_flag(fix, "fix")
  if (!is.null(comment)) {
    check_string(comment, "comment")
  }
  if (!is.null(index)) {
    check_index(index, "index")
  }
  res <- edit_add_theta_impl(
    attr(model, "model_text"),
    edit_path(model),
    init,
    lower,
    upper,
    fix,
    comment,
    if (is.null(index)) NULL else as.integer(index)
  )
  new <- carry_edit_attrs(res$model, model)
  # Later THETAs moved down one; keep their refs pointing at the same rows.
  refs <- model_refs(new)
  shift <- refs$kind == "theta" & refs$index >= res$index
  if (any(shift)) {
    refs$index[shift] <- refs$index[shift] + 1L
    attr(new, "refs") <- refs
  }
  if (!is.null(ref)) {
    new <- add_ref(new, ref, "theta", res$index)
  }
  new
}

edit_code <- function(
  model,
  record,
  append,
  lhs,
  mu_of,
  within,
  comment = list(action = "keep", value = NULL),
  replace = NULL
) {
  check_model_object(model)
  if (!is.null(replace)) {
    check_string(replace, "replace")
    if (is.null(lhs) && is.null(mu_of)) {
      rlang::abort("`replace` needs `lhs`, the statement to replace.")
    }
    if (!is.null(append) || !is.null(within)) {
      rlang::abort("Give `replace` or `append`/`within`, not both.")
    }
  }
  if (is.null(append)) {
    if (is.null(lhs) && is.null(mu_of)) {
      rlang::abort("`append` is required without a target statement.")
    }
    if (identical(comment$action, "keep") && is.null(replace)) {
      rlang::abort("Give `append`, `replace` or `comment`.")
    }
    append <- character()
  } else if (!is.character(append) || length(append) == 0 || anyNA(append)) {
    rlang::abort("`append` must be a character vector of NONMEM code.")
  }
  refs <- model_refs(model)
  res <- edit_code_impl(
    attr(model, "model_text"),
    edit_path(model),
    record,
    append,
    lhs,
    mu_of,
    within,
    refs$name,
    refs$kind,
    refs$index,
    refs$col,
    comment$action,
    comment$value,
    replace
  )
  new <- carry_edit_attrs(res$model, model)
  attr(new, "refs_used") <- union(attr(model, "refs_used"), res$used)
  new
}

#' Edit a code record
#'
#' Without `lhs`, each element of `append` is added as a new statement, and
#' may span several lines (e.g. an `IF ... ENDIF` block). Pharos places it
#' after the last line like it: after the last statement assigning the same
#' variable, a `MU_n` line after the last MU line, a line with an ETA after
#' the last line with an ETA, anything else at the end.
#'
#' With `lhs`, `append` is added to the end of that statement's right-hand
#' side, or to the end of the `within` call inside it; `replace` swaps the
#' whole right-hand side instead; and `comment` replaces the statement's
#' comment (`NULL` removes it).
#'
#' Placeholders like `{wt.theta}` are replaced with the NONMEM reference for a
#' row declared with `ref`. Suffixes: `.theta`; `.eta`, `.mu`, `.omega` for
#' OMEGAs; `.eps`, `.sigma` for SIGMAs.
#'
#' @param model A hyperion_nonmem_model object.
#' @param lhs Bare name of the statement to add to, e.g. `TVCL` or `DADT(2)`.
#' @param append NONMEM code to add.
#' @param replace New right-hand side for `lhs`, replacing the old one. The
#'   statement's comment is kept.
#' @param within Bare name of a function call inside `lhs`'s right-hand side,
#'   e.g. `SQRT`.
#' @param comment New comment for the `lhs` statement, or `NULL` to remove it.
#'   Leave out to keep it.
#' @return The edited model (not yet written; see [write_model()]).
#' @name update_code_records
#'
#' @examples \dontrun{
#' mod |>
#'   add_theta(0.75, comment = "WT-on-CL", ref = "wt_cl") |>
#'   update_pk(TVCL, append = "* (WT / 70)**{wt_cl.theta}")
#'
#' mod |> update_error(IPRED, replace = "LOG(F + 0.0001)")
#' }
NULL

#' @rdname update_code_records
#' @export
update_pk <- function(model, lhs, append, replace, within, comment) {
  lhs <- if (missing(lhs)) NULL else name_text(rlang::enexpr(lhs), "lhs")
  within <- if (missing(within)) {
    NULL
  } else {
    name_text(rlang::enexpr(within), "within")
  }
  append <- if (missing(append)) NULL else append
  replace <- if (missing(replace)) NULL else replace
  comment <- arg_change(
    !missing(comment),
    if (missing(comment)) NULL else comment,
    "comment",
    check_string
  )
  edit_code(model, "PK", append, lhs, NULL, within, comment, replace)
}

#' @rdname update_code_records
#' @export
update_error <- function(model, lhs, append, replace, within, comment) {
  lhs <- if (missing(lhs)) NULL else name_text(rlang::enexpr(lhs), "lhs")
  within <- if (missing(within)) {
    NULL
  } else {
    name_text(rlang::enexpr(within), "within")
  }
  append <- if (missing(append)) NULL else append
  replace <- if (missing(replace)) NULL else replace
  comment <- arg_change(
    !missing(comment),
    if (missing(comment)) NULL else comment,
    "comment",
    check_string
  )
  edit_code(model, "ERROR", append, lhs, NULL, within, comment, replace)
}

#' @rdname update_code_records
#' @export
update_des <- function(model, lhs, append, replace, within, comment) {
  lhs <- if (missing(lhs)) NULL else name_text(rlang::enexpr(lhs), "lhs")
  within <- if (missing(within)) {
    NULL
  } else {
    name_text(rlang::enexpr(within), "within")
  }
  append <- if (missing(append)) NULL else append
  replace <- if (missing(replace)) NULL else replace
  comment <- arg_change(
    !missing(comment),
    if (missing(comment)) NULL else comment,
    "comment",
    check_string
  )
  edit_code(model, "DES", append, lhs, NULL, within, comment, replace)
}

#' Add to a parameter's MU line
#'
#' Finds the `$PK` statement that assigns `param`, takes the one `MU_n` its
#' right-hand side uses, and adds `append` to the end of that MU line. Use
#' this in MU-referenced models, where covariates live on the MU lines.
#'
#' @param model A hyperion_nonmem_model object.
#' @param param Bare name of the parameter, e.g. `V`.
#' @param append NONMEM code to add.
#' @param replace New right-hand side for the MU line, replacing the old one.
#' @param within Bare name of a function call inside the MU line.
#' @param comment New comment for the MU line, or `NULL` to remove it. Leave
#'   out to keep it.
#' @return The edited model (not yet written; see [write_model()]).
#' @export
#'
#' @examples \dontrun{
#' mod |>
#'   add_theta(1.2, comment = "WT-on-Vc", ref = "wt_v") |>
#'   update_mu(V, append = "+ {wt_v.theta} * LOG(WT / 70)")
#' }
update_mu <- function(model, param, append, replace, within, comment) {
  param <- name_text(rlang::enexpr(param), "param")
  within <- if (missing(within)) {
    NULL
  } else {
    name_text(rlang::enexpr(within), "within")
  }
  append <- if (missing(append)) NULL else append
  replace <- if (missing(replace)) NULL else replace
  comment <- arg_change(
    !missing(comment),
    if (missing(comment)) NULL else comment,
    "comment",
    check_string
  )
  edit_code(model, "PK", append, NULL, param, within, comment, replace)
}

#' Write an edited model to disk
#'
#' Writes the model to its own file. A model that came from [copy_model()] is
#' always written. A model read with [read_model()] is not written over if it
#' has already been run, unless `overwrite = TRUE`.
#'
#' Warns when a `$MODEL` compartment that isn't `INITIALOFF` has no `DADT`
#' in `$DES`.
#'
#' @param model A hyperion_nonmem_model object.
#' @param overwrite Write even if a model read with [read_model()] has been run.
#' @return The model with no unsaved edits, invisibly.
#' @export
write_model <- function(model, overwrite = FALSE) {
  check_model_object(model)
  path <- edit_path(model)

  if (!isTRUE(attr(model, "from_copy")) && !isTRUE(overwrite)) {
    status <- get_run_status(path)
    if (!identical(status, "not_run")) {
      rlang::abort(paste0(
        "`",
        basename(path),
        "` has already been run, so its outputs would no ",
        "longer match the file. Use copy_model() to make a new model, or ",
        "write_model(overwrite = TRUE)."
      ))
    }
  }

  refs <- model_refs(model)
  unused <- setdiff(refs$name, attr(model, "refs_used"))
  if (length(unused) > 0) {
    rlang::abort(paste0(
      "Declared but never used in code: ",
      paste0("`", unused, "`", collapse = ", "),
      ". Use them in a code edit or remove the `ref`."
    ))
  }

  missing_dadt <- edit_missing_dadt_impl(attr(model, "model_text"))
  if (length(missing_dadt) > 0) {
    rlang::warn(paste0(
      "No DADT assigned in $DES for compartment ",
      paste(missing_dadt, collapse = ", "),
      ", which isn't INITIALOFF in $MODEL. NM-TRAN warns about this (WARNING 45)."
    ))
  }

  writeLines(attr(model, "model_text"), con = path, sep = "", useBytes = TRUE)
  attr(model, "unsaved_edits") <- 0L
  invisible(model)
}

# Diff ---------------------------------------------------------------------

model_text <- function(x) {
  if (inherits(x, "hyperion_nonmem_model")) {
    return(attr(x, "model_text"))
  }
  if (is.character(x) && length(x) == 1) {
    return(paste(
      readLines(from_config_relative(x), warn = FALSE),
      collapse = "\n"
    ))
  }
  rlang::abort("`parent` must be a hyperion_nonmem_model or a path.")
}

# Split model text into records, keyed like "$PK" or "$TABLE#2".
split_records <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  starts <- grepl("^\\s*\\$[A-Za-z]", lines)
  names <- toupper(sub("^\\s*(\\$[A-Za-z]+).*$", "\\1", lines[starts]))
  group <- cumsum(starts)
  keys <- c("(header)", names)
  counts <- vapply(
    seq_along(keys),
    function(i) sum(keys[seq_len(i)] == keys[i]),
    integer(1)
  )
  keys <- ifelse(counts > 1, paste0(keys, "#", counts), keys)
  records <- split(lines, factor(keys[group + 1], levels = unique(keys)))
  records[lengths(records) > 0]
}

# Line diff via longest common subsequence. Returns changed lines only,
# prefixed "- " (parent) or "+ " (model).
diff_lines <- function(a, b) {
  n <- length(a)
  m <- length(b)
  lcs <- matrix(0L, n + 1, m + 1)
  for (i in rev(seq_len(n))) {
    for (j in rev(seq_len(m))) {
      lcs[i, j] <- if (a[i] == b[j]) {
        lcs[i + 1, j + 1] + 1L
      } else {
        max(lcs[i + 1, j], lcs[i, j + 1])
      }
    }
  }
  out <- character()
  i <- 1
  j <- 1
  while (i <= n && j <= m) {
    if (a[i] == b[j]) {
      i <- i + 1
      j <- j + 1
    } else if (lcs[i + 1, j] >= lcs[i, j + 1]) {
      out <- c(out, paste0("- ", a[i]))
      i <- i + 1
    } else {
      out <- c(out, paste0("+ ", b[j]))
      j <- j + 1
    }
  }
  if (i <= n) {
    out <- c(out, paste0("- ", a[i:n]))
  }
  if (j <= m) {
    out <- c(out, paste0("+ ", b[j:m]))
  }
  out
}

#' Show what changed between two models
#'
#' Compares a model with its parent, grouped by record. With `parent = NULL`,
#' compares the model with its own file on disk, which is exactly what
#' [write_model()] will change.
#'
#' @param model A hyperion_nonmem_model object.
#' @param parent A hyperion_nonmem_model, a path to a model file, or `NULL`
#'   for the model's own file.
#' @return A `hyperion_model_diff` object; print it to see the changes.
#' @export
#'
#' @examples \dontrun{
#' mod |> add_theta(1.2, ref = "wt") |> update_mu(V, append = "+ {wt.theta}") |>
#'   diff_models()
#' diff_models(read_model("s1001.mod"), parent = "s1000.mod")
#' }
diff_models <- function(model, parent = NULL) {
  check_model_object(model)
  new_text <- attr(model, "model_text")
  old_text <- if (is.null(parent)) {
    model_text(attr(model, "model_source"))
  } else {
    model_text(parent)
  }

  old <- split_records(old_text)
  new <- split_records(new_text)
  keys <- union(names(new), names(old))

  changes <- list()
  unchanged <- character()
  for (key in keys) {
    d <- diff_lines(old[[key]] %||% character(), new[[key]] %||% character())
    if (length(d) > 0) {
      changes[[key]] <- d
    } else {
      unchanged <- c(unchanged, key)
    }
  }

  structure(
    list(changes = changes, unchanged = unchanged),
    class = "hyperion_model_diff"
  )
}

#' @exportS3Method base::print hyperion_model_diff
print.hyperion_model_diff <- function(x, ...) {
  if (length(x$changes) == 0) {
    cat("No changes.\n")
    return(invisible(x))
  }
  for (key in names(x$changes)) {
    cat(cli::style_bold(sub("#.*$", "", key)), "\n", sep = "")
    for (line in x$changes[[key]]) {
      styled <- if (startsWith(line, "+")) {
        cli::col_green(line)
      } else {
        cli::col_red(line)
      }
      cat(styled, "\n", sep = "")
    }
    cat("\n")
  }
  shown <- unique(sub("#.*$", "", setdiff(x$unchanged, "(header)")))
  if (length(shown) > 0) {
    cat(
      cli::col_grey(paste0("(unchanged: ", paste(shown, collapse = ", "), ")")),
      "\n",
      sep = ""
    )
  }
  invisible(x)
}
