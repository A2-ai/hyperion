# Record edits: $TABLE, $EST, $SUBROUTINES, $COV, $DATA, $MODEL, renames.
# See model-edit.R for the edit-session attributes.

#' Add columns to a $TABLE
#'
#' Columns go after the table's last column, before its options. Each column
#' must be an `$INPUT` column, assigned in the model's code, or a NONMEM table
#' item such as `CWRES`.
#'
#' @param model A hyperion_nonmem_model object.
#' @param file The table's `FILE=` name. Give this or `index` when the model
#'   has more than one table.
#' @param index The table's position among the `$TABLE` records.
#' @param append Column names to add.
#' @return The edited model (not yet written; see [write_model()]).
#' @export
#'
#' @examples \dontrun{
#' mod |> update_table(file = "s1002par.tab", append = "ALAG1")
#' }
update_table <- function(model, file, index, append) {
  file <- if (missing(file)) NULL else check_string(file, "file")
  index <- if (missing(index)) NULL else as.integer(check_index(index, "index"))
  if (!is.character(append) || length(append) == 0 || anyNA(append)) {
    rlang::abort("`append` must be a character vector of column names.")
  }
  apply_edit(model, edit_update_table_impl, file, index, append)
}

option_value <- function(x, name) {
  if (is.character(x) && length(x) == 1 && !is.na(x)) {
    return(x)
  }
  if (is.numeric(x) && length(x) == 1 && is.finite(x)) {
    return(
      if (x == round(x)) format(x, scientific = FALSE) else as.character(x)
    )
  }
  rlang::abort(paste0(
    "`",
    name,
    "` must be a single value, TRUE to add a flag, or NULL to remove it."
  ))
}

#' Change $EST options
#'
#' Options are named arguments using NONMEM's names in lower case. A value
#' sets or replaces the option, `TRUE` adds a flag, and `NULL` removes the
#' option. Names must match the record exactly: if it has `MAX=`, use `max =`,
#' not `maxeval =`.
#'
#' @param model A hyperion_nonmem_model object.
#' @param ... Options, e.g. `maxeval = 0`, `posthoc = TRUE`, `msfo = NULL`.
#' @param index Which `$EST` record, needed when there are several.
#' @return The edited model (not yet written; see [write_model()]).
#' @export
#'
#' @examples \dontrun{
#' mod |> update_est(maxeval = 0, msfo = "s1005.msf")
#' }
update_est <- function(model, ..., index) {
  check_model_object(model)
  opts <- est_options(rlang::list2(...), allow_remove = TRUE)
  n_est <- edit_est_count_impl(attr(model, "model_text"))
  if (missing(index)) {
    if (n_est > 1) {
      rlang::abort(paste0(
        "The model has ",
        n_est,
        " $EST records; give `index`."
      ))
    }
    index <- 1L
  }
  check_index(index, "index")
  apply_edit(
    model,
    edit_update_est_impl,
    as.integer(index) - 1L,
    opts$names,
    opts$actions,
    opts$values
  )
}

est_options <- function(opts, allow_remove) {
  if (length(opts) == 0) {
    rlang::abort("Give at least one option, e.g. `maxeval = 0`.")
  }
  nms <- names(opts)
  if (is.null(nms) || any(!nzchar(nms))) {
    rlang::abort("Every option must be named, e.g. `maxeval = 0`.")
  }
  actions <- character(length(opts))
  values <- character(length(opts))
  for (k in seq_along(opts)) {
    x <- opts[[k]]
    if (is.null(x) && allow_remove) {
      actions[k] <- "remove"
    } else if (is.null(x) || isFALSE(x)) {
      rlang::abort(paste0(
        "`",
        nms[k],
        "` must be a value or TRUE; leave an option out to not set it."
      ))
    } else if (isTRUE(x)) {
      actions[k] <- "flag"
    } else {
      actions[k] <- "value"
      values[k] <- option_value(x, nms[k])
    }
  }
  if (!allow_remove && anyDuplicated(toupper(nms))) {
    rlang::abort("Each option can be given only once.")
  }
  list(names = nms, actions = actions, values = values)
}

#' Add or remove a $EST record
#'
#' `add_est()` adds a `$EST` after the last one, e.g. an `IMP` step after
#' `SAEM`. Options are named arguments as in [update_est()]: a value sets the
#' option, `TRUE` adds a flag. `remove_est()` removes the `$EST` at `index`;
#' a model always keeps at least one.
#'
#' @param model A hyperion_nonmem_model object.
#' @param ... Options, e.g. `method = "IMP"`, `eonly = 1`, `niter = 5`.
#' @param index Which `$EST` record to remove, counting from 1.
#' @return The edited model (not yet written; see [write_model()]).
#' @name add_est
#'
#' @examples \dontrun{
#' mod |>
#'   update_est(method = "SAEM", nburn = 2000, niter = 1000) |>
#'   add_est(method = "IMP", eonly = 1, niter = 5, isample = 3000)
#'
#' mod |> remove_est(2)
#' }
NULL

#' @rdname add_est
#' @export
add_est <- function(model, ...) {
  check_model_object(model)
  opts <- est_options(rlang::list2(...), allow_remove = FALSE)
  apply_edit(model, edit_add_est_impl, opts$names, opts$actions, opts$values)
}

#' @rdname add_est
#' @export
remove_est <- function(model, index) {
  check_index(index, "index")
  apply_edit(model, edit_remove_est_impl, as.integer(index) - 1L)
}

#' Change $SUBROUTINES
#'
#' Leave an argument out to keep it; `NULL` removes `trans` or `tol`.
#' Changing `advan` never touches `trans`, and whether the combination is
#' valid is left to [check_model()].
#'
#' @param model A hyperion_nonmem_model object.
#' @param advan ADVAN number, e.g. `13`.
#' @param trans TRANS number, e.g. `1`, or `NULL` to remove.
#' @param tol `TOL=` value, or `NULL` to remove.
#' @return The edited model (not yet written; see [write_model()]).
#' @export
#'
#' @examples \dontrun{
#' mod |> update_subroutines(advan = 4, trans = 4)
#' }
update_subroutines <- function(model, advan, trans, tol) {
  advan <- arg_value(
    !missing(advan),
    if (missing(advan)) NULL else advan,
    "advan",
    check_whole
  )
  trans <- arg_change(
    !missing(trans),
    if (missing(trans)) NULL else trans,
    "trans",
    check_whole
  )
  tol <- arg_change(
    !missing(tol),
    if (missing(tol)) NULL else tol,
    "tol",
    check_whole
  )
  as_int <- function(x) if (is.null(x)) NULL else as.integer(x)
  apply_edit(
    model,
    edit_update_subroutines_impl,
    as_int(advan),
    trans$action,
    as_int(trans$value),
    tol$action,
    as_int(tol$value)
  )
}

#' Remove $COV
#'
#' @param model A hyperion_nonmem_model object.
#' @return The edited model (not yet written; see [write_model()]).
#' @export
remove_cov <- function(model) {
  apply_edit(model, edit_remove_cov_impl)
}

#' Change the $PROBLEM title
#'
#' Replaces the first line of `$PROBLEM`. The note [copy_model()] adds
#' ("created from pharos see <run>_metadata.json for details.") stays at the
#' end, and any lines below the title are kept.
#'
#' @param model A hyperion_nonmem_model object.
#' @param text The new title, one line.
#' @return The edited model (not yet written; see [write_model()]).
#' @export
#'
#' @examples \dontrun{
#' mod |> update_problem("Base model, FOCEI estimation")
#' }
update_problem <- function(model, text) {
  check_model_object(model)
  check_string(text, "text")
  apply_edit(model, edit_update_problem_impl, text)
}

#' Change the $DATA path
#'
#' `path` is written exactly as given, so it is relative to the model file,
#' like the path [get_data_path()] reads. `IGNORE=`/`ACCEPT=` are untouched.
#'
#' @param model A hyperion_nonmem_model object.
#' @param path The new `$DATA` path.
#' @return The edited model (not yet written; see [write_model()]).
#' @export
#'
#' @examples \dontrun{
#' mod |> update_data("../../../data/derived/warfarin_v2.csv")
#' }
update_data <- function(model, path) {
  check_model_object(model)
  check_string(path, "path")
  absolute <- grepl("^(/|~|[A-Za-z]:)", path)
  full <- if (absolute) {
    path.expand(path)
  } else {
    file.path(dirname(edit_path(model)), path)
  }
  if (!file.exists(full)) {
    rlang::abort(paste0(
      "No data file at `",
      path,
      "` (relative to the model file)."
    ))
  }
  apply_edit(model, edit_update_data_impl, path)
}

data_filter <- function(model, ignore, accept, action) {
  check_model_object(model)
  if (is.null(ignore) == is.null(accept)) {
    rlang::abort("Give exactly one of `ignore` or `accept`.")
  }
  kind <- if (is.null(ignore)) "accept" else "ignore"
  conditions <- if (is.null(ignore)) accept else ignore
  if (
    !is.character(conditions) || length(conditions) == 0 || anyNA(conditions)
  ) {
    rlang::abort(paste0(
      "`",
      kind,
      "` must be a character vector of conditions, e.g. \"DV.EQ.0\"."
    ))
  }
  apply_edit(model, edit_data_filter_impl, kind, action, conditions)
}

#' Add or remove $DATA filters
#'
#' Conditions are written as NONMEM writes them, e.g. `"BLQ.EQ.1"`. Each
#' one is added to `$DATA` as its own `IGNORE=(...)` or `ACCEPT=(...)`
#' option, after any already there. NONMEM doesn't allow `IGNORE` and
#' `ACCEPT` lists together, so adding one kind is refused when the other is
#' there. `IGNORE=@` and `IGNORE=#` are left as they are and can be used with
#' `ACCEPT`. Labels must be `$INPUT` columns.
#'
#' `remove_data_filter()` matches the condition ignoring case and spaces.
#'
#' @param model A hyperion_nonmem_model object.
#' @param ignore Conditions for records to drop.
#' @param accept Conditions for records to keep.
#' @return The edited model (not yet written; see [write_model()]).
#' @name data_filters
#'
#' @examples \dontrun{
#' mod |> add_data_filter(ignore = "BLQ.EQ.1")
#' mod |> remove_data_filter(ignore = "BLQ.EQ.1")
#' }
NULL

#' @rdname data_filters
#' @export
add_data_filter <- function(model, ignore = NULL, accept = NULL) {
  data_filter(model, ignore, accept, "add")
}

#' @rdname data_filters
#' @export
remove_data_filter <- function(model, ignore = NULL, accept = NULL) {
  data_filter(model, ignore, accept, "remove")
}

#' Add lines to $MODEL
#'
#' Each element of `append` is a new line at the end of `$MODEL`, e.g.
#' `"COMP=(PERIPH)"`. New compartments are numbered after the existing ones;
#' code edits are checked against the compartment count.
#'
#' @param model A hyperion_nonmem_model object.
#' @param append Lines to add.
#' @return The edited model (not yet written; see [write_model()]).
#' @export
#'
#' @examples \dontrun{
#' mod |>
#'   update_model_record(append = "COMP=(PERIPH)") |>
#'   update_des(append = "DADT(3) = K23*A(2) - K32*A(3)")
#' }
update_model_record <- function(model, append) {
  if (!is.character(append) || length(append) == 0 || anyNA(append)) {
    rlang::abort("`append` must be a character vector of $MODEL lines.")
  }
  apply_edit(model, edit_update_model_record_impl, append)
}

#' Rename a variable
#'
#' Renames a variable assigned in the model's code, as whole names, across
#' every code record and `$TABLE` columns. Comments and `$INPUT` are never
#' changed: rename a data column in the dataset instead.
#'
#' @param model A hyperion_nonmem_model object.
#' @param from Bare name of the variable, e.g. `V`.
#' @param to Bare new name, e.g. `V2`.
#' @return The edited model (not yet written; see [write_model()]).
#' @export
#'
#' @examples \dontrun{
#' mod |> rename_variable(V, V2)
#' }
rename_variable <- function(model, from, to) {
  from <- name_text(rlang::enexpr(from), "from")
  to <- name_text(rlang::enexpr(to), "to")
  apply_edit(model, edit_rename_variable_impl, from, to)
}
