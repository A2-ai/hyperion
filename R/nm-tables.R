# ---- NONMEM output tables --------------------------------------------------

#' Resolve a model's run output directory
#'
#' @param model A `hyperion_nonmem_model` object.
#' @return Absolute path to the directory holding the run's output files.
#' @noRd
model_output_dir <- function(model) {
  # ponytail: dirname(model_source)/<stem> -- does NOT honor pharos.toml
  # output_dir redirects. Swap to a pharos-resolved path once pharos exposes
  # one. Same guess summary() (build_running_summary) and parameter-info use.
  model_path <- from_config_relative(attr(model, "model_source"))
  base_name <- tools::file_path_sans_ext(basename(model_path))
  file.path(dirname(model_path), base_name)
}

#' Does a `$TABLE` record carry the FIRSTONLY option?
#'
#' @param tbl A `$TABLE` record from a parsed model (`model$tables[[i]]`).
#' @return `TRUE` if the record is FIRSTONLY (one row per subject).
#' @noRd
nm_table_is_firstonly <- function(tbl) {
  tokens <- toupper(vapply(
    tbl$options,
    function(opt) as.character(opt[[1]] %||% NA_character_),
    character(1)
  ))
  "FIRSTONLY" %in% tokens
}

#' List a model's `$TABLE` output records
#'
#' Flattens the model's parsed `$TABLE` records into a data.frame for
#' discovery: which output files exist, their index (for [read_nm_table()]),
#' and whether each is FIRSTONLY (one row per subject) or per-record.
#'
#' @param model A `hyperion_nonmem_model` object from [read_model()].
#' @return A data.frame with one row per `$TABLE` record and columns:
#'   \describe{
#'     \item{index}{position in `model$tables`, usable as `which`}
#'     \item{file}{the output file name from `FILE=`}
#'     \item{firstonly}{`TRUE` for FIRSTONLY records}
#'   }
#'   Empty when the model has no `$TABLE` records.
#' @seealso [read_nm_table()]
#' @export
#'
#' @examples \dontrun{
#' mod <- read_model("model/nonmem/run001.mod")
#' list_tables(mod)
#' }
list_tables <- function(model) {
  if (!inherits(model, "hyperion_nonmem_model")) {
    rlang::abort("model must be a hyperion_nonmem_model object")
  }
  tbls <- model$tables %||% list()
  data.frame(
    index = seq_along(tbls),
    file = vapply(tbls, function(t) t$file %||% NA_character_, character(1)),
    firstonly = vapply(tbls, nm_table_is_firstonly, logical(1)),
    stringsAsFactors = FALSE
  )
}

#' Read a NONMEM `$TABLE` output file
#'
#' Reads one of a model's `$TABLE` output files into a data.frame of doubles.
#' This is raw I/O only: every column is parsed as numeric (NONMEM writes
#' undefined values as `NaN`/`Inf`), with no decoding or metadata applied.
#'
#' @param model A `hyperion_nonmem_model` object from [read_model()].
#' @param which Which table to read. One of:
#'   `NULL` (the only table, error if there are several);
#'   an integer index into [list_tables()];
#'   a bare file name (resolved against the run output directory); or
#'   a path containing `/` (taken as a config-relative path, the manual
#'   override when the output directory cannot be derived from the model).
#' @return A data.frame with every column parsed as double.
#' @seealso [list_tables()]
#' @export
#'
#' @examples \dontrun{
#' mod <- read_model("model/nonmem/run001.mod")
#' read_nm_table(mod)                 # the only $TABLE
#' read_nm_table(mod, "patab1")       # by file name
#' read_nm_table(mod, which = 2)      # by index
#' }
read_nm_table <- function(model, which = NULL) {
  if (!inherits(model, "hyperion_nonmem_model")) {
    rlang::abort("model must be a hyperion_nonmem_model object")
  }

  path <- resolve_nm_table_path(model, which)
  if (!file.exists(path)) {
    rlang::abort(paste0("$TABLE output file not found: ", path))
  }

  # NONMEM $TABLE output is a "TABLE NO." line, a header row, then all-numeric
  # data. skip = 1 drops the "TABLE NO." line; colClasses forces every column
  # to double so a stray "NaN"/"Inf" parses to NaN/Inf instead of flipping the
  # column to character.
  # ponytail: reads only the first "TABLE NO." block; handle concatenated
  # blocks (multi-method runs) when a real model needs it.
  utils::read.table(
    path,
    skip = 1,
    header = TRUE,
    colClasses = "numeric"
  )
}

#' Resolve `which` to an output file path
#' @noRd
resolve_nm_table_path <- function(model, which) {
  if (is.character(which) && grepl("/", which, fixed = TRUE)) {
    return(from_config_relative(which))
  }

  tbls <- model$tables %||% list()
  if (length(tbls) == 0) {
    rlang::abort("model has no $TABLE records")
  }

  if (is.null(which)) {
    if (length(tbls) > 1) {
      rlang::abort(
        "model has multiple $TABLE records; specify `which` (see list_tables())"
      )
    }
    which <- 1L
  } else if (is.character(which)) {
    which <- match(
      which,
      vapply(tbls, function(t) t$file %||% NA_character_, character(1))
    )
    if (is.na(which)) {
      rlang::abort(
        "no $TABLE record matches that file name (see list_tables())"
      )
    }
  } else if (!is.numeric(which) || which < 1 || which > length(tbls)) {
    rlang::abort("`which` must be a valid table index, file name, or path")
  }

  file.path(model_output_dir(model), tbls[[which]]$file)
}
