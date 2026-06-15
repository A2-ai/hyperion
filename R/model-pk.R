#' Get a model's `$PK` block as a table of equations
#'
#' Extracts the `$PK` block from a model and returns one row per statement. The
#' `target`, `equation`, and `symbols` are all derived by pharos from the parsed
#' model AST. Comments and blank lines in the original block are not included.
#'
#' @param model A `hyperion_nonmem_model` object from [read_model()].
#' @return A `hyperion_nonmem_model_pk` object (a data.frame) with one row per
#'   `$PK` statement and columns:
#'   \describe{
#'     \item{target}{assignment left-hand side (`NA` for non-assignments)}
#'     \item{equation}{the full rendered equation, e.g. `"CL = TVCL*EXP(ETA(1))"`}
#'     \item{symbols}{list column; the identifiers and `THETA`/`ETA`/`EPS`/`ERR`
#'       references used in the statement (math functions excluded)}
#'   }
#'   Empty when the model has no `$PK` block.
#' @seealso [get_model_content()]
#' @export
#'
#' @examples \dontrun{
#' mod <- read_model("model/nonmem/run001.mod")
#' pk <- mod |> get_model_pk()
#' pk$symbols[[which(pk$target == "CL")]] # what CL depends on
#' }
get_model_pk <- function(model) {
  if (!inherits(model, "hyperion_nonmem_model")) {
    rlang::abort("model must be a hyperion_nonmem_model object")
  }

  rows <- get_pk_table(model)

  out <- data.frame(
    target = vapply(
      rows,
      function(r) r$target %||% NA_character_,
      character(1)
    ),
    equation = vapply(rows, function(r) r$equation, character(1)),
    stringsAsFactors = FALSE
  )
  out$symbols <- lapply(rows, function(r) as.character(r$symbols))
  class(out) <- c("hyperion_nonmem_model_pk", "data.frame")
  out
}

#' Print method for hyperion_nonmem_model_pk objects
#'
#' @param x A `hyperion_nonmem_model_pk` object
#' @param ... Additional arguments (ignored)
#' @return Invisible copy of `x`
#' @exportS3Method base::print hyperion_nonmem_model_pk
print.hyperion_nonmem_model_pk <- function(x, ...) {
  cli::cli_h1("$PK")
  if (nrow(x) == 0) {
    cli::cli_text("{.emph (no $PK block)}")
  } else {
    cat(x$equation, sep = "\n")
    cat("\n")
  }
  invisible(x)
}
