#' Get a model's `$PK` block as rendered equations
#'
#' Extracts the `$PK` block from a model and renders each statement back to a
#' NONMEM equation (e.g. `"CL = TVCL*EXP(ETA(1))"`). Rendering is performed by
#' pharos from the parsed model, so comments and blank lines in the original
#' block are not included — only the equations.
#'
#' @param model A `hyperion_nonmem_model` object from [read_model()].
#' @return A `hyperion_nonmem_model_pk` object: a character vector of
#'   equations, one per `$PK` statement, with a print method. Empty when the
#'   model has no `$PK` block.
#' @seealso [get_model_content()]
#' @export
#'
#' @examples \dontrun{
#' mod <- read_model("model/nonmem/run001.mod")
#' mod |> get_model_pk()
#' }
get_model_pk <- function(model) {
  if (!inherits(model, "hyperion_nonmem_model")) {
    rlang::abort("model must be a hyperion_nonmem_model object")
  }
  equations <- get_pk_statements(model)
  structure(
    equations,
    model = attr(model, "filename"),
    class = "hyperion_nonmem_model_pk"
  )
}

#' Print method for hyperion_nonmem_model_pk objects
#'
#' @param x A `hyperion_nonmem_model_pk` object
#' @param ... Additional arguments (ignored)
#' @return Invisible copy of `x`
#' @exportS3Method base::print hyperion_nonmem_model_pk
print.hyperion_nonmem_model_pk <- function(x, ...) {
  name <- attr(x, "model")
  header <- if (is.null(name)) "$PK" else paste0("$PK (", name, ")")
  cli::cli_h1(header)
  if (length(x) == 0) {
    cli::cli_text("{.emph (no $PK block)}")
  } else {
    cat(unclass(x), sep = "\n")
    cat("\n")
  }
  invisible(x)
}
