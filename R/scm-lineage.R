#' Lineage of the models an SCM process fitted
#'
#' Builds the lineage tree of an SCM process's fits from its record, the
#' same `hyperion_nonmem_tree` that [get_model_lineage()] gives for a project,
#' so it goes where a lineage goes: `hyperion.tables::apply_summary_spec()`
#' makes a run summary table of the process from it, one row per fit, and
#' its tags give the sections. Every fit is based on the model its round
#' tested against, so the summary's dOFV and p-value are the round's.
#'
#' The tree holds the base fit, in every round the attempt that counted for
#' each candidate, the forward model's re-fit and the final model. Each node
#' is tagged `"scm"` and with its round (`"reference"`, `"forward_round1"`,
#' ..., `"forward_final"`, `"final"`); a round's winner is also tagged
#' `"selected"`, and a fit that was retried `"retry"`. Its description is
#' the fit's action, such as `"add WT_CL"`.
#'
#' Unlike [get_model_lineage()], this does not need the fits' metadata files,
#' so it works wherever the process ran.
#'
#' @param scm A `hyperion_nonmem_scm` object or the SCM config (`<stem>scm.toml`).
#' @return A `hyperion_nonmem_tree` of the fits recorded so far.
#' @export
#'
#' @examples \dontrun{
#' scm <- read_scm("model/nonmem/scm/run001scm.toml")
#' get_scm_lineage(scm)
#'
#' library(hyperion.tables)
#' scm |>
#'   get_scm_lineage() |>
#'   apply_summary_spec(scm_summary_spec(scm)) |>
#'   make_summary_table()
#' }
get_scm_lineage <- function(scm) {
  record <- get_scm_summary(scm, NULL, NULL, FALSE, FALSE, FALSE)
  nodes <- scm_lineage_nodes(record)
  if (length(nodes) == 0) {
    rlang::abort("the SCM process has not started; no models to get")
  }
  structure(
    list(nodes = nodes),
    class = "hyperion_nonmem_tree",
    focal = character(),
    verbose = FALSE
  )
}

# The tree's nodes, in the order the process fitted them.
scm_lineage_nodes <- function(record) {
  out_dir <- record$out_dir
  node <- function(model, based_on, description, tags) {
    list(
      name = file.path(out_dir, model),
      model = list(
        based_on = if (is.null(based_on)) list() else list(file.path(out_dir, based_on)),
        copied_from = "",
        description = description,
        tags = as.list(tags)
      )
    )
  }
  nodes <- list()
  rounds <- record$rounds %||% list()
  directions <- vapply(rounds, function(r) r$direction %||% "", character(1))
  last_forward <- max(c(0L, which(directions == "forward")))
  for (i in seq_along(rounds)) {
    r <- rounds[[i]]
    is_reference <- identical(r$round, "reference")
    for (cnd in r$candidates %||% list()) {
      if (is.null(cnd$model) || !nzchar(cnd$model)) {
        next
      }
      tags <- c("scm", r$round)
      if (isTRUE(cnd$selected)) tags <- c(tags, "selected")
      if (length(cnd$attempts %||% list()) > 1) tags <- c(tags, "retry")
      description <- if (is_reference) "base model" else cnd$action %||% cnd$candidate
      nodes[[length(nodes) + 1]] <- node(
        cnd$model,
        if (is_reference) NULL else r$reference_model,
        description,
        tags
      )
    }
    # The forward model's re-fit follows the last forward round.
    ff <- record$forward_final
    if (i == last_forward && !is.null(ff$model)) {
      nodes[[length(nodes) + 1]] <- node(
        ff$model,
        ff$source,
        paste0("forward model: ", scm_retained_text(ff$retained)),
        c("scm", "forward_final")
      )
    }
  }
  if (!is.null(record$final_model)) {
    last <- rounds[[length(rounds)]]
    nodes[[length(nodes) + 1]] <- node(
      record$final_model,
      scm_round_outcome_model(last),
      paste0("final model: ", scm_retained_text(record$retained)),
      c("scm", "final")
    )
  }
  nodes
}

# The model a round leaves as the reference: its winner's fit, or the
# reference it started from when nothing was selected.
scm_round_outcome_model <- function(r) {
  for (cnd in r$candidates %||% list()) {
    if (isTRUE(cnd$selected) && !is.null(cnd$model)) {
      return(cnd$model)
    }
  }
  r$reference_model
}

scm_retained_text <- function(retained) {
  retained <- unlist(retained)
  if (length(retained) == 0) "none" else paste(retained, collapse = ", ")
}
