#' Lineage of the models an SCM process fitted
#'
#' Builds the lineage tree of an SCM process's fits from its record, the
#' same `hyperion_nonmem_tree` that [get_model_lineage()] gives for a project,
#' so it goes where a lineage goes: `hyperion.tables::apply_summary_spec()`
#' makes a run summary table of the process from it, one row per fit, and
#' its tags give the sections. Every fit is based on the model its round
#' tested against, so the summary's dOFV and p-value are the round's; the
#' working full model and the reduced covariate model are based on the reference fit, so
#' theirs are against it.
#'
#' The tree holds the base fit, in every round the attempt that counted for
#' each candidate, the working full model's re-fit and the reduced covariate model. Each node
#' is tagged `"scm"` and with its round (`"reference"`, `"forward_round1"`,
#' ..., `"working_full"`, `"reduced_covariate"`); a round's winner is also tagged
#' `"selected"`, and a fit that was retried `"retry"`. Its description is
#' the fit's step alone, as pharos writes it to the fits' metadata: what a
#' candidate adds or drops, such as `"add CRCL_CL"`, or the name of a fit
#' that is not a candidate, such as `"base model"`.
#'
#' Each node also carries `covariates`, the covariates its model has, from
#' the process record: what the round started with, plus the candidate it
#' adds or without the one it drops. The fits' metadata does not hold these,
#' so a lineage from [get_model_lineage()] has none;
#' `hyperion.tables::scm_summary_spec(description = "verbose")` shows them.
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
  node <- function(model, based_on, description, tags, covariates) {
    list(
      name = file.path(out_dir, model),
      model = list(
        based_on = if (is.null(based_on)) list() else list(file.path(out_dir, based_on)),
        copied_from = "",
        description = description,
        tags = as.list(tags),
        covariates = as.list(as.character(unlist(covariates)))
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
      # The reference fit is the base model, or the full model of a
      # backward-first process: its candidate is named "base" or "full".
      description <- if (is_reference) {
        paste(cnd$candidate %||% "base", "model")
      } else {
        cnd$action %||% cnd$candidate
      }
      nodes[[length(nodes) + 1]] <- node(
        cnd$model,
        if (is_reference) NULL else r$reference_model,
        description,
        tags,
        scm_model_covariates(r, cnd, is_reference)
      )
    }
    # The working full model's re-fit follows the last forward round.
    ff <- record$working_full
    if (i == last_forward && !is.null(ff$model)) {
      nodes[[length(nodes) + 1]] <- node(
        ff$model,
        scm_reference_fit_model(record),
        "working full model",
        c("scm", "working_full"),
        ff$retained
      )
    }
  }
  if (!is.null(record$reduced_covariate_model)) {
    nodes[[length(nodes) + 1]] <- node(
      record$reduced_covariate_model,
      scm_reference_fit_model(record),
      "reduced covariate model",
      c("scm", "reduced_covariate"),
      record$retained
    )
  }
  nodes
}

# The reference fit's model: the base, or the full model of a backward-first
# process. The working full model and the reduced covariate model are based on it.
scm_reference_fit_model <- function(record) {
  for (r in record$rounds %||% list()) {
    if (!identical(r$round, "reference")) {
      next
    }
    for (cnd in r$candidates %||% list()) {
      if (!is.null(cnd$model) && nzchar(cnd$model)) {
        return(cnd$model)
      }
    }
  }
  NULL
}

# The covariates a candidate's model has: what its round started with, plus
# the candidate's (forward) or without it (backward). The reference fit has
# what its round left: none for a base model, all for a full one.
scm_model_covariates <- function(r, cnd, is_reference) {
  if (is_reference) {
    return(as.character(unlist(r$retained_after)))
  }
  before <- as.character(unlist(r$retained_before))
  name <- cnd$candidate %||% ""
  if (identical(r$direction, "backward")) setdiff(before, name) else union(before, name)
}
