#' Get the lineage of the models fitted by an SCM process
#'
#' Builds the lineage tree of an SCM process's fits from its process record.
#' The result is the same `hyperion_nonmem_tree` object that
#' [get_model_lineage()] returns for a project, so it can be used wherever a
#' lineage is: `hyperion.tables::apply_summary_spec()` makes a run summary
#' table of the process from it, with one row per fit and sections taken
#' from the tags.
#'
#' The tree contains the base fit, the attempt that counted for each
#' candidate in every round, the re-fit of the working full model, and the
#' reduced covariate model. Each fit is based on the model its round tested
#' against, so the dOFV and p-value in a summary are those of the round. The
#' working full model and the reduced covariate model are based on the
#' reference fit.
#'
#' Each node is tagged `"scm"` and with its round (`"reference"`,
#' `"forward_round1"`, ..., `"working_full"`, `"reduced_covariate"`). The
#' candidate a round selected is also tagged `"selected"`, and a fit that
#' was retried is tagged `"retry"`. The description of a node is the step of
#' its fit, as pharos writes it to the fits' metadata: what a candidate adds
#' or drops (e.g., `"add CRCL_CL"`), or the name of a fit that is not a
#' candidate (e.g., `"base model"`).
#'
#' Each node also has a `covariates` element listing the covariates in its
#' model, taken from the process record. The fits' metadata does not hold
#' these, so a lineage from [get_model_lineage()] has none. Use
#' `hyperion.tables::scm_summary_spec(show_all_covariates = TRUE)` to show
#' them.
#'
#' Unlike [get_model_lineage()], this function does not need the fits'
#' metadata files, so it works wherever the process ran.
#'
#' @param scm A `hyperion_nonmem_scm` object from [read_scm()], or the path
#'   to the SCM config (`<stem>scm.toml`).
#' @return A `hyperion_nonmem_tree` S3 object of the fits recorded so far.
#' @export
#'
#' @examples \dontrun{
#' scm <- read_scm("model/nonmem/scm/run001scm.toml")
#' get_scm_lineage(scm)
#'
#' library(hyperion.tables)
#' scm |>
#'   get_scm_lineage() |>
#'   apply_summary_spec(scm_summary_spec()) |>
#'   make_summary_table()
#' }
get_scm_lineage <- function(scm) {
  record <- get_scm_summary(scm, NULL, NULL, FALSE, FALSE, FALSE)
  nodes <- scm_lineage_nodes(record)
  if (length(nodes) == 0) {
    rlang::abort("The SCM process has not started, so it has no models yet")
  }
  structure(
    list(nodes = nodes),
    class = "hyperion_nonmem_tree",
    focal = character(),
    verbose = FALSE
  )
}

#' Build the lineage nodes of an SCM process
#'
#' @param record A hyperion_nonmem_scm_summary object
#' @return List of tree nodes, in the order the process fitted them
#' @keywords internal
#' @noRd
scm_lineage_nodes <- function(record) {
  out_dir <- record$out_dir
  node <- function(model, based_on, description, tags, covariates) {
    list(
      name = file.path(out_dir, model),
      model = list(
        based_on = if (is.null(based_on)) {
          list()
        } else {
          list(file.path(out_dir, based_on))
        },
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
      if (isTRUE(cnd$selected)) {
        tags <- c(tags, "selected")
      }
      if (length(cnd$attempts %||% list()) > 1) {
        tags <- c(tags, "retry")
      }
      # The reference fit's candidate is named "base" or "full"
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
    # The working full model's re-fit follows the last forward round
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

#' Get the model of the reference fit
#'
#' The reference fit is the base model, or the full model of a process that
#' starts with backward elimination.
#'
#' @param record A hyperion_nonmem_scm_summary object
#' @return Path to the model relative to the output directory, or NULL
#' @keywords internal
#' @noRd
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

#' Get the covariates in a candidate's model
#'
#' A candidate's model has the covariates its round started with, plus the
#' candidate (forward) or without it (backward). The reference fit has none
#' for a base model and all of them for a full model.
#'
#' @param r A round from the process record
#' @param cnd A candidate of the round
#' @param is_reference Whether the round is the reference fit
#' @return Character vector of covariate names
#' @keywords internal
#' @noRd
scm_model_covariates <- function(r, cnd, is_reference) {
  if (is_reference) {
    return(as.character(unlist(r$retained_after)))
  }
  before <- as.character(unlist(r$retained_before))
  name <- cnd$candidate %||% ""
  if (identical(r$direction, "backward")) {
    setdiff(before, name)
  } else {
    union(before, name)
  }
}
