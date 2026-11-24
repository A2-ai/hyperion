#' Get a model fitted by an SCM process
#'
#' Reads one of the models fitted by an SCM process, selected by the round
#' it belongs to. The result is the same `hyperion_nonmem_model` object
#' that [read_model()] returns for any run, so `summary()` and the parameter
#' functions work on it. The model is located from the process record (see
#' [summary()]), so a round is available as soon as pharos has recorded it.
#'
#' @param scm A `hyperion_nonmem_scm` object from [read_scm()], or the path
#'   to the SCM config (`<stem>scm.toml`).
#' @param round Fit to get. One of: a selection round by name
#'   (`"forward_round1"`, `"backward_round2"`) or by number (`2`,
#'   `"round 2"`), as `summary()` accepts it; `"reference"` for the reference
#'   fit the forward or backward phase started from; `"forward_final"` for
#'   the re-fit of the forward model; or `"final"` for the
#'   final model.
#' @param candidate In a selection round, the effect whose fit to get: the
#'   effect tested in a forward round, or held out in a backward round.
#'   `NULL` (the default) returns the round's selected candidate, the model
#'   that was added or dropped. Ignored for `"reference"`, `"forward_final"`
#'   and `"final"`.
#' @return A `hyperion_nonmem_model` object for the attempt that counted. If
#'   the fit was retried, this is the retry that succeeded.
#' @export
#'
#' @examples \dontrun{
#' scm <- read_scm("model/nonmem/scm/run001scm.toml")
#' final <- get_scm_model(scm, "final")
#' summary(final)
#' get_scm_model(scm, "forward_round2")                   # selected candidate
#' get_scm_model(scm, "forward_round2", candidate = "WT_V")
#' get_scm_model(scm, 2, candidate = "WT_V")              # the same
#' get_scm_model(scm, "reference")
#' }
get_scm_model <- function(scm, round, candidate = NULL) {
  record <- get_scm_summary(scm, NULL, NULL, FALSE, FALSE, FALSE)
  rel <- scm_model_in_record(record, round, candidate)
  read_model(file.path(attr(record, "out_dir"), rel))
}

SCM_STAGES <- c("reference", "forward_final", "final")

#' Find a fitted model in an SCM process record
#'
#' @param record A hyperion_nonmem_scm_summary object
#' @param round Round name or number, or a stage
#' @param candidate Effect name, or NULL for the round's selected candidate
#' @return Path to the model, relative to the output directory
#' @keywords internal
#' @noRd
scm_model_in_record <- function(record, round, candidate = NULL) {
  if (
    !(is.character(round) || is.numeric(round)) ||
      length(round) != 1 ||
      is.na(round)
  ) {
    rlang::abort(
      "`round` must be a round name or number, \"reference\", \"forward_final\" or \"final\""
    )
  }
  if (
    !is.null(candidate) &&
      (!is.character(candidate) || length(candidate) != 1 || is.na(candidate))
  ) {
    rlang::abort("`candidate` must be a single effect name")
  }
  rounds <- record$rounds %||% list()
  if (length(rounds) == 0) {
    rlang::abort("The SCM process has not started, so it has no models yet")
  }
  key <- tolower(trimws(as.character(round)))
  if (key == "final") {
    path <- record$final_model
    if (is.null(path)) {
      rlang::abort(paste0(
        "No final model yet. The SCM process is ",
        record$status,
        "."
      ))
    }
    return(path)
  }
  if (key == "forward_final") {
    path <- record$forward_final$model
    if (is.null(path)) {
      rlang::abort(paste0(
        "No forward model yet. The SCM process is ",
        record$status,
        "."
      ))
    }
    return(path)
  }
  r <- scm_find_round(rounds, key, round)
  cands <- r$candidates %||% list()
  names <- vapply(cands, function(c) c$candidate, character(1))
  if (identical(r$round, "reference")) {
    i <- 1L
  } else if (is.null(candidate)) {
    i <- which(vapply(cands, function(c) isTRUE(c$selected), logical(1)))
    if (length(i) != 1) {
      rlang::abort(c(
        paste0("Round '", r$round, "' has no selected candidate: ", r$decision),
        i = paste0(
          "Provide `candidate`. Available candidates: ",
          paste(names, collapse = ", ")
        )
      ))
    }
  } else {
    i <- match(tolower(candidate), tolower(names))
    if (is.na(i)) {
      rlang::abort(paste0(
        "Candidate '",
        candidate,
        "' not found in ",
        r$round,
        ". Available candidates: ",
        paste(names, collapse = ", ")
      ))
    }
  }
  path <- cands[[i]]$model
  if (is.null(path)) {
    rlang::abort(paste0(
      "Candidate '",
      names[i],
      "' in ",
      r$round,
      " has no fitted model yet (",
      cands[[i]]$status,
      ")"
    ))
  }
  path
}

#' Find a round in an SCM process record
#'
#' Matches the way pharos' summary does: an exact round name
#' (case-insensitive), or the Nth selection round as "N" or "round N".
#'
#' @param rounds List of rounds from the process record
#' @param key Lowercased, trimmed round name or number
#' @param shown The round as the caller gave it, for the error message
#' @return The matching round
#' @keywords internal
#' @noRd
scm_find_round <- function(rounds, key, shown) {
  names <- vapply(rounds, function(r) r$round, character(1))
  i <- match(key, tolower(names))
  if (is.na(i)) {
    n <- suppressWarnings(as.integer(trimws(sub("^round", "", key))))
    if (!is.na(n) && n >= 1) {
      i <- match(n, vapply(rounds, function(r) as.integer(r$index), integer(1)))
    }
  }
  if (is.na(i)) {
    rlang::abort(paste0(
      "No round matching '",
      shown,
      "'. Rounds so far: ",
      paste(names, collapse = ", ")
    ))
  }
  rounds[[i]]
}
