#' Get a model fitted by an SCM process
#'
#' Reads one of the models the SCM process fitted, by the round it belongs
#' to, as a `hyperion_nonmem_model`: what [read_model()] gives for any run,
#' so `summary()` and the parameter functions work on it. The model comes
#' from the process record ([summary()]), so a round is available as soon as
#' pharos has recorded it.
#'
#' @param scm A `hyperion_nonmem_scm` object or the SCM config (`<stem>scm.toml`).
#' @param round Which fit: a selection round by name (`"forward_round1"`,
#'   `"backward_round2"`) or by its number (`2`, `"round 2"`), as
#'   `summary()` takes it; `"reference"` for the reference fit the forward or
#'   backward phase started from; `"forward_final"` for the forward model's
#'   re-fit; or `"final"` for the final model.
#' @param candidate In a selection round, the effect whose fit to get: the
#'   one tested in a forward round, or held out in a backward round. `NULL`,
#'   the default, gives the round's winner, the model that was added or
#'   dropped. Ignored for `"reference"`, `"forward_final"` and `"final"`.
#' @return A `hyperion_nonmem_model` of the attempt that counted: the retry
#'   that succeeded when a fit was retried.
#' @export
#'
#' @examples \dontrun{
#' scm <- read_scm("model/nonmem/scm/run001scm.toml")
#' final <- get_scm_model(scm, "final")
#' summary(final)
#' get_scm_model(scm, "forward_round2")                   # the round's winner
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

# The model `round` and `candidate` name, relative to the out_dir.
scm_model_in_record <- function(record, round, candidate = NULL) {
  if (!(is.character(round) || is.numeric(round)) || length(round) != 1 || is.na(round)) {
    rlang::abort("`round` must be a round name or number, \"reference\", \"forward_final\" or \"final\"")
  }
  if (!is.null(candidate) && (!is.character(candidate) || length(candidate) != 1 || is.na(candidate))) {
    rlang::abort("`candidate` must be a single effect name")
  }
  rounds <- record$rounds %||% list()
  if (length(rounds) == 0) {
    rlang::abort("the SCM process has not started; no models to get")
  }
  key <- tolower(trimws(as.character(round)))
  if (key == "final") {
    path <- record$final_model
    if (is.null(path)) {
      rlang::abort(paste0("no final model yet; the SCM process is ", record$status))
    }
    return(path)
  }
  if (key == "forward_final") {
    path <- record$forward_final$model
    if (is.null(path)) {
      rlang::abort(paste0("no forward final model yet; the SCM process is ", record$status))
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
        paste0(r$round, " has no winner: ", r$decision),
        i = paste0("give a candidate: ", paste(names, collapse = ", "))
      ))
    }
  } else {
    i <- match(tolower(candidate), tolower(names))
    if (is.na(i)) {
      rlang::abort(paste0(
        "no candidate named ", candidate, " in ", r$round, "; it has: ",
        paste(names, collapse = ", ")
      ))
    }
  }
  path <- cands[[i]]$model
  if (is.null(path)) {
    rlang::abort(paste0(names[i], " in ", r$round, " has no fitted model yet (", cands[[i]]$status, ")"))
  }
  path
}

# The round `key` names, as pharos' summary finds it: an exact round name
# (case-insensitively), or the Nth selection round as "N" or "round N".
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
      "no round matching '", shown, "'; rounds so far: ", paste(names, collapse = ", ")
    ))
  }
  rounds[[i]]
}
