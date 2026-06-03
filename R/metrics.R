#' Akaike information criterion for NONMEM models
#'
#' @param object a `hyperion_nonmem_model` object.
#' @param ... additional `hyperion_nonmem_model` objects.
#' @param k the penalty per parameter. Passed to pharos as the AIC penalty;
#'   coerced to an integer.
#'
#' @return For a single model, the AIC as a numeric scalar. For multiple
#'   models, a `data.frame` with a `df` column (number of estimated
#'   parameters) and an `AIC` column, one row per model.
#'
#' @exportS3Method stats::AIC
AIC.hyperion_nonmem_model <- function(object, ..., k = 2) {
  models <- rlang::list2(object, ...)
  ic <- lapply(models, get_information_criteria, penalty = k)
  aic <- vapply(ic, `[[`, numeric(1), "aic")
  if (length(models) == 1L) {
    return(aic)
  }
  data.frame(
    df = vapply(ic, `[[`, integer(1), "n_parameters"),
    AIC = aic,
    row.names = vapply(models, get_model_name, character(1))
  )
}

#' Bayesian information criterion for NONMEM models
#'
#' @param object a `hyperion_nonmem_model` object.
#' @param ... additional `hyperion_nonmem_model` objects.
#'
#' @return For a single model, the BIC as a numeric scalar. For multiple
#'   models, a `data.frame` with a `df` column (number of estimated
#'   parameters) and a `BIC` column, one row per model.
#'
#' @exportS3Method stats::BIC
BIC.hyperion_nonmem_model <- function(object, ...) {
  models <- rlang::list2(object, ...)
  ic <- lapply(models, get_information_criteria, penalty = NULL)
  bic <- vapply(ic, `[[`, numeric(1), "bic")
  if (length(models) == 1L) {
    return(bic)
  }
  data.frame(
    df = vapply(ic, `[[`, integer(1), "n_parameters"),
    BIC = bic,
    row.names = vapply(models, get_model_name, character(1))
  )
}

#' Compare two NONMEM runs
#'
#' Compares two runs on objective function value (OFV) and information criteria
#' (AIC, BIC), and reports a likelihood ratio test when the runs are nested.
#' Deltas are computed as `first - second`.
#'
#' @param first,second a path, run output directory, or `hyperion_nonmem_model`
#'   object for each run.
#'
#' @return a `hyperion_run_comparison` object: a list with the `first` and
#'   `second` run names, a `metrics` data.frame (OFV/AIC/BIC with `first`,
#'   `second`, and `delta` columns), the estimated-parameter counts `df`, the
#'   number of observations `n_observations`, and a likelihood ratio test `lrt`.
#' @export
#'
#' @examples \dontrun{
#' compare_runs("model/nonmem/run001", "model/nonmem/run002")
#' }
compare_runs <- function(first, second) {
  res <- compare_runs_impl(first, second)

  metrics <- data.frame(
    metric = c("OFV", "AIC", "BIC"),
    first = c(res$first$ofv, res$first$aic, res$first$bic),
    second = c(res$second$ofv, res$second$aic, res$second$bic),
    delta = c(res$delta_ofv, res$delta_aic, res$delta_bic)
  )

  structure(
    list(
      first = comparison_run_name(first),
      second = comparison_run_name(second),
      metrics = metrics,
      df = c(first = res$first$n_parameters, second = res$second$n_parameters),
      n_observations = res$first$n_observations,
      lrt = list(
        status = res$lrt_status,
        df = res$lrt_df,
        p_value = res$lrt_p_value
      )
    ),
    class = "hyperion_run_comparison"
  )
}

#' @noRd
comparison_run_name <- function(x) {
  if (inherits(x, "hyperion_nonmem_model")) {
    get_model_name(x)
  } else {
    tools::file_path_sans_ext(basename(x))
  }
}

#' @exportS3Method base::print hyperion_run_comparison
print.hyperion_run_comparison <- function(x, digits = 3, ...) {
  cli::cli_h1("Model Comparison: {x$first} vs {x$second}")

  tab <- data.frame(
    format(round(x$metrics$first, digits), nsmall = digits),
    format(round(x$metrics$second, digits), nsmall = digits),
    format(round(x$metrics$delta, digits), nsmall = digits),
    row.names = x$metrics$metric
  )
  names(tab) <- c(x$first, x$second, paste(x$first, "-", x$second))
  print(tab, right = TRUE)

  cli::cli_text("")
  cli::cli_text(
    "{.strong Parameters:} {x$df[['first']]} vs {x$df[['second']]}    {.strong Observations:} {x$n_observations}"
  )

  lrt <- comparison_lrt_fields(x)
  if (lrt$computed) {
    cli::cli_text(
      "{.strong LRT:} full = {lrt$full}, reduced = {lrt$reduced}, df = {lrt$df}, p = {lrt$p}"
    )
  } else {
    cli::cli_text("{.strong LRT:} {lrt$message}")
  }

  invisible(x)
}

#' @noRd
comparison_lrt_fields <- function(x) {
  if (!identical(x$lrt$status, "computed")) {
    msg <- switch(
      x$lrt$status,
      not_nested = "not applicable (runs are not nested)",
      no_added_parameters = "not applicable (no added parameters)",
      x$lrt$status
    )
    return(list(computed = FALSE, message = msg))
  }

  full <- if (x$df[["first"]] >= x$df[["second"]]) x$first else x$second
  reduced <- if (identical(full, x$first)) x$second else x$first
  p <- if (x$lrt$p_value < 0.001) {
    formatC(x$lrt$p_value, format = "e", digits = 2)
  } else {
    formatC(x$lrt$p_value, format = "f", digits = 4)
  }
  list(computed = TRUE, full = full, reduced = reduced, df = x$lrt$df, p = p)
}

#' @exportS3Method knitr::knit_print
knit_print.hyperion_run_comparison <- function(x, digits = 3, ...) {
  tab <- data.frame(
    metric = x$metrics$metric,
    first = format(round(x$metrics$first, digits), nsmall = digits),
    second = format(round(x$metrics$second, digits), nsmall = digits),
    delta = format(round(x$metrics$delta, digits), nsmall = digits),
    check.names = FALSE
  )
  names(tab) <- c("", x$first, x$second, paste(x$first, "-", x$second))

  lrt <- comparison_lrt_fields(x)
  lrt_line <- if (lrt$computed) {
    paste0(
      "<strong>LRT:</strong> full = ",
      lrt$full,
      ", reduced = ",
      lrt$reduced,
      ", df = ",
      lrt$df,
      ", p = ",
      lrt$p
    )
  } else {
    paste0("<strong>LRT:</strong> ", lrt$message)
  }

  output <- c(
    "",
    paste0(
      "<strong>Model Comparison: ",
      x$first,
      " vs ",
      x$second,
      "</strong>"
    ),
    "",
    as.character(knitr::kable(
      tab,
      format = "html",
      align = c("l", "r", "r", "r"),
      table.attr = 'class="table table-striped"',
      row.names = FALSE,
      escape = FALSE
    )),
    "",
    paste0(
      "<strong>Parameters:</strong> ",
      x$df[["first"]],
      " vs ",
      x$df[["second"]],
      " &nbsp;&nbsp; <strong>Observations:</strong> ",
      x$n_observations
    ),
    "",
    lrt_line
  )

  knitr::asis_output(paste(output, collapse = "\n"))
}
