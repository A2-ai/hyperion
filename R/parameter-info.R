#' Find and read model from .lst file in a directory
#' @noRd
read_model_from_lst_dir <- function(dir_path) {
  lst_candidates <- list.files(
    dir_path,
    pattern = "\\.lst$",
    ignore.case = TRUE,
    full.names = TRUE
  )
  if (length(lst_candidates) == 0) {
    rlang::abort(paste0("lst file not found in run directory: ", dir_path))
  }
  read_model_from_lst(lst_candidates[1])
}

#' Derive output directory from model path and read from .lst file
#' @noRd
read_model_from_lst_path <- function(mod_path) {
  mod_path <- from_config_relative(mod_path)
  # Derive output directory: run001.mod -> run001/
  base_name <- tools::file_path_sans_ext(basename(mod_path))
  parent_dir <- dirname(mod_path)
  output_dir <- file.path(parent_dir, base_name)

  if (!dir.exists(output_dir)) {
    rlang::abort(paste0(
      "Output directory not found for model: ",
      mod_path,
      "\nExpected: ",
      output_dir
    ))
  }

  read_model_from_lst_dir(output_dir)
}

#' Extract all parameter comments from a model as ModelComments object
#'
#' Parses parameter comments and returns structured metadata for theta, omega,
#' and sigma parameters.
#'
#' For model objects sourced from `.mod`/`.ctl` files:
#' - if run status is `"run"`, metadata is read from the corresponding `.lst`
#' - otherwise (`"not_run"`/`"running"`), metadata is read from the model file
#'
#' @param mod A hyperion_nonmem_model object or path to a run output directory
#'   containing an .lst file.
#' @param lookup_path Optional path to a TOML lookup file. If provided, fills
#'   NULL fields (display, description, unit, parameterization) from the lookup.
#'
#' @return A `ModelComments` object containing theta, omega, and sigma comments.
#'
#' @section Comment Parsing:
#' Comments are parsed by pharos according to the `[nonmem.comments]` section
#' of `pharos.toml`. Set `type = "type1"` for strict structured comments, or
#' `type = "type2"` for a more flexible structured grammar. See pharos
#' documentation for accepted formats.
#'
#' @seealso [get_parameter_transform()], [get_theta_names()], [get_comment()]
#' @export
get_model_parameter_info <- function(mod, lookup_path = NULL) {
  if (is.character(mod) && length(mod) == 1) {
    mod_path <- normalizePath(mod, mustWork = FALSE)
    if (!dir.exists(mod_path)) {
      rlang::abort(paste0(
        "mod must be a run output directory containing an .lst file: ",
        mod_path
      ))
    }
    mod <- read_model_from_lst_dir(mod_path)
  } else if (inherits(mod, "hyperion_nonmem_model")) {
    mod_path <- attr(mod, "model_source") %||% "unknown"
    if (!identical(mod_path, "unknown")) {
      mod_path <- from_config_relative(mod_path)
    }
    # If model was read from .mod/.ctl file:
    # - use .lst for completed runs
    # - keep model object for not_run/running
    if (!grepl("\\.lst$", mod_path, ignore.case = TRUE)) {
      run_status <- refresh_run_status(mod)
      if (identical(run_status, "run")) {
        if (identical(mod_path, "unknown")) {
          rlang::abort(
            "Cannot locate .lst for completed run: model_source attribute is missing."
          )
        }
        # Derive output directory from model path (e.g., run001.mod -> run001/)
        mod <- read_model_from_lst_path(mod_path)
      } else if (!run_status %in% c("not_run", "running")) {
        rlang::abort(paste0(
          "model run_status must be 'run', 'running', or 'not_run', got: ",
          run_status
        ))
      }
    }
  } else {
    rlang::abort(
      "mod must be a hyperion_nonmem_model object or path to a run output directory containing an .lst file"
    )
  }

  mod_path <- attr(mod, "model_source") %||% "unknown"
  if (!identical(mod_path, "unknown")) {
    mod_path <- from_config_relative(mod_path)
  }

  info <- get_model_comment_info(mod)

  coerce_field <- function(field, value) {
    if (identical(field, "associated_theta")) {
      if (length(value) == 0) NULL else as.character(unlist(value))
    } else {
      value
    }
  }

  build_named_list <- function(entries, constructor, fields) {
    if (length(entries) == 0) {
      return(list())
    }
    keys <- vapply(entries, function(e) e[[1]], character(1))
    out <- lapply(entries, function(e) {
      data <- e[[2]]
      data_args <- lapply(names(data), function(f) coerce_field(f, data[[f]]))
      names(data_args) <- names(data)
      args <- c(
        list(
          constructor = constructor,
          fields = fields,
          mod_path = mod_path,
          nonmem_name = e[[1]]
        ),
        data_args
      )
      do.call(create_comment_with_sources, args)
    })
    names(out) <- keys
    out
  }

  theta_comments <- build_named_list(info$thetas, ThetaComment, theta_fields())
  omega_comments <- build_named_list(info$omegas, OmegaComment, omega_fields())
  sigma_comments <- build_named_list(info$sigmas, SigmaComment, sigma_fields())

  result <- ModelComments(
    theta = theta_comments,
    omega = omega_comments,
    sigma = sigma_comments
  )

  if (!is.null(lookup_path)) {
    lookup_path <- normalizePath(lookup_path, mustWork = FALSE)
    result <- apply_lookup(result, lookup_path)
  }

  result
}
