#' Make a path relative to project root (pharos.toml directory)
#' @noRd
relative_path <- function(path) {
  if (is.null(path) || path == "default" || path == "user supplied") {
    return(path)
  }
  tryCatch(
    {
      config_path <- find_pharos_config_file()
      if (grepl("No pharos.toml", config_path)) {
        return(path)
      }
      root <- fs::path_dir(config_path)
      as.character(fs::path_rel(path, start = root))
    },
    error = function(e) path
  )
}

#' Set source paths for comment fields
#'
#' Always initializes the sources attribute to mark object as "initialized".
#' Fields with non-NULL values get source_path; NULL fields get "default".
#' @noRd
set_sources <- function(comment, fields, source_path) {
  source_path <- relative_path(source_path)
  sources <- list()
  for (f in fields) {
    val <- S7::prop(comment, f)
    if (!is.null(val)) {
      sources[[f]] <- source_path
    } else {
      sources[[f]] <- "default"
    }
  }
  attr(comment, "sources") <- sources
  comment
}

#' @noRd
normalize_comment_name <- function(name) {
  if (!is.null(name) && (!nzchar(name) || is.na(name))) {
    return(NULL)
  }
  name
}

#' @noRd
create_comment_with_sources <- function(constructor, fields, mod_path, ...) {
  comment <- constructor(...)
  set_sources(comment, fields, mod_path)
}

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

  param_names <- get_model_parameter_names(mod)
  parsed_comments <- extract_comments(mod)
  comments <- parse_comments(param_names, parsed_comments, mod_path)

  # Split into theta, omega, sigma
  theta_comments <- comments[grepl("^THETA", names(comments))]
  omega_comments <- comments[grepl("^OMEGA", names(comments))]
  sigma_comments <- comments[grepl("^SIGMA", names(comments))]

  # Create ModelComments object (this does duplicate omega name renaming)
  result <- ModelComments(
    theta = theta_comments,
    omega = omega_comments,
    sigma = sigma_comments
  )

  # Apply lookup AFTER renaming so "IIV-CL/F" matches lookup keys
  if (!is.null(lookup_path)) {
    lookup_path <- normalizePath(lookup_path, mustWork = FALSE)
    result <- apply_lookup(result, lookup_path)
  }

  result
}

#' @noRd
extract_comments <- function(mod) {
  parsed <- list()
  raw <- list()

  for (i in seq_along(mod$thetas)) {
    old_name <- paste0("THETA", i)
    parsed[[old_name]] <- mod$thetas[[i]]$parsed_comment
    raw[[old_name]] <- mod$thetas[[i]]$comment
  }

  result <- extract_block_comments(parsed, raw, mod$omega_blocks, "OMEGA")
  parsed <- result$parsed
  raw <- result$raw

  result <- extract_block_comments(parsed, raw, mod$sigma_blocks, "SIGMA")

  result$parsed
}

#' Unwrap ParsedRaneffComment enum (Omega/Sigma wrapper) to get the inner
#' parsed comment that the type1/type2 parsing functions expect.
#' @noRd
unwrap_raneff_comment <- function(parsed_comment) {
  if (is.null(parsed_comment)) {
    return(NULL)
  }
  parsed_comment$Omega %||% parsed_comment$Sigma %||% parsed_comment
}

#' @noRd
extract_block_comments <- function(parsed, raw, blocks, prefix) {
  row <- 1

  for (block in blocks) {
    struct <- block$structure

    # Handle structure as string "Diagonal" or list with named element
    is_diagonal <- identical(struct, "Diagonal") ||
      (is.list(struct) && "Diagonal" %in% names(struct))
    is_block <- is.list(struct) && "Block" %in% names(struct)
    is_block_same <- is.list(struct) && "BlockSame" %in% names(struct)

    if (is_diagonal) {
      for (param in block$parameters) {
        old_name <- sprintf("%s(%d,%d)", prefix, row, row)
        parsed[[old_name]] <- unwrap_raneff_comment(param$parsed_comment)
        raw[[old_name]] <- param$comment
        row <- row + 1
      }
    } else if (is_block) {
      block_size <- struct$Block$size
      param_idx <- 1
      start_row <- row

      for (i in seq_len(block_size)) {
        # Track elements on this row
        row_names <- character(i)

        for (j in seq_len(i)) {
          old_name <- sprintf(
            "%s(%d,%d)",
            prefix,
            start_row + i - 1,
            start_row + j - 1
          )
          row_names[j] <- old_name
          parsed[[old_name]] <- unwrap_raneff_comment(
            block$parameters[[param_idx]]$parsed_comment
          )
          raw[[old_name]] <- block$parameters[[param_idx]]$comment
          param_idx <- param_idx + 1
        }

        # Clear duplicate comments from off-diagonals
        # (when elements share a source line, they all get the same comment from parser)
        if (i > 1) {
          diag_name <- row_names[i] # Last element is diagonal (j == i)
          diag_comment <- raw[[diag_name]]
          if (!is.null(diag_comment) && nzchar(diag_comment)) {
            for (k in seq_len(i - 1)) {
              off_diag_name <- row_names[k]
              if (identical(raw[[off_diag_name]], diag_comment)) {
                raw[[off_diag_name]] <- NULL
                parsed[[off_diag_name]] <- NULL
              }
            }
          }
        }
      }
      row <- start_row + block_size
    } else if (is_block_same) {
      block_size <- struct$BlockSame$size
      row <- row + block_size
    }
  }

  list(parsed = parsed, raw = raw)
}

#' Parse structured (typed) comments from model
#' @noRd
parse_comments <- function(param_names, parsed_comments, mod_path) {
  nonmem_names <- names(param_names)

  # First pass: parse thetas to collect known theta names
  theta_names <- nonmem_names[grepl("^THETA", nonmem_names)]
  theta_comments <- lapply(theta_names, function(nonmem_name) {
    name <- param_names[[nonmem_name]]
    parsed <- parsed_comments[[nonmem_name]]
    parse_typed_theta_comment(nonmem_name, name, parsed, mod_path)
  })
  names(theta_comments) <- theta_names

  known_thetas <- vapply(
    theta_comments,
    function(c) c@name %||% "",
    character(1)
  )
  known_thetas <- known_thetas[nzchar(known_thetas)]

  # Second pass: parse omega/sigma with known_thetas context
  other_names <- nonmem_names[!grepl("^THETA", nonmem_names)]
  other_comments <- lapply(other_names, function(nonmem_name) {
    name <- param_names[[nonmem_name]]
    parsed <- parsed_comments[[nonmem_name]]

    if (grepl("^OMEGA", nonmem_name)) {
      parse_typed_omega_comment(
        nonmem_name,
        name,
        parsed,
        mod_path,
        known_thetas
      )
    } else if (grepl("^SIGMA", nonmem_name)) {
      parse_typed_sigma_comment(nonmem_name, name, parsed, mod_path)
    } else {
      rlang::abort(paste0("Unknown parameter type: ", nonmem_name))
    }
  })
  names(other_comments) <- other_names

  comments <- c(theta_comments, other_comments)
  comments[nonmem_names]
}

#' @noRd
parse_typed_theta_comment <- function(nonmem_name, name, parsed, mod_path) {
  name <- normalize_comment_name(name)

  unit <- NULL
  parameterization <- NULL

  if (!is.null(parsed)) {
    if (!is.null(parsed$Type1)) {
      type1 <- parsed$Type1
      if (!is.null(type1$WithUnit)) {
        if (is.null(name)) {
          name <- type1$WithUnit$parameter
        }
        unit <- type1$WithUnit$unit
        parameterization <- map_parameterization(
          type1$WithUnit$parametrization,
          "THETA"
        )
      } else if (!is.null(type1$Type)) {
        if (is.null(name)) {
          name <- type1$Type$typ
        }
        parameterization <- map_parameterization(
          type1$Type$parameterization,
          "THETA"
        )
      } else if (!is.null(type1$Covariate)) {
        if (is.null(name)) name <- type1$Covariate$parameter
      }
    } else if (!is.null(parsed$Type2)) {
      type2 <- parsed$Type2
      if (is.null(name)) {
        name <- type2$name
      }
      unit <- type2$unit
      parameterization <- type2$parameterization
    }
  }

  create_comment_with_sources(
    ThetaComment,
    theta_fields(),
    mod_path,
    nonmem_name = nonmem_name,
    name = name,
    unit = unit,
    parameterization = parameterization
  )
}

#' Check if an omega parameter is diagonal (variance) vs off-diagonal (covariance)
#' @noRd
is_diagonal_omega <- function(nonmem_name) {
  # Parse OMEGA(i,j) format
  match <- regmatches(
    nonmem_name,
    regexec("OMEGA\\((\\d+),(\\d+)\\)", nonmem_name)
  )[[1]]
  if (length(match) == 3) {
    return(match[2] == match[3])
  }
  # If we can't parse, assume diagonal
  TRUE
}

#' @noRd
parse_typed_omega_comment <- function(
  nonmem_name,
  name,
  parsed,
  mod_path,
  known_thetas = NULL
) {
  name <- normalize_comment_name(name)

  parameterization <- NULL
  associated_theta <- NULL

  # Pharos formats @name as "Name (theta_ref)"; keep it verbatim but also
  # extract associated_theta from the parens for downstream queries.
  if (!is.null(name) && grepl("\\(.*\\)", name)) {
    theta_part <- gsub(".*\\((.+)\\).*", "\\1", name)
    associated_theta <- split_theta_reference(theta_part, known_thetas)
  }

  if (!is.null(parsed)) {
    if (!is.null(parsed$Type1)) {
      type1 <- parsed$Type1
      if (is.null(associated_theta)) {
        associated_theta <- type1$theta_name
      }
      parameterization <- map_parameterization(
        type1$parameterization,
        "OMEGA"
      )
    } else if (!is.null(parsed$Type2)) {
      type2 <- parsed$Type2
      if (is.null(associated_theta)) {
        associated_theta <- type2$raw_theta_refs
      }
      parameterization <- type2$parameterization
    }
  }

  create_comment_with_sources(
    OmegaComment,
    omega_fields(),
    mod_path,
    nonmem_name = nonmem_name,
    name = name,
    parameterization = parameterization,
    associated_theta = associated_theta
  )
}

#' @noRd
parse_typed_sigma_comment <- function(nonmem_name, name, parsed, mod_path) {
  name <- normalize_comment_name(name)

  unit <- NULL
  parameterization <- NULL

  if (!is.null(parsed)) {
    if (!is.null(parsed$Type1)) {
      type1 <- parsed$Type1
      if (is.null(name)) {
        name <- type1$name
      }
      parameterization <- map_parameterization(
        type1$parameterization,
        "SIGMA"
      )
    } else if (!is.null(parsed$Type2)) {
      type2 <- parsed$Type2
      if (is.null(name)) {
        name <- type2$name
      }
      unit <- type2$unit
      parameterization <- type2$parameterization
    }
  }

  create_comment_with_sources(
    SigmaComment,
    sigma_fields(),
    mod_path,
    nonmem_name = nonmem_name,
    name = name,
    unit = unit,
    parameterization = parameterization
  )
}

#' Split theta reference into associated thetas
#'
#' Splits on separators unless the string matches a known theta name (case-insensitive).
#'
#' @param theta_ref Character string of the theta reference
#' @param known_thetas Character vector of known theta names for context
#' @return Character vector of associated theta names
#' @noRd
split_theta_reference <- function(theta_ref, known_thetas = NULL) {
  if (is.null(theta_ref) || !nzchar(theta_ref)) {
    return(NULL)
  }

  theta_ref <- trimws(theta_ref)

  # Check if it matches a known theta (case-insensitive)
  if (!is.null(known_thetas) && length(known_thetas) > 0) {
    if (tolower(theta_ref) %in% tolower(known_thetas)) {
      return(theta_ref)
    }

    # Preserve off-diagonal pairs like "CL/F-V2/F" when both parts
    # are known theta names.
    for (sep in c("-", ",", ":")) {
      if (grepl(sep, theta_ref, fixed = TRUE)) {
        parts <- trimws(strsplit(theta_ref, sep, fixed = TRUE)[[1]])
        if (
          length(parts) == 2 &&
            all(nzchar(parts)) &&
            all(tolower(parts) %in% tolower(known_thetas))
        ) {
          return(parts)
        }
      }
    }
  }

  # Otherwise split on separators
  if (grepl("[-/:,]", theta_ref)) {
    parts <- strsplit(theta_ref, "[-/:,]")[[1]]
    return(trimws(parts))
  }

  theta_ref
}
