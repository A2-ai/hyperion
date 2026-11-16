SCM_EFFECT_FIELDS <- c("name", "type", "initial", "fixed", "lower", "upper")
SCM_EFFECT_TYPES <- c("continuous", "categorical")

#' Create an optional numeric property for an ScmEffect setting
#'
#' @return An S7 property that accepts NULL or a single finite number
#' @keywords internal
#' @noRd
scm_number_property <- function() {
  S7::new_property(
    NULL | S7::class_numeric,
    default = NULL,
    validator = function(value) {
      if (!is.null(value) && (length(value) != 1 || !is.finite(value))) {
        "must be a single finite number"
      }
    }
  )
}

#' SCM candidate effect
#'
#' Represents one entry of an SCM config's `effects` array: a covariate
#' effect named by its `$THETA` comment in the initial model, with the
#' optional settings pharos uses when testing it. [init_scm()],
#' [add_scm_effect()] and [update_scm_effect()] accept an `ScmEffect` in
#' place of a name.
#'
#' @param name Name of the effect's `$THETA` comment in the model (e.g.,
#'   `"WT_CL"`).
#' @param type Effect type: `"continuous"` (the pharos default when omitted)
#'   or `"categorical"`.
#' @param initial Initial estimate of the theta while the effect is tested.
#' @param fixed Value the theta is fixed at while the effect is held out.
#' @param lower,upper Bounds on the theta while the effect is tested.
#' @return An `ScmEffect` object.
#' @export
#'
#' @examples \dontrun{
#' ScmEffect("SEX_V", type = "categorical", initial = 1, lower = 0, upper = 5)
#' }
ScmEffect <- S7::new_class(
  "ScmEffect",
  properties = list(
    name = S7::new_property(
      S7::class_character,
      validator = function(value) {
        if (length(value) != 1 || is.na(value) || !nzchar(trimws(value))) {
          "must be a single non-empty string"
        }
      }
    ),
    type = S7::new_property(
      NULL | S7::class_character,
      default = NULL,
      validator = function(value) {
        if (
          !is.null(value) &&
            !(length(value) == 1 && value %in% SCM_EFFECT_TYPES)
        ) {
          paste0(
            "must be ",
            paste0('"', SCM_EFFECT_TYPES, '"', collapse = " or ")
          )
        }
      }
    ),
    initial = scm_number_property(),
    fixed = scm_number_property(),
    lower = scm_number_property(),
    upper = scm_number_property()
  ),
  validator = function(self) {
    if (
      !is.null(self@lower) && !is.null(self@upper) && self@lower >= self@upper
    ) {
      "@lower must be below @upper"
    }
  }
)

#' @noRd
S7::method(print, ScmEffect) <- function(x, ...) {
  row <- scm_effect_row(x)
  numbers <- row[setdiff(names(row), c("name", "type"))]
  cat(
    "<ScmEffect> ",
    x@name,
    ": ",
    paste(
      c(
        row$type %||% "continuous",
        if (length(numbers)) paste(names(numbers), "=", unlist(numbers))
      ),
      collapse = ", "
    ),
    "\n",
    sep = ""
  )
  invisible(x)
}

#' Convert a name, a list or an ScmEffect to an ScmEffect
#'
#' @param x An effect name, a list with a `name`, or an ScmEffect
#' @return An ScmEffect object
#' @keywords internal
#' @noRd
as_scm_effect <- function(x) {
  if (S7::S7_inherits(x, ScmEffect)) {
    return(x)
  }
  if (is.character(x) && length(x) == 1) {
    return(ScmEffect(name = x))
  }
  if (!is.list(x) || is.null(x$name)) {
    rlang::abort(
      "Each effect must be a name, an ScmEffect, or a list with a `name`"
    )
  }
  unknown <- setdiff(names(x), SCM_EFFECT_FIELDS)
  if (length(unknown)) {
    rlang::abort(paste0(
      "Effect '",
      x$name[1],
      "' has unknown field(s): ",
      paste(unknown, collapse = ", ")
    ))
  }
  do.call(ScmEffect, x)
}

#' Normalize candidate effects to a list of ScmEffect objects
#'
#' @param effects NULL, a character vector of names, an ScmEffect, or a
#'   list of names, lists and ScmEffects
#' @return List of ScmEffect objects with unique names
#' @keywords internal
#' @noRd
normalize_effects <- function(effects) {
  if (is.null(effects) || length(effects) == 0) {
    return(list())
  }
  if (S7::S7_inherits(effects, ScmEffect)) {
    effects <- list(effects)
  }
  if (is.character(effects)) {
    effects <- as.list(effects)
  }
  if (!is.list(effects)) {
    rlang::abort(
      "`effects` must be a character vector of names, or a list of names, rows and ScmEffects"
    )
  }
  effects <- lapply(unname(effects), as_scm_effect)
  check_scm_effects(effects)
  effects
}

#' Normalize the effects of a config's `[covariates]` table
#'
#' @param cov The `covariates` element of an SCM config, or NULL
#' @return `cov` with its effects as a list of ScmEffect objects
#' @keywords internal
#' @noRd
normalize_covariates <- function(cov) {
  cov <- cov %||% list()
  cov$effects <- normalize_effects(cov$effects)
  cov
}

#' @keywords internal
#' @noRd
scm_effect_names <- function(effects) {
  vapply(effects, function(e) e@name, character(1))
}

#' Check that no effect is named twice
#'
#' @param effects List of ScmEffect objects
#' @return `effects`, invisibly
#' @keywords internal
#' @noRd
check_scm_effects <- function(effects) {
  names <- scm_effect_names(effects)
  if (anyDuplicated(names)) {
    rlang::abort(paste0(
      "Duplicate effect(s): ",
      paste(unique(names[duplicated(names)]), collapse = ", ")
    ))
  }
  invisible(effects)
}

#' Convert an ScmEffect to the inline table stored in the config
#'
#' @param effect An ScmEffect object
#' @return Named list of the fields that are set
#' @keywords internal
#' @noRd
scm_effect_row <- function(effect) {
  row <- lapply(SCM_EFFECT_FIELDS, function(field) S7::prop(effect, field))
  names(row) <- SCM_EFFECT_FIELDS
  row[!vapply(row, is.null, logical(1))]
}

#' Convert a config's covariates to the form written to the file
#'
#' @param cov The `covariates` element of an SCM config
#' @return `cov` with each effect as an inline table
#' @keywords internal
#' @noRd
covariates_as_rows <- function(cov) {
  cov <- normalize_covariates(cov)
  cov$effects <- lapply(cov$effects, scm_effect_row)
  cov
}

#' Collect the effect settings passed to an SCM editing function
#'
#' @param type,initial,fixed,lower,upper Effect settings, or NULL
#' @return Named list of the settings that were given
#' @keywords internal
#' @noRd
scm_effect_settings <- function(type, initial, fixed, lower, upper) {
  settings <- list(
    type = type,
    initial = initial,
    fixed = fixed,
    lower = lower,
    upper = upper
  )
  settings[!vapply(settings, is.null, logical(1))]
}

#' Describe the effects a config has, for an error message
#'
#' @param current Character vector of effect names in the config
#' @return A sentence listing the effects, or saying there are none
#' @keywords internal
#' @noRd
scm_available_effects <- function(current) {
  if (length(current) == 0) {
    return("The config has no effects.")
  }
  paste0("Available effects: ", paste(current, collapse = ", "))
}

#' Add a candidate effect to an SCM config
#'
#' Appends an effect to the config's `effects` and writes the config. Edits
#' are written to the file; use [read_scm()] to read the updated config.
#'
#' @param path Path to the SCM config (`<stem>scm.toml`), as returned by
#'   [init_scm()].
#' @param name Effect to add: the name of its `$THETA` comment in the model,
#'   or an [ScmEffect()]. A vector of several names adds several effects
#'   without settings.
#' @param type,initial,fixed,lower,upper Optional settings for the effect:
#'   the type (`"continuous"`, the pharos default, or `"categorical"`), the
#'   initial estimate while tested, the value fixed at while held out, and
#'   the bounds while tested.
#' @return Invisibly returns the path to the SCM config.
#' @seealso [update_scm_effect()], [remove_scm_effect()]
#' @export
#'
#' @examples \dontrun{
#' config <- "model/nonmem/scm/run001scm.toml"
#' add_scm_effect(config, "AGE_CL")
#' add_scm_effect(config, "SEX_V", type = "categorical", lower = 0, upper = 5)
#' read_scm(config)
#' }
add_scm_effect <- function(
  path,
  name,
  type = NULL,
  initial = NULL,
  fixed = NULL,
  lower = NULL,
  upper = NULL
) {
  scm <- read_scm(check_scm_edit_path(path))
  settings <- scm_effect_settings(type, initial, fixed, lower, upper)
  if (S7::S7_inherits(name, ScmEffect)) {
    if (length(settings)) {
      rlang::abort(
        "Settings must be given on the ScmEffect or as arguments, not both"
      )
    }
    new <- list(name)
  } else if (is.character(name) && length(name) > 1) {
    if (length(settings)) {
      rlang::abort(
        "Settings apply to a single effect. Add several names without them."
      )
    }
    new <- normalize_effects(name)
  } else {
    if (!is.character(name) || length(name) != 1) {
      rlang::abort("`name` must be an effect name or an ScmEffect")
    }
    new <- list(do.call(ScmEffect, c(list(name = name), settings)))
  }
  effects <- scm$covariates$effects
  present <- intersect(scm_effect_names(new), scm_effect_names(effects))
  if (length(present)) {
    rlang::abort(paste0(
      "Effect(s) already in the config: ",
      paste(present, collapse = ", "),
      ". Use update_scm_effect() to change one."
    ))
  }
  path <- write_scm_effects(scm, c(effects, new))
  scm_edit_note(
    paste0("Added ", paste(scm_effect_names(new), collapse = ", ")),
    path
  )
  invisible(path)
}

#' Update a candidate effect in an SCM config
#'
#' Replaces the given settings on the named effect, leaves its other
#' settings unchanged, and writes the config. Edits are written to the
#' file; use [read_scm()] to read the updated config.
#'
#' @inheritParams add_scm_effect
#' @param name Effect to update: its name, or an [ScmEffect()] that replaces
#'   the effect of that name entirely.
#' @return Invisibly returns the path to the SCM config.
#' @seealso [add_scm_effect()], [remove_scm_effect()]
#' @export
#'
#' @examples \dontrun{
#' config <- "model/nonmem/scm/run001scm.toml"
#' update_scm_effect(config, "WT_CL", initial = 0.5)
#' update_scm_effect(config, ScmEffect("SEX_V", type = "categorical", lower = 0, upper = 5))
#' read_scm(config)
#' }
update_scm_effect <- function(
  path,
  name,
  type = NULL,
  initial = NULL,
  fixed = NULL,
  lower = NULL,
  upper = NULL
) {
  scm <- read_scm(check_scm_edit_path(path))
  settings <- scm_effect_settings(type, initial, fixed, lower, upper)
  effects <- scm$covariates$effects
  current <- scm_effect_names(effects)
  whole <- S7::S7_inherits(name, ScmEffect)
  if (whole) {
    if (length(settings)) {
      rlang::abort(
        "Settings must be given on the ScmEffect or as arguments, not both"
      )
    }
    new <- name
    name <- name@name
  } else if (!is.character(name) || length(name) != 1) {
    rlang::abort("`name` must be an effect name or an ScmEffect")
  }
  i <- match(name, current)
  if (is.na(i)) {
    rlang::abort(paste0(
      "Effect '",
      name,
      "' not found in the config. ",
      scm_available_effects(current)
    ))
  }
  if (!whole) {
    new <- do.call(
      ScmEffect,
      utils::modifyList(scm_effect_row(effects[[i]]), settings)
    )
  }
  effects[[i]] <- new
  path <- write_scm_effects(scm, effects)
  scm_edit_note(paste0("Updated ", name), path)
  invisible(path)
}

#' Remove a candidate effect from an SCM config
#'
#' Removes effects from the config's `effects` by name and writes the
#' config. Edits are written to the file; use [read_scm()] to read the
#' updated config.
#'
#' @param path Path to the SCM config (`<stem>scm.toml`), as returned by
#'   [init_scm()].
#' @param name Name(s) of the effects to remove.
#' @return Invisibly returns the path to the SCM config.
#' @seealso [add_scm_effect()], [update_scm_effect()]
#' @export
#'
#' @examples \dontrun{
#' remove_scm_effect("model/nonmem/scm/run001scm.toml", "WT_V")
#' }
remove_scm_effect <- function(path, name) {
  scm <- read_scm(check_scm_edit_path(path))
  if (S7::S7_inherits(name, ScmEffect)) {
    name <- name@name
  }
  if (!is.character(name) || length(name) == 0 || anyNA(name)) {
    rlang::abort("`name` must be one or more effect names, or an ScmEffect")
  }
  effects <- scm$covariates$effects
  current <- scm_effect_names(effects)
  missing <- setdiff(name, current)
  if (length(missing)) {
    rlang::abort(paste0(
      "Effect(s) not found in the config: ",
      paste(missing, collapse = ", "),
      ". ",
      scm_available_effects(current)
    ))
  }
  path <- write_scm_effects(scm, effects[!current %in% name])
  scm_edit_note(paste0("Removed ", paste(name, collapse = ", ")), path)
  invisible(path)
}
