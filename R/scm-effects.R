SCM_EFFECT_FIELDS <- c("name", "type", "initial", "fixed", "lower", "upper")
SCM_EFFECT_TYPES <- c("continuous", "categorical")

# An optional numeric setting of an effect (pharos reads it as an f64).
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
#' One entry of an SCM config's `effects` array: a covariate effect named by
#' its `$THETA` comment in the initial model, and the optional settings
#' pharos tests it with. [init_scm()], [add_scm_effect()] and
#' [update_scm_effect()] take one in place of a name.
#'
#' @param name The effect's `$THETA` comment name in the model (e.g., `"WT_CL"`).
#' @param type `"continuous"` (pharos' default when left out) or `"categorical"`.
#' @param initial Initial estimate of the theta while the effect is tested.
#' @param fixed The value the theta is fixed at while the effect is held out.
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
        if (!is.null(value) && !(length(value) == 1 && value %in% SCM_EFFECT_TYPES)) {
          paste0("must be ", paste0('"', SCM_EFFECT_TYPES, '"', collapse = " or "))
        }
      }
    ),
    initial = scm_number_property(),
    fixed = scm_number_property(),
    lower = scm_number_property(),
    upper = scm_number_property()
  ),
  validator = function(self) {
    if (!is.null(self@lower) && !is.null(self@upper) && self@lower >= self@upper) {
      "@lower must be below @upper"
    }
  }
)

#' @noRd
S7::method(print, ScmEffect) <- function(x, ...) {
  row <- scm_effect_row(x)
  numbers <- row[setdiff(names(row), c("name", "type"))]
  cat(
    "<ScmEffect> ", x@name, ": ",
    paste(c(row$type %||% "continuous", if (length(numbers)) paste(names(numbers), "=", unlist(numbers))), collapse = ", "),
    "\n",
    sep = ""
  )
  invisible(x)
}

# A name, a row or an ScmEffect as an ScmEffect.
as_scm_effect <- function(x) {
  if (S7::S7_inherits(x, ScmEffect)) {
    return(x)
  }
  if (is.character(x) && length(x) == 1) {
    return(ScmEffect(name = x))
  }
  if (!is.list(x) || is.null(x$name)) {
    rlang::abort(
      "each effect must be a name, an ScmEffect, or a list with a `name`"
    )
  }
  unknown <- setdiff(names(x), SCM_EFFECT_FIELDS)
  if (length(unknown)) {
    rlang::abort(paste0(
      "effect ",
      x$name[1],
      " has unknown field(s): ",
      paste(unknown, collapse = ", ")
    ))
  }
  do.call(ScmEffect, x)
}

# `effects` as the config allows them, as a list of ScmEffects.
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

normalize_covariates <- function(cov) {
  cov <- cov %||% list()
  cov$effects <- normalize_effects(cov$effects)
  cov
}

scm_effect_names <- function(effects) {
  vapply(effects, function(e) e@name, character(1))
}

check_scm_effects <- function(effects) {
  names <- scm_effect_names(effects)
  if (anyDuplicated(names)) {
    rlang::abort(paste0(
      "duplicate effect(s): ",
      paste(unique(names[duplicated(names)]), collapse = ", ")
    ))
  }
  invisible(effects)
}

# An ScmEffect as the inline table the config holds.
scm_effect_row <- function(effect) {
  row <- lapply(SCM_EFFECT_FIELDS, function(field) S7::prop(effect, field))
  names(row) <- SCM_EFFECT_FIELDS
  row[!vapply(row, is.null, logical(1))]
}

# The covariates with the effects as rows, for writing.
covariates_as_rows <- function(cov) {
  cov <- normalize_covariates(cov)
  cov$effects <- lapply(cov$effects, scm_effect_row)
  cov
}

# The settings given to add_scm_effect() and update_scm_effect().
scm_effect_settings <- function(type, initial, fixed, lower, upper) {
  settings <- list(type = type, initial = initial, fixed = fixed, lower = lower, upper = upper)
  settings[!vapply(settings, is.null, logical(1))]
}

#' Add a candidate effect to an SCM config
#'
#' Appends an effect to the config's `effects` and writes the config. Every
#' edit goes to the file: [read_scm()] reads the result back.
#'
#' @param path The SCM config (`<stem>scm.toml`), as [init_scm()] returns it.
#' @param name The effect to add: its `$THETA` comment name in the model, or
#'   an [ScmEffect()]. Several names add several effects with the settings
#'   left out.
#' @param type,initial,fixed,lower,upper The effect's settings, each
#'   optional: `"continuous"` (pharos' default) or `"categorical"`, the
#'   initial estimate while tested, the value fixed at while held out, and
#'   the bounds while tested.
#' @return The config path, invisibly.
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
        "give the settings on the ScmEffect or as arguments, not both"
      )
    }
    new <- list(name)
  } else if (is.character(name) && length(name) > 1) {
    if (length(settings)) {
      rlang::abort(
        "the settings apply to one effect; add several names without them"
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
      "effect(s) already in the config: ",
      paste(present, collapse = ", "),
      "; update_scm_effect() changes one"
    ))
  }
  path <- write_scm_effects(scm, c(effects, new))
  scm_edit_note(paste0("Added ", paste(scm_effect_names(new), collapse = ", ")), path)
  invisible(path)
}

#' Update a candidate effect in an SCM config
#'
#' Replaces the settings given on the named effect, keeps its others, and
#' writes the config. Every edit goes to the file: [read_scm()] reads the
#' result back.
#'
#' @inheritParams add_scm_effect
#' @param name The effect to update: its name, or an [ScmEffect()] that
#'   replaces the effect of that name whole.
#' @return The config path, invisibly.
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
      rlang::abort("give the settings on the ScmEffect or as arguments, not both")
    }
    new <- name
    name <- name@name
  } else if (!is.character(name) || length(name) != 1) {
    rlang::abort("`name` must be an effect name or an ScmEffect")
  }
  i <- match(name, current)
  if (is.na(i)) {
    rlang::abort(paste0(
      "no effect named ",
      name,
      " in the config",
      if (length(current)) paste0("; it has: ", paste(current, collapse = ", ")) else " (it has none)"
    ))
  }
  if (!whole) {
    new <- do.call(ScmEffect, utils::modifyList(scm_effect_row(effects[[i]]), settings))
  }
  effects[[i]] <- new
  path <- write_scm_effects(scm, effects)
  scm_edit_note(paste0("Updated ", name), path)
  invisible(path)
}

#' Remove a candidate effect from an SCM config
#'
#' Drops effects from the config's `effects` by name and writes the config.
#' Every edit goes to the file: [read_scm()] reads the result back.
#'
#' @param path The SCM config (`<stem>scm.toml`), as [init_scm()] returns it.
#' @param name The name(s) of the effects to remove.
#' @return The config path, invisibly.
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
      "no effect(s) named ",
      paste(missing, collapse = ", "),
      " in the config",
      if (length(current)) paste0("; it has: ", paste(current, collapse = ", ")) else " (it has none)"
    ))
  }
  path <- write_scm_effects(scm, effects[!current %in% name])
  scm_edit_note(paste0("Removed ", paste(name, collapse = ", ")), path)
  invisible(path)
}
