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

#' SCM candidate effect class
#'
#' One entry of an SCM config's `effects` array: a candidate covariate effect,
#' named the way the initial model's `$THETA` comment names its parameter, and
#' the optional settings pharos tests it with. [init_scm()] and
#' [add_scm_effect()] make these from names or rows, [read_scm()] reads them
#' from the config, and [write_scm()] writes them back.
#'
#' @param name Character. The effect's name: its `$THETA` comment in the model
#'   (e.g., `"WT_CL"`).
#' @param type Character or NULL. `"continuous"` (pharos' default when the
#'   type is left out) or `"categorical"`.
#' @param initial Numeric or NULL. Initial estimate of the effect's theta when
#'   the effect is tested; pharos uses the config's default for the type
#'   when NULL.
#' @param fixed Numeric or NULL. The value the theta is fixed at while the
#'   effect is held out; pharos uses the config's `[covariates] fixed` when NULL.
#' @param lower,upper Numeric or NULL. Bounds on the theta while the effect is
#'   tested.
#'
#' @section Properties:
#' \describe{
#'   \item{name}{The `$THETA` comment name the effect is tested on.}
#'   \item{type}{`"continuous"` or `"categorical"`; NULL means continuous.}
#'   \item{initial}{Initial estimate while tested.}
#'   \item{fixed}{Value fixed at while held out.}
#'   \item{lower}{Lower bound while tested; must be below `upper`.}
#'   \item{upper}{Upper bound while tested.}
#' }
#'
#' @examples
#' ScmEffect("WT_CL")
#' ScmEffect("SEX_V", type = "categorical", initial = 1, lower = 0, upper = 5)
#'
#' @export
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

# An ScmEffect as the inline table write_scm() writes.
scm_effect_row <- function(effect) {
  row <- lapply(SCM_EFFECT_FIELDS, function(field) S7::prop(effect, field))
  names(row) <- SCM_EFFECT_FIELDS
  row[!vapply(row, is.null, logical(1))]
}

# The covariates with the effects as rows, for writing and comparing.
covariates_as_rows <- function(cov) {
  cov <- normalize_covariates(cov)
  cov$effects <- lapply(cov$effects, scm_effect_row)
  cov
}

#' Add a candidate effect to an SCM config
#'
#' Appends an effect to the object's `effects`. The object is changed in
#' memory only: [write_scm()] writes it to the config.
#'
#' @param scm A `hyperion_nonmem_scm` object from [read_scm()] or [init_scm()].
#' @param name The effect to add: its `$THETA` comment name in the model, or
#'   an [ScmEffect] carrying its settings. Several names add several effects
#'   with the settings left out.
#' @param type,initial,fixed,lower,upper The effect's settings; see
#'   [ScmEffect]. Each is optional.
#' @return The `hyperion_nonmem_scm` object with the effect added.
#' @seealso [remove_scm_effect()], [ScmEffect]
#' @export
#'
#' @examples \dontrun{
#' scm <- read_scm("model/nonmem/scm/run001scm.toml")
#' scm <- add_scm_effect(scm, "AGE_CL")
#' scm <- add_scm_effect(scm, "SEX_V", type = "categorical", lower = 0, upper = 5)
#' scm <- add_scm_effect(scm, ScmEffect("CRCL_CL", initial = 0.5))
#' write_scm(scm)
#' }
add_scm_effect <- function(
  scm,
  name,
  type = NULL,
  initial = NULL,
  fixed = NULL,
  lower = NULL,
  upper = NULL
) {
  check_scm_object(scm)
  settings <- list(
    type = type,
    initial = initial,
    fixed = fixed,
    lower = lower,
    upper = upper
  )
  settings <- settings[!vapply(settings, is.null, logical(1))]
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
  effects <- normalize_effects(scm$covariates$effects)
  present <- intersect(scm_effect_names(new), scm_effect_names(effects))
  if (length(present)) {
    rlang::abort(paste0(
      "effect(s) already in the config: ",
      paste(present, collapse = ", "),
      "; remove_scm_effect() first to change one"
    ))
  }
  scm$covariates$effects <- c(effects, new)
  scm
}

#' Remove a candidate effect from an SCM config
#'
#' Drops effects from the object's `effects` by name. The object is changed
#' in memory only: [write_scm()] writes it to the config.
#'
#' @param scm A `hyperion_nonmem_scm` object from [read_scm()] or [init_scm()].
#' @param name The name(s) of the effects to remove, or an [ScmEffect].
#' @return The `hyperion_nonmem_scm` object with the effects removed.
#' @seealso [add_scm_effect()], [ScmEffect]
#' @export
#'
#' @examples \dontrun{
#' scm <- read_scm("model/nonmem/scm/run001scm.toml")
#' scm <- remove_scm_effect(scm, "WT_V")
#' write_scm(scm)
#' }
remove_scm_effect <- function(scm, name) {
  check_scm_object(scm)
  if (S7::S7_inherits(name, ScmEffect)) {
    name <- name@name
  }
  if (!is.character(name) || length(name) == 0 || anyNA(name)) {
    rlang::abort("`name` must be one or more effect names, or an ScmEffect")
  }
  effects <- normalize_effects(scm$covariates$effects)
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
  scm$covariates$effects <- effects[!current %in% name]
  scm
}
