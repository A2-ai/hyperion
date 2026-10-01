# Parameter-row edits: OMEGA/SIGMA rows, updates, blocks, MU referencing.
# See model-edit.R for the edit-session attributes.

add_random <- function(model, kind, init, fix, comment, ref) {
  check_model_object(model)
  check_new_ref(model, ref)
  check_number(init, "init")
  check_flag(fix, "fix")
  if (!is.null(comment)) {
    check_string(comment, "comment")
  }
  res <- edit_add_random_impl(
    attr(model, "model_text"),
    edit_path(model),
    kind,
    init,
    fix,
    comment
  )
  new <- carry_edit_attrs(res$model, model)
  if (!is.null(ref)) {
    new <- add_ref(new, ref, kind, res$index)
  }
  new
}

#' Add an OMEGA or SIGMA row
#'
#' Appends a diagonal row, in memory. It joins the last `$OMEGA`/`$SIGMA`
#' record when that record is a plain list of diagonal values; otherwise it
#' starts a new record. Use `ref` to name it for placeholders:
#' `{name.eta}`, `{name.mu}`, `{name.omega}` for an OMEGA, `{name.eps}`,
#' `{name.sigma}` for a SIGMA.
#'
#' @param model A hyperion_nonmem_model object.
#' @param init Initial variance.
#' @param fix Whether the row is fixed.
#' @param comment Optional comment, written after `;`.
#' @param ref Optional name for use in placeholders.
#' @return The edited model (not yet written; see [write_model()]).
#' @name add_random
#'
#' @examples \dontrun{
#' mod |>
#'   add_sigma(0.25, comment = "Prop ;Proportional", ref = "prop") |>
#'   update_error(Y, append = "+ IPRED * {prop.eps}")
#' }
NULL

#' @rdname add_random
#' @export
add_omega <- function(model, init, fix = FALSE, comment = NULL, ref = NULL) {
  add_random(model, "omega", init, fix, comment, ref)
}

#' @rdname add_random
#' @export
add_sigma <- function(model, init, fix = FALSE, comment = NULL, ref = NULL) {
  add_random(model, "sigma", init, fix, comment, ref)
}

#' Update a THETA, OMEGA or SIGMA row
#'
#' Changes only the arguments given. Leave an argument out to keep it; `NULL`
#' removes a bound or the comment. `update_omega()` and `update_sigma()` edit
#' the diagonal element `index`, e.g. `index = 2` is `OMEGA(2,2)`.
#'
#' @param model A hyperion_nonmem_model object.
#' @param index Row number: `THETA(index)`, `OMEGA(index,index)` or
#'   `SIGMA(index,index)`.
#' @param init New initial estimate.
#' @param lower,upper New bounds, or `NULL` to remove.
#' @param fix `TRUE` to fix, `FALSE` to unfix.
#' @param comment New comment, or `NULL` to remove.
#' @return The edited model (not yet written; see [write_model()]).
#' @name update_params
#'
#' @examples \dontrun{
#' mod |> update_theta(2, lower = 0, comment = "V [L]")
#' mod |> update_omega(3, fix = TRUE)
#' }
NULL

#' @rdname update_params
#' @export
update_theta <- function(model, index, init, lower, upper, fix, comment) {
  check_whole(index, "index")
  init <- arg_value(
    !missing(init),
    if (missing(init)) NULL else init,
    "init",
    check_number
  )
  fix <- arg_value(
    !missing(fix),
    if (missing(fix)) NULL else fix,
    "fix",
    check_flag
  )
  lower <- arg_change(
    !missing(lower),
    if (missing(lower)) NULL else lower,
    "lower",
    check_number
  )
  upper <- arg_change(
    !missing(upper),
    if (missing(upper)) NULL else upper,
    "upper",
    check_number
  )
  comment <- arg_change(
    !missing(comment),
    if (missing(comment)) NULL else comment,
    "comment",
    check_string
  )
  apply_edit(
    model,
    edit_update_theta_impl,
    as.integer(index),
    init,
    lower$action,
    lower$value,
    upper$action,
    upper$value,
    fix,
    comment$action,
    comment$value
  )
}

update_random <- function(model, kind, index, init, fix, comment, has) {
  check_whole(index, "index")
  init <- arg_value(has[["init"]], init, "init", check_number)
  fix <- arg_value(has[["fix"]], fix, "fix", check_flag)
  comment <- arg_change(has[["comment"]], comment, "comment", check_string)
  apply_edit(
    model,
    edit_update_random_impl,
    kind,
    as.integer(index),
    init,
    fix,
    comment$action,
    comment$value
  )
}

#' @rdname update_params
#' @export
update_omega <- function(model, index, init, fix, comment) {
  has <- c(
    init = !missing(init),
    fix = !missing(fix),
    comment = !missing(comment)
  )
  update_random(
    model,
    "omega",
    index,
    if (has[["init"]]) init else NULL,
    if (has[["fix"]]) fix else NULL,
    if (has[["comment"]]) comment else NULL,
    has
  )
}

#' @rdname update_params
#' @export
update_sigma <- function(model, index, init, fix, comment) {
  has <- c(
    init = !missing(init),
    fix = !missing(fix),
    comment = !missing(comment)
  )
  update_random(
    model,
    "sigma",
    index,
    if (has[["init"]]) init else NULL,
    if (has[["fix"]]) fix else NULL,
    if (has[["comment"]]) comment else NULL,
    has
  )
}

#' Make an OMEGA block
#'
#' Turns consecutive diagonal OMEGA rows into one `$OMEGA BLOCK(n)`, written
#' one value per line. `index` may run past the last ETA to add new rows.
#'
#' `init`, `comment` and `ref` hold every element in lower-triangle order:
#' for `index = 3:4` that is `OMEGA(3,3)`, `OMEGA(4,3)`, `OMEGA(4,4)`. `NA`
#' keeps an existing diagonal's value or comment, so estimates set by
#' [copy_model()] survive; new elements need a value. A comment of `""` means
#' none. Off-diagonal refs only allow `.omega`, e.g. `{cov.omega}` for
#' `OMEGA(4,3)`.
#'
#' @param model A hyperion_nonmem_model object.
#' @param index Consecutive ETA numbers, e.g. `3:4`.
#' @param init Values in lower-triangle order.
#' @param comment Comments in lower-triangle order. Leave out to keep the
#'   existing comments.
#' @param ref Optional names in lower-triangle order; `NA` or `""` for none.
#' @param fix Whether the block is fixed.
#' @return The edited model (not yet written; see [write_model()]).
#' @export
#'
#' @examples \dontrun{
#' mod |>
#'   add_omega_block(
#'     index = 3:4,
#'     init = c(NA, 0.005, 0.03),
#'     comment = c(NA, "", "IIV ALAG1")
#'   )
#' }
add_omega_block <- function(
  model,
  index,
  init,
  comment = NULL,
  ref = NULL,
  fix = FALSE
) {
  check_model_object(model)
  if (
    !is.numeric(index) ||
      length(index) == 0 ||
      anyNA(index) ||
      any(index != round(index)) ||
      any(diff(index) != 1)
  ) {
    rlang::abort("`index` must be consecutive ETA numbers, e.g. `3:4`.")
  }
  size <- length(index)
  n <- size * (size + 1) / 2
  if (is.logical(init) && all(is.na(init))) {
    init <- as.numeric(init)
  }
  if (!is.numeric(init) || length(init) != n) {
    rlang::abort(paste0(
      "`init` must have ",
      n,
      " values for a BLOCK(",
      size,
      "), in lower-triangle order."
    ))
  }
  if (is.null(comment)) {
    comment <- rep(NA_character_, n)
  }
  if (is.logical(comment) && all(is.na(comment))) {
    comment <- as.character(comment)
  }
  if (!is.character(comment) || length(comment) != n) {
    rlang::abort(paste0(
      "`comment` must have ",
      n,
      " values for a BLOCK(",
      size,
      "), in lower-triangle order."
    ))
  }
  if (!is.null(ref)) {
    if (!is.character(ref) || length(ref) != n) {
      rlang::abort(paste0(
        "`ref` must have ",
        n,
        " values for a BLOCK(",
        size,
        "), in lower-triangle order."
      ))
    }
    named <- ref[!is.na(ref) & nzchar(ref)]
    if (anyDuplicated(named)) {
      rlang::abort("`ref` has duplicate names.")
    }
    for (r in named) {
      check_new_ref(model, r)
    }
  }
  check_flag(fix, "fix")

  new <- apply_edit(
    model,
    edit_add_omega_block_impl,
    as.integer(index[1]),
    as.integer(size),
    as.numeric(init),
    comment,
    fix
  )
  if (!is.null(ref)) {
    e <- 0
    for (r in seq_len(size)) {
      for (c in seq_len(r)) {
        e <- e + 1
        if (!is.na(ref[e]) && nzchar(ref[e])) {
          i <- index[r]
          j <- index[c]
          new <- add_ref(new, ref[e], "omega", i, if (i == j) 0L else j)
        }
      }
    }
  }
  new
}

#' MU-reference a new parameter
#'
#' Adds `MU_n = THETA(k)` and `param = EXP(MU_n + ETA(n))` to `$PK`, where
#' `theta` and `omega` are refs declared with [add_theta()] and [add_omega()].
#' The MU line goes after the last MU line and the parameter line after the
#' last line with an ETA.
#'
#' @param model A hyperion_nonmem_model object.
#' @param param Bare name of the new parameter, e.g. `ALAG1`.
#' @param theta Ref name of its THETA.
#' @param omega Ref name of its OMEGA.
#' @return The edited model (not yet written; see [write_model()]).
#' @export
#'
#' @examples \dontrun{
#' mod |>
#'   add_theta(-1.6, comment = "ALAG1 [hr]", ref = "alag") |>
#'   add_omega(0.03, comment = "IIV ALAG1 ;lognormal", ref = "iiv_alag") |>
#'   mu_reference(ALAG1, theta = alag, omega = iiv_alag)
#' }
mu_reference <- function(model, param, theta, omega) {
  check_model_object(model)
  param <- name_text(rlang::enexpr(param), "param")
  theta <- name_text(rlang::enexpr(theta), "theta")
  omega <- name_text(rlang::enexpr(omega), "omega")
  refs <- model_refs(model)
  find_ref <- function(name, kind, arg) {
    hit <- refs[refs$name == name, , drop = FALSE]
    if (nrow(hit) == 0) {
      rlang::abort(paste0(
        "`",
        arg,
        "` refers to `",
        name,
        "`, which is not declared. ",
        "Declare it with `ref = \"",
        name,
        "\"` when adding the row."
      ))
    }
    if (hit$kind != kind || hit$col != 0L) {
      rlang::abort(paste0(
        "`",
        name,
        "` must be a ",
        toupper(kind),
        " row ref."
      ))
    }
    hit
  }
  find_ref(theta, "theta", "theta")
  eta <- find_ref(omega, "omega", "omega")$index
  assigned <- edit_assigned_names_impl(attr(model, "model_text"), "PK")
  for (name in c(toupper(param), paste0("MU_", eta))) {
    if (name %in% assigned) {
      rlang::abort(paste0("`", name, "` is already assigned in $PK."))
    }
  }
  edit_code(
    model,
    "PK",
    c(
      paste0("{", omega, ".mu} = {", theta, ".theta}"),
      paste0(param, " = EXP({", omega, ".mu} + {", omega, ".eta})")
    ),
    NULL,
    NULL,
    NULL
  )
}
