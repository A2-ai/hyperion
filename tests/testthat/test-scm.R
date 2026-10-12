# SCM: plan / status / summary ----------------------------

# The standard template: each candidate theta is named by its comment
# (`; WT_CL cov`), which is what keys the request. $PK is never read, and
# writing those thetas `(0 FIX)` is the convention, not a rule --
# `covariates` alone decides what is tested.
scm_template <- paste(
  "$PROBLEM scm template",
  "$INPUT ID TIME AMT DV WT CRCL AGE",
  "$DATA data.csv IGNORE=@",
  "$SUBROUTINES ADVAN2 TRANS2",
  "$PK",
  "WT_CL = (WT/70)**THETA(4)",
  "CRCL_CL = (CRCL/100)**THETA(5)",
  "WT_V = (WT/70)**THETA(6)",
  "CL = THETA(1) * WT_CL * CRCL_CL * EXP(ETA(1))",
  "V  = THETA(2) * WT_V * EXP(ETA(2))",
  "KA = THETA(3)",
  "S2 = V",
  "$ERROR",
  "Y = F * (1 + EPS(1))",
  "$THETA (0, 3)    ; TVCL (L/h)",
  "$THETA (0, 20)   ; TVV (L)",
  "$THETA (0, 1.2)  ; TVKA (1/h)",
  "$THETA (0 FIX)   ; WT_CL cov",
  "$THETA (0 FIX)   ; CRCL_CL cov",
  "$THETA (0 FIX)   ; WT_V cov",
  "$OMEGA 0.1",
  "$OMEGA 0.1",
  "$SIGMA 0.02",
  "$ESTIMATION METHOD=1 INTER MAXEVAL=9999 NOABORT",
  "$COVARIANCE",
  sep = "\n"
)

# The same model with the covariate effects folded into the TVCL / V
# expressions instead of standing on their own. Nothing in $PK names a
# candidate theta; the $THETA comments do, which is all that is needed.
inline_scm_template <- paste(
  "$PROBLEM scm template (inline covariate effects)",
  "$INPUT ID TIME AMT DV WT CRCL AGE",
  "$DATA data.csv IGNORE=@",
  "$SUBROUTINES ADVAN2 TRANS2",
  "$PK",
  "TVCL = THETA(1) * (WT/70)**THETA(4) * (CRCL/100)**THETA(5)",
  "CL = TVCL * EXP(ETA(1))",
  "V  = THETA(2) * (WT/70)**THETA(6) * EXP(ETA(2))",
  "KA = THETA(3)",
  "S2 = V",
  "$ERROR",
  "Y = F * (1 + EPS(1))",
  "$THETA (0, 3)    ; TVCL (L/h)",
  "$THETA (0, 20)   ; TVV (L)",
  "$THETA (0, 1.2)  ; TVKA (1/h)",
  "$THETA (0 FIX)   ; WT_CL cov",
  "$THETA (0 FIX)   ; CRCL_CL cov",
  "$THETA (0 FIX)   ; WT_V cov",
  "$OMEGA 0.1",
  "$OMEGA 0.1",
  "$SIGMA 0.02",
  "$ESTIMATION METHOD=1 INTER MAXEVAL=9999 NOABORT",
  "$COVARIANCE",
  sep = "\n"
)

write_scm_fixture <- function(dir, template = scm_template,
                              comment_type = "type1") {
  model_path <- file.path(dir, "1001.mod")
  writeLines(template, model_path)
  writeLines("ID,TIME,AMT,DV,WT,CRCL,AGE", file.path(dir, "data.csv"))
  # Planning reads the `[nonmem]` settings of the pharos project the model
  # sits in -- the comment dialect its thetas are named in above all -- so
  # the fixture is a project root of its own. Without a pharos.toml the walk
  # climbs out of the temp dir and finds none; under the wrong dialect the
  # `; WT_CL cov` comments below name nothing.
  config <- readLines(system.file("pharos.toml", package = "hyperion"))
  writeLines(
    sub('^type = "type2"$', paste0('type = "', comment_type, '"'), config),
    file.path(dir, "pharos.toml")
  )
  model_path
}

# The SCM config file (TOML) defines the SCM process; `covariates` is the
# `effects` array of the [covariates] section and `direction` a TOML
# fragment, so tests can exercise both spellings and bad input. `extra` goes
# above the section (top-level keys); `section` inside it. It is written
# where `scm_init()` puts it -- in the SCM out_dir, `scm/1001` beside the
# model -- because that is where every reader looks for it.
write_scm_config <- function(dir,
                             covariates = '["WT_CL", "CRCL_CL", "WT_V"]',
                             direction = '["forward", "backward"]',
                             extra = character(),
                             section = character()) {
  out_dir <- file.path(dir, "scm", "1001")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  config_path <- file.path(out_dir, "1001scm.toml")
  writeLines(
    c(
      'model = "../../1001.mod"',
      paste0("direction = ", direction),
      extra,
      "[covariates]",
      section,
      paste0("effects = ", covariates)
    ),
    config_path
  )
  config_path
}

# Every plan built from `scm_template` warns the same way: the template
# carries a $COVARIANCE record while `cov_step` defaults off, so pharos says
# the record will be dropped from the round models. That warning has its own
# test below; muffle just it elsewhere, so an unexpected one still surfaces.
no_cov_step_warning <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("cov_step is off", conditionMessage(w), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })
}

# A plan writes its paths relative to the pharos project root; where the
# out_dir is on disk is the plan's `out_dir` attribute.
scm_dir <- function(plan) attr(plan, "out_dir")

make_plan <- function(dir, ...) {
  write_scm_fixture(dir)
  no_cov_step_warning(scm_plan(write_scm_config(dir), ...))
}

cli_text_of <- function(expr) {
  paste(cli::cli_fmt(expr), collapse = "\n")
}

# The same with every run of whitespace a space: cli wraps long lines to the
# console width, which is not what a test is about.
cli_one_line <- function(expr) {
  gsub("\\s+", " ", cli_text_of(expr))
}

test_that("scm_init writes the config into the SCM process dir it makes", {
  dir <- withr::local_tempdir()
  model <- write_scm_fixture(dir)

  expect_message(setup <- scm_init(model), "SCM setup created")
  expect_equal(setup$out_dir, file.path(dir, "scm", "1001"))
  expect_equal(setup$config, file.path(setup$out_dir, "1001scm.toml"))
  expect_true(dir.exists(setup$out_dir))

  body <- paste(readLines(setup$config), collapse = "\n")
  # the config sits two levels below the model it plans for
  expect_match(body, 'model = "../../1001.mod"', fixed = TRUE)
  expect_match(body, "[covariates]", fixed = TRUE)
  expect_match(body, "effects = []", fixed = TRUE)
  expect_match(body, 'direction = ["forward", "backward"]', fixed = TRUE)
  # out_dir and release_init are no longer config keys
  expect_no_match(body, "out_dir", fixed = TRUE)
  expect_no_match(body, "release_init", fixed = TRUE)
  # every optional setting is present at its default
  # the per-type initial estimates are a table each, not one flat default
  for (default in c("forward_alpha = 0.05", "backward_alpha = 0.001",
                    "max_retries = 3", "num_rounds = 0", "cov_step = false",
                    "forward_final_cov_step = true", "final_cov_step = true",
                    "fixed = 0",
                    "continuous  = { initial = 0.1 }",
                    "categorical = { initial = 1 }")) {
    expect_match(body, default, fixed = TRUE)
  }

  # the out_dir's .gitignore follows the project's `[nonmem.scm]
  # track_in_git`: milestones unless pharos.toml says otherwise
  gitignore <- readLines(file.path(setup$out_dir, ".gitignore"))
  expect_match(gitignore[1], 'track_in_git = "milestones"', fixed = TRUE)
  expect_true(all(c("/*", "!/1001scm.toml", "!/final/",
                    "!/base/", "!/forward_final/") %in% gitignore))
  # the plan is not a file of its own any more, so nothing tracks one
  expect_false("!/pharos_scm_plan.json" %in% gitignore)
  expect_false("!/forward_round1/" %in% gitignore)
})

test_that("the config scm_init writes only needs its covariates filled in", {
  dir <- withr::local_tempdir()
  model <- write_scm_fixture(dir)
  setup <- scm_init(model)

  # empty effects is the one thing left to fill in
  expect_error(scm_plan(setup$config), "effects")

  body <- readLines(setup$config)
  body <- sub("effects = []", 'effects = ["WT_CL", "CRCL_CL"]',
              body, fixed = TRUE)
  writeLines(body, setup$config)

  plan <- no_cov_step_warning(scm_plan(setup$config))
  expect_equal(
    vapply(plan$candidates, function(c) c$name, character(1)),
    c("WT_CL", "CRCL_CL")
  )
  # the plan writes its paths relative to the pharos project root; the
  # directory scm_init created is where it says the process runs
  expect_equal(plan$out_dir, "scm/1001")
  expect_equal(normalizePath(attr(plan, "out_dir")), normalizePath(setup$out_dir))
  expect_equal(normalizePath(attr(plan, "config")), normalizePath(setup$config))
  # the plan is a dry run: nothing lands in the out_dir
  expect_equal(list.files(setup$out_dir, all.files = TRUE, no.. = TRUE),
               c(".gitignore", "1001scm.toml"))
})

test_that("scm_init validates its inputs and never clobbers a filled-in config", {
  dir <- withr::local_tempdir()
  model <- write_scm_fixture(dir)

  expect_error(scm_init(42), "must be a single path")
  expect_error(scm_init(file.path(dir, "nope.mod")), "not found")
  expect_error(scm_init(model, overwrite = NA), "TRUE or FALSE")

  setup <- scm_init(model)
  writeLines("# mine", setup$config)
  expect_error(scm_init(model), "already exists")
  expect_equal(readLines(setup$config), "# mine")

  scm_init(model, overwrite = TRUE)
  expect_match(
    paste(readLines(setup$config), collapse = "\n"),
    "effects = []",
    fixed = TRUE
  )
})

test_that("scm_plan names candidates from their $THETA names and carries defaults", {
  dir <- withr::local_tempdir()
  plan <- make_plan(dir)

  expect_s3_class(plan, "hyperion_scm_plan")
  expect_equal(
    vapply(plan$candidates, function(c) c$name, character(1)),
    c("WT_CL", "CRCL_CL", "WT_V")
  )
  expect_equal(
    vapply(plan$candidates, function(c) as.integer(c$theta), integer(1)),
    c(4L, 5L, 6L)
  )
  expect_equal(unlist(plan$options$direction), c("forward", "backward"))
  expect_equal(plan$options$forward_alpha, 0.05)
  expect_equal(plan$options$backward_alpha, 0.001)
  expect_equal(as.integer(plan$options$max_retries), 3L)
  # every candidate continuous at its type's default: initial estimate 0.1,
  # held out at 0
  expect_equal(
    vapply(plan$candidates, function(c) unlist(c$kind), character(1)),
    rep("continuous", 3)
  )
  expect_equal(vapply(plan$candidates, function(c) c$initial, numeric(1)), rep(0.1, 3))
  expect_equal(vapply(plan$candidates, function(c) c$fixed, numeric(1)), rep(0, 3))
  expect_false(plan$options$cov_step)
  expect_true(plan$options$final_cov_step)
  expect_true(plan$options$forward_final_cov_step)
  expect_equal(plan$out_dir, "scm/1001")
  expect_match(attr(plan, "config"), "scm/1001/1001scm.toml$")
  expect_equal(normalizePath(attr(plan, "out_dir")), normalizePath(file.path(dir, "scm", "1001")))
  # a fresh out_dir: nothing for overwrite to discard, nothing in the way of
  # a run, and no NM-TRAN on this machine to check the model with
  expect_null(attr(plan, "discards"))
  expect_true(attr(plan, "resumable"))
  expect_length(attr(plan, "refusals"), 0)
  expect_match(attr(plan, "nmtran"), "not checked")
})

test_that("scm_plan validates its inputs", {
  dir <- withr::local_tempdir()
  write_scm_fixture(dir)

  # the config path itself
  expect_error(scm_plan(42), "config")
  expect_error(scm_plan(file.path(dir, "nope.toml")), "not found")

  # direction comes from the config, and is required there
  cfg <- write_scm_config(dir, direction = '["sideways"]')
  expect_error(scm_plan(cfg))
  writeLines(c('model = "1001.mod"', "[covariates]", 'effects = ["WT_CL"]'),
             file.path(dir, "scm.toml"))
  expect_error(scm_plan(file.path(dir, "scm.toml")), "direction")

  # `covariates` is the section, not a flat array of names
  writeLines(c('model = "1001.mod"', 'covariates = ["WT_CL"]',
               'direction = ["forward"]'), file.path(dir, "scm.toml"))
  expect_error(scm_plan(file.path(dir, "scm.toml")), "covariates")
  # a key that belongs inside the section is not a top-level one
  cfg <- write_scm_config(dir, extra = "initial = 0.2")
  expect_error(scm_plan(cfg), "unknown field `initial`")

  # a typo'd option fails loudly instead of silently using a default
  cfg <- write_scm_config(dir, extra = "foward_alpha = 0.01")
  expect_error(scm_plan(cfg), "foward_alpha")

  # a name no $THETA record carries cannot be a candidate
  cfg <- write_scm_config(dir, covariates = '["AGE_CL"]')
  expect_error(scm_plan(cfg), "no theta named AGE_CL")

  # call-site run-control validation is R-side, before pharos is reached
  cfg <- write_scm_config(dir)
  expect_error(scm_plan(cfg, num_rounds = -1), "num_rounds")
  expect_error(scm_plan(cfg, num_rounds = 1.5), "num_rounds")
  expect_error(scm_plan(cfg, overwrite = NA), "overwrite")
  # ... and named-only after config: a positional value is refused as such
  err <- expect_error(scm_plan(cfg, 2), "must be empty")
  expect_no_match(conditionMessage(err), "num_rounds", fixed = TRUE)
  # an initial estimate equal to the held-out value tests nothing: pharos refuses
  cfg <- write_scm_config(
    dir,
    section = c("continuous = { initial = 0 }", "fixed = 0")
  )
  expect_error(scm_plan(cfg), "equals fixed")
})

test_that("the covariates section takes names, typed rows, and per-type defaults", {
  dir <- withr::local_tempdir()
  # WT_V written as a fold-change effect, held out at 1
  fold <- sub("WT_V = (WT/70)**THETA(6)", "WT_V = THETA(6)**(WT/70)", scm_template, fixed = TRUE)
  fold <- sub("$THETA (0 FIX)   ; WT_V cov", "$THETA (1 FIX)   ; WT_V cov", fold, fixed = TRUE)
  write_scm_fixture(dir, template = fold)

  plan <- no_cov_step_warning(scm_plan(write_scm_config(
    dir,
    covariates = paste(
      '["WT_CL",',
      '{ name = "CRCL_CL", initial = 0.3 },',
      '{ name = "WT_V", type = "categorical", initial = 1.5, fixed = 1 }]'
    ),
    section = c("continuous = { initial = 0.2 }", "fixed = 0")
  )))
  expect_equal(vapply(plan$candidates, function(c) c$initial, numeric(1)), c(0.2, 0.3, 1.5))
  expect_equal(vapply(plan$candidates, function(c) c$fixed, numeric(1)), c(0, 0, 1))
  expect_equal(
    vapply(plan$candidates, function(c) unlist(c$kind), character(1)),
    c("continuous", "continuous", "categorical")
  )

  # the plan shows the type and both values per candidate
  txt <- cli_text_of(print(plan))
  # cli wraps the line, so the printed row is asserted in pieces; the knit
  # table below carries both values side by side
  expect_match(txt, "THETA(6), categorical -> initial estimate 1.5", fixed = TRUE)
  expect_match(txt, "when first tested, FIXED at", fixed = TRUE)
  knit <- as.character(knitr::knit_print(plan))
  expect_match(knit, "| WT_V | THETA(6) | categorical | 1.5 | 1 |", fixed = TRUE)

  # the type's own table supplies the default: a categorical effect whose
  # $THETA is authored at the held-out value is first tested at 1, not 0.1
  typed_dir <- withr::local_tempdir()
  write_scm_fixture(typed_dir)
  typed <- no_cov_step_warning(scm_plan(write_scm_config(
    typed_dir,
    covariates = '["WT_CL", { name = "WT_V", type = "categorical" }]'
  )))
  expect_equal(
    vapply(typed$candidates, function(c) c$initial, numeric(1)),
    c(0.1, 1)
  )

  # a per-type `initial` sets that type's default, never a row's own value
  plan <- no_cov_step_warning(scm_plan(write_scm_config(
    dir,
    covariates = '["WT_CL", { name = "CRCL_CL", initial = 0.3 }]',
    section = "continuous = { initial = 0.05 }"
  )))
  expect_equal(vapply(plan$candidates, function(c) c$initial, numeric(1)), c(0.05, 0.3))
})

test_that("the covariates section bounds effects, per row", {
  dir <- withr::local_tempdir()
  # WT_CL is authored bounded in the template; the rest are (0 FIX).
  bounded <- sub(
    "$THETA (0 FIX)   ; WT_CL cov",
    "$THETA (-2, 0.4, 2)   ; WT_CL cov",
    scm_template,
    fixed = TRUE
  )
  write_scm_fixture(dir, template = bounded)

  plan <- no_cov_step_warning(scm_plan(write_scm_config(
    dir,
    covariates = paste(
      '[{ name = "WT_CL", lower = 0 }, "CRCL_CL",',
      '{ name = "WT_V", initial = 1.2, lower = 0.01, upper = 10 }]'
    )
  )))
  bound <- function(field) {
    vapply(
      plan$candidates,
      function(c) if (is.null(c[[field]])) NA_real_ else as.numeric(c[[field]]),
      numeric(1)
    )
  }
  # the row's lower beats the template's -2, which still supplies the upper
  # the row leaves out; a row that says nothing about bounds, against a
  # theta the template leaves unbounded, stays unbounded
  expect_equal(bound("lower"), c(0, NA, 0.01))
  expect_equal(bound("upper"), c(2, NA, 10))

  txt <- cli_text_of(print(plan))
  expect_match(txt, "bounded to (0, 2)", fixed = TRUE)
  expect_match(txt, "bounded to (0.01, 10)", fixed = TRUE)
  knit <- as.character(knitr::knit_print(plan))
  expect_match(knit, "| FIXED (held out) | bounds |", fixed = TRUE)
  expect_match(knit, "| CRCL_CL | THETA(5) | continuous | 0.1 | 0 | - |", fixed = TRUE)

  # a config that says nothing about bounds leaves them out entirely, and
  # the bounds column stays off the table
  plain_dir <- withr::local_tempdir()
  write_scm_fixture(plain_dir)
  plain <- no_cov_step_warning(scm_plan(write_scm_config(plain_dir, covariates = '["CRCL_CL"]')))
  expect_null(plain$candidates[[1]]$lower)
  expect_match(
    as.character(knitr::knit_print(plain)),
    "| candidate | theta | type | initial estimate | FIXED (held out) |\n",
    fixed = TRUE
  )

  # `initial` has to sit strictly inside the bounds
  bad <- write_scm_config(
    plain_dir,
    covariates = '[{ name = "CRCL_CL", upper = 0.05 }]'
  )
  expect_error(scm_plan(bad), "must lie strictly inside its bounds")
})

test_that("covariates are keyed by theta name, case-insensitively", {
  dir <- withr::local_tempdir()
  write_scm_fixture(dir)

  # requests match case-insensitively; candidates keep the authored spelling
  plan <- no_cov_step_warning(scm_plan(write_scm_config(
    dir,
    covariates = '["WT_CL", "crcl_cl", "WT_V"]',
    direction = '["forward"]'
  )))
  expect_equal(
    vapply(plan$candidates, function(c) c$name, character(1)),
    c("WT_CL", "CRCL_CL", "WT_V")
  )
  expect_equal(
    vapply(plan$candidates, function(c) as.integer(c$theta), integer(1)),
    c(4L, 5L, 6L)
  )

  # the request order does not matter; the plan lists them in theta order
  shuffled <- no_cov_step_warning(scm_plan(write_scm_config(
    dir,
    covariates = '["WT_V", "WT_CL", "CRCL_CL"]',
    direction = '["forward"]'
  )))
  expect_equal(
    vapply(shuffled$candidates, function(c) c$name, character(1)),
    c("WT_CL", "CRCL_CL", "WT_V")
  )
})

test_that("THETA numbers are rejected, with the name spelling to use instead", {
  dir <- withr::local_tempdir()
  write_scm_fixture(dir)

  # `effects` takes theta names or rows, never the 1-based THETA numbers an
  # older pharos accepted; the parse error says what to write instead
  for (array in c("[4, 5, 6]", '[4, "WT_CL"]')) {
    expect_error(
      scm_plan(write_scm_config(dir, covariates = array)),
      "expected a theta name"
    )
  }
})

test_that("the project's comment dialect is what names a theta", {
  dir <- withr::local_tempdir()
  # Type1 names a theta by `NAME cov` or `NAME (unit)`, optionally with a
  # parametrization suffix -- the same names `pharos nonmem summary` prints.
  loose <- sub("; CRCL_CL cov", "; CRCL_CL (-) :LOG", scm_template, fixed = TRUE)
  write_scm_fixture(dir, template = loose)

  plan <- no_cov_step_warning(scm_plan(write_scm_config(dir)))
  expect_equal(
    vapply(plan$candidates, function(x) x$name, character(1)),
    c("WT_CL", "CRCL_CL", "WT_V")
  )

  # the comment is the name: rename it and the old name resolves to nothing
  renamed <- sub("; WT_V cov", "; WTONV cov", scm_template, fixed = TRUE)
  write_scm_fixture(dir, template = renamed)
  expect_error(scm_plan(write_scm_config(dir)), "no theta named WT_V")

  plan <- no_cov_step_warning(scm_plan(write_scm_config(
    dir,
    covariates = '["WT_CL", "CRCL_CL", "WTONV"]'
  )))
  expect_equal(plan$candidates[[3]]$name, "WTONV")

  # a comment the dialect cannot parse names nothing, and neither does a
  # $THETA label: the dialect is the only naming there is
  for (record in c("$THETA (0 FIX)", "$THETA (0 FIX)   ; WT_V", "$THETA WT_V=(0 FIX)")) {
    bare <- sub("$THETA (0 FIX)   ; WT_V cov", record, scm_template, fixed = TRUE)
    write_scm_fixture(dir, template = bare)
    expect_error(scm_plan(write_scm_config(dir)), "no theta named WT_V")
  }
})

test_that("a project with no comment dialect names nothing", {
  dir <- withr::local_tempdir()
  write_scm_fixture(dir)
  # strip the dialect the fixture declares
  config <- readLines(file.path(dir, "pharos.toml"))
  writeLines(config[!grepl('^type = "type1"$', config)], file.path(dir, "pharos.toml"))

  expect_error(scm_plan(write_scm_config(dir)), "comment dialect")
})

test_that("a name that could mean two thetas is refused", {
  dir <- withr::local_tempdir()
  # two thetas carrying the same name in their comments
  clash <- sub("; TVKA (1/h)", "; WT_V cov", scm_template, fixed = TRUE)
  write_scm_fixture(dir, template = clash)

  err <- expect_error(
    scm_plan(write_scm_config(dir, covariates = '["WT_V"]')),
    "WT_V is ambiguous"
  )
  # the message names both thetas
  expect_match(conditionMessage(err), "THETA\\(3\\)")
  expect_match(conditionMessage(err), "THETA\\(6\\)")
})

test_that("an inline model plans from its $THETA comments", {
  dir <- withr::local_tempdir()
  # the effects are folded into TVCL / V, so no $PK term names any candidate
  # theta -- the comments do, which is all the SCM process needs
  write_scm_fixture(dir, template = inline_scm_template)
  plan <- no_cov_step_warning(scm_plan(write_scm_config(dir)))
  expect_equal(
    vapply(plan$candidates, function(x) x$name, character(1)),
    c("WT_CL", "CRCL_CL", "WT_V")
  )
  expect_equal(
    vapply(plan$candidates, function(x) as.integer(x$theta), integer(1)),
    c(4L, 5L, 6L)
  )
})

test_that("a type2 project names its thetas the type2 way", {
  dir <- withr::local_tempdir()
  # the `; <n> NAME description...` house style: the leading position number
  # labels the theta, so the name is the word after it
  numbered <- sub("; TVCL (L/h)", "; 1 TVCL (L/h)", scm_template, fixed = TRUE)
  numbered <- sub("; TVV (L)", "; 2 TVV (L)", numbered, fixed = TRUE)
  numbered <- sub("; TVKA (1/h)", "; 3 TVKA (1/h)", numbered, fixed = TRUE)
  numbered <- sub("; WT_CL cov", "; 4 WT_CL", numbered, fixed = TRUE)
  numbered <- sub("; CRCL_CL cov", "; 5 CRCL_CL", numbered, fixed = TRUE)
  numbered <- sub("; WT_V cov", "; 6 WT_V", numbered, fixed = TRUE)
  write_scm_fixture(dir, template = numbered, comment_type = "type2")

  plan <- no_cov_step_warning(scm_plan(write_scm_config(dir)))
  expect_equal(
    vapply(plan$candidates, function(x) x$name, character(1)),
    c("WT_CL", "CRCL_CL", "WT_V")
  )

  # numbering that no longer matches the theta's position is a stale comment
  stale <- sub("; 4 WT_CL", "; 9 WT_CL", numbered, fixed = TRUE)
  write_scm_fixture(dir, template = stale, comment_type = "type2")
  no_cov_step_warning(expect_warning(scm_plan(write_scm_config(dir)), "numbering looks stale"))
})

test_that("direction accepts single directions", {
  dir <- withr::local_tempdir()
  write_scm_fixture(dir)

  fwd <- no_cov_step_warning(scm_plan(write_scm_config(dir, direction = '["forward"]')))
  expect_equal(unlist(fwd$options$direction), "forward")

  bwd <- no_cov_step_warning(scm_plan(write_scm_config(dir, direction = '["backward"]')))
  expect_equal(unlist(bwd$options$direction), "backward")
})

test_that("the plan says what becomes of the forward model", {
  dir <- withr::local_tempdir()
  write_scm_fixture(dir)

  # both phases, cov step off: the forward model is re-fitted with the cov
  # step on
  both <- no_cov_step_warning(scm_plan(write_scm_config(dir)))
  txt <- cli_text_of(print(both))
  expect_match(txt, "forward fit: re-fit the forward model with the cov step on", fixed = TRUE)
  expect_match(as.character(knitr::knit_print(both)),
               "- **forward fit:** re-fit the forward model", fixed = TRUE)

  # cov step on: it has already run the step, so it is used as it is
  on <- scm_plan(write_scm_config(dir, extra = "cov_step = true"))
  expect_match(cli_text_of(print(on)), "already runs the cov step", fixed = TRUE)

  # switched off, or a single phase: no forward fit, so no line
  off <- no_cov_step_warning(scm_plan(write_scm_config(dir, extra = "forward_final_cov_step = false")))
  expect_false(off$options$forward_final_cov_step)
  expect_no_match(cli_text_of(print(off)), "forward fit", fixed = TRUE)
  fwd <- no_cov_step_warning(scm_plan(write_scm_config(dir, direction = '["forward"]')))
  expect_no_match(cli_text_of(print(fwd)), "forward fit", fixed = TRUE)
})

test_that("max_models rides along with the plan and depends on direction", {
  dir <- withr::local_tempdir()
  write_scm_fixture(dir)

  # 3 candidates: one phase = 1 + 3(3+1)/2 = 7; both phases = 13. pharos
  # derives it from the plan rather than storing it, so it comes back as an
  # attribute rather than a plan field.
  fwd <- no_cov_step_warning(scm_plan(write_scm_config(dir, direction = '["forward"]')))
  expect_equal(attr(fwd, "max_models"), 7L)
  bwd <- no_cov_step_warning(scm_plan(write_scm_config(dir, direction = '["backward"]')))
  expect_equal(attr(bwd, "max_models"), 7L)
  both <- no_cov_step_warning(scm_plan(write_scm_config(dir)))
  expect_equal(attr(both, "max_models"), 13L)

  # and it is in the printed plan
  plan_text <- cli_text_of(print(both))
  expect_match(plan_text, "max models 13")
})

test_that("the config sets the SCM process; the call-site knobs only pace it", {
  dir <- withr::local_tempdir()
  write_scm_fixture(dir)
  cfg <- write_scm_config(dir, extra = c(
    "forward_alpha = 0.01",
    "max_retries = 5",
    "cov_step = false"
  ), section = "continuous = { initial = 0.2 }")

  # config alone (cov_step = false + a $COVARIANCE in the template warns)
  expect_warning(plan <- scm_plan(cfg), "cov_step is off")
  expect_equal(plan$options$forward_alpha, 0.01)
  expect_equal(as.integer(plan$options$max_retries), 5L)
  expect_false(plan$options$cov_step)
  expect_equal(plan$candidates[[1]]$initial, 0.2)
  expect_null(plan$options$num_rounds)

  # num_rounds / overwrite pace this run and leave the config's settings alone
  expect_warning(
    plan <- scm_plan(cfg, num_rounds = 2, overwrite = TRUE),
    "cov_step is off"
  )
  expect_equal(as.integer(plan$options$num_rounds), 2L)
  expect_null(plan$options$overwrite)
  expect_equal(as.integer(plan$options$max_retries), 5L)
  expect_false(plan$options$cov_step)
  expect_equal(plan$candidates[[1]]$initial, 0.2)
  expect_equal(plan$options$forward_alpha, 0.01)

  # the config can pace the process itself; the argument wins over it, and 0
  # means all rounds from either
  paced <- write_scm_config(dir, extra = "num_rounds = 1")
  expect_equal(as.integer(suppressWarnings(scm_plan(paced))$options$num_rounds), 1L)
  expect_equal(as.integer(suppressWarnings(scm_plan(paced, num_rounds = 3))$options$num_rounds), 3L)
  expect_null(suppressWarnings(scm_plan(paced, num_rounds = 0))$options$num_rounds)
  expect_null(suppressWarnings(scm_plan(write_scm_config(dir, extra = "num_rounds = 0")))$options$num_rounds)
})

test_that("scm_plan writes nothing; scm_status reads an unstarted process from its config", {
  dir <- withr::local_tempdir()
  expect_message(plan <- make_plan(dir), "nothing written")
  out_dir <- scm_dir(plan)

  # the dry run leaves the out_dir as it found it: the config, nothing else
  expect_equal(list.files(out_dir, all.files = TRUE, no.. = TRUE), "1001scm.toml")

  # a process that has not started is read under what its config resolves
  # to now
  st <- scm_status(plan)
  expect_s3_class(st, "hyperion_scm_status")
  expect_equal(st$status, "planned")
  expect_length(st$rounds, 0)
  expect_false(attr(st, "started"))
  expect_equal(normalizePath(attr(st, "config")), normalizePath(attr(plan, "config")))
  # no driver was ever started for it
  expect_null(attr(st, "driver"))
  expect_match(paste(capture.output(print(st)), collapse = "\n"),
               "note       : the SCM process has not started; submit its config to start it",
               fixed = TRUE)
})

test_that("plan and status display methods run", {
  dir <- withr::local_tempdir()
  plan <- make_plan(dir)

  plan_text <- cli_text_of(print(plan))
  expect_match(plan_text, "SCM plan")
  expect_match(plan_text, "WT_CL")
  expect_match(plan_text, "SCM size")

  # a plan has no summary() method -- print() is the whole display
  expect_false("summary.hyperion_scm_plan" %in% ls(asNamespace("hyperion")))

  knit <- knitr::knit_print(plan)
  expect_s3_class(knit, "knit_asis")
  expect_match(as.character(knit), "WT_CL")
  expect_match(as.character(knit), "max models")

  st <- scm_status(plan)
  expect_match(paste(capture.output(print(st)), collapse = "\n"), "planned")
  knit_st <- knitr::knit_print(st)
  expect_s3_class(knit_st, "knit_asis")
})

# The plan as pharos records it in scm_state.json when the process starts:
# a fabricated state carries the plan it ran under, which is what every
# reader (and a re-plan's diff) takes the process from.
plan_json <- function(plan) {
  bool <- function(v) if (isTRUE(v)) "true" else "false"
  num <- function(v) if (is.null(v)) "null" else format(as.numeric(v))
  candidates <- vapply(plan$candidates, function(c) {
    bounds <- c(
      if (!is.null(c$lower)) sprintf(', "lower": %s', num(c$lower)),
      if (!is.null(c$upper)) sprintf(', "upper": %s', num(c$upper))
    )
    sprintf(
      '{"name": "%s", "theta": %d, "kind": "%s", "initial": %s, "fixed": %s%s}',
      c$name, as.integer(c$theta), unlist(c$kind), num(c$initial), num(c$fixed),
      paste(bounds, collapse = "")
    )
  }, character(1))
  o <- plan$options
  sprintf(
    '{"created": "%s", "pharos_version": "%s", "model": "%s", "out_dir": "%s",
     "candidates": [%s],
     "options": {"direction": [%s], "forward_alpha": %s, "backward_alpha": %s,
                 "num_rounds": %s, "max_retries": %d, "cov_step": %s,
                 "final_cov_step": %s, "forward_final_cov_step": %s}}',
    plan$created, plan$pharos_version, plan$model, plan$out_dir,
    paste(candidates, collapse = ", "),
    paste0('"', unlist(o$direction), '"', collapse = ", "),
    num(o$forward_alpha), num(o$backward_alpha),
    if (is.null(o$num_rounds)) "null" else as.integer(o$num_rounds),
    as.integer(o$max_retries), bool(o$cov_step), bool(o$final_cov_step),
    bool(o$forward_final_cov_step %||% TRUE)
  )
}

# A fabricated completed SCM process (matching the pharos state schema) so the
# status / decision-log paths can be tested without NONMEM. It ran under
# `plan`.
fabricate_completed_state <- function(out_dir, plan) {
  state <- sprintf(
    '{
  "plan": %s,
  "plan_digest": "%s",
  "roster": [
    {"name": "WT_CL", "theta": 4, "kind": "continuous", "initial": 0.1, "fixed": 0.0},
    {"name": "CRCL_CL", "theta": 5, "kind": "continuous", "initial": 0.1, "fixed": 0.0},
    {"name": "WT_V", "theta": 6, "kind": "continuous", "initial": 0.1, "fixed": 0.0}
  ],
  "status": "completed",
  "message": null,
  "retained": ["WT_CL"],
  "reference_model": "forward_round1/1001_wt_cl.mod",
  "reference_ofv": 980.0,
  "phase": null,
  "rounds": [
    {
      "name": "reference",
      "direction": "forward",
      "reference_model": "-",
      "reference_ofv": null,
      "candidates": [
        {
          "candidate": "base",
          "action": "fit base model",
          "model": "base/1001_base.mod",
          "attempts": [{"model": "base/1001_base.mod", "outcome": "succeeded"}],
          "status": "succeeded",
          "ofv": 1000.0,
          "delta_ofv": null,
          "df": 0,
          "p_value": null,
          "significant": null,
          "heuristics": [],
          "selected": false
        }
      ],
      "winner": null,
      "decision": "base model fitted (OFV 1000.000)",
      "complete": true
    },
    {
      "name": "forward_round1",
      "direction": "forward",
      "reference_model": "base/1001_base.mod",
      "reference_ofv": 1000.0,
      "candidates": [
        {
          "candidate": "WT_CL",
          "action": "add WT_CL",
          "model": "forward_round1/1001_wt_cl_try2.mod",
          "attempts": [
            {"model": "forward_round1/1001_wt_cl.mod", "outcome": "no ofv"},
            {"model": "forward_round1/1001_wt_cl_try2.mod", "outcome": "succeeded"}
          ],
          "status": "succeeded",
          "ofv": 980.0,
          "delta_ofv": -20.0,
          "df": 1,
          "p_value": 7.7e-6,
          "significant": true,
          "heuristics": ["parameter near boundary"],
          "selected": true
        },
        {
          "candidate": "CRCL_CL",
          "action": "add CRCL_CL",
          "model": "forward_round1/1001_crcl_cl.mod",
          "attempts": [
            {"model": "forward_round1/1001_crcl_cl.mod", "outcome": "succeeded"}
          ],
          "status": "succeeded",
          "ofv": 999.0,
          "delta_ofv": -1.0,
          "df": 1,
          "p_value": 0.317,
          "significant": false,
          "heuristics": [],
          "selected": false
        },
        {
          "candidate": "WT_V",
          "action": "add WT_V",
          "model": "forward_round1/1001_wt_v_try4.mod",
          "attempts": [
            {"model": "forward_round1/1001_wt_v.mod", "outcome": "no ofv"},
            {"model": "forward_round1/1001_wt_v_try2.mod", "outcome": "no ofv"},
            {"model": "forward_round1/1001_wt_v_try3.mod", "outcome": "no ofv"},
            {"model": "forward_round1/1001_wt_v_try4.mod", "outcome": "no ofv"}
          ],
          "status": "unusable",
          "ofv": null,
          "delta_ofv": null,
          "df": 1,
          "p_value": null,
          "significant": null,
          "heuristics": [],
          "selected": false
        }
      ],
      "winner": "WT_CL",
      "decision": "added WT_CL (p = 7.700e-6, dOFV = -20.000)",
      "complete": true
    }
  ],
  "final_model": "final/1001_scm_final.mod",
  "final_ofv": 980.0,
  "had_unusable": true,
  "updated": "2026-08-19T12:00:00+00:00"
}',
    plan_json(plan),
    attr(plan, "plan_digest")
  )
  writeLines(state, file.path(out_dir, "scm_state.json"))
}

# A fabricated SCM process stopped part way through its second forward
# round, so a retune can be shown landing on a candidate the open round is
# still testing. It ran under `plan`.
fabricate_mid_round_state <- function(out_dir, plan) {
  state <- sprintf(
    '{
  "plan": %s,
  "plan_digest": "%s",
  "roster": [
    {"name": "WT_CL", "theta": 4, "kind": "continuous", "initial": 0.1, "fixed": 0.0},
    {"name": "CRCL_CL", "theta": 5, "kind": "continuous", "initial": 0.1, "fixed": 0.0},
    {"name": "WT_V", "theta": 6, "kind": "continuous", "initial": 0.1, "fixed": 0.0}
  ],
  "status": "running",
  "message": null,
  "retained": ["WT_CL"],
  "reference_model": "forward_round1/1001_wt_cl.mod",
  "reference_ofv": 980.0,
  "phase": "forward",
  "rounds": [
    {
      "name": "reference",
      "direction": "forward",
      "reference_model": "-",
      "reference_ofv": null,
      "candidates": [
        {
          "candidate": "base",
          "action": "fit base model",
          "model": "base/1001_base.mod",
          "attempts": [{"model": "base/1001_base.mod", "outcome": "succeeded"}],
          "status": "succeeded",
          "ofv": 1000.0,
          "delta_ofv": null,
          "df": 0,
          "p_value": null,
          "significant": null,
          "heuristics": [],
          "selected": false
        }
      ],
      "winner": null,
      "decision": "base model fitted (OFV 1000.000)",
      "complete": true
    },
    {
      "name": "forward_round1",
      "direction": "forward",
      "reference_model": "base/1001_base.mod",
      "reference_ofv": 1000.0,
      "candidates": [
        {
          "candidate": "WT_CL",
          "action": "add WT_CL",
          "model": "forward_round1/1001_wt_cl.mod",
          "attempts": [
            {"model": "forward_round1/1001_wt_cl.mod", "outcome": "succeeded"}
          ],
          "status": "succeeded",
          "ofv": 980.0,
          "delta_ofv": -20.0,
          "df": 1,
          "p_value": 7.7e-6,
          "significant": true,
          "heuristics": [],
          "selected": true
        },
        {
          "candidate": "CRCL_CL",
          "action": "add CRCL_CL",
          "model": "forward_round1/1001_crcl_cl.mod",
          "attempts": [
            {"model": "forward_round1/1001_crcl_cl.mod", "outcome": "succeeded"}
          ],
          "status": "succeeded",
          "ofv": 999.0,
          "delta_ofv": -1.0,
          "df": 1,
          "p_value": 0.317,
          "significant": false,
          "heuristics": [],
          "selected": false
        },
        {
          "candidate": "WT_V",
          "action": "add WT_V",
          "model": "forward_round1/1001_wt_v.mod",
          "attempts": [
            {"model": "forward_round1/1001_wt_v.mod", "outcome": "succeeded"}
          ],
          "status": "succeeded",
          "ofv": 998.0,
          "delta_ofv": -2.0,
          "df": 1,
          "p_value": 0.157,
          "significant": false,
          "heuristics": [],
          "selected": false
        }
      ],
      "winner": "WT_CL",
      "decision": "added WT_CL (p = 7.700e-6, dOFV = -20.000)",
      "complete": true
    },
    {
      "name": "forward_round2",
      "direction": "forward",
      "reference_model": "forward_round1/1001_wt_cl.mod",
      "reference_ofv": 980.0,
      "candidates": [
        {
          "candidate": "CRCL_CL",
          "action": "add CRCL_CL",
          "model": "forward_round2/1001_crcl_cl.mod",
          "attempts": [
            {"model": "forward_round2/1001_crcl_cl.mod", "outcome": "succeeded"}
          ],
          "status": "succeeded",
          "ofv": 979.0,
          "delta_ofv": -1.0,
          "df": 1,
          "p_value": 0.317,
          "significant": false,
          "heuristics": [],
          "selected": false
        },
        {
          "candidate": "WT_V",
          "action": "add WT_V",
          "model": "",
          "attempts": [],
          "status": "pending",
          "ofv": null,
          "delta_ofv": null,
          "df": 1,
          "p_value": null,
          "significant": null,
          "heuristics": [],
          "selected": false
        }
      ],
      "winner": null,
      "decision": "",
      "complete": false
    }
  ],
  "final_model": null,
  "had_unusable": false,
  "updated": "2026-08-19T12:00:00+00:00"
}',
    plan_json(plan),
    attr(plan, "plan_digest")
  )
  writeLines(state, file.path(out_dir, "scm_state.json"))
}

# A driver record as `scm submit` leaves it: this host, a pid. pharos checks a
# login-node driver with `kill -0`, so a pid no process has is a driver that
# has gone.
dead_pid <- 4194303L
fabricate_driver_record <- function(out_dir, pid = dead_pid, job_id = NULL) {
  host <- system2("hostname", stdout = TRUE)[1]
  record <- if (is.null(job_id)) {
    sprintf(
      '{"mode": "login", "host": "%s", "pid": %d, "log": "%s", "started": "2026-08-19T11:00:00+00:00"}',
      host, pid, file.path(out_dir, "scm_driver.log")
    )
  } else {
    sprintf(
      '{"mode": "slurm", "job_id": %d, "log": "%s", "started": "2026-08-19T11:00:00+00:00"}',
      job_id, file.path(out_dir, "scm_driver.log")
    )
  }
  writeLines(record, file.path(out_dir, "scm_driver.json"))
}

test_that("a plan over a fresh out_dir prints only the plan", {
  dir <- withr::local_tempdir()
  plan <- suppressWarnings(make_plan(dir))

  txt <- cli_text_of(print(plan))
  expect_no_match(txt, "Where the SCM process stands")
  expect_no_match(txt, "Changes from the plan")
  expect_null(attr(plan, "context")$progress)
  # what a run of it would do is not printed: the attributes carry it, and
  # scm_plan() reports what matters as it builds the plan
  expect_no_match(txt, "What scm_run() does", fixed = TRUE)
  expect_no_match(txt, "refused right now", fixed = TRUE)
  expect_false(attr(plan, "overwrite"))
  knit <- as.character(knitr::knit_print(plan))
  expect_no_match(knit, "What scm_run() does", fixed = TRUE)

  # a pause cap is carried by the plan and shown in its settings
  capped <- suppressWarnings(scm_plan(attr(plan, "config"), num_rounds = 2))
  expect_equal(capped$options$num_rounds, 2L)
  expect_match(
    cli_one_line(print(capped)),
    "num rounds: pause after 2 (resumable)",
    fixed = TRUE
  )
})

test_that("a plan carries the project's [nonmem.scm] settings", {
  dir <- withr::local_tempdir()
  write_scm_fixture(dir)
  cat(
    "\n[nonmem.scm]\npartition = \"fits\"\ndriver_partition = \"drv\"\naccount = \"acct\"\n",
    "max_concurrent = 4\npoll_interval = 5\n",
    file = file.path(dir, "pharos.toml"), append = TRUE, sep = ""
  )
  plan <- no_cov_step_warning(scm_plan(write_scm_config(dir)))
  settings <- attr(plan, "settings")
  expect_equal(settings$partition, "fits")
  expect_equal(settings$driver_partition, "drv")
  expect_equal(settings$account, "acct")
  expect_equal(settings$max_concurrent, 4L)
  expect_equal(settings$poll_interval, 5L)
})

test_that("a plan the SCM process cannot resume under says a run would be refused", {
  dir <- withr::local_tempdir()
  plan <- suppressWarnings(make_plan(dir))
  fabricate_completed_state(scm_dir(plan), plan)

  # tightening the forward alpha is a different SCM process, so the state
  # left in the out_dir can no longer be resumed: the dry run says so --
  # which setting moved and what to do -- and scm_run() is what refuses
  expect_message(
    replan <- suppressWarnings(scm_plan(write_scm_config(dir, extra = "forward_alpha = 0.01"))),
    "would be refused right now"
  )
  expect_false(attr(replan, "resumable"))
  why <- attr(replan, "refusals")
  expect_length(why, 1)
  expect_match(why, "cannot resume under this one", fixed = TRUE)
  expect_match(why, "forward_alpha 0.05 -> 0.01: this SCM process ran under the previous value",
               fixed = TRUE)
  expect_match(why, "run with `overwrite = TRUE`", fixed = TRUE)
  ctx <- attr(replan, "context")
  expect_true(unlist(ctx$state_is_stale))
  txt <- cli_text_of(print(replan))
  expect_match(txt, "forward_alpha: 0.05 -> 0.01 (SCM-defining)", fixed = TRUE)
  # the refusal is scm_plan()'s message and the attribute, not a printed section
  expect_no_match(txt, "refused right now", fixed = TRUE)
  expect_no_match(as.character(knitr::knit_print(replan)), "refused right now", fixed = TRUE)
  # the process is intact, and still reads under the plan it ran under
  expect_true(file.exists(file.path(scm_dir(plan), "scm_state.json")))
  expect_equal(scm_status(plan)$status, "completed")
})

test_that("overwrite previews what scm_run(overwrite = TRUE) discards, discarding nothing", {
  dir <- withr::local_tempdir()
  plan <- suppressWarnings(make_plan(dir))
  out_dir <- scm_dir(plan)
  fabricate_completed_state(out_dir, plan)

  # a plan that does not validate previews nothing
  expect_error(
    scm_plan(write_scm_config(dir, covariates = '["AGE_CL"]'), overwrite = TRUE),
    "no theta named AGE_CL"
  )

  # a different SCM process, started fresh: the message says what would go,
  # and the plan is shown against the empty out_dir the driver will make
  expect_message(
    replan <- suppressWarnings(scm_plan(
      write_scm_config(dir, extra = "forward_alpha = 0.01"),
      overwrite = TRUE
    )),
    "discards the SCM process in .*completed, 1 round complete"
  )
  expect_equal(attr(replan, "discards"), "completed, 1 round complete")
  expect_equal(replan$options$forward_alpha, 0.01)
  expect_true(attr(replan, "resumable"))
  expect_length(attr(replan, "refusals"), 0)
  expect_null(attr(replan, "context")$progress)
  txt <- cli_text_of(print(replan))
  expect_no_match(txt, "Where the SCM process stands")
  expect_true(attr(replan, "overwrite"))
  expect_no_match(txt, "overwrite:", fixed = TRUE)
  expect_no_match(as.character(knitr::knit_print(replan)), "overwrite:", fixed = TRUE)
  # without overwrite the same process is resumed; what overwrite would
  # discard is the attribute, not a printed line
  kept <- suppressWarnings(scm_plan(attr(plan, "config")))
  expect_false(attr(kept, "overwrite"))
  expect_equal(attr(kept, "discards"), "completed, 1 round complete")
  expect_no_match(cli_one_line(print(kept)), "overwrite:", fixed = TRUE)
  # ... but the dry run discarded nothing: the process is still there,
  # still under the plan it ran under
  expect_true(file.exists(file.path(out_dir, "scm_state.json")))
  expect_equal(scm_status(replan)$status, "completed")

  # nothing to discard: previewing a fresh out_dir with overwrite says nothing
  fresh <- withr::local_tempdir()
  expect_no_message(
    suppressWarnings(make_plan(fresh, overwrite = TRUE)),
    message = "discards"
  )
})

test_that("overwrite is refused while the SCM process's driver is alive", {
  dir <- withr::local_tempdir()
  plan <- suppressWarnings(make_plan(dir))
  fabricate_mid_round_state(scm_dir(plan), plan)
  # this R session stands in for the login-node driver: alive, on this host
  fabricate_driver_record(scm_dir(plan), pid = Sys.getpid())

  changed <- write_scm_config(dir, extra = "forward_alpha = 0.01")
  expect_message(
    replan <- suppressWarnings(scm_plan(changed, overwrite = TRUE)),
    "would be refused right now"
  )
  why <- attr(replan, "refusals")
  expect_match(why, "still being driven by pid", all = FALSE)
  expect_match(why, "scm_stop()", fixed = TRUE, all = FALSE)
  expect_true(file.exists(file.path(scm_dir(plan), "scm_state.json")))
  # and without overwrite, a live driver already has the process: a second
  # one is refused too
  replan <- suppressWarnings(scm_plan(changed))
  expect_match(attr(replan, "refusals"), "already being driven by pid", all = FALSE)

  # a driver that has gone is no obstacle
  fabricate_driver_record(scm_dir(plan), pid = dead_pid)
  expect_message(
    replan <- suppressWarnings(scm_plan(changed, overwrite = TRUE)),
    "discards the SCM process"
  )
  expect_length(attr(replan, "refusals"), 0)
})

test_that("re-planning without a never-selected candidate keeps the SCM process", {
  dir <- withr::local_tempdir()
  plan <- suppressWarnings(make_plan(dir))
  fabricate_completed_state(scm_dir(plan), plan)

  # WT_V lost round 1; dropping it alone is a compatible change
  replan <- suppressWarnings(scm_plan(write_scm_config(
    dir,
    covariates = '["WT_CL", "CRCL_CL"]'
  )))
  ctx <- attr(replan, "context")
  expect_false(isTRUE(unlist(ctx$state_is_stale)))
  expect_equal(unlist(ctx$removals), "WT_V")
  txt <- cli_text_of(print(replan))
  expect_match(txt, "Removing WT_V -- never selected", fixed = TRUE)
  expect_no_match(txt, "cannot resume")

  # the plan also says where the process got to
  expect_match(txt, "Where the SCM process stands")
  expect_match(txt, "progress: completed -- 1 round complete", fixed = TRUE)
  expect_match(txt, "selected: WT_CL", fixed = TRUE)
  expect_match(txt, "final model: final/1001_scm_final.mod", fixed = TRUE)
  expect_match(txt, "Changes from the config the SCM process was submitted under")
  expect_match(txt, "candidates: WT_V removed THETA(6)", fixed = TRUE)
  expect_no_match(txt, "(SCM-defining)", fixed = TRUE)
  expect_no_match(txt, "refused right now", fixed = TRUE)
  knit <- as.character(knitr::knit_print(replan))
  expect_match(knit, "Where the SCM process stands")
  expect_match(knit, "WT_V removed THETA(6)", fixed = TRUE)

  # WT_CL won round 1: dropping it is a different SCM process, refused
  # without overwrite
  replan <- suppressWarnings(scm_plan(write_scm_config(
    dir,
    covariates = '["CRCL_CL", "WT_V"]'
  )))
  expect_false(attr(replan, "resumable"))
  why <- attr(replan, "refusals")
  expect_match(why, "cannot resume", all = FALSE)
  expect_match(why, "WT_CL was selected in forward_round1", all = FALSE)
})

test_that("re-planning with a retuned initial keeps the SCM process", {
  dir <- withr::local_tempdir()
  plan <- suppressWarnings(make_plan(dir))
  fabricate_completed_state(scm_dir(plan), plan)

  # A new initial estimate on WT_CL -- the usual fix for a candidate that
  # failed on the old one -- is a retune, not a different SCM process.
  replan <- suppressWarnings(scm_plan(write_scm_config(
    dir,
    section = "continuous = { initial = 0.4 }"
  )))
  ctx <- attr(replan, "context")
  expect_false(isTRUE(unlist(ctx$state_is_stale)))
  parts <- scm_plan_context_parts(replan)
  expect_setequal(
    vapply(parts$retunes, function(r) r$name, character(1)),
    c("WT_CL", "CRCL_CL", "WT_V")
  )
  expect_match(
    parts$retunes[[1]]$label, "initial 0.1 -> 0.4", fixed = TRUE
  )

  txt <- cli_text_of(print(replan))
  expect_no_match(txt, "cannot resume")
  expect_no_match(txt, "(SCM-defining)", fixed = TRUE)
  # nothing is mid-round, so the new values only bear on models still to come
  expect_match(txt, "takes effect from the next model written", fixed = TRUE)
  # WT_CL is in the model already; the round it was fitted in stands
  expect_match(txt, "WT_CL is already in the model", fixed = TRUE)

  knit <- as.character(knitr::knit_print(replan))
  expect_match(knit, "Retuning WT_CL: initial 0.1 -> 0.4", fixed = TRUE)
})

test_that("a retune on a candidate in the open round is reported as a refit", {
  dir <- withr::local_tempdir()
  plan <- suppressWarnings(make_plan(dir))
  fabricate_mid_round_state(scm_dir(plan), plan)

  # WT_V is still being tested in forward_round2, so bounding it there means
  # that round refits it under the new bounds.
  replan <- suppressWarnings(scm_plan(write_scm_config(
    dir,
    covariates = '["WT_CL", "CRCL_CL", { name = "WT_V", lower = 0, upper = 2 }]'
  )))
  ctx <- attr(replan, "context")
  expect_false(isTRUE(unlist(ctx$state_is_stale)))

  txt <- cli_text_of(print(replan))
  expect_match(
    txt, "Retuning WT_V: bounds none -> (0, 2)", fixed = TRUE
  )
  expect_match(txt, "refitted in forward_round2 under the new values", fixed = TRUE)
  expect_no_match(txt, "cannot resume")
})

test_that("re-planning the same SCM process reports no changes", {
  dir <- withr::local_tempdir()
  plan <- suppressWarnings(make_plan(dir))
  fabricate_completed_state(scm_dir(plan), plan)
  replan <- suppressWarnings(scm_plan(write_scm_config(dir)))

  ctx <- attr(replan, "context")
  expect_length(ctx$changes, 0)
  expect_true(attr(replan, "resumable"))

  txt <- cli_text_of(print(replan))
  expect_match(txt, "none - identical to the config the SCM process was submitted under", fixed = TRUE)
  expect_match(as.character(knitr::knit_print(replan)),
               "none - identical to the config the SCM process was submitted under", fixed = TRUE)

  # with no process started there is nothing to compare the plan with, so
  # nothing is said about changes either way
  fresh <- withr::local_tempdir()
  first <- suppressWarnings(make_plan(fresh))
  again <- suppressWarnings(scm_plan(write_scm_config(fresh)))
  expect_null(attr(again, "context")$progress)
  expect_no_match(cli_text_of(print(again)), "identical", fixed = TRUE)
})

test_that("scm_status and summary read a completed SCM process", {
  dir <- withr::local_tempdir()
  plan <- make_plan(dir)
  fabricate_completed_state(scm_dir(plan), plan)

  st <- scm_status(plan)
  expect_equal(st$status, "completed")
  expect_equal(unlist(st$retained), "WT_CL")
  expect_equal(unlist(st$candidates), c("WT_CL", "CRCL_CL", "WT_V"))
  expect_equal(st$final_model, "final/1001_scm_final.mod")
  expect_equal(st$final_ofv, 980.0)
  # the reference fit is not a round
  expect_equal(as.integer(st$totals$rounds_complete), 1L)
  expect_equal(as.integer(st$totals$unusable), 1L)
  expect_equal(as.integer(st$totals$retries), 4L)

  st_text <- paste(capture.output(print(st)), collapse = "\n")
  expect_match(st_text, "<scm status> scm/1001", fixed = TRUE)
  expect_match(st_text, "candidates : WT_CL, CRCL_CL, WT_V", fixed = TRUE)
  expect_match(st_text, "status     : completed (updated ", fixed = TRUE)
  expect_match(st_text, "final model: final/1001_scm_final.mod (OFV 980.000)",
               fixed = TRUE)
  # status shows the process as its driver's terminal did: each decided
  # round's decision line, then the round at a glance -- never the summary's
  # critical values or records line
  expect_match(st_text, "reference complete: base model fitted (OFV 1000.000)", fixed = TRUE)
  expect_match(st_text, "forward_round1 complete: added WT_CL (p = 7.700e-6, dOFV = -20.000); retained: WT_CL",
               fixed = TRUE)
  expect_match(st_text, "<- added", fixed = TRUE)
  expect_match(st_text, "unusable (no ofv)", fixed = TRUE)
  expect_match(st_text, "[2 attempts]", fixed = TRUE)
  expect_no_match(st_text, "crit dOFV", fixed = TRUE)
  expect_no_match(st_text, "scm_summary.{json,md}", fixed = TRUE)
  # no driver was ever recorded for the fabricated process
  expect_no_match(st_text, "driver", fixed = TRUE)
  expect_true(attr(st, "started"))

  # summary() returns the record as a data.frame, one row per candidate per
  # round -- the same rows as.data.frame() gives a summary
  log <- summary(st)
  expect_s3_class(log, "data.frame")
  expect_equal(nrow(log), 4) # base + 3 candidates
  expect_equal(
    names(log),
    c(
      "round", "direction", "candidate", "action", "status", "model",
      "attempts", "superseded", "ofv", "reference_ofv", "delta_ofv",
      "statistic", "df", "p_value", "alpha", "critical_delta_ofv",
      "significant", "selected", "rank", "theta", "heuristics"
    )
  )
  # the reference round is fitted against nothing, so it has no reference
  # OFV and no alpha -- its own OFV is the base model's
  expect_true(is.na(log$reference_ofv[log$round == "reference"]))
  expect_true(is.na(log$alpha[log$round == "reference"]))
  expect_equal(log$ofv[log$round == "reference"], 1000)

  wt_cl <- log[log$candidate == "WT_CL", ]
  expect_equal(wt_cl$attempts, 2L)
  expect_equal(wt_cl$delta_ofv, -20.0)
  expect_equal(wt_cl$reference_ofv, 1000.0)
  expect_true(wt_cl$selected)
  expect_equal(wt_cl$rank, 1L)
  expect_equal(wt_cl$heuristics, "parameter near boundary")

  wt_v <- log[log$candidate == "WT_V", ]
  expect_equal(wt_v$status, "unusable")
  expect_equal(wt_v$attempts, 4L)
  expect_true(is.na(wt_v$p_value)) # reported, never scored
})

test_that("scm_status reports the driver and the open round fit by fit", {
  dir <- withr::local_tempdir()
  plan <- make_plan(dir)
  out_dir <- scm_dir(plan)

  # a completed process whose login-node driver has exited
  fabricate_completed_state(out_dir, plan)
  fabricate_driver_record(out_dir)
  st <- scm_status(plan)
  driver <- attr(st, "driver")
  expect_equal(driver$mode, "login")
  expect_equal(driver$pid, dead_pid)
  expect_equal(driver$liveness, "gone")
  st_text <- paste(capture.output(print(st)), collapse = "\n")
  expect_match(st_text, sprintf("driver     : exited (pid %d on ", dead_pid), fixed = TRUE)
  expect_match(st_text, "; see .*scm_driver.log")

  # a process still running whose driver has gone: it stopped before
  # finishing, and running its config again resumes it
  fabricate_mid_round_state(out_dir, plan)
  st <- scm_status(plan)
  st_text <- paste(capture.output(print(st)), collapse = "\n")
  expect_match(st_text, "driver     : NOT RUNNING (pid", fixed = TRUE)
  expect_match(st_text, "the SCM process stopped before finishing", fixed = TRUE)
  expect_match(st_text, "submit its config again to resume", fixed = TRUE)
  # ... and the open round, fit by fit
  # the reference fit, three round-1 fits and one round-2 fit so far
  expect_match(st_text, "progress   : 5/13 fits · forward round 2", fixed = TRUE)
  expect_match(st_text, "forward_round1 complete: added WT_CL", fixed = TRUE)
  expect_match(st_text, "forward_round2: 1/2 done · 1 pending", fixed = TRUE)
  expect_match(st_text, "✓ CRCL_CL  fitted on attempt 1, OFV 979.000", fixed = TRUE)
  expect_match(st_text, "○ WT_V     pending", fixed = TRUE)

  # this very session as the driver: alive
  fabricate_driver_record(out_dir, pid = Sys.getpid())
  st <- scm_status(plan)
  expect_equal(attr(st, "driver")$liveness, "alive")
  expect_match(paste(capture.output(print(st)), collapse = "\n"),
               sprintf("driver     : running (pid %d on ", Sys.getpid()), fixed = TRUE)

  # a slurm driver can only be checked where squeue is
  fabricate_driver_record(out_dir, job_id = 4242L)
  st <- scm_status(plan)
  driver <- attr(st, "driver")
  expect_equal(driver$job_id, 4242L)
  expect_true(driver$liveness %in% c("alive", "gone", "unknown"))
  if (driver$liveness == "unknown") {
    expect_match(driver$detail, "squeue")
    expect_match(paste(capture.output(print(st)), collapse = "\n"),
                 "driver     : slurm job 4242 — cannot check it from here", fixed = TRUE)
  }
})

test_that("scm_summary renders every round by default and drills into one", {
  dir <- withr::local_tempdir()
  plan <- make_plan(dir)
  fabricate_completed_state(scm_dir(plan), plan)

  # every round to date
  sm <- scm_summary(plan)
  expect_s3_class(sm, "hyperion_scm_summary")
  expect_equal(sm$status, "completed")
  expect_length(sm$rounds, 2) # reference + forward_round1
  txt <- paste(capture.output(print(sm)), collapse = "\n")
  expect_match(txt, "<scm summary>", fixed = TRUE)
  # the path the rounds took, and where the model stands now
  expect_match(txt, "path       : +WT_CL (forward 1) => WT_CL", fixed = TRUE)
  expect_match(txt, "final model: final/1001_scm_final.mod (OFV 980.000)", fixed = TRUE)
  expect_match(txt, "records    : scm_summary.{json,md}", fixed = TRUE)
  expect_match(txt, "forward_round1   added WT_CL (p = 7.700e-6, dOFV = -20.000) [4 retries]", fixed = TRUE)
  expect_match(txt, "                 ref OFV 1000.000 · alpha 0.05 · crit dOFV 3.841", fixed = TRUE)
  expect_match(txt, "<- added", fixed = TRUE)
  # sorted winner-first: WT_CL, then CRCL_CL, then the unusable WT_V
  expect_lt(regexpr("  WT_CL   ", txt, fixed = TRUE),
            regexpr("  CRCL_CL   ", txt, fixed = TRUE))
  expect_match(txt, "unusable")

  # one round: number, "round N", full name, and "reference" all resolve;
  # so do the plan / out_dir / config addressing forms
  rd <- scm_summary(plan, 1)
  expect_length(rd$rounds, 1)
  expect_equal(rd$rounds[[1]]$round, "forward_round1")
  expect_equal(scm_summary(scm_dir(plan), "round 1")$rounds[[1]]$round, "forward_round1")
  expect_equal(scm_summary(attr(plan, "config"), "round 1")$rounds[[1]]$round, "forward_round1")
  expect_equal(scm_summary(plan, "forward_round1")$rounds[[1]]$round, "forward_round1")
  expect_equal(scm_summary(plan, "reference")$rounds[[1]]$round, "reference")
  expect_error(scm_summary(plan, 7), "forward_round1")

  # a single round lists every attempt, retries included
  txt <- paste(capture.output(print(rd)), collapse = "\n")
  expect_match(txt, "forward_round1/1001_wt_cl.mod", fixed = TRUE)
  expect_match(txt, "no ofv")
  expect_match(txt, "forward_round1/1001_wt_cl_try2.mod", fixed = TRUE)
  expect_match(txt, "heuristics: parameter near boundary")

  # the detail flags stack
  long <- paste(capture.output(print(scm_summary(plan, long = TRUE))), collapse = "\n")
  expect_match(long, "candidate             OFV       dOFV         p", fixed = TRUE)
  expect_match(long, "base/1001_base.mod", fixed = TRUE)
  # the detail line under each candidate: the estimate needs the run's own
  # output, which a fabricated process has none of, so df, tries and the
  # heuristics are what is left
  expect_match(long, "df 1 · tries 2 · heuristics parameter near boundary", fixed = TRUE)
  # a trace keeps only the rounds the candidate was tested in, and only its
  # own row in them
  trace_sm <- scm_summary(plan, candidate = "CRCL_CL")
  expect_length(trace_sm$rounds, 1)
  expect_equal(
    vapply(trace_sm$rounds[[1]]$candidates, function(c) c$candidate, character(1)),
    "CRCL_CL"
  )
  trace <- paste(capture.output(print(trace_sm)), collapse = "\n")
  expect_match(trace, "CRCL_CL           999.000", fixed = TRUE)
  expect_no_match(trace, "WT_CL   ", fixed = TRUE)
  expect_error(scm_summary(plan, candidate = "AGE_CL"), "no candidate named AGE_CL")

  # as.data.frame: one row per candidate per round
  df <- as.data.frame(sm)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 4) # base + 3 candidates
  wt_cl <- df[df$candidate == "WT_CL", ]
  expect_equal(wt_cl$delta_ofv, -20)
  expect_equal(wt_cl$rank, 1L)
  expect_true(wt_cl$selected)
  expect_equal(wt_cl$theta, 4L)
  expect_true(abs(wt_cl$critical_delta_ofv - 3.841) < 1e-3)
  expect_equal(df$status[df$candidate == "WT_V"], "unusable")

  # knit_print emits pharos's round markdown: a section per round with its
  # facts, and a table of its candidates
  knit <- knitr::knit_print(sm)
  expect_s3_class(knit, "knit_asis")
  md <- as.character(knit)
  expect_match(md, "## reference\n\n- model: `base/1001_base.mod` (succeeded)", fixed = TRUE)
  expect_match(md, "## forward_round1\n\n- reference: `base/1001_base.mod` (OFV 1000.000)", fixed = TRUE)
  expect_match(md, "- alpha: 0.05", fixed = TRUE)
  expect_match(md, "- all models minimized: no", fixed = TRUE)
  expect_match(md, "- heuristic checks fired: yes", fixed = TRUE)
  expect_match(md, "- decision: added WT_CL (p = 7.700e-6, dOFV = -20.000)", fixed = TRUE)
  expect_match(md, "- retained after this round: WT_CL", fixed = TRUE)
  expect_match(md, "| candidate | model | attempts | status | OFV | \u0394OFV | p | significant | selected | estimate (RSE%) | heuristic checks |",
               fixed = TRUE)
  expect_match(md, "| WT_CL | `forward_round1/1001_wt_cl_try2.mod` | 2 | succeeded | 980.000 | -20.000 | 7.7000e-6 | yes | **yes** |  | parameter near boundary |",
               fixed = TRUE)
  expect_match(md, "| WT_V | `forward_round1/1001_wt_v_try4.mod` | 4 | unusable |  |  |  |  |  |  |  |", fixed = TRUE)
  expect_match(md, "_Unusable candidates are reported above", fixed = TRUE)
  # a selection keeps only its rounds (and rows)
  expect_no_match(as.character(knitr::knit_print(rd)), "## reference", fixed = TRUE)
  expect_no_match(as.character(knitr::knit_print(trace_sm)), "| WT_CL |", fixed = TRUE)

  # input validation
  expect_error(scm_summary(plan, round = 0), "whole number")
  expect_error(scm_summary(plan, candidate = 3), "candidate")
  expect_error(scm_summary(plan, long = "yes"), "TRUE or FALSE")
})

test_that("scm_status resolves plans, statuses, dirs, and config paths", {
  dir <- withr::local_tempdir()
  plan <- make_plan(dir)

  st1 <- scm_status(plan)
  st2 <- scm_status(scm_dir(plan))
  st3 <- scm_status(attr(plan, "config"))
  st4 <- scm_status(st1)
  expect_equal(st1$status, st2$status)
  expect_equal(st2$status, st3$status)
  expect_equal(st3$status, st4$status)

  expect_error(scm_status(file.path(dir, "nope")), "no SCM output")
  # the model's directory is not an SCM out_dir: it holds no config
  expect_error(scm_status(dir), "no SCM config")
  expect_error(scm_status(42), "hyperion_scm_plan")
})

test_that("scm_run takes a plan, a status, a config path or an out_dir", {
  dir <- withr::local_tempdir()
  plan <- make_plan(dir)
  config <- attr(plan, "config")
  expect_equal(scm_config_path(plan), config)
  expect_equal(scm_config_path(config), config)
  expect_equal(normalizePath(scm_config_path(scm_dir(plan))), normalizePath(config))
  expect_equal(normalizePath(scm_config_path(scm_status(plan))), normalizePath(config))

  expect_error(scm_run(42), "hyperion_scm_plan")
  expect_error(scm_run(file.path(dir, "nope.toml")), "no SCM config")
  # the model's directory holds no config
  expect_error(scm_run(dir), "no SCM config")
})

test_that("scm_run refuses a plan whose config is gone", {
  dir <- withr::local_tempdir()
  plan <- make_plan(dir)
  unlink(attr(plan, "config"))
  expect_error(scm_run(plan), "config is missing")
})

test_that("scm_run validates max_concurrent", {
  dir <- withr::local_tempdir()
  plan <- make_plan(dir)

  expect_error(scm_run(plan, max_concurrent = -1), "max_concurrent")
  expect_error(scm_run(plan, max_concurrent = 1.5), "max_concurrent")
  expect_error(scm_run(plan, max_concurrent = Inf), "max_concurrent")
  expect_error(scm_run(plan, max_concurrent = NA), "max_concurrent")
  expect_error(scm_run(plan, max_concurrent = c(1, 2)), "max_concurrent")

  # num_rounds is run control: 0 (all rounds) up, whole numbers only
  expect_error(scm_run(plan, num_rounds = -1), "num_rounds")
  expect_error(scm_run(plan, num_rounds = 1.5), "num_rounds")
  expect_error(scm_run(plan, num_rounds = NA), "num_rounds")

  # everything after x is named-only: a stray positional value (here a
  # quoted "name = value") is refused as such, not validated as whichever
  # argument comes next in the signature
  err <- expect_error(scm_run(plan, "max_concurrent = 4"), "must be empty")
  expect_match(conditionMessage(err), "max_concurrent = 4", fixed = TRUE)
  expect_no_match(conditionMessage(err), "num_rounds", fixed = TRUE)
  expect_error(scm_run(plan, 2), "must be empty")
  expect_error(scm_run(plan, driver = "login", 2), "must be empty")
})

test_that("scm_run validates the driver and slurm arguments", {
  dir <- withr::local_tempdir()
  plan <- make_plan(dir)

  expect_error(scm_run(plan, driver = "local"), "driver")
  expect_error(scm_run(plan, partition = 1), "partition")
  expect_error(scm_run(plan, account = c("a", "b")), "account")
  expect_error(scm_run(plan, shared_node = "yes"), "shared_node")
  expect_error(scm_run(plan, overwrite = NA), "overwrite")

  # the driver partition only means something for a driver job of its own
  expect_error(
    scm_run(plan, driver = "login", driver_partition = "p"),
    "driver_partition"
  )
  expect_error(
    scm_run(plan, shared_node = TRUE, driver_partition = "p"),
    "driver_partition"
  )

  # `slurm = FALSE` (a local run) no longer exists in pharos: an unknown
  # named argument is refused by name
  err <- expect_error(scm_run(plan, slurm = FALSE), "must be empty")
  expect_match(conditionMessage(err), "slurm = FALSE", fixed = TRUE)
})

# A stand-in for the pharos executable: a shell script that prints `output`
# and exits with `status`, after `delay` seconds. What `scm_run()` does with
# pharos's answer can be tested without a cluster.
fake_pharos <- function(output, status = 0L, delay = 0) {
  script <- tempfile("pharos")
  writeLines(c(
    "#!/bin/sh",
    if (delay > 0) paste("sleep", delay),
    # what hyperion typed, so the test can see the config and flags it passed
    'echo "args: $@"',
    paste0("echo ", shQuote(output)),
    paste("exit", status)
  ), script)
  Sys.chmod(script, "755")
  script
}

test_that("scm_run reports what pharos answered", {
  dir <- withr::local_tempdir()
  plan <- make_plan(dir)
  out_dir <- scm_dir(plan)

  # the driver queued as a slurm job: pharos's own lines are passed through
  withr::local_options(hyperion.pharos_exe = fake_pharos(
    "<scm slurm submit> submitted the SCM driver as slurm job 42 (scm_1001)"
  ))
  expect_message(
    res <- scm_run(plan),
    "submitted the SCM driver as slurm job 42"
  )
  expect_equal(res$out_dir, out_dir)
  expect_equal(res$config, attr(plan, "config"))
  expect_equal(res$log, file.path(out_dir, "scm_run.log"))
  # the config is what pharos gets, with the run control as flags
  expect_match(readLines(res$log), paste("args: nonmem scm slurm submit", attr(plan, "config")),
               fixed = TRUE, all = FALSE)
  res <- scm_run(attr(plan, "config"), num_rounds = 2, partition = "big", max_concurrent = 4,
                 overwrite = TRUE)
  expect_match(readLines(res$log),
               "--num-rounds 2 --partition big --max-concurrent 4 --overwrite",
               fixed = TRUE, all = FALSE)
  res <- scm_run(scm_dir(plan), driver = "login", num_rounds = 0, shared_node = TRUE)
  Sys.sleep(0.5)
  expect_match(readLines(res$log), "args: nonmem scm submit", fixed = TRUE, all = FALSE)
  expect_match(readLines(res$log), "--num-rounds 0 --shared-node", fixed = TRUE, all = FALSE)

  # pharos refusing (a driver already queued for this process) is an error
  # here, not a line in the log -- for either driver
  refusal <- "the SCM process in scm/1001 already has a driver in the queue: slurm job 42 (scm_1001)"
  withr::local_options(hyperion.pharos_exe = fake_pharos(refusal, status = 1L))
  err <- expect_error(scm_run(plan), "pharos failed to submit the SCM driver")
  expect_match(conditionMessage(err), "already has a driver", fixed = TRUE)
  err <- expect_error(scm_run(plan, driver = "login"), "pharos refused to start the SCM driver")
  expect_match(conditionMessage(err), "already has a driver", fixed = TRUE)

  # a login-node driver that is still going after a moment is left to run
  withr::local_options(hyperion.pharos_exe = fake_pharos("driver started (login)", delay = 5))
  expect_message(scm_run(plan, driver = "login"), "launched in the background")
  Sys.sleep(5)
  expect_match(readLines(file.path(out_dir, "scm_run.log")), "driver started", all = FALSE)

  # the executable named by the option has to exist
  withr::local_options(hyperion.pharos_exe = file.path(dir, "nope"))
  expect_error(scm_run(plan), "pharos executable not found at")
})

# `scm_run()` launches a real pharos, so the subcommands it types have to be
# ones the installed pharos answers to.
test_that("scm_run launches pharos subcommands that exist", {
  found <- detect_pharos()
  skip_if(is.na(found$path), "pharos not on PATH")
  for (driver in c("slurm", "login")) {
    expect_equal(
      system2(found$path, c(scm_run_subcommand(driver), "--help"),
              stdout = FALSE, stderr = FALSE),
      0L,
      label = driver
    )
  }
  # ... and the ones scm_plan() / scm_stop() stand in for
  for (command in c("plan", "stop", "status", "summary")) {
    expect_equal(
      system2(found$path, c("nonmem", "scm", command, "--help"),
              stdout = FALSE, stderr = FALSE),
      0L,
      label = command
    )
  }
})

test_that("scm_plan validates num_rounds", {
  dir <- withr::local_tempdir()

  # invalid values error before pharos is reached
  expect_error(make_plan(dir, num_rounds = -1), "num_rounds")
  expect_error(make_plan(dir, num_rounds = 1.5), "num_rounds")
  expect_error(make_plan(dir, num_rounds = Inf), "num_rounds")
  expect_error(make_plan(dir, num_rounds = NA), "num_rounds")
  expect_error(make_plan(dir, num_rounds = c(1, 2)), "num_rounds")

  # pacing is run control: the plan previews it, scm_run() applies it
  plan <- make_plan(dir, num_rounds = 2)
  expect_equal(plan$options$num_rounds, 2)
  expect_match(cli_text_of(print(plan)), "num rounds: pause after 2 (resumable)", fixed = TRUE)
  # 0 runs all rounds
  expect_null(make_plan(dir, num_rounds = 0)$options$num_rounds)
})

test_that("scm_stop stops a login-node driver and says how to resume", {
  dir <- withr::local_tempdir()
  plan <- make_plan(dir)
  out_dir <- scm_dir(plan)
  fabricate_mid_round_state(out_dir, plan)

  expect_error(scm_stop(plan, fits = NA), "TRUE or FALSE")
  # no driver was ever started for it
  expect_error(scm_stop(plan), "no driver has been started")

  # a driver that has gone: nothing to stop, and the way back
  fabricate_driver_record(out_dir, pid = dead_pid)
  expect_message(res <- scm_stop(plan), "has already exited")
  expect_false(res$was_alive)
  expect_null(res$fits)
  expect_null(res$allocation)
  expect_equal(normalizePath(res$config), normalizePath(attr(plan, "config")))
  expect_message(scm_stop(scm_dir(plan)), "resume it with")
  expect_message(scm_stop(attr(plan, "config")), "scm_status")

  # a live one: a sleeping shell process stands in for the login-node driver
  # (its output detached from the pipe this call reads, so the shell's exit
  # ends the call while the sleep lives on)
  pid <- as.integer(system2(
    "sh", c("-c", shQuote("sleep 60 >/dev/null 2>&1 & echo $!")),
    stdout = TRUE
  ))
  withr::defer(suppressWarnings(system2("kill", pid, stdout = FALSE, stderr = FALSE)))
  fabricate_driver_record(out_dir, pid = pid)
  expect_equal(attr(scm_status(plan), "driver")$liveness, "alive")
  expect_message(res <- scm_stop(scm_status(plan)), "told to stop")
  expect_true(res$was_alive)
  expect_match(res$driver, sprintf("pid %d on ", pid), fixed = TRUE)
  Sys.sleep(0.5)
  expect_equal(attr(scm_status(plan), "driver")$liveness, "gone")

  # cancelling the fits still in the queue needs squeue
  if (!nzchar(Sys.which("squeue"))) {
    expect_error(scm_stop(plan, fits = TRUE), "squeue")
  } else {
    expect_message(res <- scm_stop(plan, fits = TRUE), "none in the slurm queue")
    expect_length(res$fits, 0)
  }
})
