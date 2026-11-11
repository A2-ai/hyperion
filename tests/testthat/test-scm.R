test_that("read_scm() reads the config into a hyperion_nonmem_scm object", {
  scm <- read_scm(scm_fixture("scm", "1001scm.toml"))
  expect_s3_class(scm, "hyperion_nonmem_scm")
  expect_equal(attr(scm, "config_source"), "extdata/models/scm/scm/1001scm.toml")
  expect_equal(attr(scm, "filename"), "1001")
  expect_equal(scm$model, "extdata/models/scm/1001.mod")
  expect_equal(scm$direction, c("forward", "backward"))
  expect_equal(scm$forward_alpha, 0.05)
  expect_equal(vapply(scm$covariates$effects, function(e) e@name, character(1)), c("WT_CL", "CRCL_CL", "WT_V"))
  expect_equal(scm$covariates$effects[[1]], ScmEffect("WT_CL"))
})

test_that("read_scm() reads an effects array that mixes names and inline tables", {
  p <- local_scm_project()
  scm <- suppressMessages(init_scm(p$model))
  cfg <- readLines(p$config)
  writeLines(
    sub("^effects = \\[\\]", 'effects = ["WT_CL", { name = "CRCL_CL", type = "categorical" }, "WT_V"]', cfg),
    p$config
  )
  scm <- read_scm(p$config)
  expect_equal(
    scm$covariates$effects,
    list(ScmEffect("WT_CL"), ScmEffect("CRCL_CL", type = "categorical"), ScmEffect("WT_V"))
  )
  expect_equal(scm$direction, c("forward", "backward"))
  expect_type(scm$max_retries, "integer")
  expect_equal(scm$num_rounds, 0L)
})

test_that("read_scm() validates its input", {
  expect_error(read_scm(scm_fixture("1001.mod")), "scm.toml")
  expect_error(read_scm("nope/1001scm.toml"), "not found")
  expect_error(read_scm(c("a", "b")), "single path")
})

test_that("ScmEffect() validates an effect as pharos reads it", {
  e <- ScmEffect("WT_V", type = "categorical", initial = 1, fixed = 0, lower = 0, upper = 5)
  expect_true(S7::S7_inherits(e, ScmEffect))
  expect_equal(e@name, "WT_V")
  expect_equal(e@upper, 5)
  expect_null(ScmEffect("WT_CL")@type)

  expect_error(ScmEffect(""), "non-empty string")
  expect_error(ScmEffect(c("A", "B")), "non-empty string")
  expect_error(ScmEffect("A", type = "binary"), '"continuous" or "categorical"')
  expect_error(ScmEffect("A", initial = "x"), "not <character>")
  expect_error(ScmEffect("A", initial = c(1, 2)), "single finite number")
  expect_error(ScmEffect("A", upper = Inf), "single finite number")
  expect_error(ScmEffect("A", lower = 1, upper = 0), "@lower must be below @upper")
  e@lower <- 2
  expect_error(e@upper <- 1, "@lower must be below @upper")
})

test_that("init_scm() takes the effects as names, rows or ScmEffects and validates them", {
  p <- local_scm_project()
  config <- suppressMessages(init_scm(
    p$model,
    effects = list(
      "WT_CL",
      list(name = "CRCL_CL"),
      ScmEffect("WT_V", type = "categorical", initial = 1, fixed = 0, lower = 0, upper = 5)
    )
  ))
  expect_equal(normalizePath(config), normalizePath(p$config))
  expect_equal(
    read_scm(config)$covariates$effects,
    list(
      ScmEffect("WT_CL"),
      ScmEffect("CRCL_CL"),
      ScmEffect("WT_V", type = "categorical", initial = 1, fixed = 0, lower = 0, upper = 5)
    )
  )

  suppressMessages(init_scm(p$model, effects = c("WT_CL", "CRCL_CL"), overwrite = TRUE))
  expect_equal(read_scm(p$config)$covariates$effects, list(ScmEffect("WT_CL"), ScmEffect("CRCL_CL")))
  suppressMessages(init_scm(p$model, effects = ScmEffect("WT_CL"), overwrite = TRUE))
  expect_equal(read_scm(p$config)$covariates$effects, list(ScmEffect("WT_CL")))

  before <- readLines(p$config)
  bad <- function(effects) init_scm(p$model, effects = effects, overwrite = TRUE)
  expect_error(bad(c("WT_CL", "WT_CL")), "duplicate effect")
  expect_error(bad(42), "character vector of names")
  expect_error(bad(list(42)), "name, an ScmEffect, or a list")
  expect_error(bad(list(list(type = "categorical"))), "with a `name`")
  expect_error(bad(list(list(name = "A", bound = 1))), "unknown field")
  expect_equal(readLines(p$config), before)
})

test_that("add_scm_effect() appends validated effects to the config", {
  p <- local_scm_project(effects = "WT_CL")

  expect_message(out <- add_scm_effect(p$config, "CRCL_CL"), "Added CRCL_CL in")
  expect_equal(normalizePath(out), normalizePath(p$config))
  suppressMessages({
    add_scm_effect(p$config, "WT_V", type = "categorical", initial = 1, lower = 0, upper = 5)
    add_scm_effect(p$config, ScmEffect("AGE_CL", fixed = 0))
  })
  scm <- read_scm(p$config)
  expect_equal(attr(scm, "config_source"), "model/scm/1001scm.toml")
  expect_equal(
    scm$covariates$effects,
    list(
      ScmEffect("WT_CL"),
      ScmEffect("CRCL_CL"),
      ScmEffect("WT_V", type = "categorical", initial = 1, lower = 0, upper = 5),
      ScmEffect("AGE_CL", fixed = 0)
    )
  )
  expect_message(add_scm_effect(p$config, c("SEX_CL", "SEX_V")), "Added SEX_CL, SEX_V")
  expect_equal(
    scm_effect_names(read_scm(p$config)$covariates$effects),
    c("WT_CL", "CRCL_CL", "WT_V", "AGE_CL", "SEX_CL", "SEX_V")
  )
  written <- readLines(p$config)
  expect_true("# SCM setup for 1001.mod." %in% written)
  expect_true(any(grepl("^forward_alpha = 0.05 .*# p-value", written)))
  expect_snapshot(writeLines(written[grep("^\\[covariates\\]", written):length(written)]))

  expect_error(add_scm_effect(p$config, "WT_CL"), "already in the config: WT_CL")
  expect_error(add_scm_effect(p$config, c("SEX_CL", "SEX_CL")), "duplicate effect")
  expect_error(add_scm_effect(p$config, c("SEX_CL", "SEX_V"), initial = 1), "one effect")
  expect_error(add_scm_effect(p$config, ScmEffect("SEX_CL"), initial = 1), "not both")
  expect_error(add_scm_effect(p$config, 42), "effect name or an ScmEffect")
  expect_error(add_scm_effect(scm, "SEX_CL"), "not the object")
  expect_error(add_scm_effect(list(), "SEX_CL"), "SCM config")
  expect_equal(readLines(p$config), written)
})

test_that("remove_scm_effect() drops effects by name", {
  p <- local_scm_project(effects = c("WT_CL", "CRCL_CL", "WT_V"))

  expect_message(out <- remove_scm_effect(p$config, "CRCL_CL"), "Removed CRCL_CL in")
  expect_equal(normalizePath(out), normalizePath(p$config))
  expect_equal(read_scm(p$config)$covariates$effects, list(ScmEffect("WT_CL"), ScmEffect("WT_V")))
  suppressMessages(remove_scm_effect(p$config, ScmEffect("WT_V")))
  expect_equal(read_scm(p$config)$covariates$effects, list(ScmEffect("WT_CL")))

  expect_error(remove_scm_effect(p$config, "AGE_CL"), "no effect\\(s\\) named AGE_CL in the config; it has: WT_CL")
  expect_error(remove_scm_effect(p$config, c("WT_CL", "AGE_CL", "SEX_V")), "named AGE_CL, SEX_V")
  expect_error(remove_scm_effect(p$config, character()), "one or more effect names")
  expect_error(remove_scm_effect(p$config, 1), "one or more effect names")
  suppressMessages(remove_scm_effect(p$config, "WT_CL"))
  expect_length(read_scm(p$config)$covariates$effects, 0)
  expect_error(remove_scm_effect(p$config, "WT_CL"), "it has none")
  expect_error(remove_scm_effect(p$scm, "WT_CL"), "not the object")
  expect_error(remove_scm_effect("not an scm", "WT_CL"), "SCM config")
})

test_that("update_scm_effect() replaces the settings given on an effect", {
  p <- local_scm_project(effects = c("WT_CL", "WT_V"))

  expect_message(
    out <- update_scm_effect(p$config, "WT_V", type = "categorical", initial = 1, lower = 0, upper = 5),
    "Updated WT_V in"
  )
  expect_equal(normalizePath(out), normalizePath(p$config))
  effects <- function() read_scm(p$config)$covariates$effects
  expect_equal(effects()[[2]], ScmEffect("WT_V", type = "categorical", initial = 1, lower = 0, upper = 5))
  suppressMessages(update_scm_effect(p$config, "WT_V", initial = 2))
  expect_equal(effects()[[2]], ScmEffect("WT_V", type = "categorical", initial = 2, lower = 0, upper = 5))
  suppressMessages(update_scm_effect(p$config, ScmEffect("WT_V", fixed = 0)))
  expect_equal(effects(), list(ScmEffect("WT_CL"), ScmEffect("WT_V", fixed = 0)))

  expect_error(update_scm_effect(p$config, "AGE_CL"), "no effect named AGE_CL in the config; it has: WT_CL, WT_V")
  expect_error(update_scm_effect(p$config, ScmEffect("AGE_CL")), "no effect named AGE_CL")
  expect_error(update_scm_effect(p$config, ScmEffect("WT_V"), initial = 1), "not both")
  expect_error(update_scm_effect(p$config, c("WT_CL", "WT_V")), "effect name or an ScmEffect")
  expect_error(update_scm_effect(p$config, "WT_CL", lower = 1, upper = 0), "@lower must be below @upper")
  expect_error(update_scm_effect(p$scm, "WT_CL", initial = 1), "not the object")
  expect_error(update_scm_effect("not an scm", "WT_CL"), "SCM config")
})

test_that("update_scm_settings() writes the settings given and keeps the others", {
  p <- local_scm_project(effects = "WT_CL")

  expect_message(
    out <- update_scm_settings(p$config, direction = "forward", forward_alpha = 0.01, max_retries = 5, cov_step = TRUE),
    "Set direction, forward_alpha, max_retries, cov_step in"
  )
  expect_equal(normalizePath(out), normalizePath(p$config))
  scm <- read_scm(p$config)
  expect_equal(scm$direction, "forward")
  expect_equal(scm$forward_alpha, 0.01)
  expect_equal(scm$backward_alpha, 0.001)
  expect_identical(scm$max_retries, 5L)
  expect_true(scm$cov_step)
  expect_equal(scm$covariates$effects, list(ScmEffect("WT_CL")))
  written <- readLines(p$config)
  expect_true('direction = ["forward"]' %in% written)
  expect_true("max_retries = 5" %in% written)
  expect_true(any(grepl("^backward_alpha = 0.001 .*# p-value", written)))
  plan <- check_scm(scm)
  expect_equal(unlist(plan$options$direction), "forward")
  expect_equal(plan$options$max_retries, 5)

  suppressMessages(update_scm_settings(p$config, direction = c("forward", "backward"), num_rounds = 2))
  scm <- read_scm(p$config)
  expect_equal(scm$direction, c("forward", "backward"))
  expect_identical(scm$num_rounds, 2L)
  expect_match(attr(check_scm(scm), "text"), "pause after 2")

  expect_error(update_scm_settings(p$config), "at least one setting")
  expect_error(update_scm_settings(p$config, direction = "sideways"), '`direction` must be "forward", "backward", or both')
  expect_error(update_scm_settings(p$config, direction = c("forward", "forward")), "`direction` must be")
  expect_error(update_scm_settings(p$config, forward_alpha = "0.05"), "`forward_alpha` must be a single number")
  expect_error(update_scm_settings(p$config, max_retries = 1.5), "`max_retries` must be a single whole number")
  expect_error(update_scm_settings(p$config, cov_step = NA), "`cov_step` must be TRUE or FALSE")
  expect_error(update_scm_settings(scm, cov_step = TRUE), "not the object")
  expect_error(update_scm_settings(p$model, cov_step = TRUE), "SCM config")
})

test_that("init_scm() writes the starter config and returns its path", {
  p <- local_scm_project()
  expect_snapshot(config <- init_scm(p$model))
  expect_equal(normalizePath(config), normalizePath(p$config))
  scm <- read_scm(config)
  expect_s3_class(scm, "hyperion_nonmem_scm")
  expect_equal(attr(scm, "config_source"), "model/scm/1001scm.toml")
  expect_equal(scm$model, "model/1001.mod")
  expect_length(scm$covariates$effects, 0)
  expect_true(file.exists(p$config))
  expect_true(dir.exists(p$out_dir))
  expect_true(file.exists(file.path(p$out_dir, ".gitignore")))

  expect_no_error(suppressMessages(init_scm(p$model, overwrite = TRUE)))
  expect_no_error(suppressMessages(init_scm(read_model(p$model), overwrite = TRUE)))
  expect_error(init_scm(file.path(p$root, "model", "nope.mod")), "File not found")
})

test_that("init_scm() reports a config that already exists and returns its path", {
  p <- local_scm_project(effects = c("WT_CL", "CRCL_CL"))
  before <- readLines(p$config)
  expect_snapshot(config <- init_scm(p$model))
  expect_equal(normalizePath(config), normalizePath(p$config))
  expect_equal(readLines(p$config), before)

  # The same names in another order are the config's; others are a warning.
  expect_no_warning(suppressMessages(init_scm(p$model, effects = c("CRCL_CL", "WT_CL"))))
  expect_warning(
    suppressMessages(init_scm(p$model, effects = c("WT_CL", "WT_V"))),
    "`effects` differ from the config's"
  )
  expect_equal(readLines(p$config), before)

  suppressMessages(init_scm(p$model, effects = "WT_V", overwrite = TRUE))
  expect_equal(read_scm(p$config)$covariates$effects, list(ScmEffect("WT_V")))
})

test_that("check_scm() returns the plan with its warnings", {
  scm <- read_scm(scm_fixture("scm", "1001scm.toml"))
  expect_no_warning(plan <- check_scm(scm))
  expect_s3_class(plan, "hyperion_nonmem_scm_plan")
  expect_equal(plan$model, "extdata/models/scm/1001.mod")
  expect_equal(plan$out_dir, "extdata/models/scm/scm/1001")
  expect_equal(
    vapply(plan$candidates, function(c) c$name, character(1)),
    c("WT_CL", "CRCL_CL", "WT_V")
  )
  expect_equal(vapply(plan$candidates, function(c) c$theta, numeric(1)), c(4, 5, 6))
  expect_equal(plan$options$forward_alpha, 0.05)
  expect_match(attr(plan, "warnings"), "cov_step is off")
  expect_true(attr(plan, "resumable"))
  expect_type(attr(plan, "out_dir"), "character")
  expect_equal(normalizePath(attr(plan, "project_root")), normalizePath(system.file(package = "hyperion")))
  expect_match(attr(plan, "text"), "max models : 13")
  expect_match(
    attr(check_scm(scm_fixture("scm", "1001scm.toml"), num_rounds = 1), "text"),
    "num rounds : pause after 1 \\(resumable\\)"
  )
})

test_that("check_scm() reports a started process", {
  comp <- read_scm(scm_complete_fixture("scm", "1001scm.toml"))
  plan <- check_scm(comp)
  expect_true(attr(plan, "resumable"))
  expect_match(attr(plan, "text"), "changes      : none")
  plan <- check_scm(comp, overwrite = TRUE)
  expect_match(attr(plan, "text"), "overwrite = TRUE: submit discards the SCM process")
})

test_that("check_scm() errors as pharos does, with the dialect hint", {
  p <- local_scm_project()
  config <- suppressMessages(init_scm(p$model))
  expect_error(check_scm(config), "effects must name at least one theta")
  expect_error(check_scm(read_scm(config)), "effects must name at least one theta")

  suppressMessages(add_scm_effect(config, "AGE_CL"))
  err <- expect_error(check_scm(config), "no theta named AGE_CL")
  expect_match(conditionMessage(err), "named by a comment: CRCL_CL, TVCL, TVKA, TVV, WT_CL, WT_V")
  expect_match(conditionMessage(err), "comments are parsed as type2")

  expect_error(check_scm(file.path(p$root, "model", "scm", "nopescm.toml")), "no SCM config at")
  expect_error(check_scm(p$model), "named <stem>scm.toml")

  cfg <- readLines(p$config)
  writeLines(sub("^direction = .*", 'direction = ["foward"]', cfg), p$config)
  expect_error(check_scm(p$config), "unknown variant")
})

test_that("check_scm() refuses a pharos.toml that is not the model's project", {
  p <- local_scm_project(effects = "WT_CL")
  other <- withr::local_tempdir()
  file.copy(system.file("pharos.toml", package = "hyperion"), other)
  withr::local_options(hyperion.config_dir = other)
  expect_error(check_scm(p$config), "is not the project the model belongs to")
})

test_that("get_scm_status() reads a planned process", {
  scm <- read_scm(scm_fixture("scm", "1001scm.toml"))
  status <- get_scm_status(scm)
  expect_s3_class(status, c("hyperion_nonmem_scm_status", "hyperion_nonmem_scm_record"))
  expect_equal(status$status, "planned")
  expect_null(status$updated)
  expect_match(status$message, "has not started")
  expect_equal(unlist(status$candidates), c("WT_CL", "CRCL_CL", "WT_V"))
  expect_length(status$rounds, 0)
  expect_null(status$driver)
  expect_equal(status$pharos_version, utils::packageDescription("hyperion", fields = "Config/PharosVersion"))
  expect_snapshot(print(status))

  by_config <- get_scm_status(scm_fixture("scm", "1001scm.toml"))
  expect_equal(attr(by_config, "text"), attr(status, "text"))
  expect_error(get_scm_status(scm_fixture("1001.mod")), "named <stem>scm.toml")
  expect_error(get_scm_status(file.path(scm_fixture("scm"), "1001")), "named <stem>scm.toml")
})

test_that("summary() reads a planned process", {
  scm <- read_scm(scm_fixture("scm", "1001scm.toml"))
  s <- summary(scm)
  expect_s3_class(s, c("hyperion_nonmem_scm_summary", "hyperion_nonmem_scm_record"))
  expect_equal(s$status, "planned")
  expect_match(attr(s, "text"), "path         : none")
  expect_error(summary(scm, round = "9"), "has not started; no rounds to summarize")
  expect_error(summary(scm, candidate = "AGE_CL"), "no candidate named AGE_CL")
})

test_that("stop_scm() refuses a process with no driver", {
  expect_error(stop_scm(scm_fixture("scm", "1001scm.toml")), "no driver has been started")
})

test_that("summary() and get_scm_status() read a completed process", {
  comp <- read_scm(scm_complete_fixture("scm", "1001scm.toml"))
  s <- summary(comp)
  expect_equal(s$status, "completed")
  expect_equal(s$totals$rounds_complete, 5)
  expect_equal(s$totals$models_fitted, 11)
  expect_equal(s$totals$retries, 1)
  expect_equal(s$final_model, "final/1001_scm_final.mod")
  expect_equal(s$final_ofv, 979.5)
  expect_equal(unlist(s$retained), "WT_CL")
  expect_equal(s$forward_final$model, "forward_final/1001_scm_forward_final.mod")
  expect_equal(
    vapply(s$rounds, function(r) r$round, character(1)),
    c("reference", "forward_round1", "forward_round2", "forward_round3", "backward_round1", "backward_round2")
  )
  wt_v <- s$rounds[[3]]$candidates[[2]]
  expect_equal(wt_v$candidate, "WT_V")
  expect_length(wt_v$attempts, 2)
  expect_equal(wt_v$p_value, 0.2206714, tolerance = 1e-6)

  json <- jsonlite::fromJSON(scm_complete_fixture("scm", "1001", "scm_summary.json"), simplifyVector = FALSE)
  drop <- c("generated", "models_running", "models_queued")
  expect_equal(unclass(s)[setdiff(names(json), drop)], json[setdiff(names(json), drop)])

  status <- get_scm_status(comp)
  expect_equal(status$status, "completed")
})

test_that("summary() renders the long, timing and files views", {
  comp <- read_scm(scm_complete_fixture("scm", "1001scm.toml"))
  txt <- attr(summary(comp, long = TRUE, timing = TRUE, files = TRUE), "text")
  expect_match(txt, "retained before this round: WT_CL")
  expect_match(txt, "3 fits · wall 1m 30s")
  expect_match(txt, "lst      base/1001_base/1001_base.lst")
  expect_snapshot(print(summary(comp, round = "forward_round2", candidate = "WT_V", long = TRUE)))
  r2 <- summary(comp, round = "2")
  expect_match(attr(r2, "text"), "forward_round2")
  expect_false(grepl("backward_round1", attr(r2, "text")))
  expect_error(summary(comp, round = "9"), "no round matching '9'")
  expect_error(summary(comp, candidate = "AGE_CL"), "no candidate named AGE_CL")
})

test_that("get_scm_model() reads a fitted model by stage, round and candidate", {
  comp <- read_scm(scm_complete_fixture("scm", "1001scm.toml"))
  stem <- function(m) attr(m, "filename")

  final <- get_scm_model(comp, "final")
  expect_s3_class(final, "hyperion_nonmem_model")
  expect_equal(stem(final), "1001_scm_final")
  expect_no_error(summary(final))

  expect_equal(stem(get_scm_model(comp, "forward_final")), "1001_scm_forward_final")
  expect_equal(stem(get_scm_model(comp, "reference")), "1001_base")
  expect_equal(stem(get_scm_model(comp, "reference", candidate = "ignored")), "1001_base")
  expect_equal(stem(get_scm_model(comp, "forward_round2")), "1001_crcl_cl")
  expect_equal(stem(get_scm_model(comp, "backward_round1")), "1001_crcl_cl")
  # The attempt that counted: WT_V was retried in forward round 2.
  expect_equal(stem(get_scm_model(comp, "forward_round2", candidate = "WT_V")), "1001_wt_v_try2")
  expect_equal(stem(get_scm_model(comp, 2, candidate = "wt_v")), "1001_wt_v_try2")
  expect_equal(stem(get_scm_model(comp, "round 2", candidate = "WT_V")), "1001_wt_v_try2")
  expect_equal(stem(get_scm_model(scm_complete_fixture("scm", "1001scm.toml"), "FINAL")), "1001_scm_final")

  expect_error(get_scm_model(comp, "forward_round3"), "forward_round3 has no winner: no candidate significant")
  expect_error(
    get_scm_model(comp, "forward_round2", candidate = "AGE_CL"),
    "no candidate named AGE_CL in forward_round2; it has: CRCL_CL, WT_V"
  )
  expect_error(get_scm_model(comp, "9"), "no round matching '9'; rounds so far: reference, forward_round1")
  expect_error(get_scm_model(comp, c("final", "reference")), "`round` must be")
  expect_error(get_scm_model(comp, "final", candidate = 1), "`candidate` must be")
  expect_error(get_scm_model(read_scm(scm_fixture("scm", "1001scm.toml")), "final"), "has not started")
  expect_error(get_scm_model(scm_fixture("1001.mod"), "final"), "named <stem>scm.toml")
})

test_that("submit_scm_to_slurm() checks its arguments and the pharos CLI first", {
  p <- local_scm_project(effects = "WT_CL")
  expect_error(
    submit_scm_to_slurm(p$scm, shared_node = TRUE, driver_partition = "cpu"),
    "cannot be used with `shared_node = TRUE`"
  )
  local_mocked_bindings(detect_pharos = function() list(path = NA_character_, version = NA_character_))
  expect_error(submit_scm_to_slurm(p$scm), "pharos CLI not found on PATH")
  local_mocked_bindings(detect_pharos = function() list(path = "/usr/bin/pharos", version = "0.0.1"))
  expect_error(submit_scm_to_slurm(p$scm), "version mismatch: installed 0.0.1")
})

test_that("submit_scm_to_slurm() stops before writing when slurm or pharos is missing", {
  p <- local_scm_project(effects = "WT_CL")
  local_mocked_bindings(detect_pharos = function() list(path = "/usr/bin/pharos", version = "0.0.1"))
  # An empty PATH: the submit must stop at its first check.
  withr::local_envvar(
    PATH = withr::local_tempdir(),
    SLURM_NTASKS = "4",
    SLURM_MEM_PER_NODE = "1000"
  )
  expect_error(
    submit_scm_to_slurm(p$scm, allow_version_mismatch = TRUE),
    "Failed to locate pharos executable"
  )
  expect_equal(Sys.getenv("SLURM_NTASKS"), "4")
  expect_equal(Sys.getenv("SLURM_MEM_PER_NODE"), "1000")
  expect_false(file.exists(file.path(p$out_dir, "scm_driver.json")))
})

test_that("without_slurm_env() unsets the inherited slurm variables and restores them", {
  withr::local_envvar(SLURM_NTASKS = "4", SLURM_CPUS_PER_TASK = NA)
  expect_equal(without_slurm_env(Sys.getenv("SLURM_NTASKS", unset = NA)), NA_character_)
  expect_equal(Sys.getenv("SLURM_NTASKS"), "4")
  expect_equal(Sys.getenv("SLURM_CPUS_PER_TASK", unset = NA), NA_character_)
})

test_that("get_scm_status() warns from R about an unreadable driver record", {
  p <- local_scm_project(effects = "WT_CL")
  writeLines('{ "mode": "slurm", "job_id": ', file.path(p$out_dir, "scm_driver.json"))
  expect_warning(status <- get_scm_status(p$config), "scm_driver.json is unreadable")
  expect_equal(status$status, "planned")
  expect_null(status$driver)
  # The warning is a condition R can handle, not a print from Rust.
  caught <- NULL
  withCallingHandlers(
    get_scm_status(p$scm),
    warning = function(w) {
      caught <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_match(caught, "the driver is treated as never started")
  expect_no_warning(suppressWarnings(get_scm_status(p$config)))
})

test_that("get_scm_status() says so while a submitted driver has not written its state yet", {
  p <- local_scm_project(effects = "WT_CL")
  cat('\n[nonmem.scm]\nout_dir = "scm_runs/{{name}}_sel"\n', file = file.path(p$root, "pharos.toml"), append = TRUE)
  elsewhere <- file.path(p$root, "model", "scm_runs", "1001_sel")
  dir.create(elsewhere, recursive = TRUE)
  writeLines(
    '{ "mode": "slurm", "job_id": 7, "log": "driver.out", "started": "2026-01-01T00:00:00+00:00" }',
    file.path(elsewhere, "scm_driver.json")
  )
  err <- expect_error(get_scm_status(p$config), "no SCM config")
  expect_match(conditionMessage(err), "has a driver that has not written its state yet")
})

test_that("get_scm_lineage() builds the tree of the process's fits from its record", {
  comp <- read_scm(scm_complete_fixture("scm", "1001scm.toml"))
  tree <- get_scm_lineage(comp)
  expect_s3_class(tree, "hyperion_nonmem_tree")
  expect_identical(tree, get_scm_lineage(scm_complete_fixture("scm", "1001scm.toml")))

  out_dir <- "extdata/models/scm-complete/scm/1001"
  name <- function(n) sub(paste0(out_dir, "/"), "", n$name, fixed = TRUE)
  parent <- function(n) {
    if (length(n$model$based_on) == 0) return(NA_character_)
    sub(paste0(out_dir, "/"), "", n$model$based_on[[1]], fixed = TRUE)
  }
  tags <- function(n) paste(unlist(n$model$tags), collapse = ",")
  nodes <- data.frame(
    name = vapply(tree$nodes, name, ""),
    parent = vapply(tree$nodes, parent, ""),
    description = vapply(tree$nodes, function(n) n$model$description, ""),
    tags = vapply(tree$nodes, tags, ""),
    stringsAsFactors = FALSE
  )
  expect_equal(nodes$name, c(
    "base/1001_base.mod",
    "forward_round1/1001_wt_cl.mod", "forward_round1/1001_crcl_cl.mod", "forward_round1/1001_wt_v.mod",
    "forward_round2/1001_crcl_cl.mod", "forward_round2/1001_wt_v_try2.mod",
    "forward_round3/1001_wt_v.mod",
    "forward_final/1001_scm_forward_final.mod",
    "backward_round1/1001_wt_cl.mod", "backward_round1/1001_crcl_cl.mod",
    "backward_round2/1001_wt_cl.mod",
    "final/1001_scm_final.mod"
  ))
  # Every fit is based on the model its round tested against; the working
  # full model and the reduced covariate model on the reference fit.
  expect_equal(nodes$parent, c(
    NA, rep("base/1001_base.mod", 3),
    rep("forward_round1/1001_wt_cl.mod", 2),
    "forward_round2/1001_crcl_cl.mod", "base/1001_base.mod",
    rep("forward_round2/1001_crcl_cl.mod", 2),
    "backward_round1/1001_crcl_cl.mod",
    "base/1001_base.mod"
  ))
  # The action, then the covariates the model has.
  expect_equal(nodes$description, c(
    "base model; covariates: none",
    "add WT_CL; covariates: WT_CL", "add CRCL_CL; covariates: CRCL_CL", "add WT_V; covariates: WT_V",
    "add CRCL_CL; covariates: WT_CL, CRCL_CL", "add WT_V; covariates: WT_CL, WT_V",
    "add WT_V; covariates: WT_CL, CRCL_CL, WT_V",
    "working full model; covariates: WT_CL, CRCL_CL",
    "drop WT_CL; covariates: CRCL_CL", "drop CRCL_CL; covariates: WT_CL",
    "drop WT_CL; covariates: none",
    "reduced covariate model; covariates: WT_CL"
  ))
  expect_equal(nodes$tags, c(
    "scm,reference",
    "scm,forward_round1,selected", "scm,forward_round1", "scm,forward_round1",
    "scm,forward_round2,selected", "scm,forward_round2,retry",
    "scm,forward_round3",
    "scm,forward_final",
    "scm,backward_round1", "scm,backward_round1,selected",
    "scm,backward_round2",
    "scm,final"
  ))
  expect_true(all(file.exists(file.path(attr(summary(comp), "project_root"), vapply(tree$nodes, `[[`, "", "name")))))
  expect_snapshot(print(tree))

  expect_error(get_scm_lineage(read_scm(scm_fixture("scm", "1001scm.toml"))), "has not started")
  expect_error(get_scm_lineage(scm_fixture("1001.mod")), "named <stem>scm.toml")
})
