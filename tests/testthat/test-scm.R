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
  scm <- suppressMessages(init_scm(
    p$model,
    effects = list(
      "WT_CL",
      list(name = "CRCL_CL"),
      ScmEffect("WT_V", type = "categorical", initial = 1, fixed = 0, lower = 0, upper = 5)
    )
  ))
  expect_equal(
    scm$covariates$effects,
    list(
      ScmEffect("WT_CL"),
      ScmEffect("CRCL_CL"),
      ScmEffect("WT_V", type = "categorical", initial = 1, fixed = 0, lower = 0, upper = 5)
    )
  )
  expect_equal(read_scm(p$config)$covariates$effects, scm$covariates$effects)

  scm <- suppressMessages(init_scm(p$model, effects = c("WT_CL", "CRCL_CL"), overwrite = TRUE))
  expect_equal(scm$covariates$effects, list(ScmEffect("WT_CL"), ScmEffect("CRCL_CL")))
  scm <- suppressMessages(init_scm(p$model, effects = ScmEffect("WT_CL"), overwrite = TRUE))
  expect_equal(scm$covariates$effects, list(ScmEffect("WT_CL")))

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

  scm <- add_scm_effect(p$scm, "CRCL_CL")
  scm <- add_scm_effect(p$config, "WT_V", type = "categorical", initial = 1, lower = 0, upper = 5)
  scm <- add_scm_effect(p$scm, ScmEffect("AGE_CL", fixed = 0))
  expect_s3_class(scm, "hyperion_nonmem_scm")
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
  expect_equal(read_scm(p$config)$covariates$effects, scm$covariates$effects)
  more <- add_scm_effect(scm, c("SEX_CL", "SEX_V"))
  expect_equal(scm_effect_names(more$covariates$effects), c("WT_CL", "CRCL_CL", "WT_V", "AGE_CL", "SEX_CL", "SEX_V"))
  written <- readLines(p$config)
  expect_true("# SCM setup for 1001.mod." %in% written)
  expect_true(any(grepl("^forward_alpha = 0.05 .*# p-value", written)))
  expect_snapshot(writeLines(written[grep("^\\[covariates\\]", written):length(written)]))

  expect_error(add_scm_effect(scm, "WT_CL"), "already in the config: WT_CL")
  expect_error(add_scm_effect(scm, c("SEX_CL", "SEX_CL")), "duplicate effect")
  expect_error(add_scm_effect(scm, c("SEX_CL", "SEX_V"), initial = 1), "one effect")
  expect_error(add_scm_effect(scm, ScmEffect("SEX_CL"), initial = 1), "not both")
  expect_error(add_scm_effect(scm, 42), "effect name or an ScmEffect")
  expect_error(add_scm_effect(list(), "SEX_CL"), "SCM config")
  expect_equal(readLines(p$config), written)
})

test_that("remove_scm_effect() drops effects by name", {
  p <- local_scm_project(effects = c("WT_CL", "CRCL_CL", "WT_V"))

  scm <- remove_scm_effect(p$scm, "CRCL_CL")
  expect_equal(scm$covariates$effects, list(ScmEffect("WT_CL"), ScmEffect("WT_V")))
  expect_equal(read_scm(p$config)$covariates$effects, scm$covariates$effects)
  expect_equal(
    remove_scm_effect(p$config, ScmEffect("WT_V"))$covariates$effects,
    list(ScmEffect("WT_CL"))
  )

  expect_error(remove_scm_effect(scm, "AGE_CL"), "no effect\\(s\\) named AGE_CL in the config; it has: WT_CL")
  expect_error(remove_scm_effect(scm, c("WT_CL", "AGE_CL", "SEX_V")), "named AGE_CL, SEX_V")
  expect_error(remove_scm_effect(scm, character()), "one or more effect names")
  expect_error(remove_scm_effect(scm, 1), "one or more effect names")
  expect_length(remove_scm_effect(scm, "WT_CL")$covariates$effects, 0)
  expect_error(remove_scm_effect(scm, "WT_CL"), "it has none")
  expect_error(remove_scm_effect("not an scm", "WT_CL"), "SCM config")
})

test_that("update_scm_effect() replaces the settings given on an effect", {
  p <- local_scm_project(effects = c("WT_CL", "WT_V"))

  scm <- update_scm_effect(p$scm, "WT_V", type = "categorical", initial = 1, lower = 0, upper = 5)
  expect_equal(scm$covariates$effects[[2]], ScmEffect("WT_V", type = "categorical", initial = 1, lower = 0, upper = 5))
  scm <- update_scm_effect(p$config, "WT_V", initial = 2)
  expect_equal(scm$covariates$effects[[2]], ScmEffect("WT_V", type = "categorical", initial = 2, lower = 0, upper = 5))
  scm <- update_scm_effect(scm, ScmEffect("WT_V", fixed = 0))
  expect_equal(scm$covariates$effects, list(ScmEffect("WT_CL"), ScmEffect("WT_V", fixed = 0)))
  expect_equal(read_scm(p$config)$covariates$effects, scm$covariates$effects)

  expect_error(update_scm_effect(scm, "AGE_CL"), "no effect named AGE_CL in the config; it has: WT_CL, WT_V")
  expect_error(update_scm_effect(scm, ScmEffect("AGE_CL")), "no effect named AGE_CL")
  expect_error(update_scm_effect(scm, ScmEffect("WT_V"), initial = 1), "not both")
  expect_error(update_scm_effect(scm, c("WT_CL", "WT_V")), "effect name or an ScmEffect")
  expect_error(update_scm_effect(scm, "WT_CL", lower = 1, upper = 0), "@lower must be below @upper")
  expect_error(update_scm_effect("not an scm", "WT_CL"), "SCM config")
})

test_that("update_scm_settings() writes the settings given and keeps the others", {
  p <- local_scm_project(effects = "WT_CL")

  scm <- update_scm_settings(p$scm, direction = "forward", forward_alpha = 0.01, max_retries = 5, cov_step = TRUE)
  expect_equal(scm$direction, "forward")
  expect_equal(scm$forward_alpha, 0.01)
  expect_equal(scm$backward_alpha, 0.001)
  expect_identical(scm$max_retries, 5L)
  expect_true(scm$cov_step)
  expect_equal(scm$covariates$effects, list(ScmEffect("WT_CL")))
  expect_equal(read_scm(p$config), scm)
  written <- readLines(p$config)
  expect_true('direction = ["forward"]' %in% written)
  expect_true("max_retries = 5" %in% written)
  expect_true(any(grepl("^backward_alpha = 0.001 .*# p-value", written)))
  plan <- check_scm(scm)
  expect_equal(unlist(plan$options$direction), "forward")
  expect_equal(plan$options$max_retries, 5)

  scm <- update_scm_settings(p$config, direction = c("forward", "backward"), num_rounds = 2)
  expect_equal(scm$direction, c("forward", "backward"))
  expect_identical(scm$num_rounds, 2L)
  expect_match(attr(check_scm(scm), "text"), "pause after 2")

  expect_error(update_scm_settings(scm), "at least one setting")
  expect_error(update_scm_settings(scm, direction = "sideways"), '`direction` must be "forward", "backward", or both')
  expect_error(update_scm_settings(scm, direction = c("forward", "forward")), "`direction` must be")
  expect_error(update_scm_settings(scm, forward_alpha = "0.05"), "`forward_alpha` must be a single number")
  expect_error(update_scm_settings(scm, max_retries = 1.5), "`max_retries` must be a single whole number")
  expect_error(update_scm_settings(scm, cov_step = NA), "`cov_step` must be TRUE or FALSE")
  expect_error(update_scm_settings(p$model, cov_step = TRUE), "SCM config")
})

test_that("init_scm() writes the starter config and returns the object", {
  p <- local_scm_project()
  expect_snapshot(scm <- init_scm(p$model))
  expect_s3_class(scm, "hyperion_nonmem_scm")
  expect_equal(attr(scm, "config_source"), "model/scm/1001scm.toml")
  expect_equal(scm$model, "model/1001.mod")
  expect_length(scm$covariates$effects, 0)
  expect_true(file.exists(p$config))
  expect_true(dir.exists(p$out_dir))
  expect_true(file.exists(file.path(p$out_dir, ".gitignore")))

  expect_error(suppressMessages(init_scm(p$model)), "already exists")
  expect_no_error(suppressMessages(init_scm(p$model, overwrite = TRUE)))
  expect_no_error(suppressMessages(init_scm(read_model(p$model), overwrite = TRUE)))
  expect_error(init_scm(file.path(p$root, "model", "nope.mod")), "File not found")
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
  scm <- suppressMessages(init_scm(p$model))
  expect_error(check_scm(scm), "effects must name at least one theta")
  expect_error(check_scm(p$config), "effects must name at least one theta")

  scm <- add_scm_effect(scm, "AGE_CL")
  err <- expect_error(check_scm(scm), "no theta named AGE_CL")
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
