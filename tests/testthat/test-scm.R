# ---- read_scm ----------------------------------------------------------------

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
  # and writes it back the same
  scm$forward_alpha <- 0.01
  write_scm(scm)
  expect_equal(read_scm(p$config)$covariates$effects, scm$covariates$effects)
})

test_that("read_scm() validates its input", {
  expect_error(read_scm(scm_fixture("1001.mod")), "scm.toml")
  expect_error(read_scm("nope/1001scm.toml"), "not found")
  expect_error(read_scm(c("a", "b")), "single path")
})

# ---- effects -------------------------------------------------------------------

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
  expect_error(bad(""), "non-empty string")
  expect_error(bad(42), "character vector of names")
  expect_error(bad(list(42)), "name, an ScmEffect, or a list")
  expect_error(bad(list(list(type = "categorical"))), "with a `name`")
  expect_error(bad(list(list(name = "A", type = "binary"))), "categorical")
  expect_error(bad(list(list(name = "A", initial = "x"))), "not <character>")
  expect_error(bad(list(list(name = "A", lower = 1, upper = 0))), "below @upper")
  expect_error(bad(list(list(name = "A", bound = 1))), "unknown field")
  # nothing is written when the effects are rejected
  expect_equal(readLines(p$config), before)
})

test_that("add_scm_effect() appends validated effects to the object", {
  p <- local_scm_project(effects = "WT_CL")
  scm <- p$scm

  scm <- add_scm_effect(scm, "CRCL_CL")
  scm <- add_scm_effect(scm, "WT_V", type = "categorical", initial = 1, lower = 0, upper = 5)
  scm <- add_scm_effect(scm, ScmEffect("AGE_CL", fixed = 0))
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
  # several names at once
  more <- add_scm_effect(scm, c("SEX_CL", "SEX_V"))
  expect_equal(scm_effect_names(more$covariates$effects), c("WT_CL", "CRCL_CL", "WT_V", "AGE_CL", "SEX_CL", "SEX_V"))

  # in memory only until written
  expect_equal(scm_effect_names(read_scm(p$config)$covariates$effects), "WT_CL")
  expect_error(check_scm(scm), "changes that are not written")
  scm <- write_scm(scm)
  expect_equal(read_scm(p$config)$covariates$effects, scm$covariates$effects)
  expect_no_error(check_scm_written(scm))

  expect_error(add_scm_effect(scm, "WT_CL"), "already in the config: WT_CL")
  expect_error(add_scm_effect(scm, c("SEX_CL", "SEX_CL")), "duplicate effect")
  expect_error(add_scm_effect(scm, c("SEX_CL", "SEX_V"), initial = 1), "one effect")
  expect_error(add_scm_effect(scm, ScmEffect("SEX_CL"), initial = 1), "not both")
  expect_error(add_scm_effect(scm, 42), "effect name or an ScmEffect")
  expect_error(add_scm_effect(scm, "SEX_CL", type = "binary"), "categorical")
  expect_error(add_scm_effect(scm, "SEX_CL", lower = 1, upper = 0), "below @upper")
  expect_error(add_scm_effect(list(), "SEX_CL"), "hyperion_nonmem_scm object")

  # effects assigned as rows by hand are still accepted
  scm$covariates$effects <- list("WT_CL", list(name = "CRCL_CL", type = "categorical"))
  scm <- add_scm_effect(scm, "WT_V")
  expect_equal(
    scm$covariates$effects,
    list(ScmEffect("WT_CL"), ScmEffect("CRCL_CL", type = "categorical"), ScmEffect("WT_V"))
  )
})

test_that("remove_scm_effect() drops effects by name", {
  p <- local_scm_project(effects = c("WT_CL", "CRCL_CL", "WT_V"))
  scm <- p$scm

  scm <- remove_scm_effect(scm, "CRCL_CL")
  expect_equal(scm$covariates$effects, list(ScmEffect("WT_CL"), ScmEffect("WT_V")))
  expect_equal(scm_effect_names(read_scm(p$config)$covariates$effects), c("WT_CL", "CRCL_CL", "WT_V"))
  scm <- write_scm(scm)
  expect_equal(read_scm(p$config)$covariates$effects, scm$covariates$effects)

  expect_equal(
    remove_scm_effect(scm, ScmEffect("WT_V"))$covariates$effects,
    list(ScmEffect("WT_CL"))
  )
  expect_length(remove_scm_effect(scm, c("WT_CL", "WT_V"))$covariates$effects, 0)

  expect_error(remove_scm_effect(scm, "AGE_CL"), "no effect\\(s\\) named AGE_CL in the config; it has: WT_CL, WT_V")
  expect_error(remove_scm_effect(scm, c("WT_CL", "AGE_CL", "SEX_V")), "named AGE_CL, SEX_V")
  expect_error(remove_scm_effect(scm, character()), "one or more effect names")
  expect_error(remove_scm_effect(scm, 1), "one or more effect names")
  empty <- remove_scm_effect(scm, c("WT_CL", "WT_V"))
  expect_error(remove_scm_effect(empty, "WT_CL"), "it has none")
  expect_error(remove_scm_effect("not an scm", "WT_CL"), "hyperion_nonmem_scm object")
})

# ---- init_scm / write_scm ------------------------------------------------------

test_that("init_scm() writes the starter config and returns the object", {
  p <- local_scm_project()
  scm <- suppressMessages(init_scm(p$model))
  expect_s3_class(scm, "hyperion_nonmem_scm")
  expect_equal(attr(scm, "config_source"), "model/scm/1001scm.toml")
  expect_equal(scm$model, "model/1001.mod")
  expect_length(scm$covariates$effects, 0)
  expect_true(file.exists(p$config))
  expect_true(dir.exists(p$out_dir))
  expect_true(file.exists(file.path(p$out_dir, ".gitignore")))
  expect_snapshot(writeLines(readLines(p$config)))
  expect_snapshot(writeLines(readLines(file.path(p$out_dir, ".gitignore"))))

  expect_error(suppressMessages(init_scm(p$model)), "already exists")
  expect_no_error(suppressMessages(init_scm(p$model, overwrite = TRUE)))
  expect_no_error(suppressMessages(init_scm(read_model(p$model), overwrite = TRUE)))
  expect_error(init_scm(file.path(p$root, "model", "nope.mod")), "File not found")
})

test_that("init_scm() prints where it wrote", {
  p <- local_scm_project()
  expect_snapshot(scm <- init_scm(p$model))
})

test_that("write_scm() writes effects as rows and keeps untouched comments", {
  p <- local_scm_project()
  scm <- suppressMessages(init_scm(
    p$model,
    effects = list(
      "WT_CL",
      "CRCL_CL",
      list(name = "WT_V", type = "categorical", initial = 1, lower = 0, upper = 5)
    )
  ))
  written <- readLines(p$config)
  expect_true("# SCM setup for 1001.mod." %in% written)
  expect_true(any(grepl("^forward_alpha = 0.05 .*# p-value", written)))
  expect_snapshot(writeLines(written[grep("^\\[covariates\\]", written):length(written)]))

  back <- read_scm(p$config)
  expect_equal(back$covariates$effects, scm$covariates$effects)

  # an option edited by assignment persists; an unchanged file keeps its text
  scm$forward_alpha <- 0.01
  write_scm(scm)
  expect_equal(read_scm(p$config)$forward_alpha, 0.01)
  before <- readLines(p$config)
  write_scm(read_scm(p$config))
  expect_equal(readLines(p$config), before)

  # effects edited by assignment are validated on write
  scm$covariates$effects <- list(list(name = "WT_CL"), list(name = "WT_CL"))
  expect_error(write_scm(scm), "duplicate effect")
  scm$covariates$effects <- list("WT_CL", "AGE_CL")
  write_scm(scm)
  expect_equal(read_scm(p$config)$covariates$effects, list(ScmEffect("WT_CL"), ScmEffect("AGE_CL")))

  # to another path
  other <- file.path(p$root, "model", "scm", "otherscm.toml")
  written_obj <- write_scm(scm, other)
  expect_equal(attr(written_obj, "config_source"), "model/scm/otherscm.toml")
  expect_equal(read_scm(other)$forward_alpha, 0.01)
  expect_error(write_scm(scm, file.path(p$root, "x.toml")), "scm.toml")
})

# ---- check_scm -----------------------------------------------------------------

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
  expect_equal(attr(plan, "changes"), list())
  expect_type(attr(plan, "out_dir"), "character")
  expect_equal(normalizePath(attr(plan, "project_root")), normalizePath(system.file(package = "hyperion")))
  expect_snapshot(print(plan))
  expect_snapshot(print(check_scm(scm_fixture("scm", "1001scm.toml"), num_rounds = 1)))
})

test_that("check_scm() reports a started process", {
  comp <- read_scm(scm_complete_fixture("scm", "1001scm.toml"))
  plan <- check_scm(comp)
  expect_true(attr(plan, "resumable"))
  expect_match(attr(plan, "text"), "changes      : none")
  expect_snapshot(cat(scrub_scm(attr(plan, "text"))))
  plan <- check_scm(comp, overwrite = TRUE)
  expect_match(attr(plan, "text"), "overwrite = TRUE: submit discards the SCM process")
})

test_that("check_scm() errors as pharos does, with the dialect hint", {
  p <- local_scm_project()
  scm <- suppressMessages(init_scm(p$model))
  expect_error(check_scm(scm), "effects must name at least one theta")
  expect_error(check_scm(p$config), "effects must name at least one theta")

  scm$covariates$effects <- list(list(name = "AGE_CL"))
  expect_error(check_scm(scm), "changes that are not written")
  scm <- write_scm(scm)
  err <- expect_error(check_scm(scm), "no theta named AGE_CL")
  expect_match(conditionMessage(err), "named by a comment: CRCL_CL, TVCL, TVKA, TVV, WT_CL, WT_V")
  expect_match(conditionMessage(err), "comments are parsed as type2")

  expect_error(check_scm(file.path(p$root, "model", "scm", "nopescm.toml")), "no SCM config at")
  expect_error(check_scm(p$model), "is a model file")

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

# ---- get_scm_status / summary ----------------------------------------------------

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
  by_dir <- get_scm_status(file.path(scm_fixture("scm"), "1001"))
  expect_equal(attr(by_config, "text"), attr(status, "text"))
  expect_equal(attr(by_dir, "text"), attr(status, "text"))
  expect_error(get_scm_status(scm_fixture("1001.mod")), "is a model file")
  expect_error(get_scm_status(scm_fixture()), "no SCM config")
})

test_that("summary() reads a planned process", {
  scm <- read_scm(scm_fixture("scm", "1001scm.toml"))
  s <- summary(scm)
  expect_s3_class(s, c("hyperion_nonmem_scm_summary", "hyperion_nonmem_scm_record"))
  expect_equal(s$status, "planned")
  expect_snapshot(print(s))
  expect_error(summary(scm, round = "9"), "has not started; no rounds to summarize")
  expect_error(summary(scm, candidate = "AGE_CL"), "no candidate named AGE_CL")
})

test_that("stop_scm() refuses a process with no driver", {
  expect_error(stop_scm(scm_fixture("scm", "1001scm.toml")), "no driver has been started")
})

# ---- the completed process -----------------------------------------------------

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
  expect_snapshot(print(status))
})

test_that("summary() renders the long, timing and files views", {
  comp <- read_scm(scm_complete_fixture("scm", "1001scm.toml"))
  expect_snapshot(print(summary(comp, long = TRUE, timing = TRUE, files = TRUE)))
  expect_snapshot(print(summary(comp, round = "forward_round2", candidate = "WT_V", long = TRUE)))
  r2 <- summary(comp, round = "2")
  expect_match(attr(r2, "text"), "forward_round2")
  expect_false(grepl("backward_round1", attr(r2, "text")))
  expect_error(summary(comp, round = "9"), "no round matching '9'")
  expect_error(summary(comp, candidate = "AGE_CL"), "no candidate named AGE_CL")
})

test_that("get_scm_candidates() flattens the rounds", {
  comp <- read_scm(scm_complete_fixture("scm", "1001scm.toml"))
  cand <- get_scm_candidates(comp)
  expect_s3_class(cand, "data.frame")
  expect_equal(nrow(cand), 10)
  expect_equal(cand$round[1:4], c("reference", rep("forward_round1", 3)))
  expect_equal(cand$candidate[2:4], c("WT_CL", "CRCL_CL", "WT_V"))
  expect_equal(cand$selected, c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(cand$attempts[6], 2)
  expect_equal(cand$delta_ofv[2], -20)
  expect_equal(cand$p_value[2], 7.744216e-06, tolerance = 1e-6)
  expect_true(is.na(cand$p_value[1]))
  expect_equal(cand$run_dir[2], "forward_round1/1001_wt_cl")
  expect_true(all(c("statistic", "df", "critical_delta_ofv", "heuristics") %in% names(cand)))

  timed <- get_scm_candidates(summary(comp), timing = TRUE)
  expect_equal(timed$wall_seconds[2], 90)
  expect_equal(timed$estimation_seconds[6], 2)

  planned <- get_scm_candidates(scm_fixture("scm", "1001scm.toml"))
  expect_equal(nrow(planned), 0)
  expect_equal(names(planned), names(cand))
  expect_error(get_scm_candidates(42), "must be an SCM summary")
})

test_that("read_scm_model() reads the process's models", {
  comp <- read_scm(scm_complete_fixture("scm", "1001scm.toml"))
  s <- summary(comp)
  final <- read_scm_model(s)
  expect_s3_class(final, "hyperion_nonmem_model")
  expect_equal(attr(final, "model_source"), "extdata/models/scm-complete/scm/1001/final/1001_scm_final.mod")
  expect_equal(summary(final)$run_name, "1001_scm_final")
  expect_equal(attr(read_scm_model(s, "forward"), "model_source"), "extdata/models/scm-complete/scm/1001/forward_final/1001_scm_forward_final.mod")
  expect_equal(attr(read_scm_model(s, "reference"), "model_source"), "extdata/models/scm-complete/scm/1001/base/1001_base.mod")
  expect_equal(attr(read_scm_model(comp, "initial"), "model_source"), "extdata/models/scm-complete/1001.mod")
  expect_equal(attr(read_scm_model(s, round = "forward_round1"), "model_source"), "extdata/models/scm-complete/scm/1001/forward_round1/1001_wt_cl.mod")
  expect_equal(attr(read_scm_model(s, round = "2", candidate = "wt_v"), "model_source"), "extdata/models/scm-complete/scm/1001/forward_round2/1001_wt_v_try2.mod")
  expect_error(read_scm_model(s, round = "9"), "no round matching")
  expect_error(read_scm_model(s, round = "forward_round3"), "has no winner")
  expect_error(read_scm_model(s, round = "1", candidate = "AGE_CL"), "no candidate named AGE_CL")
  expect_error(read_scm_model(s, candidate = "WT_CL"), "needs `round`")

  planned <- summary(read_scm(scm_fixture("scm", "1001scm.toml")))
  expect_error(read_scm_model(planned), "no final model yet \\(status: planned\\)")
})

# ---- submit_scm_to_slurm: everything before slurm ----------------------------------

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
  # An empty PATH: neither pharos nor the slurm tools can be found, so the
  # submit must stop at its first check and never reach sbatch.
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

test_that("strip_slurm_env() unsets the inherited slurm variables and restores them", {
  withr::local_envvar(SLURM_NTASKS = "4", SLURM_CPUS_PER_TASK = NA)
  old <- strip_slurm_env()
  expect_equal(Sys.getenv("SLURM_NTASKS", unset = NA), NA_character_)
  restore_slurm_env(old)
  expect_equal(Sys.getenv("SLURM_NTASKS"), "4")
  expect_equal(Sys.getenv("SLURM_CPUS_PER_TASK", unset = NA), NA_character_)
})
