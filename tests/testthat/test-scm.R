# SCM: plan / status / decision log ----------------------------

scm_template <- paste(
  "$PROBLEM scm template",
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

write_scm_fixture <- function(dir, template = scm_template) {
  model_path <- file.path(dir, "1001.mod")
  writeLines(template, model_path)
  writeLines("ID,TIME,AMT,DV,WT,CRCL,AGE", file.path(dir, "data.csv"))
  model_path
}

# The SCM config file (TOML) defines the search; covariates/direction are
# TOML fragments so tests can exercise both spellings and bad input.
write_scm_config <- function(dir,
                             covariates = "[4, 5, 6]",
                             direction = '["forward", "backward"]',
                             extra = character()) {
  config_path <- file.path(dir, "scm.toml")
  writeLines(
    c(
      'model = "1001.mod"',
      paste0("covariates = ", covariates),
      paste0("direction = ", direction),
      extra
    ),
    config_path
  )
  config_path
}

make_plan <- function(dir, ...) {
  write_scm_fixture(dir)
  scm_plan(write_scm_config(dir), ...)
}

cli_text_of <- function(expr) {
  paste(cli::cli_fmt(expr), collapse = "\n")
}

test_that("scm_plan names candidates from their comments and carries defaults", {
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
  expect_equal(plan$options$release_init, 0.1)
  expect_true(plan$options$cov_step)
  expect_false(plan$options$overwrite)
  expect_match(plan$out_dir, "scm/1001$")
  expect_equal(attr(plan, "plan_path"), file.path(plan$out_dir, "plan.json"))
})

test_that("scm_plan validates its inputs", {
  dir <- withr::local_tempdir()
  write_scm_fixture(dir)

  # the config path itself
  expect_error(scm_plan(42), "config")
  expect_error(scm_plan(file.path(dir, "nope.toml")), "not found")

  # a name only resolves against a $PK assignment holding the effect;
  # this template writes the effects inline, so there is none to find
  cfg <- write_scm_config(dir, covariates = '["WT_CL"]')
  expect_error(scm_plan(cfg), "no \\$PK term named WT_CL")

  # covariates are one form or the other, never mixed
  cfg <- write_scm_config(dir, covariates = '[4, "WT_CL"]')
  expect_error(scm_plan(cfg))

  # direction comes from the config, and is required there
  cfg <- write_scm_config(dir, direction = '["sideways"]')
  expect_error(scm_plan(cfg))
  writeLines(c('model = "1001.mod"', "covariates = [4, 5, 6]"),
             file.path(dir, "scm.toml"))
  expect_error(scm_plan(file.path(dir, "scm.toml")), "direction")

  # a typo'd option fails loudly instead of silently using a default
  cfg <- write_scm_config(dir, extra = "foward_alpha = 0.01")
  expect_error(scm_plan(cfg), "foward_alpha")

  # THETA(1) is a structural theta, not a `(0 FIX)` candidate
  cfg <- write_scm_config(dir, covariates = "[1]")
  expect_error(scm_plan(cfg), "must be fixed")

  # call-site override validation is R-side, before pharos is reached
  cfg <- write_scm_config(dir)
  expect_error(scm_plan(cfg, max_retries = -1), "max_retries")
  expect_error(scm_plan(cfg, max_retries = 1.5), "max_retries")
  expect_error(scm_plan(cfg, cov_step = "yes"), "cov_step")
  expect_error(scm_plan(cfg, release_init = 0), "release_init")
})

test_that("covariates can be keyed by $PK term name", {
  dir <- withr::local_tempdir()
  # the standardized-template style: each candidate effect is its own
  # named $PK assignment
  named <- sub(
    "TVCL = THETA(1) * (WT/70)**THETA(4) * (CRCL/100)**THETA(5)",
    paste(
      "WT_CL = (WT/70)**THETA(4)",
      "CRCL_CL = (CRCL/100)**THETA(5)",
      "WT_V = (WT/70)**THETA(6)",
      "TVCL = THETA(1) * WT_CL * CRCL_CL",
      sep = "\n"
    ),
    scm_template,
    fixed = TRUE
  )
  named <- sub(
    "V  = THETA(2) * (WT/70)**THETA(6) * EXP(ETA(2))",
    "V  = THETA(2) * WT_V * EXP(ETA(2))",
    named,
    fixed = TRUE
  )
  write_scm_fixture(dir, template = named)

  # requests match case-insensitively; candidates keep the authored spelling
  plan <- scm_plan(write_scm_config(
    dir,
    covariates = '["WT_CL", "crcl_cl", "WT_V"]',
    direction = '["forward"]'
  ))
  expect_equal(
    vapply(plan$candidates, function(c) c$name, character(1)),
    c("WT_CL", "CRCL_CL", "WT_V")
  )
  expect_equal(
    vapply(plan$candidates, function(c) as.integer(c$theta), integer(1)),
    c(4L, 5L, 6L)
  )

  # TVCL resolves to THETA(1), a structural theta -- not a `(0 FIX)` candidate
  expect_error(
    scm_plan(write_scm_config(dir, covariates = '["TVCL"]',
                              direction = '["forward"]')),
    "must be fixed"
  )
})

test_that("candidate names come from comments in any form", {
  dir <- withr::local_tempdir()
  loose <- sub("; WT_CL cov", "; WT_CL", scm_template, fixed = TRUE)
  loose <- sub("; CRCL_CL cov", "; CRCL_CL (-) :LOG", loose, fixed = TRUE)
  loose <- sub("$THETA (0 FIX)   ; WT_V cov", "$THETA (0 FIX)", loose, fixed = TRUE)
  write_scm_fixture(dir, template = loose)

  # the uncommented theta is named for its position, and says so
  expect_warning(
    plan <- scm_plan(write_scm_config(dir)),
    "named THETA6"
  )
  expect_equal(
    vapply(plan$candidates, function(x) x$name, character(1)),
    c("WT_CL", "CRCL_CL", "THETA6")
  )
})

test_that("numbered comments name the candidate, not the number", {
  dir <- withr::local_tempdir()
  # the `; <n> NAME description...` house style: the leading position number
  # labels the theta, it does not name it
  numbered <- sub("; WT_CL cov", "; 4 WT_CL WT on clearance", scm_template, fixed = TRUE)
  numbered <- sub("; CRCL_CL cov", "; 5 CRCL_CL CRCL on clearance", numbered, fixed = TRUE)
  numbered <- sub("; WT_V cov", "; 6 WT_V cov", numbered, fixed = TRUE)
  write_scm_fixture(dir, template = numbered)

  plan <- scm_plan(write_scm_config(dir))
  expect_equal(
    vapply(plan$candidates, function(x) x$name, character(1)),
    c("WT_CL", "CRCL_CL", "WT_V")
  )
})

test_that("direction accepts single directions", {
  dir <- withr::local_tempdir()
  write_scm_fixture(dir)

  fwd <- scm_plan(write_scm_config(dir, direction = '["forward"]'))
  expect_equal(unlist(fwd$options$direction), "forward")

  bwd <- scm_plan(write_scm_config(dir, direction = '["backward"]'))
  expect_equal(unlist(bwd$options$direction), "backward")
})

test_that("max_models is in the plan and depends on direction", {
  dir <- withr::local_tempdir()
  write_scm_fixture(dir)

  # 3 candidates: one phase = 1 + 3(3+1)/2 = 7; both phases = 13
  fwd <- scm_plan(write_scm_config(dir, direction = '["forward"]'))
  expect_equal(as.integer(fwd$max_models), 7L)
  bwd <- scm_plan(write_scm_config(dir, direction = '["backward"]'))
  expect_equal(as.integer(bwd$max_models), 7L)
  both <- scm_plan(write_scm_config(dir))
  expect_equal(as.integer(both$max_models), 13L)

  # it's in plan.json on disk, and in the printed plan
  plan_json <- paste(readLines(attr(both, "plan_path")), collapse = "\n")
  expect_match(plan_json, '"max_models": 13', fixed = TRUE)
  plan_text <- cli_text_of(print(both))
  expect_match(plan_text, "max models 13")
})

test_that("unrequested (0 FIX) thetas raise a warning, annotated or not", {
  dir <- withr::local_tempdir()
  write_scm_fixture(dir)
  expect_warning(
    scm_plan(write_scm_config(dir, covariates = "[4, 5]")),
    "WT_V.*not requested"
  )

  # the (0 FIX) shape alone flags it -- no `cov` annotation needed
  bare <- sub("$THETA (0 FIX)   ; WT_V cov", "$THETA (0 FIX)",
              scm_template, fixed = TRUE)
  write_scm_fixture(dir, template = bare)
  expect_warning(
    scm_plan(write_scm_config(dir, covariates = "[4, 5]")),
    "THETA\\(6\\).*not requested"
  )
})

test_that("call-site overrides beat the config", {
  dir <- withr::local_tempdir()
  write_scm_fixture(dir)
  cfg <- write_scm_config(dir, extra = c(
    "forward_alpha = 0.01",
    "max_retries = 5",
    "cov_step = false",
    "release_init = 0.2"
  ))

  # config alone (cov_step = false + a $COVARIANCE in the template warns)
  expect_warning(plan <- scm_plan(cfg), "cov_step is off")
  expect_equal(plan$options$forward_alpha, 0.01)
  expect_equal(as.integer(plan$options$max_retries), 5L)
  expect_false(plan$options$cov_step)
  expect_equal(plan$options$release_init, 0.2)
  expect_null(plan$options$num_rounds)

  # overrides win; untouched config values survive
  plan <- scm_plan(cfg,
                   num_rounds = 2, max_retries = 1,
                   cov_step = TRUE, release_init = 0.05, overwrite = TRUE)
  expect_equal(as.integer(plan$options$num_rounds), 2L)
  expect_equal(as.integer(plan$options$max_retries), 1L)
  expect_true(plan$options$cov_step)
  expect_equal(plan$options$release_init, 0.05)
  expect_true(plan$options$overwrite)
  expect_equal(plan$options$forward_alpha, 0.01)
})

test_that("scm_plan writes plan.json that scm_status reads back", {
  dir <- withr::local_tempdir()
  plan <- make_plan(dir)

  path <- attr(plan, "plan_path")
  expect_true(file.exists(path))
  expect_equal(basename(path), "plan.json")
  expect_equal(normalizePath(dirname(path)), normalizePath(plan$out_dir))

  st <- scm_status(plan)
  expect_s3_class(st, "hyperion_scm_status")
  expect_equal(st$status, "planned")
  expect_length(st$rounds, 0)
})

test_that("plan and status display methods run", {
  dir <- withr::local_tempdir()
  plan <- make_plan(dir)

  plan_text <- cli_text_of(print(plan))
  expect_match(plan_text, "SCM plan")
  expect_match(plan_text, "WT_CL")
  expect_match(plan_text, "Search size")

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

# A fabricated completed search (matching the pharos state schema) so the
# status / decision-log paths can be tested without NONMEM.
fabricate_completed_state <- function(out_dir) {
  state <- sprintf(
    '{
  "schema_version": 1,
  "plan_digest": "test-digest",
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
  "had_unusable": true,
  "updated": "2026-08-19T12:00:00+00:00"
}'
  )
  writeLines(state, file.path(out_dir, "scm_state.json"))
}

test_that("scm_status and summary read a completed search", {
  dir <- withr::local_tempdir()
  plan <- make_plan(dir)
  fabricate_completed_state(plan$out_dir)

  st <- scm_status(plan)
  expect_equal(st$status, "completed")
  expect_equal(unlist(st$retained), "WT_CL")
  expect_equal(st$reference_ofv, 980.0)
  expect_equal(st$rounds_complete, 1) # the reference fit is not a round
  expect_true(st$had_unusable)
  expect_equal(st$final_model, "final/1001_scm_final.mod")

  st_text <- paste(capture.output(print(st)), collapse = "\n")
  expect_match(st_text, "candidates : WT_CL, CRCL_CL, WT_V", fixed = TRUE)
  expect_match(st_text, "added WT_CL")
  expect_match(st_text, "unusable")
  # no reference line; the search is completed, so retained shows and the
  # final model carries the last reference fit's OFV
  expect_no_match(st_text, "reference  :", fixed = TRUE)
  expect_match(st_text, "retained   : WT_CL", fixed = TRUE)
  expect_match(st_text, "final model: final/1001_scm_final.mod (OFV 980.000)",
               fixed = TRUE)

  # summary() returns the decision log as a data.frame
  log <- suppressMessages(summary(st))
  expect_s3_class(log, "data.frame")
  expect_equal(nrow(log), 4) # base + 3 candidates
  expect_equal(
    names(log),
    c(
      "round", "direction", "candidate", "model", "attempts",
      "status", "reference_ofv", "delta_ofv", "df", "p_value",
      "significant", "selected", "heuristics", "decision"
    )
  )
  # the reference round has no reference: its OFV lives in the decision
  # text ("base model fitted (OFV ...)"), never in the reference_ofv column
  expect_true(is.na(log$reference_ofv[log$round == "reference"]))
  expect_match(log$decision[log$round == "reference"], "OFV 1000")

  wt_cl <- log[log$candidate == "WT_CL", ]
  expect_equal(wt_cl$attempts, 2L)
  expect_equal(wt_cl$delta_ofv, -20.0)
  expect_true(wt_cl$selected)
  expect_equal(wt_cl$heuristics, "parameter near boundary")

  wt_v <- log[log$candidate == "WT_V", ]
  expect_equal(wt_v$status, "unusable")
  expect_equal(wt_v$attempts, 4L)
  expect_true(is.na(wt_v$p_value)) # reported, never scored

  expect_true(file.exists(file.path(plan$out_dir, "scm_decision_log.csv")))
  expect_true(file.exists(file.path(plan$out_dir, "scm_decision_log.md")))
  expect_equal(attr(log, "retained"), "WT_CL")

  # summary(write = FALSE) does not rewrite
  unlink(file.path(plan$out_dir, "scm_decision_log.csv"))
  log2 <- summary(st, write = FALSE)
  expect_false(file.exists(file.path(plan$out_dir, "scm_decision_log.csv")))
  expect_equal(nrow(log2), 4)
})

test_that("scm_summary drills into a single round", {
  dir <- withr::local_tempdir()
  plan <- make_plan(dir)
  fabricate_completed_state(plan$out_dir)

  rd <- scm_summary(plan, 1)
  expect_s3_class(rd, "hyperion_scm_round")
  expect_equal(rd$round$name, "forward_round1")

  # number, "round N", full name, and "reference" all resolve; so do the
  # plan / out_dir / plan.json addressing forms
  expect_equal(scm_summary(plan$out_dir, "round 1")$round$name, "forward_round1")
  expect_equal(scm_summary(plan, "forward_round1")$round$name, "forward_round1")
  expect_equal(scm_summary(plan, "reference")$round$name, "reference")

  txt <- paste(capture.output(print(rd)), collapse = "\n")
  # every model run that round, retries included, each with its outcome
  expect_match(txt, "forward_round1/1001_wt_cl.mod", fixed = TRUE)
  expect_match(txt, "no ofv")
  expect_match(txt, "forward_round1/1001_wt_cl_try2.mod", fixed = TRUE)
  expect_match(txt, "<- selected", fixed = TRUE)
  expect_match(txt, "unusable")
  expect_match(txt, "heuristics: parameter near boundary")
  expect_match(txt, "round_summary.md")

  # once the per-round markdown exists, the pointer names it
  dir.create(file.path(plan$out_dir, "forward_round1"), recursive = TRUE)
  writeLines("x", file.path(plan$out_dir, "forward_round1", "round_summary.md"))
  rd2 <- scm_summary(plan, 1)
  expect_equal(rd2$summary_md, "forward_round1/round_summary.md")

  # unknown rounds error and name what exists; bad input is caught R-side
  expect_error(scm_summary(plan, 9), "forward_round1")
  expect_error(scm_summary(plan, "sideways"), "rounds so far")
  expect_error(scm_summary(plan, 0), "round")
  expect_error(scm_summary(plan), "required")
})

test_that("scm_status resolves plans, dirs, and plan.json paths", {
  dir <- withr::local_tempdir()
  plan <- make_plan(dir)
  path <- attr(plan, "plan_path")

  st1 <- scm_status(plan)
  st2 <- scm_status(plan$out_dir)
  st3 <- scm_status(path)
  expect_equal(st1$status, st2$status)
  expect_equal(st2$status, st3$status)

  expect_error(scm_status(file.path(dir, "nope")), "no SCM output")
  expect_error(scm_status(dir), "plan.json")
})

test_that("scm_run rejects inputs that are not plans", {
  expect_error(scm_run(42), "hyperion_scm_plan")
})

test_that("scm_run refuses a plan whose plan.json is gone", {
  dir <- withr::local_tempdir()
  plan <- make_plan(dir)
  unlink(attr(plan, "plan_path"))
  expect_error(scm_run(plan), "scm_plan\\(\\)")
})

test_that("scm_plan validates num_rounds", {
  dir <- withr::local_tempdir()

  # invalid values error before anything is written
  expect_error(make_plan(dir, num_rounds = 0), "num_rounds")
  expect_error(make_plan(dir, num_rounds = -1), "num_rounds")
  expect_error(make_plan(dir, num_rounds = 1.5), "num_rounds")
  expect_error(make_plan(dir, num_rounds = Inf), "num_rounds")
  expect_error(make_plan(dir, num_rounds = NA), "num_rounds")
  expect_error(make_plan(dir, num_rounds = c(1, 2)), "num_rounds")

  # pacing lives in the plan, not in scm_run()
  plan <- make_plan(dir, num_rounds = 2)
  expect_equal(plan$options$num_rounds, 2)
  expect_error(scm_run(plan, num_rounds = 1), "unused argument")
})
