test_that("print.hyperion_nonmem_scm shows the config", {
  scm <- read_scm(scm_fixture("scm", "1001scm.toml"))
  expect_snapshot(print(scm))
  scm$covariates$effects[[3]] <- list(name = "WT_V", type = "categorical", initial = 1, lower = 0, upper = 5)
  expect_snapshot(print(scm))
  scm$covariates$effects <- list()
  expect_snapshot(print(scm))
})

test_that("print methods for the SCM records show pharos' text under a title", {
  comp <- read_scm(scm_complete_fixture("scm", "1001scm.toml"))
  expect_snapshot(print(summary(comp)))
  expect_snapshot(print(get_scm_status(comp)))

  plan <- check_scm(comp)
  attr(plan, "resumable") <- FALSE
  expect_snapshot(print(plan), transform = scrub_scm)

  s <- summary(comp)
  s$totals$unusable <- 2
  expect_snapshot(print(s))
  expect_snapshot(str(s, max.level = 1), transform = scrub_scm_paths)
})
