test_that("hyperion.nonmem-model print works", {
  mod_dir <- testthat::test_path("testdata", "mod")
  mods <- list.files(mod_dir, pattern = "\\.mod$", full.names = TRUE)

  for (p in mods) {
    mod <- read_model(p)
    expect_snapshot(print(mod))
  }
})
