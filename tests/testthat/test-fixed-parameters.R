test_that("get_fixed_parameters finds fixed thetas", {
  mod <- read_model(system.file("extdata", "mod", "iiv-cov.mod", package = "hyperion"))
  expect_equal(get_fixed_parameters(mod, kind = "THETA"), "THETA4")

  mod <- read_model(system.file("extdata", "mod", "iov.mod", package = "hyperion"))
  expect_equal(get_fixed_parameters(mod, kind = "THETA"), c("THETA6", "THETA7"))
})

test_that("get_fixed_parameters finds fixed omegas, including block off-diagonals", {
  mod <- read_model(system.file("extdata", "mod", "iov.mod", package = "hyperion"))
  expect_equal(
    get_fixed_parameters(mod, kind = "OMEGA"),
    c("OMEGA(4,4)", "OMEGA(6,6)")
  )

  mod <- read_model(system.file("extdata", "mod", "everything.mod", package = "hyperion"))
  expect_equal(
    get_fixed_parameters(mod, kind = "OMEGA"),
    c(
      "OMEGA(7,7)", "OMEGA(8,7)", "OMEGA(8,8)",
      "OMEGA(26,26)", "OMEGA(27,26)", "OMEGA(27,27)",
      "OMEGA(28,26)", "OMEGA(28,27)", "OMEGA(28,28)"
    )
  )
})

test_that("get_fixed_parameters finds fixed sigmas", {
  mod <- read_model(system.file("extdata", "mod", "iiv-cov.mod", package = "hyperion"))
  expect_equal(get_fixed_parameters(mod, kind = "SIGMA"), "SIGMA(2,2)")

  mod <- read_model(system.file("extdata", "mod", "everything.mod", package = "hyperion"))
  expect_equal(get_fixed_parameters(mod, kind = "SIGMA"), "SIGMA(3,3)")
})

test_that("get_fixed_parameters returns all kinds when kind is NULL", {
  mod <- read_model(system.file("extdata", "mod", "iiv-cov.mod", package = "hyperion"))
  expect_equal(
    get_fixed_parameters(mod),
    c("THETA4", "OMEGA(4,4)", "SIGMA(2,2)")
  )
})

test_that("get_fixed_parameters returns empty when nothing is fixed", {
  mod <- read_model(system.file("extdata", "mod", "example1.mod", package = "hyperion"))
  expect_equal(get_fixed_parameters(mod), character(0))
  expect_equal(get_fixed_parameters(mod, kind = "OMEGA"), character(0))
})

test_that("get_fixed_parameters rejects an unknown kind", {
  mod <- read_model(system.file("extdata", "mod", "iiv-cov.mod", package = "hyperion"))
  expect_error(get_fixed_parameters(mod, kind = "ETA"), "kind must be one of")
})

test_that("get_fixed_parameters treats BLOCK SAME as fixed when the block it repeats is", {
  root <- withr::local_tempdir()
  file.copy(system.file("pharos.toml", package = "hyperion"), root)
  withr::local_options(hyperion.config_dir = root)
  template <- readLines(system.file("extdata", "models", "onecmt", "run001.mod", package = "hyperion"))
  omega <- grep("^\\$OMEGA", template)
  sigma <- grep("^\\$SIGMA", template)
  fixed_omegas <- function(...) {
    path <- tempfile(tmpdir = root, fileext = ".mod")
    writeLines(c(template[seq_len(omega - 1)], c(...), template[sigma:length(template)]), path)
    get_fixed_parameters(read_model(path), kind = "OMEGA")
  }

  expect_equal(
    fixed_omegas("$OMEGA BLOCK(2) FIX", "0.1", "0.01 0.1", "$OMEGA BLOCK(2) SAME"),
    c("OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)", "OMEGA(3,3)", "OMEGA(4,3)", "OMEGA(4,4)")
  )
  expect_equal(
    fixed_omegas("$OMEGA BLOCK(2)", "0.1", "0.01 0.1", "$OMEGA BLOCK(2) SAME"),
    character(0)
  )
  expect_equal(
    fixed_omegas("$OMEGA BLOCK(1) 0.1 FIX", "$OMEGA BLOCK(1) SAME(2)"),
    c("OMEGA(1,1)", "OMEGA(2,2)", "OMEGA(3,3)")
  )
  expect_equal(
    fixed_omegas("$OMEGA 0.1", "$OMEGA BLOCK(1) 0.1 FIX", "$OMEGA BLOCK(1) SAME"),
    c("OMEGA(2,2)", "OMEGA(3,3)")
  )
  expect_equal(
    fixed_omegas(
      "$OMEGA BLOCK(1) 0.1 FIX", "$OMEGA BLOCK(1) SAME",
      "$OMEGA BLOCK(1) 0.2", "$OMEGA BLOCK(1) SAME"
    ),
    c("OMEGA(1,1)", "OMEGA(2,2)")
  )
})
