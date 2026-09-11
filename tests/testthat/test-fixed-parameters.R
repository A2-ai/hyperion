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
