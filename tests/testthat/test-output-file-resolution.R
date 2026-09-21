# Resolving a model's run outputs goes through pharos's ModelLayout, which needs
# a live R session for the config lookup, so these branches are covered here
# rather than in the Rust unit tests.

fixture <- function(...) {
  system.file("extdata", "models", "onecmt", ..., package = "hyperion")
}

test_that("get_parameters resolves every supported input shape", {
  expected <- get_parameters(fixture("run001.mod"))

  expect_equal(get_parameters(fixture("run001")), expected)
  expect_equal(get_parameters(fixture("run001", "run001.ext")), expected)
  expect_equal(get_parameters(fixture("run001_metadata.json")), expected)
  expect_equal(get_parameters(read_model(fixture("run001.mod"))), expected)
})

test_that("shrinkage is picked up from the run directory", {
  params <- get_parameters(fixture("run001.mod"))
  omega <- params[params$kind == "OMEGA", ]

  expect_gt(nrow(omega), 0)
  expect_false(all(is.na(omega$shrinkage)))
})

test_that("a model with no run outputs errors", {
  expect_error(get_parameters(fixture("example.mod")))
})
