test_that("get_model_pk returns a table of $PK equations", {
  mod <- read_model(system.file(
    "extdata", "mod/1001.mod",
    package = "hyperion"
  ))
  pk <- get_model_pk(mod)

  expect_s3_class(pk, "hyperion_nonmem_model_pk")
  expect_s3_class(pk, "data.frame")
  expect_equal(names(pk), c("target", "equation", "symbols"))
  expect_equal(nrow(pk), 10L)

  expect_equal(pk$target[1], "TVCL")
  expect_equal(pk$equation[1], "TVCL = THETA(1)")
})

test_that("get_model_pk symbols capture RHS dependencies", {
  mod <- read_model(system.file(
    "extdata", "mod/1001.mod",
    package = "hyperion"
  ))
  pk <- get_model_pk(mod)
  sym <- function(target) pk$symbols[[which(pk$target == target)]]

  expect_type(pk$symbols, "list")
  # plain identifiers + indexed THETA/ETA refs; math fns (EXP) excluded
  expect_equal(sym("TVCL"), "THETA(1)")
  expect_equal(sym("CL"), c("TVCL", "ETA(1)"))
  expect_equal(sym("S2"), "VC")
  expect_equal(sym("K20"), c("CL", "VC"))
})

test_that("get_model_pk errors on non-model input", {
  expect_error(get_model_pk("not a model"), "hyperion_nonmem_model")
})
