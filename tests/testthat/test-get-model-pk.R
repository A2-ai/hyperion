test_that("get_model_pk renders $PK statements as equations", {
  mod <- read_model(system.file(
    "extdata", "mod/1001.mod",
    package = "hyperion"
  ))
  pk <- get_model_pk(mod)

  expect_s3_class(pk, "hyperion_nonmem_model_pk")
  expect_type(pk, "character")
  expect_length(pk, 10)

  eqs <- as.character(pk)
  expect_equal(eqs[1], "TVCL = THETA(1)")
  # binary ops render in pharos's normalized form (no spaces around * and /)
  expect_true("CL = TVCL*EXP(ETA(1))" %in% eqs)
  expect_true("S2 = VC/1000" %in% eqs)
})

test_that("get_model_pk errors on non-model input", {
  expect_error(get_model_pk("not a model"), "hyperion_nonmem_model")
})
