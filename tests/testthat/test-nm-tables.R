fixture_model <- function() {
  read_model(system.file("extdata", "mod/1001.mod", package = "hyperion"))
}

test_that("list_tables flattens $TABLE records and detects FIRSTONLY", {
  tbls <- list_tables(fixture_model())

  expect_s3_class(tbls, "data.frame")
  expect_equal(names(tbls), c("index", "file", "firstonly"))
  expect_equal(tbls$index, c(1L, 2L))
  expect_equal(tbls$file, c("run.output.tab", "run.param.tab"))
  expect_equal(tbls$firstonly, c(FALSE, TRUE))
})

test_that("read_nm_table resolves which and reports a missing output file", {
  mod <- fixture_model()
  # 1001 has no run output dir, so resolution succeeds but the read fails on a
  # clear "not found" -- this exercises model_output_dir + the existence guard.
  expect_error(read_nm_table(mod, "run.param.tab"), "not found")
  expect_error(read_nm_table(mod, which = 1), "not found")
})

test_that("read_nm_table errors on bad selectors", {
  mod <- fixture_model()
  expect_error(read_nm_table(mod), "multiple")          # NULL + several tables
  expect_error(read_nm_table(mod, "nope.tab"), "matches")
  expect_error(read_nm_table(mod, which = 9), "index")
  expect_error(read_nm_table("not a model"), "hyperion_nonmem_model")
})
