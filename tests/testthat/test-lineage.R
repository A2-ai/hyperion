test_that("get_model_lineage() returns the whole project tree", {
  expect_snapshot(get_model_lineage())
})

test_that("get_model_lineage(model) returns the model's full lineage", {
  expect_snapshot(get_model_lineage("extdata/models/onecmt/run003.mod"))
})

test_that("get_model_lineage(from, to) slices between two models", {
  expect_snapshot(
    get_model_lineage(
      from = "extdata/models/onecmt/run001.mod",
      to = "extdata/models/onecmt/run003b1.mod"
    )
  )
})

test_that("lineage helpers return project-relative paths", {
  expect_snapshot(get_model_ancestors("extdata/models/onecmt/run003b1.mod"))
  expect_snapshot(get_model_descendants("extdata/models/onecmt/run001.mod"))
  expect_snapshot(
    are_models_in_lineage(
      "extdata/models/onecmt/run001.mod",
      "extdata/models/onecmt/run003b1.mod"
    )
  )
})
