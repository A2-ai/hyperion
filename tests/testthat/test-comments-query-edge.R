test_that("get_comment returns NULL for non-parameter names", {
  info <- ModelComments()
  expect_null(get_comment(info, "OTHER1"))
})

test_that("get_theta_names rejects non-ModelComments input", {
  expect_error(
    get_theta_names(list()),
    "model_comments must be a ModelComments object"
  )
})

test_that("get_parameter_names returns empty data frame when no rows", {
  info <- ModelComments()
  result <- get_parameter_names(info)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("name", "display"))
})
