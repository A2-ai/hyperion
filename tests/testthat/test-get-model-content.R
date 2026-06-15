test_that("get_model_content returns the model file text", {
  p <- system.file("extdata", "mod", "1001.mod", package = "hyperion")
  mod <- read_model(p)

  content <- mod |> get_model_content()

  expect_type(content, "character")
  expect_length(content, 1)
  expect_equal(trimws(content), trimws(paste(readLines(p), collapse = "\n")))
})
