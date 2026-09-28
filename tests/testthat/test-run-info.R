test_that("significant digits keep the fractional value from the listing", {
  model <- system.file(
    "extdata", "models", "onecmt", "run001.mod",
    package = "hyperion"
  )
  details <- get_run_info(model)$run_details

  # run001.lst reports "NO. OF SIG. DIGITS IN FINAL EST.:  4.5"
  expect_equal(details$significant_digits, 4.5)
})
