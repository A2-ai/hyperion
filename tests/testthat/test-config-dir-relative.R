test_that("a relative config_dir resolves model paths without doubling the prefix", {
  config_dir <- system.file(package = "hyperion")
  withr::local_dir(dirname(config_dir))
  withr::local_options(hyperion.config_dir = basename(config_dir))

  resolved <- from_config_relative("extdata/models/onecmt/run001.mod")

  # Absolute, so a second call is a no-op
  expect_equal(resolved, normalizePath(resolved, mustWork = TRUE))
  expect_equal(from_config_relative(resolved), resolved)

  mod <- read_model(resolved)
  expect_equal(
    get_eta_labels(get_model_parameter_info(mod)),
    c("ETA1//ETA-TVCL", "ETA2//ETA-TVV", "ETA3//ETA-TVKA")
  )
})
