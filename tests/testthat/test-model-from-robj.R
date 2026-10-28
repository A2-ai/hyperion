# from_robj() aborted R on a model with an empty record options map.
test_that("model objects with a bare $COVARIANCE record round-trip to Rust", {
  root <- withr::local_tempdir()
  file.copy(system.file("pharos.toml", package = "hyperion"), root)
  withr::local_options(hyperion.config_dir = root)
  template <- readLines(
    system.file("extdata", "models", "onecmt", "run001.mod", package = "hyperion")
  )
  path <- file.path(root, "run001.mod")
  writeLines(sub("^\\$COV.*$", "$COVARIANCE", template), path)

  mod <- read_model(path)
  expect_identical(mod$covariance$options, structure(list(), names = character(0)))

  expect_equal(get_fixed_parameters(mod), c("OMEGA(3,3)", "SIGMA(2,2)"))
  expect_true(all(c("THETA1", "OMEGA(1,1)", "SIGMA(1,1)") %in% names(get_model_parameter_names(mod))))
  expect_equal(get_model_comment_info(mod)$thetas[[1]][[2]]$name, "TVCL")
  # The print method's Rust call, without cli output leaking into later snapshots.
  parts <- build_model_display_parts(mod)
  expect_equal(parts$tables$theta$data$Parameter, c("THETA1", "THETA2", "THETA3"))
})

test_that("model objects without source text error instead of aborting", {
  mod <- structure(list(problem = list(text = "x")), class = "hyperion_nonmem_model")
  expect_error(get_fixed_parameters(mod), "missing its `source` text")
  expect_error(get_fixed_parameters(list()), "must be a hyperion_nonmem_model object")
})
