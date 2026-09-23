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

local_custom_run <- function(.local_envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = .local_envir)
  file.copy(system.file("pharos.toml", package = "hyperion"), root)
  file.copy(fixture("run001.mod"), root)
  run <- file.path(root, "custom-run001")
  dir.create(run)
  file.copy(list.files(fixture("run001"), full.names = TRUE), run)
  start <- file.path(run, "pharos_start.json")
  writeLines(sub(
    "extdata/models/onecmt/run001.mod", "run001.mod",
    readLines(start), fixed = TRUE
  ), start)
  withr::local_options(hyperion.config_dir = root, .local_envir = .local_envir)
  list(root = root, run = run, model = file.path(root, "run001.mod"))
}

test_that("recorded custom runs agree across status, summaries and comments", {
  project <- local_custom_run()
  model <- read_model(project$model)

  expect_equal(attr(model, "run_status"), "run")
  expect_equal(get_run_status(model), "run")
  expect_equal(get_run_status(project$run), "run")
  expect_equal(
    summary(model)$parameters[c("name", "estimate")],
    get_parameters(model)[c("name", "estimate")]
  )
  expect_equal(
    get_model_summary(model),
    get_model_summary(project$run)
  )
  expect_equal(
    get_model_parameter_info(model),
    get_model_parameter_info(project$run)
  )
})

test_that("running summaries resolve custom outputs and missing early-run files", {
  project <- local_custom_run()
  lst <- file.path(project$run, "run001.lst")
  contents <- readLines(lst)
  writeLines(contents[!grepl("^\\s*Stop Time:", contents)], lst)
  model <- read_model(project$model)

  expect_equal(attr(model, "run_status"), "running")
  result <- summary(model, n_iterations = 2)
  expect_equal(result$iterations, tail(read_ext_file(model, parameters_only = TRUE), 2))
  expect_equal(result$gradients, tail(get_gradients(model), 2))
  expect_gt(nrow(result$iterations), 0)
  expect_gt(nrow(result$gradients), 0)

  unlink(file.path(project$run, c("run001.ext", "run001.grd")))
  early <- summary(model)
  expect_equal(early$run_status, "running")
  expect_null(early$iterations)
  expect_null(early$gradients)
})

test_that("copy_model finds recorded estimates and honors explicit ext_file", {
  project <- local_custom_run()
  automatic <- file.path(project$root, "automatic.mod")
  explicit <- file.path(project$root, "explicit.mod")

  copy_model(project$model, automatic, update = "theta", description = "test", no_metadata = TRUE)
  copy_model(
    project$model, explicit, update = "theta", description = "test", no_metadata = TRUE,
    ext_file = file.path(project$run, "run001.ext")
  )
  expect_equal(read_model(automatic)$thetas, read_model(explicit)$thetas)
  expect_false(identical(read_model(automatic)$thetas, read_model(project$model)$thetas))
})

test_that("an explicit run selects its outputs when the model has several runs", {
  project <- local_custom_run()
  expected <- get_parameters(project$model)
  second <- file.path(project$root, "second-run001")
  dir.create(second)
  file.copy(list.files(project$run, full.names = TRUE), second)

  expect_error(get_parameters(project$model), "multiple run outputs")
  for (run in c(project$run, second)) {
    expect_equal(get_parameters(run), expected)
    expect_equal(get_parameters(file.path(run, "run001.ext")), expected)
    expect_equal(read_ext_file(run), read_ext_file(file.path(run, "run001.ext")))
    expect_equal(get_run_status(run), "run")
    expect_gt(nrow(get_gradients(run)), 0)
  }
})

test_that("configured output templates work without a recorded run", {
  project <- local_custom_run()
  config <- file.path(project$root, "pharos.toml")
  writeLines(sub(
    "[nonmem]", '[nonmem]\noutput_dir = "custom-{{name}}"',
    readLines(config), fixed = TRUE
  ), config)
  unlink(file.path(project$run, "pharos_start.json"))

  model <- read_model(project$model)
  expect_equal(attr(model, "run_status"), "run")
  expect_gt(nrow(get_parameters(model)), 0)
  expect_equal(get_model_parameter_info(model), get_model_parameter_info(project$run))
  unlink(file.path(project$run, "run001.lst"))
  expect_equal(get_run_status(model), "not_run")
})
