local_project_root <- function(.envir = parent.frame()) {
  # Bundled fixtures use `inst/pharos.toml` as their project root, so
  # `based_on` strings in `*_metadata.json` are stored as
  # `extdata/models/onecmt/...`. Point the option at the install root
  # (where `pharos.toml` lives) so pharos generates matching keys.
  withr::local_options(
    hyperion.config_dir = system.file(package = "hyperion"),
    .local_envir = .envir
  )
}

test_that("get_model_lineage() returns the whole project tree", {
  local_project_root()
  expect_snapshot(get_model_lineage())
})

test_that("get_model_lineage(model) returns the model's full lineage", {
  local_project_root()
  expect_snapshot(get_model_lineage("extdata/models/onecmt/run003.mod"))
})

test_that("get_model_lineage(from, to) slices between two models", {
  local_project_root()
  expect_snapshot(
    get_model_lineage(
      from = "extdata/models/onecmt/run001.mod",
      to = "extdata/models/onecmt/run003b1.mod"
    )
  )
})

test_that("lineage helpers return project-relative paths", {
  local_project_root()
  expect_snapshot(get_model_ancestors("extdata/models/onecmt/run003b1.mod"))
  expect_snapshot(get_model_descendants("extdata/models/onecmt/run001.mod"))
  expect_snapshot(
    are_models_in_lineage(
      "extdata/models/onecmt/run001.mod",
      "extdata/models/onecmt/run003b1.mod"
    )
  )
})
