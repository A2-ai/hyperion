local_model <- function(.local_envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = .local_envir)
  file.copy(system.file("pharos.toml", package = "hyperion"), root)
  file.copy(system.file("extdata", "models", "onecmt", "run001.mod", package = "hyperion"), root)
  withr::local_options(hyperion.config_dir = root, .local_envir = .local_envir)
  read_model(file.path(root, "run001.mod"))
}

test_that("metadata setters return the updated metadata", {
  mod <- local_model()

  set <- mod |> set_metadata_file(description = "Base model", tags = "1mt")
  expect_s3_class(set, "hyperion_model_metadata")
  expect_equal(set$description, "Base model")
  expect_equal(unlist(set$tags), "1mt")

  updated <- mod |> update_metadata_file(tags = "key")
  expect_equal(unlist(updated$tags), c("1mt", "key"))
  expect_equal(updated, get_model_metadata(mod))

  cleared <- mod |> clear_metadata_file(tags = TRUE)
  expect_length(cleared$tags, 0)
  expect_equal(cleared$description, "Base model")
  expect_equal(cleared, get_model_metadata(mod))
})
