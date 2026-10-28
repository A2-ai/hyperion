scm_fixture <- function(...) {
  system.file("extdata", "models", "scm", ..., package = "hyperion")
}

scm_complete_fixture <- function(...) {
  system.file("extdata", "models", "scm-complete", ..., package = "hyperion")
}

# A temp pharos project holding the SCM fixture model, for tests that write.
# pharos finds the project by walking up from the model, so the project's
# pharos.toml has to be copied in; hyperion.config_dir points at the same dir.
local_scm_project <- function(effects = NULL, .local_envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = .local_envir)
  file.copy(system.file("pharos.toml", package = "hyperion"), root)
  dir.create(file.path(root, "model"))
  file.copy(scm_fixture(c("1001.mod", "data.csv")), file.path(root, "model"))
  withr::local_options(hyperion.config_dir = root, .local_envir = .local_envir)
  model <- file.path(root, "model", "1001.mod")
  scm <- NULL
  if (!is.null(effects)) {
    scm <- suppressMessages(init_scm(model, effects = effects))
  }
  list(
    root = root,
    model = model,
    scm = scm,
    config = file.path(root, "model", "scm", "1001scm.toml"),
    out_dir = file.path(root, "model", "scm", "1001")
  )
}

scrub_scm <- function(x) {
  x <- gsub(
    "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?(Z|[+-]\\d{2}:\\d{2})?",
    "[TIMESTAMP]",
    x
  )
  x <- gsub("\\b[0-9a-f]{64}\\b", "[DIGEST]", x)
  gsub(
    "(running|after|finished) (\\d+h \\d+m|\\d+m \\d+s|\\d+\\.\\d+s)",
    "\\1 [AGE]",
    x
  )
}

# Absolute paths in an SCM record's attributes depend on where the package is
# installed; snapshots show them relative to the package root.
scrub_scm_paths <- function(x) {
  root <- normalizePath(system.file(package = "hyperion"), mustWork = FALSE)
  x <- gsub(root, "[PKG]", x, fixed = TRUE)
  scrub_scm(x)
}
