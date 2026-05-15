# Pin the project root for the test session to the install root (where
# inst/pharos.toml lives). Without this, pharos's CWD-walk lands on
# tests/pharos.toml and treats `tests/` as the project root — fixtures
# under inst/extdata/ are then "outside the project root" for
# `to_root_relative` and every read_model() call errors.
old_opts <- options(
  hyperion.config_dir = system.file(package = "hyperion")
)
withr::defer(options(old_opts), teardown_env())
