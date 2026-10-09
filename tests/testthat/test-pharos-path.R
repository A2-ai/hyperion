# Fake pharos executables in temp dirs, PATH pointed at them, and a mocked
# bundled_pharos_path() exercise every rule without an installed CLI.

# A shell script standing in for pharos. `--version` prints `version`.
local_fake_pharos <- function(version = "0.6.1", env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  path <- file.path(dir, "pharos")
  writeLines(c("#!/bin/sh", paste0("echo \"pharos ", version, "\"")), path)
  Sys.chmod(path, "0755")
  path
}

# An empty directory to use as PATH, so no real pharos is found.
local_empty_path <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(PATH = dir, .local_envir = env)
  invisible(dir)
}

local_no_bundled <- function(env = parent.frame()) {
  local_mocked_bindings(bundled_pharos_path = function() "", .env = env)
}

local_bundled <- function(path, env = parent.frame()) {
  local_mocked_bindings(bundled_pharos_path = function() path, .env = env)
}

# Mocks Rust entry point `fn`; the returned environment's `args` holds the
# arguments it was called with.
local_capture_rust <- function(fn, env = parent.frame()) {
  seen <- new.env()
  local_mocked_bindings(!!fn := function(...) seen$args <- list(...), .env = env)
  seen
}

expected_pharos_version <- function() {
  version <- utils::packageDescription("hyperion", fields = "Config/PharosVersion")
  skip_if(is.na(version), "hyperion is not installed")
  version
}

# -- Where the bundled CLI is installed --------------------------------------

test_that("bundled_pharos_relpath() matches install.libs.R's bin + R_ARCH", {
  expect_identical(bundled_pharos_relpath("", "unix"), "bin/pharos")
  expect_identical(bundled_pharos_relpath("x64", "windows"), "bin/x64/pharos.exe")
  expect_identical(bundled_pharos_relpath("", "windows"), "bin/pharos.exe")
})

test_that("bundled_pharos_path() finds the CLI in an installed layout", {
  lib <- withr::local_tempdir()
  pkg <- file.path(lib, "hyperion")
  dir.create(file.path(pkg, "bin", "x64"), recursive = TRUE)
  writeLines(
    c("Package: hyperion", "Version: 0.0.0"),
    file.path(pkg, "DESCRIPTION")
  )
  file.create(file.path(pkg, "bin", "pharos"))
  file.create(file.path(pkg, "bin", "x64", "pharos.exe"))

  expect_identical(
    normalizePath(bundled_pharos_path("", "unix", lib.loc = lib)),
    normalizePath(file.path(pkg, "bin", "pharos"))
  )
  expect_identical(
    normalizePath(bundled_pharos_path("x64", "windows", lib.loc = lib)),
    normalizePath(file.path(pkg, "bin", "x64", "pharos.exe"))
  )
  # Installed without the CLI: "" so resolve_pharos() falls through to PATH.
  expect_identical(bundled_pharos_path("i386", "unix", lib.loc = lib), "")
})

# -- Rule 1: "pharos" means PATH only ------------------------------------------

test_that("rule 1: 'pharos' uses PATH and ignores the bundled binary", {
  skip_on_os("windows")
  on_path <- local_fake_pharos("0.5.0")
  withr::local_envvar(PATH = dirname(on_path))
  local_bundled(local_fake_pharos("0.6.1"))

  res <- resolve_pharos("pharos")

  expect_identical(normalizePath(res$path), normalizePath(on_path))
  expect_identical(res$source, "path")
})

test_that("rule 1: 'pharos' errors naming the argument when not on PATH", {
  skip_on_os("windows")
  local_empty_path()
  local_bundled(local_fake_pharos())

  expect_error(resolve_pharos("pharos"), "pharos_exec_path.*found on")
})

# -- Rule 2: any other value is a path ---------------------------------------

test_that("rule 2: an absolute path is used as an override", {
  skip_on_os("windows")
  exe <- local_fake_pharos()
  local_empty_path()
  local_bundled(local_fake_pharos())

  res <- resolve_pharos(exe)

  expect_identical(res$path, normalizePath(exe))
  expect_identical(res$source, "override")
})

test_that("rule 2: a relative path is resolved against getwd() at call time", {
  skip_on_os("windows")
  exe <- local_fake_pharos()
  local_no_bundled()
  withr::local_dir(dirname(dirname(exe)))

  res <- resolve_pharos(file.path(basename(dirname(exe)), "pharos"))

  expect_identical(res$path, normalizePath(exe))
  expect_true(startsWith(res$path, "/"))
  expect_identical(res$source, "override")
})

test_that("rule 2: a missing path errors", {
  local_no_bundled()
  missing <- file.path(withr::local_tempdir(), "no-such-pharos")

  expect_error(resolve_pharos(missing), "does not exist")
})

test_that("rule 2: a file that is not executable errors", {
  skip_on_os("windows")
  local_no_bundled()
  exe <- local_fake_pharos()
  Sys.chmod(exe, "0644")

  expect_error(resolve_pharos(exe), "not an executable")
})

test_that("rule 2: a directory errors", {
  skip_on_os("windows")
  local_no_bundled()

  expect_error(resolve_pharos(withr::local_tempdir()), "not an executable")
})

test_that("pharos_exec_path must be NULL or a single string", {
  expect_error(resolve_pharos(c("a", "b")), "pharos_exec_path")
  expect_error(resolve_pharos(NA_character_), "pharos_exec_path")
  expect_error(resolve_pharos(""), "pharos_exec_path")
  expect_error(resolve_pharos(1), "pharos_exec_path")
})

# -- Rules 3 and 4: NULL means bundled, then PATH ----------------------------

test_that("rule 3: NULL prefers the bundled binary over PATH", {
  skip_on_os("windows")
  bundled <- local_fake_pharos("0.6.1")
  on_path <- local_fake_pharos("0.5.0")
  withr::local_envvar(PATH = dirname(on_path))
  local_bundled(bundled)

  res <- resolve_pharos(NULL)

  expect_identical(res$path, bundled)
  expect_identical(res$source, "bundled")
})

test_that("rule 4: NULL falls back to PATH without a bundled binary", {
  skip_on_os("windows")
  on_path <- local_fake_pharos()
  withr::local_envvar(PATH = dirname(on_path))
  local_no_bundled()

  res <- resolve_pharos(NULL)

  expect_identical(normalizePath(res$path), normalizePath(on_path))
  expect_true(startsWith(res$path, "/"))
  expect_identical(res$source, "path")
})

# -- Rule 5: nothing found ----------------------------------------------------

test_that("rule 5: the not-found error gives the reason and both remedies", {
  local_empty_path()
  local_no_bundled()

  err <- tryCatch(resolve_pharos(NULL), error = function(e) e)

  expect_s3_class(err, "error")
  msg <- conditionMessage(err)
  expect_match(msg, "only needed for run submission on Linux clusters")
  expect_match(msg, "pharos_exec_path")
  expect_match(msg, "HYPERION_SKIP_PHAROS_CLI=false")
})

# -- pharos_path() ------------------------------------------------------------

test_that("pharos_path() returns the resolved path as a single string", {
  skip_on_os("windows")
  bundled <- local_fake_pharos()
  local_empty_path()
  local_bundled(bundled)

  expect_identical(pharos_path(), bundled)

  override <- local_fake_pharos()
  expect_identical(pharos_path(override), normalizePath(override))
})

test_that("pharos_path() errors when nothing resolves", {
  local_empty_path()
  local_no_bundled()

  expect_error(pharos_path(), "No pharos CLI found")
})

# -- detect_pharos() ----------------------------------------------------------

test_that("detect_pharos() reports path, version and source", {
  skip_on_os("windows")
  bundled <- local_fake_pharos("0.6.1")
  local_empty_path()
  local_bundled(bundled)

  expect_identical(
    detect_pharos(),
    list(path = bundled, version = "0.6.1", source = "bundled")
  )
})

test_that("detect_pharos() never errors when no pharos is found", {
  local_empty_path()
  local_no_bundled()

  expect_identical(
    expect_no_error(detect_pharos()),
    list(path = NA_character_, version = NA_character_, source = NA_character_)
  )
})

test_that("detect_pharos() never errors when the bundled binary is unusable", {
  skip_on_os("windows")
  bundled <- local_fake_pharos()
  Sys.chmod(bundled, "0644")
  local_empty_path()
  local_bundled(bundled)

  res <- expect_no_error(detect_pharos())
  expect_identical(res$path, NA_character_)
  expect_identical(res$source, NA_character_)
})

test_that("detect_pharos() keeps the path when --version is unparseable", {
  skip_on_os("windows")
  bundled <- local_fake_pharos("unknown")
  local_empty_path()
  local_bundled(bundled)

  res <- expect_no_error(detect_pharos())
  expect_identical(res$path, bundled)
  expect_identical(res$version, NA_character_)
  expect_identical(res$source, "bundled")
})

# -- Status display -----------------------------------------------------------

test_that("the attach message shows the source and flags a version mismatch", {
  expected_pharos_version()
  local_mocked_bindings(
    detect_pharos = function() {
      list(path = "/lib/hyperion/bin/pharos", version = "0.0.1", source = "bundled")
    }
  )

  msg <- cli::ansi_strip(hyperion_options_message())

  expect_match(msg, "pharos CLI version mismatch: installed 0.0.1", fixed = TRUE)
  expect_match(msg, "bundled: /lib/hyperion/bin/pharos", fixed = TRUE)
})

test_that("the attach message labels a PATH pharos", {
  expected <- expected_pharos_version()
  local_mocked_bindings(
    detect_pharos = function() {
      list(path = "/usr/bin/pharos", version = expected, source = "path")
    }
  )

  msg <- cli::ansi_strip(hyperion_options_message())

  expect_match(msg, "PATH: /usr/bin/pharos", fixed = TRUE)
  expect_no_match(msg, "mismatch")
})

# -- Submission hands the resolved path to Rust ------------------------------

test_that("submit wrappers keep their arguments and add pharos_exec_path last", {
  expect_identical(
    names(formals(submit_model_to_slurm)),
    c(
      "model", "overwrite", "dry_run", "run_in_output_dir", "ncpu",
      "partition", "clean_level", "parafile", "template", "account",
      "verbose", "pharos_exec_path"
    )
  )
  expect_null(formals(submit_model_to_slurm)$pharos_exec_path)

  expect_identical(
    names(formals(submit_model_to_sge)),
    c(
      "model", "overwrite", "dry_run", "run_in_output_dir", "ncpu",
      "clean_level", "parafile", "template", "verbose", "pharos_exec_path"
    )
  )
  expect_null(formals(submit_model_to_sge)$pharos_exec_path)
})

test_that("submit_model_to_slurm() passes every argument to Rust by name", {
  skip_on_os("windows")
  exe <- local_fake_pharos()
  local_no_bundled()
  local_empty_path()
  rust_args <- names(formals(.submit_model_to_slurm))
  seen <- local_capture_rust(".submit_model_to_slurm")
  # A relative override must still reach the job script as an absolute path.
  withr::local_dir(dirname(exe))

  # Every argument gets a distinct non-default value, so any swap shows up.
  submit_model_to_slurm(
    "run001.mod",
    overwrite = TRUE,
    dry_run = "dry",
    run_in_output_dir = "in-out",
    ncpu = 7,
    partition = "part",
    clean_level = 3,
    parafile = "para.pnm",
    template = "tmpl.sh",
    account = "acct",
    verbose = "verbose",
    pharos_exec_path = "./pharos"
  )

  expect_identical(
    seen$args,
    list(
      model = "run001.mod",
      overwrite = TRUE,
      dry_run = "dry",
      run_in_output_dir = "in-out",
      ncpu = 7,
      partition = "part",
      clean_level = 3,
      parafile = "para.pnm",
      template = "tmpl.sh",
      account = "acct",
      verbose = "verbose",
      pharos_exe_path = normalizePath(exe)
    )
  )
  expect_identical(names(seen$args), rust_args)
})

test_that("submit_model_to_sge() passes every argument to Rust by name", {
  skip_on_os("windows")
  bundled <- local_fake_pharos()
  local_empty_path()
  local_bundled(bundled)
  rust_args <- names(formals(.submit_model_to_sge))
  seen <- local_capture_rust(".submit_model_to_sge")

  # pharos_exec_path left NULL: the bundled binary is what reaches Rust.
  submit_model_to_sge(
    "run001.mod",
    overwrite = TRUE,
    dry_run = "dry",
    run_in_output_dir = "in-out",
    ncpu = 7,
    clean_level = 3,
    parafile = "para.pnm",
    template = "tmpl.sh",
    verbose = "verbose"
  )

  expect_identical(
    seen$args,
    list(
      model = "run001.mod",
      overwrite = TRUE,
      dry_run = "dry",
      run_in_output_dir = "in-out",
      ncpu = 7,
      clean_level = 3,
      parafile = "para.pnm",
      template = "tmpl.sh",
      verbose = "verbose",
      pharos_exe_path = bundled
    )
  )
  expect_identical(names(seen$args), rust_args)
})

test_that("submission errors before calling Rust when no pharos resolves", {
  local_empty_path()
  local_no_bundled()
  called <- FALSE
  local_mocked_bindings(
    .submit_model_to_slurm = function(...) called <<- TRUE,
    .submit_model_to_sge = function(...) called <<- TRUE
  )

  expect_error(submit_model_to_slurm("run001.mod"), "No pharos CLI found")
  expect_error(submit_model_to_sge("run001.mod"), "No pharos CLI found")
  expect_false(called)
})
