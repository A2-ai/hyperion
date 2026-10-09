# Fake pharos executables in temp dirs, PATH pointed at them, and a mocked
# bundled_pharos_path() exercise every rule without an installed CLI.

# A shell script standing in for pharos. `--version` prints
# "pharos <version>" and exits with `status`.
local_fake_pharos <- function(
  version = "0.6.1",
  status = 0,
  env = parent.frame()
) {
  dir <- withr::local_tempdir(.local_envir = env)
  path <- file.path(dir, "pharos")
  writeLines(
    c(
      "#!/bin/sh",
      paste0("echo \"pharos ", version, "\""),
      paste0("exit ", status)
    ),
    path
  )
  Sys.chmod(path, "0755")
  path
}

# Sets the hyperion.pharos_exec_path option for the calling test.
local_exec_path_option <- function(value, env = parent.frame()) {
  withr::local_options(hyperion.pharos_exec_path = value, .local_envir = env)
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

# The option is unset by default; tests that need it set it themselves.
withr::local_options(
  hyperion.pharos_exec_path = NULL,
  .local_envir = testthat::teardown_env()
)

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

  local_exec_path_option("pharos")

  res <- resolve_pharos()

  expect_identical(normalizePath(res$path), normalizePath(on_path))
  expect_identical(res$source, "path")
})

test_that("rule 1: 'pharos' errors naming the option when not on PATH", {
  skip_on_os("windows")
  local_empty_path()
  local_bundled(local_fake_pharos())
  local_exec_path_option("pharos")

  expect_error(resolve_pharos(), "hyperion.pharos_exec_path.*found on")
})

# -- Rule 2: any other value is a path ---------------------------------------

test_that("rule 2: an absolute path is used as an override", {
  skip_on_os("windows")
  exe <- local_fake_pharos()
  local_empty_path()
  local_bundled(local_fake_pharos())

  local_exec_path_option(exe)

  res <- resolve_pharos()

  expect_identical(res$path, normalizePath(exe))
  expect_identical(res$source, "override")
})

test_that("rule 2: a relative path is resolved against getwd() at call time", {
  skip_on_os("windows")
  exe <- local_fake_pharos()
  local_no_bundled()
  withr::local_dir(dirname(dirname(exe)))
  local_exec_path_option(file.path(basename(dirname(exe)), "pharos"))

  res <- resolve_pharos()

  expect_identical(res$path, normalizePath(exe))
  expect_true(startsWith(res$path, "/"))
  expect_identical(res$source, "override")
})

test_that("rule 2: a missing path errors", {
  local_no_bundled()
  missing <- file.path(withr::local_tempdir(), "no-such-pharos")

  local_exec_path_option(missing)

  expect_error(resolve_pharos(), "hyperion.pharos_exec_path.*does not exist")
})

test_that("rule 2: a file that is not executable errors", {
  skip_on_os("windows")
  local_no_bundled()
  exe <- local_fake_pharos()
  Sys.chmod(exe, "0644")
  local_exec_path_option(exe)

  expect_error(resolve_pharos(), "not an executable")
})

test_that("rule 2: a directory errors", {
  skip_on_os("windows")
  local_no_bundled()

  local_exec_path_option(withr::local_tempdir())

  expect_error(resolve_pharos(), "not an executable")
})

test_that("the option must be unset or a single non-empty string", {
  for (bad in list(c("a", "b"), NA_character_, "", 1)) {
    local({
      local_exec_path_option(bad)
      expect_error(resolve_pharos(), "hyperion.pharos_exec_path.*single")
    })
  }
})

test_that("resolve_pharos() takes no arguments", {
  # The override is only ever the option, so there is no argument to bypass it.
  expect_identical(formals(resolve_pharos), NULL)
})

test_that("resolve_pharos() reads the option rather than ignoring it", {
  skip_on_os("windows")
  exe <- local_fake_pharos()
  local_bundled(local_fake_pharos())
  local_empty_path()

  expect_identical(resolve_pharos()$source, "bundled")
  local_exec_path_option(exe)
  expect_identical(
    resolve_pharos(),
    list(path = normalizePath(exe), source = "override")
  )
})

# -- Rules 3 and 4: unset means bundled, then PATH ---------------------------

test_that("rule 3: unset prefers the bundled binary over PATH", {
  skip_on_os("windows")
  bundled <- local_fake_pharos("0.6.1")
  on_path <- local_fake_pharos("0.5.0")
  withr::local_envvar(PATH = dirname(on_path))
  local_bundled(bundled)

  res <- resolve_pharos()

  expect_identical(res$path, bundled)
  expect_identical(res$source, "bundled")
})

test_that("rule 4: unset falls back to PATH without a bundled binary", {
  skip_on_os("windows")
  on_path <- local_fake_pharos()
  withr::local_envvar(PATH = dirname(on_path))
  local_no_bundled()

  res <- resolve_pharos()

  expect_identical(normalizePath(res$path), normalizePath(on_path))
  expect_true(startsWith(res$path, "/"))
  expect_identical(res$source, "path")
})

# -- Rule 5: nothing found ----------------------------------------------------

test_that("rule 5: the not-found error gives the reason and both remedies", {
  local_empty_path()
  local_no_bundled()

  err <- tryCatch(resolve_pharos(), error = function(e) e)

  expect_s3_class(err, "error")
  msg <- conditionMessage(err)
  expect_match(msg, "only needed for run submission on Linux clusters")
  expect_match(msg, "options(hyperion.pharos_exec_path = ", fixed = TRUE)
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
  local_exec_path_option(override)
  expect_identical(pharos_path(), normalizePath(override))
})

test_that("pharos_path() takes no arguments", {
  expect_identical(formals(pharos_path), NULL)
})

test_that("pharos_path() errors when nothing resolves", {
  local_empty_path()
  local_no_bundled()

  expect_error(pharos_path(), "No pharos CLI found")
})

# -- pharos_version() ---------------------------------------------------------

test_that("pharos_version() takes no arguments", {
  expect_identical(formals(pharos_version), NULL)
})

test_that("pharos_version() parses 'pharos 0.6.1' as a numeric_version", {
  skip_on_os("windows")
  local_empty_path()
  local_bundled(local_fake_pharos("0.6.1"))

  v <- pharos_version()

  expect_s3_class(v, "numeric_version")
  expect_identical(v, numeric_version("0.6.1"))
  expect_true(v >= "0.6.1")
  expect_false(v >= "0.6.2")
})

test_that("pharos_version() accepts a 'v' prefix", {
  skip_on_os("windows")
  local_empty_path()
  local_bundled(local_fake_pharos("v0.12.3"))

  expect_identical(pharos_version(), numeric_version("0.12.3"))
})

test_that("pharos_version() uses the executable the option selects", {
  skip_on_os("windows")
  local_empty_path()
  local_bundled(local_fake_pharos("0.6.1"))
  local_exec_path_option(local_fake_pharos("0.5.0"))

  expect_identical(pharos_version(), numeric_version("0.5.0"))
})

test_that("pharos_version() prefers the 'pharos' line over earlier output", {
  skip_on_os("windows")
  # stderr is merged into the output, so a warning can come before the
  # version line and carry a version number of its own.
  dir <- withr::local_tempdir()
  exe <- file.path(dir, "pharos")
  writeLines(
    c(
      "#!/bin/sh",
      "echo \"warning: libfoo 1.2.3 deprecated\" >&2",
      "echo \"pharos 0.6.1\""
    ),
    exe
  )
  Sys.chmod(exe, "0755")
  local_empty_path()
  local_bundled(exe)

  expect_identical(pharos_version(), numeric_version("0.6.1"))
})

test_that("pharos_version() falls back to the first version anywhere", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  exe <- file.path(dir, "pharos")
  writeLines(c("#!/bin/sh", "echo \"build v0.7.0 (abc123)\""), exe)
  Sys.chmod(exe, "0755")
  local_empty_path()
  local_bundled(exe)

  expect_identical(pharos_version(), numeric_version("0.7.0"))
})

test_that("pharos_version() errors when no version is printed", {
  skip_on_os("windows")
  local_empty_path()
  local_bundled(local_fake_pharos("unknown"))

  expect_error(pharos_version(), "printed no version number")
})

test_that("pharos_version() errors when --version exits non-zero", {
  skip_on_os("windows")
  local_empty_path()
  local_bundled(local_fake_pharos("0.6.1", status = 2))

  expect_error(pharos_version(), "exited with status 2")
})

test_that("pharos_version() errors with W1's message when nothing resolves", {
  local_empty_path()
  local_no_bundled()

  # Just the error: no "restarting interrupted promise evaluation" warning.
  expect_no_warning(expect_error(pharos_version(), "No pharos CLI found"))
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

test_that("detect_pharos() reports source 'override' when the option is set", {
  skip_on_os("windows")
  exe <- local_fake_pharos("0.5.0")
  local_empty_path()
  local_bundled(local_fake_pharos("0.6.1"))
  local_exec_path_option(exe)

  expect_identical(
    detect_pharos(),
    list(path = normalizePath(exe), version = "0.5.0", source = "override")
  )
})

test_that("detect_pharos() never errors on an unusable option value", {
  local_bundled("")
  local_exec_path_option(file.path(withr::local_tempdir(), "no-such-pharos"))

  expect_identical(
    expect_no_error(detect_pharos()),
    list(path = NA_character_, version = NA_character_, source = NA_character_)
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

test_that("detect_pharos() keeps the path when --version exits non-zero", {
  skip_on_os("windows")
  bundled <- local_fake_pharos("0.6.1", status = 1)
  local_empty_path()
  local_bundled(bundled)

  res <- expect_no_error(detect_pharos())
  expect_identical(res$path, bundled)
  expect_identical(res$version, NA_character_)
})

test_that("detect_pharos() parses a 'v' prefixed version", {
  skip_on_os("windows")
  bundled <- local_fake_pharos("v0.6.1")
  local_empty_path()
  local_bundled(bundled)

  expect_identical(detect_pharos()$version, "0.6.1")
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

test_that("the attach message labels a pharos chosen by the option", {
  expected <- expected_pharos_version()
  local_mocked_bindings(
    detect_pharos = function() {
      list(path = "/opt/pharos", version = expected, source = "override")
    }
  )

  msg <- cli::ansi_strip(hyperion_options_message())

  expect_match(msg, "option: /opt/pharos", fixed = TRUE)
})

# The start-up message itself, as library(hyperion) prints it.
startup_message <- function() {
  msgs <- testthat::capture_messages(.onAttach("", "hyperion"))
  cli::ansi_strip(paste(msgs, collapse = ""))
}

# The line directly after the "pharos CLI" line.
line_after_cli <- function(msg) {
  lines <- strsplit(msg, "\n", fixed = TRUE)[[1]]
  i <- grep("pharos CLI", lines, fixed = TRUE)
  expect_length(i, 1)
  lines[[i + 1]]
}

test_that("the start-up message shows hyperion.pharos_exec_path unset", {
  expected <- expected_pharos_version()
  local_mocked_bindings(
    detect_pharos = function() {
      list(path = "/lib/hyperion/bin/pharos", version = expected, source = "bundled")
    }
  )

  expect_identical(
    line_after_cli(startup_message()),
    "    \u2514 hyperion.pharos_exec_path : (unset)"
  )
})

test_that("the start-up message shows hyperion.pharos_exec_path when set", {
  expected <- expected_pharos_version()
  local_exec_path_option("/opt/pharos")
  local_mocked_bindings(
    detect_pharos = function() {
      list(path = "/opt/pharos", version = expected, source = "override")
    }
  )

  msg <- startup_message()

  expect_identical(
    line_after_cli(msg),
    "    \u2514 hyperion.pharos_exec_path : /opt/pharos"
  )
  expect_no_match(msg, "hyperion.pharos_exec_path : (unset)", fixed = TRUE)
})

test_that("the start-up message shows the option in every pharos CLI state", {
  expected_pharos_version()
  local_exec_path_option("/opt/pharos")
  states <- list(
    not_found = list(path = NA_character_, version = NA_character_, source = NA_character_),
    no_version = list(path = "/opt/pharos", version = NA_character_, source = "override"),
    mismatch = list(path = "/opt/pharos", version = "0.0.1", source = "override")
  )
  for (state in states) {
    local({
      local_mocked_bindings(detect_pharos = function() state)
      expect_identical(
        line_after_cli(startup_message()),
        "    \u2514 hyperion.pharos_exec_path : /opt/pharos"
      )
    })
  }
})

test_that("the start-up message blames the option when it is set but unusable", {
  local_bundled("")
  local_exec_path_option("/nope/pharos")

  msg <- startup_message()

  expect_match(
    msg,
    "pharos CLI not usable at hyperion.pharos_exec_path",
    fixed = TRUE
  )
  expect_no_match(msg, "not bundled with hyperion", fixed = TRUE)
  expect_identical(
    line_after_cli(msg),
    "    \u2514 hyperion.pharos_exec_path : /nope/pharos"
  )
})

test_that("the start-up message says not bundled, not on PATH when unset", {
  local_empty_path()
  local_no_bundled()

  msg <- startup_message()

  expect_match(
    msg,
    "pharos CLI not found (not bundled with hyperion, not on PATH)",
    fixed = TRUE
  )
  expect_no_match(msg, "not usable at", fixed = TRUE)
})

test_that("the start-up message quotes an empty-string option", {
  local_exec_path_option("")

  msg <- startup_message()

  expect_identical(
    line_after_cli(msg),
    "    \u2514 hyperion.pharos_exec_path : \"\""
  )
  expect_match(msg, "not usable at hyperion.pharos_exec_path", fixed = TRUE)
})

test_that("the start-up message never fails on an unformattable option", {
  # library(hyperion) must not error whatever the option holds.
  cases <- list(
    list(value = sum, shown = "<function>"),
    list(value = new.env(), shown = "<environment>")
  )
  for (case in cases) {
    local({
      local_exec_path_option(case$value)
      msg <- expect_no_error(startup_message())
      expect_identical(
        line_after_cli(msg),
        paste0("    \u2514 hyperion.pharos_exec_path : ", case$shown)
      )
    })
  }
})

# -- Submission hands the resolved path to Rust ------------------------------

test_that("submit wrappers keep exactly their 0.6.0 signatures", {
  # Copied from the 0.6.0 (ef9c2ea4) extendr wrappers. The pharos override is
  # an option, so no argument may be added, removed, reordered or re-defaulted.
  baseline_slurm <- function(
    model,
    overwrite = FALSE,
    dry_run = FALSE,
    run_in_output_dir = FALSE,
    ncpu = 1,
    partition = NULL,
    clean_level = 1,
    parafile = NULL,
    template = NULL,
    account = NULL,
    verbose = FALSE
  ) NULL
  baseline_sge <- function(
    model,
    overwrite = FALSE,
    dry_run = FALSE,
    run_in_output_dir = FALSE,
    ncpu = 1,
    clean_level = 1,
    parafile = NULL,
    template = NULL,
    verbose = FALSE
  ) NULL

  expect_identical(formals(submit_model_to_slurm), formals(baseline_slurm))
  expect_identical(formals(submit_model_to_sge), formals(baseline_sge))
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
  local_exec_path_option("./pharos")

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

  # Option unset: the bundled binary is what reaches Rust.
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

  # Option set: the executable it names is what reaches Rust.
  override <- local_fake_pharos()
  local_exec_path_option(override)
  submit_model_to_sge("run001.mod")
  expect_identical(seen$args$pharos_exe_path, normalizePath(override))
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
