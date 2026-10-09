#' Locate the pharos CLI used for run submission
#'
#' Returns the absolute path of the `pharos` executable that
#' [submit_model_to_slurm()] and [submit_model_to_sge()] write into job
#' scripts. On Linux, hyperion builds pharos from the same pinned release as
#' the R package and installs it with the package, so the CLI always matches
#' hyperion's version.
#'
#' @section Choosing a pharos executable:
#' Set the option `hyperion.pharos_exec_path` to choose which pharos
#' executable is used. It is unset by default.
#'   * Unset (`NULL`, the default): the pharos bundled with hyperion, then
#'     `pharos` on `PATH`.
#'   * `"pharos"`: only `pharos` on `PATH`.
#'   * Any other string: a path to a pharos executable, absolute or relative
#'     to the R working directory when the path is resolved.
#'
#' The bundled CLI is built by default on Linux only, because the pharos CLI
#' is only needed for run submission on Linux clusters. Elsewhere, set
#' `options(hyperion.pharos_exec_path = "path/to/pharos")` or reinstall
#' hyperion with the environment variable `HYPERION_SKIP_PHAROS_CLI=false` to
#' build it.
#'
#' The path is written into job scripts, so it must be readable from the
#' compute nodes. This holds for the bundled CLI when the R library is on
#' shared storage.
#'
#' @return The absolute path to the pharos executable, as a single string.
#'   Errors if no pharos executable can be found.
#' @export
#'
#' @examples \dontrun{
#' pharos_path()
#'
#' # Only use pharos on PATH
#' options(hyperion.pharos_exec_path = "pharos")
#' pharos_path()
#'
#' # Use a specific pharos executable
#' options(hyperion.pharos_exec_path = "~/bin/pharos")
#' pharos_path()
#' }
pharos_path <- function() {
  resolve_pharos()$path
}

#' Version of the pharos CLI used for run submission
#'
#' Runs `pharos --version` on the executable [pharos_path()] returns and parses
#' the version it reports. Use it to check that the CLI is new enough for a
#' feature, or that it matches the version hyperion was built against.
#'
#' The executable is chosen the same way as for [pharos_path()], including the
#' `hyperion.pharos_exec_path` option.
#'
#' @return The version as a [numeric_version()], e.g.
#'   `numeric_version("0.6.1")`. Errors if no pharos executable can be found,
#'   if `pharos --version` fails, or if it prints no recognisable version.
#' @export
#'
#' @examples \dontrun{
#' pharos_version()
#' pharos_version() >= "0.6.1"
#' }
pharos_version <- function() {
  # Resolve first: read_pharos_version()'s error handler would otherwise
  # re-force a failed `path` promise and add a "restarting interrupted
  # promise evaluation" warning to W1's error.
  path <- resolve_pharos()$path
  numeric_version(read_pharos_version(path))
}

#' Resolve the pharos executable
#'
#' Used by [pharos_path()], `detect_pharos()` and the submit functions. The
#' override comes from the `hyperion.pharos_exec_path` option. The first
#' matching rule wins:
#'
#' 1. `"pharos"`: `pharos` on `PATH`, or an error.
#' 2. Any other string: that file, absolute or relative to [getwd()].
#' 3. Unset: the bundled pharos, if hyperion was installed with one.
#' 4. Unset: `pharos` on `PATH`.
#' 5. Otherwise, an error explaining why there is no bundled CLI.
#'
#' The path is always absolute because job scripts run in another working
#' directory on a compute node.
#'
#' @return A list with `path` and `source`: `"override"` (rule 2),
#'   `"bundled"` (rule 3) or `"path"` (rules 1 and 4).
#' @keywords internal
#' @noRd
resolve_pharos <- function() {
  pharos_exec_path <- getOption("hyperion.pharos_exec_path")
  if (!is.null(pharos_exec_path)) {
    if (!rlang::is_string(pharos_exec_path) || !nzchar(pharos_exec_path)) {
      cli::cli_abort(
        "The {.code hyperion.pharos_exec_path} option must be unset ({.code NULL}) or a single non-empty string."
      )
    }

    # Rule 1: PATH only, never the bundled binary.
    if (identical(pharos_exec_path, "pharos")) {
      path <- unname(Sys.which("pharos"))
      if (!nzchar(path)) {
        cli::cli_abort(c(
          "The {.code hyperion.pharos_exec_path} option is {.val pharos}, but no {.code pharos} executable was found on {.envvar PATH}.",
          "i" = "Unset it with {.code options(hyperion.pharos_exec_path = NULL)} for the pharos bundled with hyperion, or set it to the full path of a pharos executable."
        ))
      }
      return(list(path = absolute_path(path), source = "path"))
    }

    # Rule 2: resolve a relative path against getwd() now, not on the node.
    path <- tryCatch(
      normalizePath(pharos_exec_path, mustWork = TRUE),
      error = function(e) NULL
    )
    if (is.null(path)) {
      cli::cli_abort(c(
        "The {.code hyperion.pharos_exec_path} option {.path {pharos_exec_path}} does not exist.",
        "i" = "Relative paths are resolved against the working directory {.path {getwd()}}."
      ))
    }
    if (!is_executable(path)) {
      cli::cli_abort(
        "The {.code hyperion.pharos_exec_path} option {.path {path}} is not an executable file."
      )
    }
    return(list(path = path, source = "override"))
  }

  # Rule 3: the bundled CLI.
  bundled <- bundled_pharos_path()
  if (nzchar(bundled)) {
    if (!is_executable(bundled)) {
      cli::cli_abort(c(
        "The pharos CLI bundled with hyperion at {.path {bundled}} is not executable.",
        "i" = "Reinstall hyperion, or set {.code options(hyperion.pharos_exec_path = \"path/to/pharos\")}."
      ))
    }
    return(list(path = absolute_path(bundled), source = "bundled"))
  }

  # Rule 4: PATH.
  path <- unname(Sys.which("pharos"))
  if (nzchar(path)) {
    return(list(path = absolute_path(path), source = "path"))
  }

  # Rule 5: a missing bundled CLI is the surprising part on macOS and Windows.
  cli::cli_abort(c(
    "No pharos CLI found: hyperion has no bundled pharos and none is on {.envvar PATH}.",
    "i" = "The pharos CLI is only needed for run submission on Linux clusters, so hyperion builds it by default on Linux only.",
    "*" = "Set {.code options(hyperion.pharos_exec_path = \"path/to/pharos\")}, or",
    "*" = "reinstall hyperion with the environment variable {.envvar HYPERION_SKIP_PHAROS_CLI=false} to build the bundled CLI."
  ))
}

#' Run `pharos --version` and parse the version
#'
#' Shared by [pharos_version()], which surfaces the error, and
#' `detect_pharos()`, which catches it.
#'
#' @param path Absolute path to a pharos executable.
#' @return The version as a string without a leading `v`, e.g. `"0.6.1"`.
#' @keywords internal
#' @noRd
read_pharos_version <- function(path) {
  out <- tryCatch(
    suppressWarnings(system2(path, "--version", stdout = TRUE, stderr = TRUE)),
    error = function(e) {
      cli::cli_abort(
        "Could not run {.code {path} --version}: {conditionMessage(e)}"
      )
    }
  )
  # system2() sets a status attribute only for a non-zero exit.
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    cli::cli_abort(c(
      "{.code {path} --version} exited with status {status}.",
      if (length(out)) c("i" = "Output: {.val {out}}")
    ))
  }

  # pharos prints "pharos 0.6.1"; accept a "v" prefix like install.libs.R.
  # stderr is merged in, so a warning naming another library's version can
  # come first: prefer the line that starts with "pharos", and only fall back
  # to the first version anywhere when no such line has one.
  pattern <- "\\bv?([0-9]+\\.[0-9]+\\.[0-9]+)\\b"
  pharos_lines <- out[grepl("^pharos", trimws(out), ignore.case = TRUE)]
  m <- Filter(
    length,
    regmatches(pharos_lines, regexec(pattern, pharos_lines, perl = TRUE))
  )
  if (!length(m)) {
    m <- Filter(length, regmatches(out, regexec(pattern, out, perl = TRUE)))
  }
  if (!length(m)) {
    cli::cli_abort(c(
      "{.code {path} --version} printed no version number.",
      "i" = "Output: {.val {out}}"
    ))
  }
  m[[1]][[2]]
}

#' Path of the pharos CLI installed with hyperion
#'
#' A separate function so tests can mock it. The arguments let tests
#' exercise other platforms and a fake installed library.
#'
#' @return The path, or `""` when hyperion was installed without the CLI.
#' @keywords internal
#' @noRd
bundled_pharos_path <- function(
  r_arch = .Platform$r_arch,
  os_type = .Platform$OS.type,
  lib.loc = NULL
) {
  system.file(
    bundled_pharos_relpath(r_arch, os_type),
    package = "hyperion",
    lib.loc = lib.loc
  )
}

# Must match install.libs.R, which copies the CLI to "bin" + R_ARCH. R_ARCH
# has a leading slash ("/x64") but r_arch does not, hence file.path().
bundled_pharos_relpath <- function(r_arch, os_type) {
  exe <- if (identical(os_type, "windows")) "pharos.exe" else "pharos"
  if (nzchar(r_arch)) {
    file.path("bin", r_arch, exe)
  } else {
    file.path("bin", exe)
  }
}

# file.access() reports directories with the search bit as executable.
is_executable <- function(path) {
  file.access(path, 1) == 0 && !dir.exists(path)
}

# Leave absolute paths alone rather than normalizePath() them: resolving a
# symlink on shared storage can give a target that compute nodes do not mount
# under the same name. Only a relative path (e.g. "." on PATH) is normalised.
absolute_path <- function(path) {
  if (grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", path)) {
    path
  } else {
    normalizePath(path, mustWork = TRUE)
  }
}
