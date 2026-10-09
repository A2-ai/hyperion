#' Locate the pharos CLI used for run submission
#'
#' Returns the absolute path of the `pharos` executable that
#' [submit_model_to_slurm()] and [submit_model_to_sge()] write into job
#' scripts. On Linux, hyperion builds pharos from the same pinned release as
#' the R package and installs it with the package, so the CLI always matches
#' hyperion's version.
#'
#' @param pharos_exec_path Which pharos executable to use.
#'   * `NULL` (the default): the pharos bundled with hyperion, then `pharos`
#'     on `PATH`.
#'   * `"pharos"`: only `pharos` on `PATH`.
#'   * Any other string: a path to a pharos executable, absolute or relative
#'     to the R working directory.
#'
#'   The bundled CLI is built by default on Linux only, because the pharos CLI
#'   is only needed for run submission on Linux clusters. Elsewhere, pass a
#'   path here or reinstall hyperion with the environment variable
#'   `HYPERION_SKIP_PHAROS_CLI=false` to build it.
#'
#' @return The absolute path to the pharos executable, as a single string.
#'   Errors if no pharos executable can be found.
#' @export
#'
#' @examples \dontrun{
#' pharos_path()
#' pharos_path("pharos")
#' pharos_path("~/bin/pharos")
#' }
pharos_path <- function(pharos_exec_path = NULL) {
  resolve_pharos(pharos_exec_path)$path
}

#' Resolve the pharos executable
#'
#' Used by [pharos_path()], `detect_pharos()` and the submit functions. The
#' first matching rule wins:
#'
#' 1. `"pharos"`: `pharos` on `PATH`, or an error.
#' 2. Any other string: that file, absolute or relative to [getwd()].
#' 3. `NULL`: the bundled pharos, if hyperion was installed with one.
#' 4. `NULL`: `pharos` on `PATH`.
#' 5. Otherwise, an error explaining why there is no bundled CLI.
#'
#' The path is always absolute because job scripts run in another working
#' directory on a compute node.
#'
#' @return A list with `path` and `source`: `"override"` (rule 2),
#'   `"bundled"` (rule 3) or `"path"` (rules 1 and 4).
#' @keywords internal
#' @noRd
resolve_pharos <- function(pharos_exec_path = NULL) {
  if (!is.null(pharos_exec_path)) {
    if (!rlang::is_string(pharos_exec_path) || !nzchar(pharos_exec_path)) {
      cli::cli_abort(
        "{.arg pharos_exec_path} must be {.code NULL} or a single non-empty string."
      )
    }

    # Rule 1: PATH only, never the bundled binary.
    if (identical(pharos_exec_path, "pharos")) {
      path <- unname(Sys.which("pharos"))
      if (!nzchar(path)) {
        cli::cli_abort(c(
          "{.arg pharos_exec_path} is {.val pharos}, but no {.code pharos} executable was found on {.envvar PATH}.",
          "i" = "Use {.code pharos_exec_path = NULL} for the pharos bundled with hyperion, or pass the full path to a pharos executable."
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
        "{.arg pharos_exec_path} {.path {pharos_exec_path}} does not exist.",
        "i" = "Relative paths are resolved against the working directory {.path {getwd()}}."
      ))
    }
    if (!is_executable(path)) {
      cli::cli_abort(
        "{.arg pharos_exec_path} {.path {path}} is not an executable file."
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
        "i" = "Reinstall hyperion, or pass {.arg pharos_exec_path} with the path to a pharos executable."
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
    "*" = "Pass {.arg pharos_exec_path} with the path to a pharos executable, or",
    "*" = "reinstall hyperion with the environment variable {.envvar HYPERION_SKIP_PHAROS_CLI=false} to build the bundled CLI."
  ))
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
