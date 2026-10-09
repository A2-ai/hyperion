# Custom installation of the compiled code ("Writing R Extensions" 1.1.5).
# R runs this from `src/` instead of its default step, so it must install the
# shared library itself as well as the bundled pharos CLI.

# --- shared library (R's default behaviour) ---------------------------------
libs <- file.path(R_PACKAGE_DIR, paste0("libs", R_ARCH))
dir.create(libs, recursive = TRUE, showWarnings = FALSE)
lib <- paste0("hyperion", SHLIB_EXT)
file.copy(lib, libs, overwrite = TRUE)
# R's default installer sets 0755 regardless of umask, keeping a shared site
# library readable by other users.
if (!WINDOWS) {
  Sys.chmod(file.path(libs, lib), mode = "0755", use_umask = FALSE)
}
# written by R CMD INSTALL for R CMD check's native-symbol scan
if (file.exists("symbols.rds")) {
  file.copy("symbols.rds", libs, overwrite = TRUE)
}

# --- bundled pharos CLI -----------------------------------------------------
# Makevars copies the binary to `src/` unless tools/config.R wrote the
# `pharos-cli-skipped` marker, so a missing binary without the marker means
# the build failed to produce it.
exe <- if (WINDOWS) "pharos.exe" else "pharos"
if (file.exists(exe)) {
  bin <- file.path(R_PACKAGE_DIR, paste0("bin", R_ARCH))
  dir.create(bin, recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(exe, bin, overwrite = TRUE)) {
    stop("failed to copy the pharos CLI into ", bin, call. = FALSE)
  }
  installed <- file.path(bin, exe)
  Sys.chmod(installed, mode = "0755", use_umask = FALSE)

  # the CLI must be the pharos release the package links against
  desc_path <- file.path(R_PACKAGE_DIR, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    desc_path <- file.path("..", "DESCRIPTION")
  }
  expected <- unname(read.dcf(desc_path, fields = "Config/PharosVersion")[1, 1])
  if (is.na(expected) || !nzchar(expected)) {
    stop("DESCRIPTION has no Config/PharosVersion field", call. = FALSE)
  }

  reported <- tryCatch(
    suppressWarnings(system2(installed, "--version", stdout = TRUE, stderr = TRUE)),
    error = function(e) structure(conditionMessage(e), status = -1L)
  )
  # system2() only sets "status" on a non-zero exit
  status <- attr(reported, "status")
  reported <- paste(reported, collapse = "\n")
  if (!is.null(status) && status != 0) {
    stop(
      "the bundled pharos CLI failed `--version` (exit status ", status,
      "): ", reported,
      call. = FALSE
    )
  }
  # exact token match so that "0.6.1" does not accept "0.6.10"
  tokens <- strsplit(trimws(reported), "[[:space:]]+")[[1]]
  if (!any(tokens %in% c(expected, paste0("v", expected)))) {
    stop(
      "the bundled pharos CLI reports version '", reported, "' but ",
      "Config/PharosVersion is '", expected, "'.",
      call. = FALSE
    )
  }
  message("Installed the pharos CLI (", reported, ") to ", installed)
} else if (!file.exists("pharos-cli-skipped")) {
  stop(
    "pharos CLI was not built: expected `src/", exe, "` because configure ",
    "did not skip it. Set HYPERION_SKIP_PHAROS_CLI=true to install without it.",
    call. = FALSE
  )
}
