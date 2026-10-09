# Bundling the pharos CLI in hyperion — Spec v1

This spec is the implementation authority. Implementers follow it and do not
redesign. Later decisions are appended as new numbered sections that supersede
earlier text explicitly.

Installing hyperion on Linux compiles the pharos CLI from the same pinned pharos
tag and installs it at `<lib>/hyperion/bin/pharos`. Slurm/SGE submission, which
is the CLI's only consumer, resolves the executable through one R resolver.

## §0 Diagnosis

hyperion links pharos v0.6.0 as library crates, but the pharos CLI is a separate
install that nothing keeps at the same version.

- **No CLI on most platforms.** pharos publishes only
  `pharos_0.6.0_linux_amd64.tar.gz` via `curl | bash` to `~/.local/bin`.
- **Version skew is undetectable until it bites.** hyperion's Rust code is
  pinned to pharos tag v0.6.0 (`Config/PharosVersion`). The `pharos` on PATH is
  whatever the user last installed. `detect_pharos()` (`R/utils.R:408`) can only
  report a mismatch.
- **Failing case:** a user writes a model config with hyperion 0.6.0 (pharos
  0.6.0 semantics) and submits to slurm. The scheduler finds a PATH pharos 0.5.x
  via `which("pharos")` (`src/rust/scheduler/src/lib.rs:181`, `:275`) and writes
  it into the job script. The CLI and the R package disagree about the same file.
- **Why it can't just be a dependency:** pharos's root crate is the CLI and has
  no `lib.rs`. All logic lives in a ~1,086-line `src/main.rs`, and cargo never
  builds a dependency's binaries.
- **Why the current build drops it:** every install already compiles a bin
  (`document`), but `rust_clean` deletes `src/rust/target` before R installs
  anything except the shared library.

## D — Decisions

| ID | Decision | Note |
| --- | --- | --- |
| D1 | Ship a real native `pharos` binary, compiled from source during `R CMD INSTALL`. | Not a download, not an `Rscript` shim. |
| D2 | Refactor pharos upstream: move the `main.rs` logic into `src/lib.rs` exposing `pub fn run`, keep a 3-line `main.rs`, tag v0.6.1. No fork. | R5 |
| D3 | Add a workspace member `src/rust/pharos-cli` (package `hyperion-pharos-cli`, `[[bin]] name = "pharos"`) that depends only on `pharos`. | No extendr or libR-sys in its graph. |
| D4 | Build the staticlib and the CLI in ONE `cargo build` call with the same target dir and profile. | Shared crates compile once. |
| D5 | The CLI uses the existing release profile. Run an install-time diagnostic later if needed. | R6 |
| D6 | Install the binary with `src/install.libs.R` into `<lib>/hyperion/bin$(R_ARCH)/pharos`. | WRE-sanctioned; no `inst/bin`, no `exec/`. |
| D7 | One resolver (W1) finds the pharos executable for `pharos_path()`, `detect_pharos()` and slurm/SGE submission. | Replaces `which("pharos")` at `src/rust/scheduler/src/lib.rs:181` and `:275`. |
| D8 | hyperion never edits PATH, at install or at call time. Submission passes W1's absolute path to the scheduler, which already takes one. | R14 |
| D9 | Bundled CLI version = linked crate tag = `Config/PharosVersion`, always bumped together. | R12 |
| D10 | The CLI builds by default on Linux only; macOS and Windows skip it. The code states why: the CLI is only needed for run submission on Linux clusters (I10). | R7, R16 |
| D11 | pharos gets an MIT license in P1. | R10 |
| D12 | The CLI build is skipped when `HYPERION_SKIP_PHAROS_CLI` is true, or when it is unset and the platform is not Linux or the build is `devtools::load_all()`. `HYPERION_SKIP_PHAROS_CLI=false` forces a build anywhere. | R8, R16 |
| D13 | `pharos_exec_path = NULL` is an argument on `submit_model_to_slurm()`, `submit_model_to_sge()` and `pharos_path()`. `NULL` means bundled binary, then PATH. `"pharos"` means PATH only. Any other value is a path relative to the R working dir, or absolute. No global option or env var. | R13, R15, R20 |

## M — Layout and types

**M1 — upstream `pharos/src/lib.rs`** (moved from `main.rs`; clap types become public):

```rust
pub use cli::{Cli, Commands};
pub fn run<I, T>(args: I) -> anyhow::Result<()>
where I: IntoIterator<Item = T>, T: Into<std::ffi::OsString> + Clone;
```

**M2 — upstream `pharos/src/main.rs`:**

```rust
fn main() {
    if let Err(e) = pharos::run(std::env::args_os()) {
        eprintln!("Error: {e:?}");
        std::process::exit(1);
    }
}
```

**M3 — `src/rust/Cargo.toml`:** add `"pharos-cli"` to `[workspace] members`. Add
`pharos = { git = "https://github.com/a2-ai/pharos", package = "pharos", tag = "v0.6.1" }`
to `[workspace.dependencies]`, and bump the other four pharos crates to the same tag.

**M4 — `src/rust/pharos-cli/Cargo.toml`:**

```toml
[package]
name = "hyperion-pharos-cli"
version = "0.1.0"
edition = "2024"
publish = false

[[bin]]
name = "pharos"
path = "main.rs"

[dependencies]
pharos = { workspace = true }
```

**M5 — `src/rust/pharos-cli/main.rs`:** byte-identical to M2.

**M6 — `src/install.libs.R`** (new; runs with `R_PACKAGE_DIR`, `R_ARCH`, `WINDOWS`, `SHLIB_EXT` in scope):

```r
libs <- file.path(R_PACKAGE_DIR, paste0("libs", R_ARCH))
dir.create(libs, recursive = TRUE, showWarnings = FALSE)
file.copy(paste0("hyperion", SHLIB_EXT), libs, overwrite = TRUE)

exe <- if (WINDOWS) "pharos.exe" else "pharos"
if (file.exists(exe)) {
  bin <- file.path(R_PACKAGE_DIR, paste0("bin", R_ARCH))
  dir.create(bin, recursive = TRUE, showWarnings = FALSE)
  file.copy(exe, bin, overwrite = TRUE)
  # I2: version check, fails install on mismatch
} else if (!file.exists("pharos-cli-skipped")) {
  stop("pharos CLI was not built")  # I6
}
```

**M7 — `detect_pharos()` return:** `list(path = chr, version = chr, source = c("override", "bundled", "path", NA))`.

## I — Invariants

| ID | Invariant | Enforced by |
| --- | --- | --- |
| I1 | The `hyperion-pharos-cli` dependency graph contains no `extendr-api` or `libR-sys`. | CI: `cargo tree -p hyperion-pharos-cli -i extendr-api` must find nothing. |
| I2 | `bin/pharos --version` reports `Config/PharosVersion`. | `install.libs.R` runs it and stops on mismatch. |
| I3 | Install writes only under `R_PACKAGE_DIR` (and the build's temp dirs). No PATH or shell rc edits, ever (D8). | Review; `R CMD check`. |
| I4 | The FFI crates and the CLI resolve to the same pharos commit (one `Cargo.lock`). | Existing `pharos-dependency-check.yaml`, extended to the `pharos` package. |
| I5 | The source tarball contains no binaries. `inst/bin` never exists. | `.Rbuildignore` + CI tarball scan. |
| I6 | A missing CLI fails the install unless D12 skipped the build. | `install.libs.R` reads the same skip decision configure made. |
| I7 | `pharos-cli/main.rs` contains no logic beyond calling `pharos::run`. | Review rule; drift goes upstream. |
| I8 | The job script always receives an absolute path to an existing, executable pharos. A relative `pharos_exec_path` is resolved against `getwd()` at call time. | W1: `normalizePath(mustWork = TRUE)` + `file.access(path, 1)`. |
| I9 | The resolved path must be readable from compute nodes. Documented only. | Holds when the R library is on shared storage (R19). |
| I10 | The reason for skipping ("the pharos CLI is only needed for run submission on Linux clusters") is stated where a user meets the skip. | The `tools/config.R` skip message, the W1 "not found" error, and the `pharos_exec_path` roxygen docs. |

## F — Build flow

1. **configure** (`tools/config.R`) makes the D12 skip decision, prints the I10
   reason when it skips, and fills a new `@CLI_TARGETS@` placeholder:
   `--bin pharos -p hyperion-pharos-cli`, or empty when skipped. It writes the
   decision to `src/pharos-cli-skipped` so `install.libs.R` can apply I6. A
   `load_all()` build is detected by `DEBUG`, which `tools/config.R` already
   reads (verify in P3 that pkgbuild sets it).
2. **Makevars `$(STATLIB)`:** `cargo build @CRAN_FLAGS@ --lib -p hyperion @CLI_TARGETS@ @PROFILE@ --manifest-path=./rust/Cargo.toml --target-dir $(TARGET_DIR) @TARGET@`
3. **Makevars** keeps running `cargo run --bin document` as today.
4. **Makevars:** `if [ -f $(LIBDIR)/pharos ]; then cp $(LIBDIR)/pharos ./pharos; fi`, before `rust_clean`.
5. **rust_clean** deletes the target dir as today. The copied binary survives in `src/`.
6. **`install.libs.R`** copies the shared library into `libs/` and `pharos` into `bin/`, then runs the I2 version check.
7. **`cleanup`** additionally runs `rm -f src/pharos src/pharos.exe src/pharos-cli-skipped`.

Step 2's target selection across two packages (`--lib` plus `--bin`) still needs
checking in P2. The fallback is two cargo calls on the same target dir, which
still reuse compiled dependencies.

## W — Executable resolution

**W1 — `resolve_pharos(pharos_exec_path = NULL)`** (internal). The first rule that matches wins:

1. `pharos_exec_path` is `"pharos"`: use `Sys.which("pharos")`. If that is empty, error naming the argument.
2. `pharos_exec_path` is any other non-NULL value: use `normalizePath(value, mustWork = TRUE)`, relative to `getwd()`. It must be executable; otherwise error.
3. `pharos_exec_path` is `NULL` and the bundled binary `system.file(paste0("bin", .Platform$r_arch), "pharos", package = "hyperion")` exists: use it.
4. `pharos_exec_path` is `NULL` and `Sys.which("pharos")` is non-empty: use it (R18).
5. Otherwise error: no pharos CLI found. The message gives the I10 reason and says how to pass `pharos_exec_path` or reinstall with `HYPERION_SKIP_PHAROS_CLI=false`.

Returns `list(path = <absolute>, source = "override" | "bundled" | "path")`.

**W2 — Submission.** The Rust functions become internal (`.submit_model_to_slurm`,
`.submit_model_to_sge`) and take `pharos_exe_path: String` instead of calling
`which`. Exported R wrappers in `R/submit.R` keep today's arguments, add
`pharos_exec_path = NULL`, call W1 once, and pass `path` through (R17). The
roxygen docs move from the Rust doc comments to `R/submit.R`.

## A — R API / C — CLI

- **A1** `pharos_path(pharos_exec_path = NULL)`, exported. Returns W1's `path` as `character(1)`. Errors with W1's message when nothing resolves.
- **A2** `detect_pharos()`, internal. Runs W1 with `NULL`, then `--version`. Returns M7 and never errors.
- **A3** The status display (`R/utils.R:488-577`) shows `source` and warns when the resolved binary's version differs from `Config/PharosVersion`.
- **A4** No `pharos_run()` and no `pharos_link()` in v1 (R9).
- **A5** `submit_model_to_slurm()` and `submit_model_to_sge()` keep every current argument and add `pharos_exec_path = NULL` as the last argument (W2).
- **C1** `pharos` commands, flags and exit codes are exactly upstream's at the pinned tag. hyperion adds no subcommands.
- **C2** Exit code is 1 on any error, with the message on stderr (from M2).

## P — Phases

| Phase | Scope | Files | Depends on |
| --- | --- | --- | --- |
| P1 | Upstream: add the MIT license, move `main.rs` to `lib.rs` + `run`, tag v0.6.1. | `a2-ai/pharos`: `src/lib.rs`, `src/main.rs`, `LICENSE`, `Cargo.toml` | — |
| P2 | Cargo: add the workspace member, bump the tag, prove the single-invocation build. | `src/rust/Cargo.toml`, `src/rust/Cargo.lock`, `src/rust/pharos-cli/**` | P1 |
| P3 | Build plumbing: the D12 skip decision, `@CLI_TARGETS@`, the copy step, `install.libs.R`, cleanup, ignore files. | `tools/config.R`, `src/Makevars*.in`, `src/install.libs.R`, `cleanup*`, `.gitignore`, `.Rbuildignore` | P2 |
| P4 | R side and submission: W1, `pharos_path()`, `detect_pharos()`, status display, the W2 hand-off, tests, NEWS, DESCRIPTION. | `R/utils.R`, `R/pharos-path.R`, `R/submit.R`, `src/rust/scheduler/src/lib.rs`, `R/extendr-wrappers.R`, `tests/testthat/**`, `NEWS.md`, `NAMESPACE`, `man/**` | P2 |
| P5 | CI: I1 tree check, I4 lock check, Linux release artifacts must contain `bin/pharos`. | `.github/workflows/**` | P3, P4 |

## Resolved questions

| ID | Question | Answer | Decided by |
| --- | --- | --- | --- |
| R1 | Native binary or an `Rscript` shim in `exec/`? | Native binary. A shim needs R on PATH, costs 150–300 ms per call, and does not work on Windows. | Research |
| R2 | `inst/bin`, `exec/`, or `install.libs.R`? | `install.libs.R`. A binary in `inst/bin` ships in the source tarball, and `exec/` is for scripts. | Research (WRE §1.1.5) |
| R3 | `cargo install --git ... --root` from configure? | Rejected: second full compile, separate lockfile, needs network. | Research |
| R4 | Can the CLI link the extendr lib? | No. extendr needs R symbols at link time, so the bin has its own crate (D3). | Research |
| R5 | Q1: upstream refactor or vendored fork? | Upstream refactor, no fork. | Wes |
| R6 | Q2: build profile for the CLI? | Same release profile; diagnostic later if needed. | Wes |
| R7 | Q3: Windows? | Out of scope for v1, skipped by default. The CLI is only needed for slurm submission, which can't be done from Windows. | Wes |
| R8 | Q4: opt-out env var? | Yes, and `devtools::load_all()` skips the CLI by default (D12). | Wes |
| R9 | Q5: how users reach the binary? | `pharos_path()` plus docs. No `pharos_run()` or `pharos_link()` in v1. | Wes |
| R10 | Q6: pharos license? | MIT. | Wes |
| R11 | Q7: CRAN as a target? | Not for now. No vendoring in this work. | Wes |
| R12 | Q8: does a pharos CLI fix force a hyperion release? | Yes. The two are linked going forward. | Wes |
| R13 | D7 note: override? | Add `pharos_exec_path` (D13). | Wes |
| R14 | D8 note: PATH changes? | Only call-local, if at all. Submission already passes an explicit path, so no PATH change is needed (D8). | Wes + code read |
| R15 | Q9: where does `pharos_exec_path` live? | An argument on the function calls, defaulting to `NULL` (D13). | Wes |
| R16 | Q10: macOS default? | Skip, as long as the code states the CLI is only needed for run submission on Linux clusters (D10, I10). | Wes |
| R17 | Q11: how the path reaches Rust? | Internal Rust functions take the path; exported R wrappers resolve it (W2). | Wes |
| R18 | Q12: PATH fallback when nothing else resolves? | Yes (W1 step 4). | Wes |
| R19 | Q13: compute-node visibility? | Document only (I9). | Wes |
| R20 | Does `pharos_path()` take the argument too; global option? | Yes to the argument; no global option or env var. Reversible. | Claude |

## Still open

None.

**Checks for implementation, not decisions:**

- P2: confirm that one `cargo build` can select `--lib -p hyperion` and `--bin pharos -p hyperion-pharos-cli` together. The fallback is two calls on one target dir.
- P3: confirm that `devtools::load_all()` sets `DEBUG`. If it doesn't, D12 needs another signal.

## §1 Resolutions from implementation (R21–R23)

These supersede earlier clauses where they conflict.

| ID | Decision | Supersedes | Decided by |
| --- | --- | --- | --- |
| R21 | During development, `src/rust/Cargo.toml` carries a `[patch."https://github.com/a2-ai/pharos"]` section pointing all five pharos crates (`pharos`, `nonmem`, `nonmem-parser`, `config`, `scheduler`) at the local checkout via relative paths (`../../../pharos`, `../../../pharos/components/<name>`). The `[workspace.dependencies]` entries keep their git + tag form. **Pre-merge gate:** before the hyperion branch merges, the patch section is removed, all pharos entries point at `tag = "v0.6.1"`, and `Cargo.lock` is regenerated so every pharos crate resolves from the same git commit (I4). | M3 (until the v0.6.1 tag exists) | Wes |
| R22 | New invariant **I11**: the merged tree contains no `[patch."https://github.com/a2-ai/pharos"]` section and no path-sourced pharos crate in `Cargo.lock`. Enforced by a CI check added in P5. While R21 is in effect, hyperion CI on the branch is expected to fail to build (the runners have no `../../../pharos`). | P5 scope (adds I11 check) | Wes + Claude |
| R23 | P1 keeps `license = "MIT"` on the five component crates and the new `tests/cli.rs` (beyond P1's file list). pharos is excluded from the final simplification pass, so the reviewed branch is final for tagging. | P1 file list | Claude (small call) |
| R24 | `load_all()` is detected by `DEBUG` **or** `DEVTOOLS_LOAD` (set by pkgload during `load_all`), because pkgbuild sets `DEBUG` only when its extra compiler flags are enabled. `DEVTOOLS_LOAD` affects only the CLI skip, not the cargo profile. | F1 ("detected by `DEBUG`") | Claude (small call, implements R8) |
| R25 | `install.libs.R` sets mode 0755 on the installed shared library (matching R's default installer); strip/dSYM handling is not replicated. The I2 check requires a zero exit status and an exact version token match (not a substring). | M6 | Claude (small call) |
| R26 | W1 step 3 must look in the same directory `install.libs.R` writes to: `bin` + `R_ARCH` at install time equals `file.path("bin", .Platform$r_arch)` when `r_arch` is non-empty, else `bin`. The spec's `paste0("bin", .Platform$r_arch)` was a typo. | W1 step 3 | Claude (small call) |
| R27 | The pharos change (P1) is **contingent on bundling working**. It is not committed, pushed or tagged until a full hyperion `R CMD INSTALL` against the local pharos checkout (R21) has built, installed and version-checked `bin/pharos`, and the P2 review is clean. If bundling proves too hard, the P1 branch is discarded and pharos stays unchanged. | P1 ordering ("P1 … blocks everything else") | Wes |
| R28 | F4's copy is also guarded on the skip marker being absent (`[ ! -f pharos-cli-skipped ] && [ -f $(LIBDIR)/pharos ]`), so a stale binary in a persistent DEBUG target dir is never re-copied after a skip. | F4 | Claude (small call) |
| R29 | On Windows the bundled lookup (W1 step 3) looks for `pharos.exe`, so a forced Windows build (D12) resolves. The submit wrappers pass named arguments to the internal Rust functions. `pharos_path` is listed in `_starlightr.toml`. | W1 step 3 (`"pharos"` literal) | Claude (small call) |
| R30 | The R21 dev patch uses **absolute** paths to the local pharos checkout (`/Users/wescummings/projects/pharos/...`), because `rv sync` and other copy-then-build installs move the source tree and break relative paths. Machine-specific, acceptable only because the patch is removed before merge. | R21 (relative paths) | Claude (small call) |

## §2 Decisions found at divergence review (recorded after the fact)

These shipped without an earlier written resolution. Listed so this spec stays the authority; each supersedes the earlier clause it names.

| ID | What shipped | Clause | Decided by |
| --- | --- | --- | --- |
| R31 | `HYPERION_SKIP_PHAROS_CLI` accepts `true/1/yes` (skip) and `false/0/no` (build); any other value prints a message and is treated as unset. | D12 | Claude (implementer) |
| R32 | `configure` deletes stale `src/pharos` / `src/pharos.exe` on every run; `Makevars.win.in` has the same guarded copy for `pharos.exe`. | F1, F4 | Claude (review fix) |
| R33 | `install.libs.R` also copies `symbols.rds`, sets 0755 on `bin/pharos`, stops if the copy fails, and the version check accepts `<version>` or `v<version>`. | M6, R25 | Claude (implementer) |
| R34 | `resolve_pharos()` rejects a `pharos_exec_path` that is not NULL or one non-empty string, rejects directories, errors (no PATH fallback) when the bundled binary exists but is not executable, and does not resolve symlinks for PATH/bundled results (only rule 2 uses `normalizePath`). Rule 1 (`"pharos"`) reports `source = "path"`. | W1, D13, I8 | Claude (implementer) |
| R35 | Status line labels the source `bundled` / `PATH` / `unknown source`. | A3 | Claude (implementer) |
| R36 | pharos logic lives in a private module `src/cli.rs`; `lib.rs` wraps it. Public API is exactly M1. M2's `Error: ` prefix changes pharos's stderr versus v0.6.0. | M1, M2 | Claude (implementer) |
| R37 | CI checks are wider than specified: I1 also forbids `extendr-ffi`; I11/I4 also cover the transitive `utils` crate; I5 scans magic bytes and rejects `src/rust/target/` in a dedicated job; D10's artifact check also verifies `--version` and runs in R-CMD-check on Linux. | I1, I4, I5, I11, D10 | Claude (implementer) |
| R38 | `rproject.toml` sets `HYPERION_SKIP_PHAROS_CLI = 'false'` for rv installs of this project, so dev installs build the CLI on every platform. | D10 (dev installs only) | Wes (edited outside the run) |
