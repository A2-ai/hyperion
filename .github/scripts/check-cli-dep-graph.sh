#!/usr/bin/env bash
# The bundled CLI crate must not depend on extendr-api or R's FFI crates
# (libR-sys, or extendr-ffi which replaced it in extendr-api >= 0.9): those
# need R's symbols at link time.
#
# `cargo tree -i <dep>` exits 101 with "did not match any packages" when <dep>
# is absent, but also exits 101 for any other failure, so a pass needs both.
#
# Usage: check-cli-dep-graph.sh [path/to/Cargo.toml]   (default src/rust/Cargo.toml)
set -uo pipefail

manifest="${1:-src/rust/Cargo.toml}"
pkg="hyperion-pharos-cli"
forbidden=(extendr-api libR-sys extendr-ffi)

# Positive control: a broken manifest or renamed package must not pass.
if ! out=$(cargo tree --locked --manifest-path "$manifest" -p "$pkg" --depth 0 2>&1); then
  echo "$out"
  echo "::error::cargo tree could not resolve $pkg"
  exit 1
fi

fail=0
for dep in "${forbidden[@]}"; do
  out=$(cargo tree --locked --manifest-path "$manifest" -p "$pkg" -i "$dep" 2>&1)
  status=$?
  if [ "$status" -eq 0 ]; then
    echo "$out"
    echo "::error::$pkg depends on $dep"
    fail=1
  elif [ "$status" -eq 101 ] && grep -qF "package ID specification \`$dep\` did not match any packages" <<<"$out"; then
    echo "ok: $dep is not in the $pkg dependency graph"
  else
    echo "$out"
    echo "::error::cargo tree -i $dep failed unexpectedly (exit $status)"
    fail=1
  fi
done
exit "$fail"
