#!/usr/bin/env bash
# Pre-merge gate: fails while the dev-only [patch] pointing pharos at a local
# checkout is in Cargo.toml, or while Cargo.lock still resolves any pharos
# crate from somewhere other than the pharos git repo (e.g. a local path).
# Registry crates that merely share a name (e.g. `config`) are ignored.
#
# Usage: check-no-dev-pharos-patch.sh [rust-dir]   (default src/rust)
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/pharos-lock.sh"

dir="${1:-src/rust}"
toml="$dir/Cargo.toml"
lock="$dir/Cargo.lock"
pharos_crates="pharos nonmem nonmem-parser config scheduler utils"

for f in "$toml" "$lock"; do
  [ -f "$f" ] || { echo "::error::$f not found"; exit 1; }
done

fail=0

if grep -niE "^[[:space:]]*\[patch\.[\"']https?://github\.com/a2-ai/pharos(\.git)?/?[\"']\]" "$toml"; then
  echo "::error file=$toml::Dev-only [patch] section for the pharos repo is present. Remove it, pin every pharos crate to the release tag, and regenerate Cargo.lock before merging."
  fail=1
fi

bad=$(pharos_lock_packages "$lock" | awk -v names="$pharos_crates" '
  BEGIN { n = split(names, a, " "); for (i = 1; i <= n; i++) want[a[i]] = 1 }
  ($1 in want) && $3 !~ /^(pharos#|registry\+|sparse\+)/ { print $1, $2, "(source: " $3 ")" }
')
if [ -n "$bad" ]; then
  echo "$bad" | sed 's/^/  /'
  echo "::error file=$lock::Cargo.lock has pharos crates not resolved from git+https://github.com/a2-ai/pharos. Remove the dev [patch] and regenerate Cargo.lock."
  fail=1
fi

[ "$fail" -eq 1 ] || echo "ok: no dev pharos patch in $toml and every pharos crate in $lock comes from the pharos git repo"
exit "$fail"
