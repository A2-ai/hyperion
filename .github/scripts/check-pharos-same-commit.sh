#!/usr/bin/env bash
# The FFI crates hyperion links and the `pharos` crate the bundled CLI is built
# from must all resolve to one pharos commit, so the CLI matches the library.
#
# Usage: check-pharos-same-commit.sh [path/to/Cargo.lock]  (default src/rust/Cargo.lock)
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/pharos-lock.sh"

lock="${1:-src/rust/Cargo.lock}"
required="pharos nonmem nonmem-parser config scheduler"

[ -f "$lock" ] || { echo "::error::$lock not found"; exit 1; }

# "name sha" for each crate from the pharos git repo.
entries=$(pharos_lock_packages "$lock" | awk '$3 ~ /^pharos#/ { print $1, substr($3, 8) }')
echo "pharos-repo crates in $lock:"
echo "${entries:-(none)}" | sed 's/^/  /'

fail=0
for crate in $required; do
  if ! grep -q "^$crate " <<<"$entries"; then
    echo "::error file=$lock::$crate is not resolved from git+https://github.com/a2-ai/pharos"
    fail=1
  fi
done

shas=$(awk '{ print $2 }' <<<"$entries" | sort -u)
if [ "$(wc -l <<<"$shas")" -gt 1 ]; then
  echo "::error file=$lock::pharos crates resolve to more than one commit: $(echo $shas)"
  fail=1
fi

[ "$fail" -eq 1 ] || echo "ok: all pharos crates resolve to commit $shas"
exit "$fail"
