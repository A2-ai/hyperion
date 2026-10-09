#!/usr/bin/env bash
# The source tarball must contain no compiled binaries (ELF, Mach-O, PE/COFF,
# ar) and none of the pharos CLI build outputs: the CLI is built at install
# time, and a leaked binary would be platform-specific.
#
# Usage: check-source-tarball.sh <pkg_version.tar.gz>
set -euo pipefail

archive="${1:?usage: check-source-tarball.sh <pkg_version.tar.gz>}"
[ -f "$archive" ] || { echo "::error::source tarball not found: $archive"; exit 1; }

fail=0

# Paths relative to the package root.
paths=$(tar -tzf "$archive" | sed -e 's#^\./##' -e 's#^[^/]*/##')
forbidden=$(grep -E '^(inst/bin(/|$)|src/pharos(\.exe)?$|src/pharos-cli-skipped$|src/rust/target(/|$))' <<<"$paths" || true)
if [ -n "$forbidden" ]; then
  echo "$forbidden" | sed 's/^/  /'
  echo "::error::source tarball contains forbidden build outputs"
  fail=1
fi

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
tar -xzf "$archive" -C "$work"

# Walk the archive listing rather than `find`, which minimal build containers
# do not always ship.
binaries=""
while IFS= read -r rel; do
  f="$work/$rel"
  [ -f "$f" ] && [ ! -L "$f" ] || continue
  case "$(head -c 8 "$f" | od -An -tx1 | tr -d ' \n')" in
    7f454c46*)                                         kind="ELF" ;;
    feedface*|feedfacf*|cefaedfe*|cffaedfe*|cafebabe*) kind="Mach-O" ;;
    4d5a*)                                             kind="PE/COFF" ;;
    213c617263683e0a)                                  kind="ar archive" ;;
    *)                                                 continue ;;
  esac
  binaries+="  ${rel#./} ($kind)"$'\n'
done < <(tar -tzf "$archive")

if [ -n "$binaries" ]; then
  printf '%s' "$binaries"
  echo "::error::source tarball contains compiled binaries"
  fail=1
fi

[ "$fail" -eq 1 ] || echo "ok: $archive contains no binaries and no inst/bin"
exit "$fail"
