#!/usr/bin/env bash
# A Linux hyperion binary package must ship bin/pharos, and its `--version`
# must report the package's Config/PharosVersion.
#
# Usage: check-bundled-pharos.sh <binary-package.tar.gz | installed-pkg-dir> [package-name]
set -euo pipefail

target="${1:?usage: check-bundled-pharos.sh <binary-package.tar.gz | installed-pkg-dir> [package-name]}"
pkg="${2:-hyperion}"

if [ -d "$target" ]; then
  pkgdir="$target"
  if [ ! -e "$pkgdir/bin/pharos" ]; then
    echo "::error::$pkgdir has no bin/pharos"
    exit 1
  fi
elif [ -f "$target" ]; then
  # Capture the listing first: `grep -q` exiting early would SIGPIPE tar under
  # pipefail. Strip the optional leading "./" some packers add.
  listing=$(tar -tzf "$target" | sed 's#^\./##')
  if ! grep -qx "$pkg/bin/pharos" <<<"$listing"; then
    echo "::error::$target does not contain $pkg/bin/pharos"
    echo "bin/ entries in the archive:"
    grep "^$pkg/bin" <<<"$listing" || echo "  (none)"
    exit 1
  fi
  work=$(mktemp -d)
  trap 'rm -rf "$work"' EXIT
  tar -xzf "$target" -C "$work"
  pkgdir="$work/$pkg"
else
  echo "::error::not found: $target"
  exit 1
fi

exe="$pkgdir/bin/pharos"
if [ ! -x "$exe" ]; then
  echo "::error::$pkg/bin/pharos in $target is not executable"
  exit 1
fi

expected=$(sed -n 's/^Config\/PharosVersion:[[:space:]]*//p' "$pkgdir/DESCRIPTION" | tr -d '[:space:]')
if [ -z "$expected" ]; then
  echo "::error::$pkg/DESCRIPTION in $target has no Config/PharosVersion"
  exit 1
fi

if ! reported=$("$exe" --version 2>&1); then
  echo "$reported"
  echo "::error::$pkg/bin/pharos --version exited non-zero"
  exit 1
fi

# Exact token match, as in install.libs.R: "0.6.1" must not accept "0.6.10".
for tok in $reported; do
  if [ "$tok" = "$expected" ] || [ "$tok" = "v$expected" ]; then
    echo "ok: $pkg/bin/pharos reports '$reported' (Config/PharosVersion $expected)"
    exit 0
  fi
done

echo "::error::$pkg/bin/pharos reports '$reported' but Config/PharosVersion is '$expected'"
exit 1
