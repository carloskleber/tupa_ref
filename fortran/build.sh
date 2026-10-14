#!/bin/bash
# Optimised build of the Fortran solver, driven entirely by fpm.
# SLATEC (git submodule) is a path dependency in fpm.toml, so it is compiled
# with the project — no separate install, no ~/.local/lib, no network access
# beyond the one-time fetch of the other dependencies.
# The solver is installed at a stable path: fortran/bin/Tupa
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Fetch the pinned SLATEC commit if the submodule hasn't been checked out yet
git -C "$SCRIPT_DIR/.." submodule update --init "$SCRIPT_DIR/slatec"

cd "$SCRIPT_DIR"

# Version banner: write src/version_generated.inc (gitignored) and touch
# Version.F90 only when it changes, so a new commit recompiles just that file
# (fpm does not track includes, and -D flags would rebuild everything).
VERSION=$(sed -n 's/^version *= *"\(.*\)"/\1/p' fpm.toml | head -1)
COMMIT=$(git -C "$SCRIPT_DIR" describe --always --dirty 2>/dev/null || echo unknown)
INC=src/version_generated.inc
NEW="  character(len=*), parameter :: VERSION_STRING = \"$VERSION\"
  character(len=*), parameter :: COMMIT_STRING = \"$COMMIT\""
if [ ! -f "$INC" ] || [ "$(cat "$INC")" != "$NEW" ]; then
  printf '%s\n' "$NEW" > "$INC"
  touch src/Version.F90
fi

# -std=legacy & co. are needed by the SLATEC (F77) sources; fpm applies --flag to all packages.
fpm install --profile release --prefix . \
  --flag "-std=legacy -Wno-argument-mismatch -fallow-invalid-boz -O3 -funroll-loops -ffast-math -finline-functions -ftree-vectorize -march=native -mtune=native -fopenmp -fno-range-check -ffree-line-length-none"

# Man page, generated from the binary's --help/--version output
if command -v help2man >/dev/null 2>&1; then
  mkdir -p man
  help2man --no-info --locale=C.UTF-8 --name="TUPA electromagnetic transient solver" \
    --section=1 --output=man/tupa.1 "$SCRIPT_DIR/bin/Tupa"
  echo "Man page: $SCRIPT_DIR/man/tupa.1"
else
  echo "help2man not found: man page skipped"
fi

echo "Build completed successfully: $SCRIPT_DIR/bin/Tupa"
