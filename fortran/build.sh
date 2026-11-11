#!/bin/bash
# Optimised build of the Fortran solver, driven entirely by fpm.
# There is no SLATEC dependency (ADR 0031): the Bessel functions are the in-repo
# mBessel, so the only external inputs are the fpm dependencies in fpm.toml
# (fetched once) plus system LAPACK/BLAS.
# The solver is installed at a stable path: fortran/bin/Tupa
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

cd "$SCRIPT_DIR"
git update-index --skip-worktree src/version_generated.inc 2>/dev/null || true

# Version banner: write src/version_generated.inc (tracked, skip-worktree) and touch
# Version.F90 only when it changes, so a new commit recompiles just that file
# (the committed default is "unknown"; build.sh marks it skip-worktree so builds do not dirty git status) (fpm does not track includes, and -D flags would rebuild everything). The build
# date has day granularity for the same reason: it triggers at most one rebuild/day.
VERSION=$(sed -n 's/^version *= *"\(.*\)"/\1/p' fpm.toml | head -1)
COMMIT=$(git -C "$SCRIPT_DIR" describe --always --dirty 2>/dev/null || echo unknown)
BUILD_DATE=$(date +%Y-%m-%d)
INC=src/version_generated.inc
NEW="  character(len=*), parameter :: VERSION_STRING = \"$VERSION\"
  character(len=*), parameter :: COMMIT_STRING = \"$COMMIT\"
  character(len=*), parameter :: BUILD_DATE_STRING = \"$BUILD_DATE\""
if [ ! -f "$INC" ] || [ "$(cat "$INC")" != "$NEW" ]; then
  printf '%s\n' "$NEW" > "$INC"
  touch src/Version.F90
fi

# -fno-finite-math-only: the geometry code detects NaN/Inf (Geometry.f90 parallelGeometryFactor)
# and falls back to quadrature; -ffinite-math-only (part of -ffast-math) optimises that
# check away and the whole solution turns NaN (labuat_impulse).
fpm install --profile release --prefix . \
  --flag "-O3 -funroll-loops -ffast-math -fno-finite-math-only -finline-functions -ftree-vectorize -march=native -mtune=native -fopenmp -fno-range-check -ffree-line-length-none"

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
