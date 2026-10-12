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
# -std=legacy & co. are needed by the SLATEC (F77) sources; fpm applies --flag to all packages.
fpm install --profile release --prefix . \
  --flag "-std=legacy -Wno-argument-mismatch -fallow-invalid-boz -O3 -funroll-loops -ffast-math -finline-functions -ftree-vectorize -march=native -mtune=native -fopenmp -fno-range-check -ffree-line-length-none"

echo "Build completed successfully: $SCRIPT_DIR/bin/Tupa"
