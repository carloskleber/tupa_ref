#!/bin/bash
# An optimized build script for the Fortran project using fpm and integrating the SLATEC library.
# This script initializes the SLATEC git submodule, installs it with specific
# compiler flags, and builds the main project.
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$SCRIPT_DIR/slatec"
MAIN_PROJECT_DIR="$SCRIPT_DIR/.."

# Fetch the pinned SLATEC commit if the submodule hasn't been checked out yet
cd "$MAIN_PROJECT_DIR"
git submodule update --init "$REPO_DIR"

cd "$REPO_DIR"
fpm install --profile release --flag "-std=legacy -Wno-argument-mismatch -fallow-invalid-boz"

# `fpm install` above places libslatec.a in the default prefix (~/.local/lib),
# which is not on the linker's default search path — needed since Impedance.f90
# links against SLATEC's ZBESI for internal (skin-effect) impedance.
export LIBRARY_PATH="$HOME/.local/lib:$LIBRARY_PATH"

# Build main project
cd "$SCRIPT_DIR"
fpm build --profile release --flag "-O3 -funroll-loops -ffast-math -fopt-info-vec-optimized -finline-functions -ftree-vectorize -march=native -mtune=native -fopenmp -fno-range-check -ffree-line-length-none"

echo "Build completed successfully!"