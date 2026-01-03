#!/bin/bash
# An optimized build script for the Fortran project using fpm and integrating the SLATEC library.
# This script clones the SLATEC repository if it doesn't exist, pulls the latest changes,
# installs dependencies with specific compiler flags, and builds the main project.
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$SCRIPT_DIR/slatec"
MAIN_PROJECT_DIR="$SCRIPT_DIR/.."

# Checkout repository if not exists
if [ ! -d "$REPO_DIR" ]; then
    git clone https://github.com/carloskleber/slatec "$REPO_DIR"
fi

cd "$REPO_DIR"
git pull
fpm install --profile release --flag "-std=legacy -Wno-argument-mismatch -fallow-invalid-boz"

# Build main project
cd "$SCRIPT_DIR"
fpm build --profile release --flag "-O3 -funroll-loops -ffast-math -fopt-info-vec-optimized -finline-functions -ftree-vectorize -march=native -mtune=native -fopenmp -fno-range-check -ffree-line-length-none"

echo "Build completed successfully!"