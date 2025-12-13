#!/bin/bash
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
fpm install "$MAIN_PROJECT_DIR" --profile release --flag "-std=legacy -Wno-argument-mismatch -fallow-invalid-boz"

# Build main project
cd "$MAIN_PROJECT_DIR"
fpm build

echo "Build completed successfully!"