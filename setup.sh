#!/bin/bash
# setup.sh - Build project with local SLATEC library

set -e

echo "Installing build dependencies..."
sudo apt-get update
sudo apt-get install -y cmake gfortran liblapack-dev

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SLATEC_DIR="$SCRIPT_DIR/slatec"
SLATEC_BUILD="$SLATEC_DIR/build"
SLATEC_INSTALL="$SCRIPT_DIR/slatec-install"

echo "Setting up SLATEC v4.2..."

# Clone SLATEC if not already present
if [ ! -d "$SLATEC_DIR" ]; then
    echo "Cloning SLATEC v4.2..."
    git clone --depth 1 --branch v4.2 https://github.com/MehdiChinoune/SLATEC "$SLATEC_DIR"
fi

# Build SLATEC locally
echo "Building SLATEC library (local installation)..."
cd "$SLATEC_DIR"
rm -rf "$SLATEC_BUILD"
mkdir -p "$SLATEC_BUILD"
cd "$SLATEC_BUILD"
cmake -DCMAKE_INSTALL_PREFIX="$SLATEC_INSTALL" ..
make -j$(nproc)
make install

# Build main project
echo "Building main project with fpm..."
export LIBRARY_PATH="$SLATEC_INSTALL/lib:$LIBRARY_PATH"
export LD_LIBRARY_PATH="$SLATEC_INSTALL/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="$SLATEC_INSTALL/lib/pkgconfig:$PKG_CONFIG_PATH"

cd "$SCRIPT_DIR/fortran"
fpm build

echo "✓ Build complete!"
echo ""
echo "To use your project, set the environment:"
echo "  export LD_LIBRARY_PATH=$SLATEC_INSTALL/lib:\$LD_LIBRARY_PATH"
