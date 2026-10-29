#!/usr/bin/env bash
# Run every internal-consistency check script of this folder against one
# solver executable (Fortran `Tupa` or `rust/target/release/tupa`; same CLI).
# Unlike run_all.sh these need no digitized data and no plotting packages
# (phase10_image_model_check.py and channel_checks.py need the latter two).
#
#   ./docs/validation/run_checks.sh <solver executable> [--all]
#
# Default: the stdlib-only scripts behind phase9-transient-options.md,
# sources-validation.md and gpr-potentials.md. --all adds the channel and
# Phase 10 image-model checks (numpy and the requirements.txt packages).
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EXE="${1:-}"
if [[ -z "$EXE" || ! -x "$EXE" ]]; then
    echo "usage: $0 <solver executable> [--all]" >&2
    exit 2
fi
PYTHON_BIN="${PYTHON_BIN:-$(command -v python3)}"

for script in phase9_transient_checks.py sources_checks.py phase11_checks.py; do
    echo "=== $script"
    "$PYTHON_BIN" "$SCRIPT_DIR/$script" "$EXE"
    echo
done

if [[ "${2:-}" == "--all" ]]; then
    echo "=== channel_checks.py"
    "$PYTHON_BIN" "$SCRIPT_DIR/channel_checks.py" --fortran-bin "$EXE"
    echo "=== phase10_image_model_check.py"
    "$PYTHON_BIN" "$SCRIPT_DIR/phase10_image_model_check.py" --fortran-bin "$EXE"
fi
