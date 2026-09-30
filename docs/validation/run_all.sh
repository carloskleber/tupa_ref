#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
MODE="${1:-all}"

case "$MODE" in
    all|--plots-only) ;;
    -h|--help)
        echo "usage: $0 [--plots-only]"
        echo "  no argument    run solvers and regenerate every comparison"
        echo "  --plots-only   reuse existing Fortran JSON results"
        exit 0
        ;;
    *)
        echo "usage: $0 [--plots-only]" >&2
        exit 2
        ;;
esac

PYTHON_BIN="${PYTHON_BIN:-$REPO_ROOT/.venv/bin/python}"
if [[ ! -x "$PYTHON_BIN" ]]; then
    PYTHON_BIN="$(command -v python3 || true)"
fi
if [[ -z "$PYTHON_BIN" ]]; then
    echo "Python 3 was not found." >&2
    exit 1
fi
if ! "$PYTHON_BIN" -c 'import matplotlib, numpy, openpyxl, pandas' >/dev/null 2>&1; then
    echo "Validation Python dependencies are missing." >&2
    echo "Run:" >&2
    echo "  python3 -m venv '$REPO_ROOT/.venv'" >&2
    echo "  '$REPO_ROOT/.venv/bin/python' -m pip install -r '$SCRIPT_DIR/requirements.txt'" >&2
    exit 1
fi

export MPLCONFIGDIR="${MPLCONFIGDIR:-${TMPDIR:-/tmp}/tupa-matplotlib-cache}"
mkdir -p "$MPLCONFIGDIR"

frequency_cases=(
    grcev_fig12_l10_rho30.json
    grcev_fig12_l10_rho300.json
    grcev_fig12_l10_rho3000.json
    grcev_fig12_l100_rho30.json
    grcev_fig12_l100_rho300.json
    grcev_fig12_l100_rho3000.json
    lima_fig6.json
    lima_fig7_case10.json
    lima_fig7_case11.json
    poljak_fig4.json
    silva2025_rho100.json
    silva2025_rho300.json
    silva2025_rho1000.json
    silva2025_rho2400.json
)
transient_cases=(
    silva2025_rho100_transient.json
    silva2025_rho300_transient.json
    silva2025_rho1000_transient.json
    silva2025_rho2400_transient.json
)

if [[ "$MODE" == "all" ]]; then
    command -v fpm >/dev/null 2>&1 || { echo "fpm was not found." >&2; exit 1; }
    "$REPO_ROOT/fortran/build.sh"
    for case_file in "${frequency_cases[@]}" "${transient_cases[@]}"; do
        echo "Running Fortran validation case: $case_file"
        (
            cd "$REPO_ROOT/fortran"
            export LIBRARY_PATH="${HOME}/.local/lib:${LIBRARY_PATH:-}"
            fpm run --profile release -- -q "$REPO_ROOT/common/$case_file"
        )
    done
fi

plot_scripts=(
    plot_silva2025_fig3.py
    plot_silva2025_fig4.py
    plot_grcev_fig12.py
    plot_lima_fig6.py
    plot_lima_fig7.py
    plot_poljak_fig4.py
)
for plot_script in "${plot_scripts[@]}"; do
    echo "Generating validation figure: $plot_script"
    "$PYTHON_BIN" "$SCRIPT_DIR/$plot_script"
done

echo "Comparing TUPA (Fortran) with the mHEM prototype: compare_fortran_mhem.py"
JULIA_RESULTS="$SCRIPT_DIR/julia-grcev-l10-results.csv"
if [[ "$MODE" == "all" ]] && command -v julia >/dev/null 2>&1; then
    julia --project="$REPO_ROOT/julia" -e 'using Pkg; Pkg.instantiate()'
    julia --project="$REPO_ROOT/julia" "$REPO_ROOT/julia/comparison/run_tupa_grcev_l10.jl" \
        "$REPO_ROOT" "$JULIA_RESULTS"
else
    echo "Not rerunning Julia; reusing $JULIA_RESULTS" >&2
fi
"$PYTHON_BIN" "$SCRIPT_DIR/compare_fortran_mhem.py"

echo "All validation comparisons completed."
