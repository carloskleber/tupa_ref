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

command -v julia >/dev/null 2>&1 || { echo "Julia was not found." >&2; exit 1; }

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

MHEM_ZIP="${MHEM_ZIP:-$REPO_ROOT/transient-analysis-grounding-systems-julia-tupa-aligned.zip}"
if [[ ! -f "$MHEM_ZIP" ]]; then
    echo "Aligned mHEM archive not found: $MHEM_ZIP" >&2
    echo "Set MHEM_ZIP to its location, or place it in the repository root." >&2
    exit 1
fi

TEMP_ROOT="$(mktemp -d "${TMPDIR:-/tmp}/tupa-validation.XXXXXX")"
trap 'rm -rf "$TEMP_ROOT"' EXIT
unzip -q "$MHEM_ZIP" -d "$TEMP_ROOT"
MHEM_DIR="$TEMP_ROOT/transient-analysis-grounding-systems-julia"

julia --project="$MHEM_DIR" -e 'using Pkg; Pkg.instantiate()'
julia --project="$MHEM_DIR" "$MHEM_DIR/examples/tupa_grcev_fig12_l10.jl"
julia --project="$MHEM_DIR" "$MHEM_DIR/examples/tupa_rod.jl"

TUPA_GRCEV="$TEMP_ROOT/tupa_grcev.csv"
TUPA_ROD="$TEMP_ROOT/tupa_rod.csv"
MHEM_GRCEV="$MHEM_DIR/examples/tupa_matched_grcev_l10_mhem.csv"
MHEM_ROD="$MHEM_DIR/examples/tupa_matched_rod_mhem.csv"

julia --project="$REPO_ROOT/julia" "$REPO_ROOT/julia/comparison/run_tupa_grcev_l10.jl" "$REPO_ROOT" "$TUPA_GRCEV"
julia --project="$REPO_ROOT/julia" "$REPO_ROOT/julia/comparison/run_tupa_rod.jl" "$REPO_ROOT" "$TUPA_ROD"

COMBINED_RESULTS="$SCRIPT_DIR/tupa-mhem-grcev-l10-results.csv"
METRICS="$SCRIPT_DIR/tupa-mhem-grcev-l10-metrics.csv"
MOM_POINTS="$SCRIPT_DIR/tupa-mhem-mom-reference-points.csv"
"$PYTHON_BIN" "$SCRIPT_DIR/compare_tupa_mhem.py" \
    --tupa "$TUPA_GRCEV" \
    --mhem "$MHEM_GRCEV" \
    --reference "$SCRIPT_DIR/grcev_fig12.xlsx" \
    --results "$COMBINED_RESULTS" \
    --metrics "$METRICS" \
    --reference-results "$MOM_POINTS"

FIGURES_DIR="$REPO_ROOT/docs/figures"
julia --project="$REPO_ROOT/julia" "$REPO_ROOT/julia/comparison/plot_tupa_mhem_grcev_l10.jl" \
    "$COMBINED_RESULTS" "$MOM_POINTS" \
    "$FIGURES_DIR/tupa-mhem-grcev-l10-comparison.png" \
    "$FIGURES_DIR/tupa-mhem-grcev-l10-comparison.svg" \
    "$FIGURES_DIR/tupa-mhem-grcev-l10-comparison.pdf"

julia --project="$REPO_ROOT/julia" "$REPO_ROOT/julia/comparison/plot_tupa_mhem_aligned_cases.jl" \
    "$COMBINED_RESULTS" "$TUPA_ROD" "$MHEM_ROD" "$MOM_POINTS" \
    "$FIGURES_DIR/tupa-mhem-mom-comparison.png" \
    "$FIGURES_DIR/tupa-mhem-mom-comparison.svg" \
    "$FIGURES_DIR/tupa-mhem-mom-comparison.pdf"

echo "All validation comparisons completed."
echo "Figures: $FIGURES_DIR"
