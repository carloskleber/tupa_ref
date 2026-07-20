#!/usr/bin/env python3
"""Regenerate docs/figures/lima-fig7-comparison.svg.

Overlays TUPÃ's computed |Z(omega)| (from the fortran/*_results.json produced
by running common/lima_fig7_case{10,11}.json) against the digitized reference
points in Lima_fig7.xlsx: Lima et al. 2020's Fig. 7, the harmonic input
impedance of two grounding grids of different size (Case #10: 20x20 m,
2x2 mesh; Case #11: 40x40 m, 4x4 mesh). Only the MHEM curve was digitized
(one Z column per case) — same rationale as lima-fig6.md: the paper's HEM
curve is visually coincident with MHEM below ~4 MHz, and "dif" is a derived
diagnostic, not a second independent reference curve.

Usage (from repo root):
    ./fortran/build.sh   # if the Tupa binary isn't built yet
    cd fortran
    fpm run --profile release -- -q ../common/lima_fig7_case10.json
    fpm run --profile release -- -q ../common/lima_fig7_case11.json
    cd .. && python3 docs/validation/plot_lima_fig7.py
"""
import json
import math
from pathlib import Path

import matplotlib.pyplot as plt
import openpyxl

plt.rcParams["svg.fonttype"] = "path"

REPO_ROOT = Path(__file__).resolve().parents[2]
DIGITIZED_XLSX = REPO_ROOT / "docs" / "validation" / "Lima_fig7.xlsx"
RESULTS_DIR = REPO_ROOT / "fortran"
OUTPUT_SVG = REPO_ROOT / "docs" / "figures" / "lima-fig7-comparison.svg"

XLSX_HEADER_ROWS = 6  # data starts on row 7 (0-indexed row 6)

CASES = [10, 11]
# (frequency column, impedance column) per case, 0-indexed, in Lima_fig7.xlsx
XLSX_COLUMNS = {10: (0, 1), 11: (2, 3)}

# Fixed categorical order (dataviz skill palette.md slots 1/2), by case.
CASE_COLOR = {10: "#2a78d6", 11: "#e87ba4"}


def load_digitized_points() -> dict[int, tuple[list[float], list[float]]]:
    """Read all raw digitized (f, |Z|) MHEM points per case out of the xlsx."""
    wb = openpyxl.load_workbook(DIGITIZED_XLSX, read_only=True, data_only=True)
    ws = wb.active
    rows = list(ws.iter_rows(values_only=True))[XLSX_HEADER_ROWS:]

    points: dict[int, list[tuple[float, float]]] = {case: [] for case in CASES}
    for case, (f_col, z_col) in XLSX_COLUMNS.items():
        for row in rows:
            f, z = row[f_col], row[z_col]
            if f is not None and z is not None:
                points[case].append((f, z))
        points[case].sort()

    return {
        case: ([p[0] for p in pts], [p[1] for p in pts])
        for case, pts in points.items()
    }


def load_tupa_curve(case: int) -> tuple[list[float], list[float]]:
    """Load TUPÃ's |Z(omega)| curve from its results JSON."""
    path = RESULTS_DIR / f"lima_fig7_case{case}_results.json"
    if not path.exists():
        raise FileNotFoundError(
            f"{path} not found — run `fpm run --profile release -- "
            f"-q ../common/lima_fig7_case{case}.json` from fortran/ first"
        )
    data = json.loads(path.read_text())
    freqs = data["frequencies"]
    impedances = [math.hypot(z["re"], z["im"]) for z in data["derived"]["inputImpedance"]]
    return freqs, impedances


def main() -> None:
    fig, ax = plt.subplots(figsize=(9, 6))

    digitized = load_digitized_points()

    grid_sizes = {10: "20x20 m, 2x2 mesh", 11: "40x40 m, 4x4 mesh"}
    for case in CASES:
        color = CASE_COLOR[case]
        tupa_f, tupa_z = load_tupa_curve(case)
        dig_f, dig_z = digitized[case]

        ax.loglog(
            tupa_f, tupa_z, color=color, linewidth=1.8,
            label=f"TUPÃ, Case #{case} ({grid_sizes[case]})",
        )
        ax.loglog(
            dig_f, dig_z, color=color, linewidth=0, marker="o",
            markersize=4, markerfacecolor="none", markeredgewidth=1.1,
            label=f"Lima et al. 2020, Fig. 7, MHEM, Case #{case}",
        )

    ax.set_xlabel("Frequency (Hz)")
    ax.set_ylabel(r"$|Z(\omega)|$ ($\Omega$)")
    ax.grid(True, which="both", alpha=0.3)
    ax.legend(loc="upper left", fontsize=9)

    fig.tight_layout()
    OUTPUT_SVG.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(OUTPUT_SVG, format="svg")
    print(f"Wrote {OUTPUT_SVG}")


if __name__ == "__main__":
    main()
