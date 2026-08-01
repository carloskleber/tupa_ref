#!/usr/bin/env python3
"""Regenerate docs/figures/grcev-fig12-comparison.svg.

Overlays TUPÃ's computed |Z(omega)| (from the fortran/*_results.json produced
by running common/grcev_fig12_l{10,100}_rho{30,300,3000}.json) against the
digitized reference points in grcev_fig12.xlsx (all raw digitized points, not
a coarser resampled table).

Usage (from repo root):
    fortran/build.sh   # if the Tupa binary isn't built yet
    cd fortran
    for f in ../common/grcev_fig12_l*.json; do
        fpm run --profile release -- -q "$f"
    done
    cd .. && python3 docs/validation/plot_grcev_fig12.py
"""
import json
import math
from pathlib import Path

import matplotlib.pyplot as plt
import openpyxl

plt.rcParams["svg.fonttype"] = "path"

REPO_ROOT = Path(__file__).resolve().parents[2]
DIGITIZED_XLSX = REPO_ROOT / "docs" / "validation" / "grcev_fig12.xlsx"
RESULTS_DIR = REPO_ROOT / "fortran"
OUTPUT_SVG = REPO_ROOT / "docs" / "figures" / "grcev-fig12-comparison.svg"

LENGTHS = [10, 100]
RHO_VALUES = [30, 300, 3000]
SUBPLOT_LABELS = ["a", "b"]

# (frequency column, impedance column) per (length, rho), 0-indexed, in grcev_fig12.xlsx
XLSX_COLUMNS = {
    (10, 30): (0, 1), (10, 300): (2, 3), (10, 3000): (4, 5),
    (100, 30): (6, 7), (100, 300): (8, 9), (100, 3000): (10, 11),
}
XLSX_HEADER_ROWS = 3  # data starts on row 4 (0-indexed row 3)

# Fixed categorical order (dataviz skill palette.md slots 1/2/3), by rho0.
RHO_COLOR = {30: "#2a78d6", 300: "#008300", 3000: "#e87ba4"}


def load_digitized_points() -> dict[tuple[int, int], tuple[list[float], list[float]]]:
    """Read all raw digitized (f, |Z|) points per (length, rho0) out of the xlsx."""
    wb = openpyxl.load_workbook(DIGITIZED_XLSX, read_only=True, data_only=True)
    ws = wb.active
    rows = list(ws.iter_rows(values_only=True))[XLSX_HEADER_ROWS:]

    points: dict[tuple[int, int], list[tuple[float, float]]] = {
        key: [] for key in XLSX_COLUMNS
    }
    for key, (f_col, z_col) in XLSX_COLUMNS.items():
        for row in rows:
            f, z = row[f_col], row[z_col]
            if f is not None and z is not None:
                points[key].append((f, z))
        points[key].sort()

    return {
        key: ([p[0] for p in pts], [p[1] for p in pts])
        for key, pts in points.items()
    }


def load_tupa_curve(length: int, rho: int) -> tuple[list[float], list[float]]:
    """Load TUPÃ's |Z(omega)| curve from its results JSON."""
    path = RESULTS_DIR / f"grcev_fig12_l{length}_rho{rho}_results.json"
    if not path.exists():
        raise FileNotFoundError(
            f"{path} not found — run `fpm run --profile release -- "
            f"-q ../common/grcev_fig12_l{length}_rho{rho}.json` from fortran/ first"
        )
    data = json.loads(path.read_text())
    freqs = data["frequencies"]
    impedances = [math.hypot(z["re"], z["im"]) for z in data["derived"]["inputImpedance"]]
    return freqs, impedances


def main() -> None:
    fig, axes = plt.subplots(2, 1, figsize=(8, 8))
    #fig.suptitle(
    #    "TUPÃ vs. Grcev et al. 2018 (IEEE TPWRD) Fig. 12 — "
    #    "horizontal grounding electrode, 0.5 m depth"
    #)

    digitized = load_digitized_points()
    handles: list = []

    for ax, length, label in zip(axes, LENGTHS, SUBPLOT_LABELS):
        for rho in RHO_VALUES:
            color = RHO_COLOR[rho]
            tupa_f, tupa_z = load_tupa_curve(length, rho)
            dig_f, dig_z = digitized[(length, rho)]

            (line,) = ax.loglog(
                tupa_f, tupa_z, color=color, linewidth=1.8,
                label=f"TUPÃ, ρ = {rho} Ω·m",
            )
            (points,) = ax.loglog(
                dig_f, dig_z, color=color, linewidth=0, marker="o",
                markersize=4, markerfacecolor="none", markeredgewidth=1.1,
                label=f"Grcev, ρ = {rho} Ω·m",
            )
            if length == LENGTHS[0]:
                handles += [line, points]

        ax.set_xlabel("Frequency (Hz)")
        ax.set_ylabel(r"$|Z(\omega)|$ ($\Omega$)")
        ax.set_title(f"({label}) l = {length} m")
        ax.grid(True, which="both", alpha=0.3)

    fig.legend(handles=handles, loc="lower center", ncol=3, fontsize=10,
               bbox_to_anchor=(0.5, 0.0))

    fig.tight_layout(rect=(0.0, 0.08, 1.0, 1.0))
    OUTPUT_SVG.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(OUTPUT_SVG, format="svg")
    print(f"Wrote {OUTPUT_SVG}")


if __name__ == "__main__":
    main()
