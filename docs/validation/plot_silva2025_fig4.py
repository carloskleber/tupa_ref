#!/usr/bin/env python3
"""Regenerate docs/figures/silva2025-fig4-comparison.svg.

Overlays TUPÃ's computed GPR(t) (from the fortran/*_transient_results.json
produced by running common/silva2025_rho*_transient.json) against the
digitized reference points in silva_fig4.xlsx (all raw digitized points, not
the coarser common time grid in silva2025-fig4.md).

Usage (from repo root):
    fortran/build.sh   # if the Tupa binary isn't built yet
    cd fortran && fpm run --profile release -- -q ../common/silva2025_rho100_transient.json
    fpm run --profile release -- -q ../common/silva2025_rho300_transient.json
    fpm run --profile release -- -q ../common/silva2025_rho1000_transient.json
    fpm run --profile release -- -q ../common/silva2025_rho2400_transient.json
    cd .. && python3 docs/validation/plot_silva2025_fig4.py
"""
import json
from pathlib import Path

import matplotlib.pyplot as plt
import openpyxl

plt.rcParams["svg.fonttype"] = "path"

REPO_ROOT = Path(__file__).resolve().parents[2]
DIGITIZED_XLSX = REPO_ROOT / "docs" / "validation" / "silva_fig4.xlsx"
RESULTS_DIR = REPO_ROOT / "fortran"
OUTPUT_SVG = REPO_ROOT / "docs" / "figures" / "silva2025-fig4-comparison.svg"

RHO_VALUES = [100, 300, 1000, 2400]
SUBPLOT_LABELS = ["a", "b", "c", "d"]

# (time column, voltage column) per rho0, 0-indexed, in silva_fig4.xlsx
XLSX_COLUMNS = {100: (0, 1), 300: (2, 3), 1000: (4, 5), 2400: (6, 7)}
XLSX_HEADER_ROWS = 3  # data starts on row 4 (0-indexed row 3)

TUPA_COLOR = "#1a73e8"
DIGITIZED_COLOR = "#d93025"


def load_digitized_points() -> dict[int, tuple[list[float], list[float]]]:
    """Read all raw digitized (t, V) points per rho0 out of the xlsx.

    xlsx units are seconds and volts; converted here to microseconds and MV
    to match the paper's plotted axes.
    """
    wb = openpyxl.load_workbook(DIGITIZED_XLSX, read_only=True, data_only=True)
    ws = wb.active
    rows = list(ws.iter_rows(values_only=True))[XLSX_HEADER_ROWS:]

    points: dict[int, list[tuple[float, float]]] = {rho: [] for rho in RHO_VALUES}
    for rho, (t_col, v_col) in XLSX_COLUMNS.items():
        for row in rows:
            t, v = row[t_col], row[v_col]
            if t is not None and v is not None:
                points[rho].append((t * 1e6, v * 1e-6))
        points[rho].sort()

    return {
        rho: ([p[0] for p in pts], [p[1] for p in pts])
        for rho, pts in points.items()
    }


def load_tupa_curve(rho: int) -> tuple[list[float], list[float]]:
    """Load TUPÃ's GPR(t) curve at the source node from its results JSON."""
    path = RESULTS_DIR / f"silva2025_rho{rho}_transient_transient_results.json"
    if not path.exists():
        raise FileNotFoundError(
            f"{path} not found — run `fpm run --profile release -- "
            f"-q ../common/silva2025_rho{rho}_transient.json` from fortran/ first"
        )
    data = json.loads(path.read_text())
    time_us = [t * 1e6 for t in data["time"]]
    node = next(n for n in data["nodes"] if n["id"] == data["sourceNode"])
    voltage_mv = [v * 1e-6 for v in node["voltage"]]
    return time_us, voltage_mv


def main() -> None:
    fig, axes = plt.subplots(2, 2, figsize=(9, 7))
    fig.suptitle(
        "TUPÃ vs. Silva et al. 2025 (SBAI) Fig. 4 — "
        "60 m buried electrode, Alipio-Visacro soil, GPR(t)"
    )

    digitized = load_digitized_points()

    for ax, rho, label in zip(axes.flat, RHO_VALUES, SUBPLOT_LABELS):
        tupa_t, tupa_v = load_tupa_curve(rho)
        dig_t, dig_v = digitized[rho]

        ax.plot(tupa_t, tupa_v, color=TUPA_COLOR, linewidth=1.8,
                label="TUPÃ (this work)")
        ax.plot(dig_t, dig_v, color=DIGITIZED_COLOR, linewidth=1.5,
                label="Silva et al. 2025, Fig. 4 (digitized)")

        ax.set_xlim(0, 36)
        ax.set_xlabel(r"$t$ ($\mu$s)")
        ax.set_ylabel("GPR (MV)")
        ax.set_title(f"({label}) {rho} Ω·m")
        ax.grid(True, alpha=0.3)

    axes.flat[0].legend(loc="lower right", fontsize=9)

    fig.tight_layout()
    OUTPUT_SVG.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(OUTPUT_SVG, format="svg")
    print(f"Wrote {OUTPUT_SVG}")


if __name__ == "__main__":
    main()
