#!/usr/bin/env python3
"""Regenerate docs/figures/silva2025-fig3-comparison.svg.

Overlays TUPÃ's computed |Z(omega)| (from the fortran/*_results.json produced
by running common/silva2025_rho*.json) against the digitized reference points
in silva_fig3.xlsx (all raw digitized points, not the coarser table in
silva2025-fig3.md).

Usage (from repo root):
    fortran/build.sh   # if the Tupa binary isn't built yet
    cd fortran && fpm run --profile release -- -q ../common/silva2025_rho100.json
    fpm run --profile release -- -q ../common/silva2025_rho300.json
    fpm run --profile release -- -q ../common/silva2025_rho1000.json
    fpm run --profile release -- -q ../common/silva2025_rho2400.json
    cd .. && python3 docs/validation/plot_silva2025_fig3.py
"""
import json
import math
from pathlib import Path

import matplotlib.pyplot as plt
import openpyxl

plt.rcParams["svg.fonttype"] = "path"

REPO_ROOT = Path(__file__).resolve().parents[2]
DIGITIZED_XLSX = REPO_ROOT / "docs" / "validation" / "silva_fig3.xlsx"
RESULTS_DIR = REPO_ROOT / "fortran"
OUTPUT_SVG = REPO_ROOT / "docs" / "figures" / "silva2025-fig3-comparison.svg"

RHO_VALUES = [100, 300, 1000, 2400]
SUBPLOT_LABELS = ["a", "b", "c", "d"]

# (frequency column, impedance column) per rho0, 0-indexed, in silva_fig3.xlsx
XLSX_COLUMNS = {100: (0, 1), 300: (2, 3), 1000: (4, 5), 2400: (6, 7)}
XLSX_HEADER_ROWS = 4  # data starts on row 5 (0-indexed row 4)

TUPA_COLOR = "#1a73e8"
DIGITIZED_COLOR = "#d93025"


def load_digitized_points() -> dict[int, tuple[list[float], list[float]]]:
    """Read all raw digitized (f, |Z|) points per rho0 out of the xlsx."""
    wb = openpyxl.load_workbook(DIGITIZED_XLSX, read_only=True, data_only=True)
    ws = wb.active
    rows = list(ws.iter_rows(values_only=True))[XLSX_HEADER_ROWS:]

    points: dict[int, list[tuple[float, float]]] = {rho: [] for rho in RHO_VALUES}
    for rho, (f_col, z_col) in XLSX_COLUMNS.items():
        for row in rows:
            f, z = row[f_col], row[z_col]
            if f is not None and z is not None:
                points[rho].append((f, z))
        points[rho].sort()

    return {
        rho: ([p[0] for p in pts], [p[1] for p in pts])
        for rho, pts in points.items()
    }


def load_tupa_curve(rho: int) -> tuple[list[float], list[float]]:
    """Load TUPÃ's |Z(omega)| curve from its results JSON."""
    path = RESULTS_DIR / f"silva2025_rho{rho}_results.json"
    if not path.exists():
        raise FileNotFoundError(
            f"{path} not found — run `fpm run --profile release -- "
            f"-q ../common/silva2025_rho{rho}.json` from fortran/ first"
        )
    data = json.loads(path.read_text())
    freqs = data["frequencies"]
    impedances = [math.hypot(z["re"], z["im"]) for z in data["derived"]["inputImpedance"]]
    return freqs, impedances


def main() -> None:
    fig, axes = plt.subplots(2, 2, figsize=(9, 7))
    fig.suptitle(
        "TUPÃ vs. Silva et al. 2025 (SBAI) Fig. 3 — "
        "60 m buried electrode, Alipio-Visacro soil"
    )

    digitized = load_digitized_points()

    for ax, rho, label in zip(axes.flat, RHO_VALUES, SUBPLOT_LABELS):
        tupa_f, tupa_z = load_tupa_curve(rho)
        dig_f, dig_z = digitized[rho]

        ax.semilogx(tupa_f, tupa_z, color=TUPA_COLOR, linewidth=1.8,
                    label="TUPÃ (this work)")
        ax.semilogx(dig_f, dig_z, color=DIGITIZED_COLOR, linewidth=1.5,
                    label="Silva et al. 2025, Fig. 3 (digitized)")

        ax.set_xlabel("Frequency (Hz)")
        ax.set_ylabel(r"$|Z(\omega)|$ ($\Omega$)")
        ax.set_title(f"({label}) {rho} Ω·m")
        ax.grid(True, alpha=0.3)

    axes.flat[0].legend(loc="upper left", fontsize=9)

    fig.tight_layout()
    OUTPUT_SVG.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(OUTPUT_SVG, format="svg")
    print(f"Wrote {OUTPUT_SVG}")


if __name__ == "__main__":
    main()
