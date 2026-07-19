#!/usr/bin/env python3
"""Regenerate docs/figures/lima-fig6-comparison.svg.

Overlays TUPÃ's computed |Z(omega)| (from the fortran/*_results.json produced
by running common/lima_fig6.json) against the digitized reference points in
Lima_fig6.xlsx (all raw digitized points). Lima et al. 2019's Fig. 6 plots
three curves (MHEM, HEM, and their absolute difference "dif"); only the
MHEM curve was digitized (Lima_fig6.xlsx has a single Z column) — the paper
itself shows MHEM and HEM as visually coincident, and "dif" is a derived
diagnostic, not a second independent reference curve.

Usage (from repo root):
    fortran/build.sh   # if the Tupa binary isn't built yet
    cd fortran && fpm run --profile release -- -q ../common/lima_fig6.json
    cd .. && python3 docs/validation/plot_lima_fig6.py
"""
import json
import math
from pathlib import Path

import matplotlib.pyplot as plt
import openpyxl

plt.rcParams["svg.fonttype"] = "path"

REPO_ROOT = Path(__file__).resolve().parents[2]
DIGITIZED_XLSX = REPO_ROOT / "docs" / "validation" / "Lima_fig6.xlsx"
RESULTS_DIR = REPO_ROOT / "fortran"
OUTPUT_SVG = REPO_ROOT / "docs" / "figures" / "lima-fig6-comparison.svg"

XLSX_HEADER_ROWS = 3  # data starts on row 4 (0-indexed row 3)

TUPA_COLOR = "#1a73e8"
DIGITIZED_COLOR = "#d93025"


def load_digitized_points() -> tuple[list[float], list[float]]:
    """Read all raw digitized (f, |Z|) MHEM points out of the xlsx."""
    wb = openpyxl.load_workbook(DIGITIZED_XLSX, read_only=True, data_only=True)
    ws = wb.active
    rows = list(ws.iter_rows(values_only=True))[XLSX_HEADER_ROWS:]

    points = sorted(
        (row[0], row[1]) for row in rows if row[0] is not None and row[1] is not None
    )
    return [p[0] for p in points], [p[1] for p in points]


def load_tupa_curve() -> tuple[list[float], list[float]]:
    """Load TUPÃ's |Z(omega)| curve from its results JSON."""
    path = RESULTS_DIR / "lima_fig6_results.json"
    if not path.exists():
        raise FileNotFoundError(
            f"{path} not found — run `fpm run --profile release -- "
            f"-q ../common/lima_fig6.json` from fortran/ first"
        )
    data = json.loads(path.read_text())
    freqs = data["frequencies"]
    impedances = [math.hypot(z["re"], z["im"]) for z in data["derived"]["inputImpedance"]]
    return freqs, impedances


def main() -> None:
    fig, ax = plt.subplots(figsize=(9, 6))
    fig.suptitle(
        "TUPÃ vs. Lima et al. 2019 Fig. 6 — Case #9 distribution tower\n"
        "grounding (MHEM only)"
    )

    tupa_f, tupa_z = load_tupa_curve()
    dig_f, dig_z = load_digitized_points()

    ax.loglog(
        tupa_f, tupa_z, color=TUPA_COLOR, linewidth=1.8,
        label="TUPÃ (this work)",
    )
    ax.loglog(
        dig_f, dig_z, color=DIGITIZED_COLOR, linewidth=0, marker="o",
        markersize=4, markerfacecolor="none", markeredgewidth=1.1,
        label="Lima et al. 2019, Fig. 6, MHEM (digitized)",
    )

    ax.set_xlabel("Frequency (Hz)")
    ax.set_ylabel(r"$|Z(\omega)|$ ($\Omega$)")
    ax.grid(True, which="both", alpha=0.3)
    ax.legend(loc="lower left", fontsize=9)

    fig.tight_layout()
    OUTPUT_SVG.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(OUTPUT_SVG, format="svg")
    print(f"Wrote {OUTPUT_SVG}")


if __name__ == "__main__":
    main()
