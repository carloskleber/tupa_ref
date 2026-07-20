#!/usr/bin/env python3
"""Regenerate docs/figures/poljak-fig4-comparison.svg.

Overlays TUPÃ's computed |Z(omega)| (from the fortran/*_results.json produced
by running common/poljak_fig4.json) against the digitized reference points in
Poljak_fig4.xlsx: Poljak & Doric 2006's Fig. 4, the input-impedance spectrum
of a single L=2 m, radius a=5 mm vertical grounding electrode buried at
d=0.5 m in rho=5400 Ohm.m, epsilon_r=10 soil, DC-100 MHz.

The xlsx frequency column was originally digitized 10x too low (the plot's
"f (Hz) x 10^7" axis multiplier was misread as x10^6 — confirmed by tracing
the source figure's pixels directly and by the L=2 m, epsilon_r=10 quarter-
wave resonance estimate, ~1.2e7 Hz, landing on the first null); it has since
been corrected in place (frequency column x10). The paper's own axis is
linear-linear; this script plots log-log instead to match the style of the
other comparisons in this folder.

Usage (from repo root):
    ./fortran/build.sh   # if the Tupa binary isn't built yet
    cd fortran && fpm run --profile release -- -q ../common/poljak_fig4.json
    cd .. && python3 docs/validation/plot_poljak_fig4.py
"""
import json
import math
from pathlib import Path

import matplotlib.pyplot as plt
import openpyxl

plt.rcParams["svg.fonttype"] = "path"

REPO_ROOT = Path(__file__).resolve().parents[2]
DIGITIZED_XLSX = REPO_ROOT / "docs" / "validation" / "Poljak_fig4.xlsx"
RESULTS_DIR = REPO_ROOT / "fortran"
OUTPUT_SVG = REPO_ROOT / "docs" / "figures" / "poljak-fig4-comparison.svg"

XLSX_HEADER_ROWS = 5  # data starts on row 6 (0-indexed row 5)

TUPA_COLOR = "#1a73e8"
DIGITIZED_COLOR = "#d93025"


def load_digitized_points() -> tuple[list[float], list[float]]:
    """Read all raw digitized (f, |Z|) points out of the xlsx."""
    wb = openpyxl.load_workbook(DIGITIZED_XLSX, read_only=True, data_only=True)
    ws = wb.active
    rows = list(ws.iter_rows(values_only=True))[XLSX_HEADER_ROWS:]

    points = sorted(
        (row[0], row[1]) for row in rows if row[0] is not None and row[1] is not None
    )
    return [p[0] for p in points], [p[1] for p in points]


def load_tupa_curve() -> tuple[list[float], list[float]]:
    """Load TUPÃ's |Z(omega)| curve from its results JSON."""
    path = RESULTS_DIR / "poljak_fig4_results.json"
    if not path.exists():
        raise FileNotFoundError(
            f"{path} not found — run `./fortran/build.sh` and then `fpm run --profile release -- -q ../common/poljak_fig4.json` from fortran/ first"
        )
    data = json.loads(path.read_text())
    freqs = data["frequencies"]
    impedances = [math.hypot(z["re"], z["im"]) for z in data["derived"]["inputImpedance"]]
    return freqs, impedances


def main() -> None:
    fig, ax = plt.subplots(figsize=(9, 6))

    tupa_f, tupa_z = load_tupa_curve()
    dig_f, dig_z = load_digitized_points()

    ax.loglog(
        tupa_f, tupa_z, color=TUPA_COLOR, linewidth=1.8,
        label="TUPÃ",
    )
    ax.loglog(
        dig_f, dig_z, color=DIGITIZED_COLOR, linewidth=0, marker="o",
        markersize=4, markerfacecolor="none", markeredgewidth=1.1,
        label="Poljak & Doric 2006, Fig. 4",
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
