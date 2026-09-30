#!/usr/bin/env python3
"""Compare TUPÃ (Fortran) with the TAGS/mHEM prototype on Grcev et al. Fig. 12.

Inputs are the 10 m horizontal-electrode studies common/grcev_fig12_l10_rho*.json
(run them first, see plot_grcev_fig12.py), the mHEM results tabulated in
mhem-grcev-l10-results.csv, the Julia port's results in
julia-grcev-l10-results.csv (julia/comparison/run_tupa_grcev_l10.jl), and the
digitized full-wave reference grcev_fig12.xlsx. Writes a metrics CSV and a
comparison figure.

Usage (from repo root):
    .venv/bin/python docs/validation/compare_fortran_mhem.py
"""
import csv
import json
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import openpyxl

plt.rcParams["svg.fonttype"] = "path"

REPO_ROOT = Path(__file__).resolve().parents[2]
VALIDATION = REPO_ROOT / "docs" / "validation"
MHEM_CSV = VALIDATION / "mhem-grcev-l10-results.csv"
JULIA_CSV = VALIDATION / "julia-grcev-l10-results.csv"
DIGITIZED_XLSX = VALIDATION / "grcev_fig12.xlsx"
RESULTS_DIR = REPO_ROOT / "fortran"
METRICS_CSV = VALIDATION / "tupa-vs-mhem-metrics.csv"
OUTPUT_SVG = REPO_ROOT / "docs" / "figures" / "tupa-vs-mhem-grcev-l10.svg"

RHO_VALUES = (30, 300, 3000)
XLSX_COLUMNS = {30: (0, 1), 300: (2, 3), 3000: (4, 5)}  # (f, |Z|), length 10 m
XLSX_HEADER_ROWS = 3
BANDS = (("100 Hz-10 MHz", 1e99), ("100 Hz-1 MHz", 1e6))
RHO_COLOR = {30: "#2a78d6", 300: "#008300", 3000: "#e87ba4"}


def load_tupa(rho):
    path = RESULTS_DIR / f"grcev_fig12_l10_rho{rho}_results.json"
    if not path.exists():
        raise FileNotFoundError(
            f"{path} not found - run `fpm run --profile release -- "
            f"-q ../common/grcev_fig12_l10_rho{rho}.json` from fortran/ first"
        )
    data = json.loads(path.read_text())
    z = np.array([complex(v["re"], v["im"]) for v in data["derived"]["inputImpedance"]])
    return np.array(data["frequencies"], dtype=float), z


def load_solver_csv(path):
    rows = {rho: [] for rho in RHO_VALUES}
    with path.open() as fh:
        for r in csv.DictReader(fh):
            rows[int(float(r["rho_ohm_m"]))].append(
                (float(r["frequency_hz"]), complex(float(r["z_real_ohm"]), float(r["z_imag_ohm"])))
            )
    out = {}
    for rho, pts in rows.items():
        pts.sort()
        out[rho] = (np.array([p[0] for p in pts]), np.array([p[1] for p in pts]))
    return out


def load_reference():
    ws = openpyxl.load_workbook(DIGITIZED_XLSX, read_only=True, data_only=True).active
    rows = list(ws.iter_rows(values_only=True))[XLSX_HEADER_ROWS:]
    out = {}
    for rho, (fc, zc) in XLSX_COLUMNS.items():
        pts = sorted((r[fc], r[zc]) for r in rows if r[fc] is not None and r[zc] is not None)
        out[rho] = (np.array([p[0] for p in pts], float), np.array([p[1] for p in pts], float))
    return out


def on_grid(f_new, f, z):
    """Interpolate a complex curve onto f_new, linear in log10(f)."""
    x, xn = np.log10(f), np.log10(f_new)
    return np.interp(xn, x, z.real) + 1j * np.interp(xn, x, z.imag)


def vs_reference(f_ref, z_ref, f, z):
    est = np.abs(on_grid(f_ref, f, z))
    return est, (est - z_ref) / z_ref


def main():
    reference = load_reference()
    mhem, julia = load_solver_csv(MHEM_CSV), load_solver_csv(JULIA_CSV)
    metrics, curves = [], {}
    for rho in RHO_VALUES:
        f_t, z_t = load_tupa(rho)
        f_m, z_m = mhem[rho]
        f_r, z_r = reference[rho]
        f_j, z_j = julia[rho]
        z_m_on_t = on_grid(f_t, f_m, z_m)
        z_j_on_t = on_grid(f_t, f_j, z_j)
        curves[rho] = (f_t, z_t, f_m, z_m, f_r, z_r)

        for band, fmax in BANDS:
            mask = f_r <= fmax
            for solver, (f, z) in (("TUPA (Fortran)", (f_t, z_t)), ("TUPA (Julia)", (f_j, z_j)),
                                   ("mHEM", (f_m, z_m))):
                est, err = vs_reference(f_r[mask], z_r[mask], f, z)
                metrics.append({
                    "comparison": f"{solver} vs full-wave reference", "rho_ohm_m": rho,
                    "band": band, "points": int(mask.sum()),
                    "mean_abs_percent": 100 * np.mean(np.abs(err)),
                    "max_abs_percent": 100 * np.max(np.abs(err)),
                    "relative_l2_percent": 100 * np.linalg.norm(est - z_r[mask]) / np.linalg.norm(z_r[mask]),
                })
            m = f_t <= fmax
            for label, other in (("TUPA (Fortran) vs mHEM", z_m_on_t),
                                 ("TUPA (Fortran) vs TUPA (Julia)", z_j_on_t)):
                diff = np.abs(np.abs(z_t[m]) - np.abs(other[m])) / np.abs(other[m])
                metrics.append({
                    "comparison": label + " (Fortran grid)", "rho_ohm_m": rho,
                    "band": band, "points": int(m.sum()),
                    "mean_abs_percent": 100 * np.mean(diff),
                    "max_abs_percent": 100 * np.max(diff),
                    "relative_l2_percent": 100 * np.linalg.norm(z_t[m] - other[m]) / np.linalg.norm(other[m]),
                })

    with METRICS_CSV.open("w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(metrics[0]))
        w.writeheader()
        for row in metrics:
            w.writerow({k: (f"{v:.4g}" if isinstance(v, float) else v) for k, v in row.items()})
    print(f"Wrote {METRICS_CSV}")
    for row in metrics:
        print(f"{row['comparison']:46s} rho={row['rho_ohm_m']:<5} {row['band']:14s} "
              f"n={row['points']:<3} mean {row['mean_abs_percent']:6.2f}%  max {row['max_abs_percent']:7.2f}%  "
              f"L2 {row['relative_l2_percent']:6.2f}%")

    fig, (ax, axd) = plt.subplots(2, 1, figsize=(8, 8), sharex=True, height_ratios=(3, 1.4))
    for rho in RHO_VALUES:
        f_t, z_t, f_m, z_m, f_r, z_r = curves[rho]
        c = RHO_COLOR[rho]
        ax.loglog(f_t, np.abs(z_t), "-", color=c, label=f"TUPÃ, ρ = {rho} Ω·m")
        ax.loglog(f_m, np.abs(z_m), "--", color="black", lw=0.9)
        f_j, z_j = julia[rho]
        ax.loglog(f_j, np.abs(z_j), ":", color="black", lw=1.2)
        ax.loglog(f_r, z_r, "o", color=c, ms=3.5, mfc="none")
        axd.semilogx(f_t, 100 * (np.abs(z_t) / np.abs(on_grid(f_t, f_m, z_m)) - 1), color=c)
        axd.semilogx(f_t, 100 * (np.abs(z_t) / np.abs(on_grid(f_t, f_j, z_j)) - 1), ":", color=c)
    ax.plot([], [], "--", color="black", lw=0.9, label="mHEM prototype")
    ax.plot([], [], ":", color="black", lw=1.2, label="TUPÃ Julia port")
    ax.plot([], [], "o", color="gray", ms=3.5, mfc="none", label="Grcev et al. 2018 Fig. 12 (digitized)")
    ax.set_ylabel("|Z| (Ω)")
    ax.legend(fontsize=8)
    ax.grid(True, which="both", alpha=0.3)
    axd.axhline(0, color="gray", lw=0.6)
    axd.set_xlabel("Frequency (Hz)")
    axd.set_ylabel("Fortran vs mHEM (solid),\nvs Julia (dotted) (%)", fontsize=8)
    axd.grid(True, which="both", alpha=0.3)
    fig.tight_layout()
    OUTPUT_SVG.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(OUTPUT_SVG, format="svg")
    fig.savefig(OUTPUT_SVG.with_suffix(".pdf"), format="pdf")
    print(f"Wrote {OUTPUT_SVG} and .pdf")


if __name__ == "__main__":
    main()
