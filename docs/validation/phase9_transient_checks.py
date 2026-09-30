#!/usr/bin/env python3
"""Regenerate the ROADMAP Phase 9 transient-option checks
(docs/validation/phase9-transient-options.md).

Usage: phase9_transient_checks.py <tupa executable> [workdir]

Builds case variants of common/silva2025_rho*_transient.json (scan-fed vs
per-bin transfer function) and of a slow-surge Portela 1997 conductor
(NLT vs FFT against a 16x longer record), runs them with the given TUPA
executable (Fortran or Rust; same CLI and output files) and prints the
tables of the writeup. Standard library only.
"""
import copy
import csv
import json
import os
import subprocess
import sys
import tempfile
import time
from collections import defaultdict

ROOT = os.path.normpath(os.path.join(os.path.dirname(__file__), "..", ".."))
COMMON = os.path.join(ROOT, "common")


def series(csv_file):
    out = defaultdict(list)
    with open(csv_file) as f:
        rows = csv.reader(f)
        next(rows)
        for t, q, i, v in rows:
            out[(q, i)].append(float(v))
    return out


def run(exe, case, workdir, name):
    path = os.path.join(workdir, name + ".json")
    with open(path, "w") as f:
        json.dump(case, f)
    t0 = time.perf_counter()
    subprocess.run([exe, "-q", path], cwd=workdir, check=True, stdout=subprocess.DEVNULL)
    return series(os.path.join(workdir, name + "_transient_results.csv")), time.perf_counter() - t0


def scan_fed(exe, workdir):
    print("| rho0 (Ohm.m) | GPR Node_1 | V Node_2 (60 m) | i1 Line_1_e30 | i2 Line_1_e30 | time full / interpolated (s) |")
    print("| --- | --- | --- | --- | --- | --- |")
    for rho in (100, 300, 1000, 2400):
        with open(os.path.join(COMMON, f"silva2025_rho{rho}_transient.json")) as f:
            base = json.load(f)
        base["signal"].update(freqZeroHz=100.0, observeNodes=["Node_1", "Node_2"],
                              observeElectrodes=["Line_1_e30"])
        interp = copy.deepcopy(base)
        interp["signal"]["transferFunction"] = "interpolated"
        interp["frequencies"] = {"min": 100.0, "max": 4.0e6, "pointsPerDecade": 27.6}
        full, tf = run(exe, base, workdir, f"full{rho}")
        fed, ti = run(exe, interp, workdir, f"interp{rho}")
        cells = []
        for key in (("voltage", "Node_1"), ("voltage", "Node_2"), ("i1", "Line_1_e30"), ("i2", "Line_1_e30")):
            a, b = full[key], fed[key]
            peak = max(abs(x) for x in a)
            cells.append(f"{max(abs(x - y) for x, y in zip(a, b)) / peak:.1e}")
        print(f"| {rho} | " + " | ".join(cells) + f" | {tf:.2f} / {ti:.2f} |")


def nlt(exe, workdir):
    with open(os.path.join(COMMON, "portela1997_transient.json")) as f:
        base = json.load(f)
    base["signal"].update(front="f250_2500", imax=1000.0, nyquistHz=1.0e4, fftPoints=256,
                          observeNodes=["Node_1"])
    base["signal"].pop("observeElectrodes", None)
    long_ = copy.deepcopy(base)
    long_["signal"]["fftPoints"] = 4096
    nltc = copy.deepcopy(base)
    nltc["signal"]["transform"] = "nlt"
    ref, _ = run(exe, long_, workdir, "long")
    fft, _ = run(exe, base, workdir, "fft")
    lap, _ = run(exe, nltc, workdir, "nlt")
    key = ("voltage", "Node_1")
    print("| Samples compared (of 256) | FFT, 256 samples | NLT, 256 samples |")
    print("| --- | --- | --- |")
    for n in (64, 128, 192, 256):
        peak = max(abs(x) for x in ref[key][:n])
        e = [max(abs(r[key][k] - ref[key][k]) for k in range(n)) / peak for r in (fft, lap)]
        print(f"| first {n} | {e[0]:.1e} | {e[1]:.1e} |")


def main():
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    exe = os.path.abspath(sys.argv[1])
    workdir = sys.argv[2] if len(sys.argv) > 2 else tempfile.mkdtemp(prefix="tupa_phase9_")
    os.makedirs(workdir, exist_ok=True)
    print("## Scan-fed vs per-bin transfer function (max |error| / series peak)\n")
    scan_fed(exe, workdir)
    print("\n## NLT vs FFT against a 16x longer FFT record (max |error| / peak over the span)\n")
    nlt(exe, workdir)


if __name__ == "__main__":
    main()
