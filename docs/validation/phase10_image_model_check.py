#!/usr/bin/env python3
"""Effect of the Phase 10 default image model on the published-curve comparisons.

ROADMAP Phase 10 item 2 makes the frequency-dependent image reflection
coefficient Gamma(w) the default; the writeups in this folder were produced
with ideal images (Gamma = +-1). This script runs every comparison case with
both models (`--image-model ideal | frequency-dependent`, the 1-D geometry
kernel in both) and reports, against the digitized points of the same
reference figures, the mean absolute percentage error (MAPE) and the worst
point, below 1 MHz and over the whole band. It reuses the digitized-point
loaders of the plot scripts.

Usage (from the repo root; needs the Fortran executable and the packages of
requirements.txt):

    python3 docs/validation/phase10_image_model_check.py --fortran-bin PATH
"""
import argparse
import importlib.util
import json
import math
import subprocess
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]


def load_module(name):
    spec = importlib.util.spec_from_file_location(name, HERE / f"{name}.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def run_case(fortran_bin, case, model, tmp):
    subprocess.run([fortran_bin, "-q", "--image-model", model, str(ROOT / "common" / f"{case}.json")],
                   cwd=tmp, check=True, capture_output=True)
    data = json.loads((Path(tmp) / f"{case}_results.json").read_text())
    return data["frequencies"], [math.hypot(z["re"], z["im"]) for z in data["derived"]["inputImpedance"]]


def interp_loglog(f, fs, zs):
    """|Z| at f from the (fs, zs) curve, log-log interpolation."""
    for k in range(len(fs) - 1):
        if fs[k] <= f <= fs[k + 1]:
            t = (math.log(f) - math.log(fs[k])) / (math.log(fs[k + 1]) - math.log(fs[k]))
            return math.exp(math.log(zs[k]) + t * (math.log(zs[k + 1]) - math.log(zs[k])))
    return None


def stats(dig_f, dig_z, fs, zs):
    out = {}
    for name, hi in (("<= 1 MHz", 1e6), ("all", math.inf)):
        errs = []
        for f, z in zip(dig_f, dig_z):
            if f > hi:
                continue
            t = interp_loglog(f, fs, zs)
            if t is not None:
                errs.append((t - z) / z * 100)
        out[name] = (sum(abs(e) for e in errs) / len(errs), max(errs, key=abs), len(errs))
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--fortran-bin", required=True)
    args = ap.parse_args()
    grcev, lima6, lima7 = load_module("plot_grcev_fig12"), load_module("plot_lima_fig6"), load_module("plot_lima_fig7")
    poljak, silva3 = load_module("plot_poljak_fig4"), load_module("plot_silva2025_fig3")

    cases = []  # (label, case file stem, digitized f, z)
    dg = grcev.load_digitized_points()
    for (length, rho), (f, z) in sorted(dg.items()):
        cases.append((f"Grcev Fig. 12, l={length} m, rho={rho}", f"grcev_fig12_l{length}_rho{rho}", f, z))
    f, z = lima6.load_digitized_points()
    cases.append(("Lima Fig. 6 (case 9)", "lima_fig6", f, z))
    for c, (f, z) in sorted(lima7.load_digitized_points().items()):
        cases.append((f"Lima Fig. 7 (case {c})", f"lima_fig7_case{c}", f, z))
    f, z = poljak.load_digitized_points()
    cases.append(("Poljak-Doric Fig. 4", "poljak_fig4", f, z))
    for rho, (f, z) in sorted(silva3.load_digitized_points().items()):
        cases.append((f"Silva Fig. 3, rho0={rho}", f"silva2025_rho{rho}", f, z))

    lines = ["| Comparison | n (≤ 1 MHz / all) | MAPE ≤ 1 MHz: ideal → Γ(ω) | worst ≤ 1 MHz: ideal → Γ(ω) | MAPE all: ideal → Γ(ω) | worst all: ideal → Γ(ω) |",
             "| --- | --- | --- | --- | --- | --- |"]
    with tempfile.TemporaryDirectory() as tmp:
        for label, case, f, z in cases:
            res = {m: stats(f, z, *run_case(args.fortran_bin, case, m, tmp)) for m in ("ideal", "frequency-dependent")}
            i, d = res["ideal"], res["frequency-dependent"]
            row = (f"| {label} | {i['<= 1 MHz'][2]} / {i['all'][2]} | {i['<= 1 MHz'][0]:.2f} → {d['<= 1 MHz'][0]:.2f} % "
                   f"| {i['<= 1 MHz'][1]:+.1f} → {d['<= 1 MHz'][1]:+.1f} % | {i['all'][0]:.2f} → {d['all'][0]:.2f} % "
                   f"| {i['all'][1]:+.1f} → {d['all'][1]:+.1f} % |")
            print(row)
            lines.append(row)
    print("\n".join(lines), file=sys.stderr)


if __name__ == "__main__":
    main()
