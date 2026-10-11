#!/usr/bin/env python3
"""Regenerate the ROADMAP Phase 11 checks (docs/validation/gpr-potentials.md):
surface potentials, ground potential rise (GPR), touch and step voltage.

Usage: phase11_checks.py <tupa executable> [workdir]

  1. flush vertical rod: GPR against the Dwight/Sunde rod resistance, surface
     potential, touch and step voltage against the line-source closed form
  2. 16 x 16 m grid: GPR / resistance against Laurent-Niemann and Sverak,
     remote surface potential against the point-source field
  3. potential next to a conductor against the solved node voltage, over
     frequency
  4. the grid_safety fixture against its committed golden file

Runs with the Fortran or the Rust executable (same CLI and output files).
Standard library only.
"""
import csv
import json
import math
import os
import subprocess
import sys
import tempfile
from collections import defaultdict

ROOT = os.path.normpath(os.path.join(os.path.dirname(__file__), "..", ".."))
COMMON = os.path.join(ROOT, "common")
SIGMA = 0.01  # rod soil, S/m
# The kernel carries the soil attenuation exp(-gamma R), gamma = sqrt(j w mu sigma): the
# closed forms below are static, so the comparison is made where the skin depth
# (sqrt(2/(w mu sigma)) = 50 km at 10 mHz, 100 ohm.m) is far beyond the geometry.
F_LOW = 0.01  # Hz


def cx(re, im=0.0):
    return {"re": re, "im": im}


def run(exe, case, workdir, name):
    path = os.path.join(workdir, name + ".json")
    with open(path, "w") as f:
        json.dump(case, f)
    subprocess.run([exe, "-q", path], cwd=workdir, check=True, stdout=subprocess.DEVNULL)
    return os.path.join(workdir, name)


def read_potentials(stem):
    out = defaultdict(dict)  # (quantity, id) -> {f: complex}
    with open(stem + "_potentials.csv") as f:
        rows = csv.reader(f)
        next(rows)
        for fq, q, i, x, y, z, re, im in rows:
            out[(q, i)][float(fq)] = complex(float(re), float(im or 0.0))
    return out


def read_results(stem):
    out = defaultdict(dict)
    with open(stem + "_results.csv") as f:
        rows = csv.reader(f)
        next(rows)
        for fq, q, i, re, im in rows:
            out[(q, i)][float(fq)] = complex(float(re), float(im))
    with open(stem + "_results.json") as f:
        res = json.load(f)
    zin = res.get("derived", {}).get("inputImpedance")
    if zin:
        out[("inputImpedance", "")] = {float(fq): complex(z["re"], z["im"])
                                       for fq, z in zip(res["frequencies"], zin)}
    return out


def rod_case(radius=0.01, length=3.0):
    return {
        "title": "flush rod",
        "soil": {"conductivity": SIGMA, "permittivity": 10.0, "permeability": 1.0},
        "nodes": [{"id": "R_top", "position": [0, 0, 0.0]},
                  {"id": "R_bot", "position": [0, 0, -length]}],
        "materials": [{"id": "copper", "epsilonr": 1.0, "mur": 1.0, "sigma": 5.96e7}],
        "elements": [{"type": "line", "id": "Rod", "from": "R_top", "to": "R_bot",
                      "radius": radius, "segments": 12, "material": "copper"}],
        "sources": [{"node": "R_top", "current": cx(1.0)}],
        "frequencies": {"min": F_LOW, "max": F_LOW, "pointsPerDecade": 1},
        "outputs": {"nodes": ["R_top"], "quantities": ["voltage", "inputImpedance"]},
    }


def rod_checks(exe, workdir):
    L, a, rho = 3.0, 0.01, 1.0 / SIGMA
    radii = [0.5, 1.0, 2.0, 5.0, 10.0, 20.0, 50.0, 100.0, 400.0]
    case = rod_case(a, L)
    case["observation"] = {
        "points": [{"id": f"r{r:g}", "position": [r, 0.0, 0.0]} for r in radii],
        "touch": [{"id": "rod", "node": "R_top", "radius": 1.0, "points": 36}],
        "steps": [{"id": f"s{r:g}", "from": [r, 0.0, 0.0], "to": [r + 1.0, 0.0, 0.0]}
                  for r in (1.0, 2.0, 5.0, 10.0)],
    }
    stem = run(exe, case, workdir, "rod")
    pot = read_potentials(stem)
    res = read_results(stem)
    f0 = F_LOW

    def psi_exact(r):
        return rho / (2 * math.pi * L) * math.asinh(L / r)

    r_dwight = rho / (2 * math.pi * L) * (math.log(4 * L / a) - 1.0)
    gpr = res[("voltage", "R_top")][f0].real
    print(f"GPR of the flush rod (1 A, 10 mHz): {gpr:.3f} V; Dwight/Sunde "
          f"rho/(2 pi L) (ln(4L/a) - 1) = {r_dwight:.3f} ohm ({gpr / r_dwight - 1:+.2%})\n")
    print("| r (m) | TUPA psi (V) | line source psi (V) | error |")
    print("| --- | --- | --- | --- |")
    for r in radii:
        p = pot[("potential", f"r{r:g}")][f0].real
        e = psi_exact(r)
        print(f"| {r:g} | {p:.3f} | {e:.3f} | {p / e - 1:+.2%} |")
    t = pot[("touch", "rod")][f0].real
    t_exact = gpr - psi_exact(1.0)
    print(f"\nTouch voltage at 1 m: TUPA {t:.3f} V; GPR - psi_exact(1 m) = {t_exact:.3f} V "
          f"({t / t_exact - 1:+.2%}); GPR as reported by the touch block "
          f"{pot[('gpr', 'rod')][f0].real:.3f} V\n")
    print("| step pair (m) | TUPA (V) | line source (V) | error |")
    print("| --- | --- | --- | --- |")
    for r in (1.0, 2.0, 5.0, 10.0):
        s = abs(pot[("step", f"s{r:g}")][f0])
        e = psi_exact(r) - psi_exact(r + 1.0)
        print(f"| {r:g} - {r + 1:g} | {s:.4f} | {e:.4f} | {s / e - 1:+.2%} |")


def grid_checks(exe, workdir):
    with open(os.path.join(COMMON, "grid_safety.json")) as f:
        case = json.load(f)
    # grid alone: no riser, injected at a corner
    case["elements"] = [e for e in case["elements"] if e["type"] == "mesh"]
    case["nodes"] = []
    case["sources"] = [{"node": "g-0000", "current": cx(1000.0)}]
    case["frequencies"] = {"min": F_LOW, "max": F_LOW, "pointsPerDecade": 1}
    case["outputs"] = {"nodes": ["g-0000"], "quantities": ["voltage", "inputImpedance"]}
    case["observation"] = {"points": [{"id": "centre", "position": [8.0, 8.0, 0.0]},
                                      {"id": "remote", "position": [40.0, 0.0, 0.0]},
                                      {"id": "far", "position": [200.0, 0.0, 0.0]}]}
    stem = run(exe, case, workdir, "grid")
    res = read_results(stem)
    pot = read_potentials(stem)
    f0 = F_LOW
    rho, area, depth, lt = 1.0 / case["soil"]["conductivity"], 16.0 * 16.0, 0.5, 6 * 16.0
    r_eq = math.sqrt(area / math.pi)
    r_ln = rho / (4 * r_eq) + rho / lt
    r_sv = rho * (1 / lt + 1 / math.sqrt(20 * area) * (1 + 1 / (1 + depth * math.sqrt(20 / area))))
    z = res[("inputImpedance", "")][f0].real
    print(f"Grid resistance at 10 mHz (rho = {rho:g} ohm.m, A = 256 m2, L_T = 96 m, h = 0.5 m): "
          f"TUPA {z:.3f} ohm; equal-area disc rho/(4 r) {rho / (4 * r_eq):.3f} (lower bound); Laurent-Niemann {r_ln:.3f} ({z / r_ln - 1:+.1%}); "
          f"Sverak {r_sv:.3f} ({z / r_sv - 1:+.1%})\n")
    gpr = z * 1000.0
    print("| point | TUPA psi (V) | psi / GPR | point-source rho I / (2 pi d) (V) | error |")
    print("| --- | --- | --- | --- | --- |")
    for pid, (x, y) in {"centre": (8.0, 8.0), "remote": (40.0, 0.0), "far": (200.0, 0.0)}.items():
        p = pot[("potential", pid)][f0].real
        d = math.hypot(x - 8.0, y - 8.0)  # from the grid centre
        e = rho * 1000.0 / (2 * math.pi * d) if d > 20 else float("nan")
        err = f"{p / e - 1:+.2%}" if e == e else "—"
        es = f"{e:.1f}" if e == e else "—"
        print(f"| {pid} | {p:.1f} | {p / gpr:.3f} | {es} | {err} |")


def conductor_checks(exe, workdir):
    """psi beside a mid-rod segment against the mean of its two node voltages."""
    a = 0.01
    case = rod_case(a, 3.0)
    case["nodes"] = [{"id": "R_top", "position": [0, 0, -0.5]},
                     {"id": "R_bot", "position": [0, 0, -3.5]}]
    case["frequencies"] = {"min": 10.0, "max": 1.0e6, "pointsPerDecade": 1}
    case["elements"][0]["segments"] = 6
    case["outputs"] = {"quantities": ["voltage"]}
    # segment 3 of 6 runs from z = -1.5 to -2.0; observe beside its middle
    case["observation"] = {"points": [{"id": "beside", "position": [a, 0.0, -1.75]}]}
    stem = run(exe, case, workdir, "beside")
    pot = read_potentials(stem)
    res = read_results(stem)
    print("| f (Hz) | psi beside the segment | mean of its node voltages | difference |")
    print("| --- | --- | --- | --- |")
    for f in sorted(pot[("potential", "beside")]):
        if f not in (10.0, 1e3, 1e4, 1e5, 1e6):
            continue
        p = pot[("potential", "beside")][f]
        m = 0.5 * (res[("voltage", "Rod_n2")][f] + res[("voltage", "Rod_n3")][f])
        print(f"| {f:g} | {p.real:.4f} {p.imag:+.4f}j | {m.real:.4f} {m.imag:+.4f}j | {abs(p - m) / abs(m):.2%} |")


def fixture_check(exe, workdir):
    with open(os.path.join(COMMON, "grid_safety.json")) as f:
        case = json.load(f)
    stem = run(exe, case, workdir, "grid_safety")
    worst, n = 0.0, 0
    with open(stem + "_potentials.csv") as a, \
            open(os.path.join(COMMON, "grid_safety_potentials_expected.csv")) as b:
        ra, rb = csv.reader(a), csv.reader(b)
        next(ra)
        next(rb)
        peak = defaultdict(float)
        rows = list(zip(ra, rb))
        for x, y in rows:
            peak[y[1]] = max(peak[y[1]], abs(complex(float(y[6]), float(y[7] or 0))))
        for x, y in rows:
            d = abs(complex(float(x[6]), float(x[7] or 0)) - complex(float(y[6]), float(y[7] or 0)))
            worst = max(worst, d / peak[y[1]])
            n += 1
    print(f"{n} rows; worst |difference| / peak of its quantity: {worst:.1e}")


def main():
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    exe = os.path.abspath(sys.argv[1])
    workdir = sys.argv[2] if len(sys.argv) > 2 else tempfile.mkdtemp(prefix="tupa_phase11_")
    os.makedirs(workdir, exist_ok=True)
    print("## 1. Flush vertical rod (L = 3 m, a = 10 mm, 100 ohm.m, 1 A, 10 mHz)\n")
    rod_checks(exe, workdir)
    print("\n## 2. 16 x 16 m grid (200 ohm.m, 1 kA at a corner)\n")
    grid_checks(exe, workdir)
    print("\n## 3. Potential beside a conductor vs its node voltages\n")
    conductor_checks(exe, workdir)
    print("\n## 4. grid_safety fixture against the committed golden file\n")
    fixture_check(exe, workdir)


if __name__ == "__main__":
    main()
