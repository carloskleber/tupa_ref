#!/usr/bin/env python3
"""Regenerate the voltage-source and two-node-source checks
(docs/validation/sources-validation.md).

Usage: sources_checks.py <tupa executable> [workdir]

Two vertical 3 m rods 10 m apart (nodes A_top, B_top at 0.5 m depth) in
sigma = 0.01 S/m soil. The reference is the nodal impedance matrix Z the
code itself returns for unit current injections; every source variant is
compared against what superposition predicts from it:

  harmonic   ideal voltage source, mixed voltage + current sources,
             two-node current dipole, two-node (delta-gap) voltage source;
  transient  slow surges, where the response must collapse to the DC
             resistance (single-node voltage source, two-node voltage
             source, two-node current source), and a fast surge, where the
             pinned voltage must follow the waveform.

Runs with the Fortran or the Rust executable (same CLI and output files).
Standard library only.
"""
import copy
import csv
import json
import os
import subprocess
import sys
import tempfile
from collections import defaultdict

OUT_NODES = ["A_top", "B_top"]


def base():
    return {
        "title": "two rods",
        "soil": {"conductivity": 0.01, "permittivity": 10.0, "permeability": 1.0},
        "nodes": [
            {"id": "A_top", "position": [0, 0, -0.5]},
            {"id": "A_bot", "position": [0, 0, -3.5]},
            {"id": "B_top", "position": [10, 0, -0.5]},
            {"id": "B_bot", "position": [10, 0, -3.5]},
        ],
        "materials": [{"id": "copper", "epsilonr": 1.0, "mur": 1.0, "sigma": 5.96e7}],
        "elements": [
            {"type": "line", "id": "Rod_A", "from": "A_top", "to": "A_bot",
             "radius": 0.01, "segments": 6, "material": "copper"},
            {"type": "line", "id": "Rod_B", "from": "B_top", "to": "B_bot",
             "radius": 0.01, "segments": 6, "material": "copper"},
        ],
        "frequencies": {"min": 10.0, "max": 1.0e6, "pointsPerDecade": 4},
        "outputs": {"nodes": OUT_NODES, "electrodes": ["Rod_A_e1", "Rod_B_e1"],
                    "quantities": ["voltage", "i1", "i2", "inputImpedance"]},
    }


def cx(re, im=0.0):
    return {"re": re, "im": im}


class Runner:
    def __init__(self, exe, workdir):
        self.exe, self.workdir, self.n = exe, workdir, 0

    def go(self, case):
        self.n += 1
        name = f"case{self.n}"
        with open(os.path.join(self.workdir, name + ".json"), "w") as f:
            json.dump(case, f)
        subprocess.run([self.exe, "-q", name + ".json"], cwd=self.workdir, check=True,
                       stdout=subprocess.DEVNULL)
        return os.path.join(self.workdir, name)

    def harmonic(self, case):
        stem = self.go(case)
        out = defaultdict(dict)
        with open(stem + "_results.csv") as f:
            rows = csv.reader(f)
            next(rows)
            for fq, q, i, re, im in rows:
                out[(q, i)][float(fq)] = complex(float(re), float(im))
        # the single-port input impedance is a derived quantity: JSON only
        with open(stem + "_results.json") as f:
            res = json.load(f)
        zin = res.get("derived", {}).get("inputImpedance")
        if zin:
            out[("inputImpedance", "")] = {float(fq): complex(z["re"], z["im"])
                                           for fq, z in zip(res["frequencies"], zin)}
        return out

    def transient(self, case):
        stem = self.go(case)
        out = defaultdict(list)
        with open(stem + "_transient_results.csv") as f:
            rows = csv.reader(f)
            next(rows)
            for t, q, i, v in rows:
                out[(q, i)].append(float(v))
        return out


def maxrel(a, b):
    """max |a-b| / max |b| over matching keys of two {freq: complex} dicts."""
    return max(abs(a[k] - b[k]) for k in b) / max(abs(v) for v in b.values())


def harmonic_checks(r):
    freqs = None
    z = {}  # (field, source) -> {f: V}
    for src in ("A_top", "B_top"):
        c = base()
        c["sources"] = [{"node": src, "current": cx(1.0)}]
        h = r.harmonic(c)
        z[src] = {n: h[("voltage", n)] for n in OUT_NODES}
        freqs = sorted(z[src]["A_top"])
    Z11, Z21 = z["A_top"]["A_top"], z["A_top"]["B_top"]
    Z12, Z22 = z["B_top"]["A_top"], z["B_top"]["B_top"]
    f = freqs
    rows = []

    rows.append(("reciprocity Z_AB vs Z_BA", max(abs(Z12[k] - Z21[k]) / abs(Z21[k]) for k in f)))

    # 1. ideal voltage source at A_top (100 V)
    U = 100.0
    c = base()
    c["sources"] = [{"node": "A_top", "voltage": cx(U)}]
    h = r.harmonic(c)
    pin = max(abs(h[("voltage", "A_top")][k] - U) for k in f) / U
    zin_v = h[("inputImpedance", "")]
    rows.append(("voltage source: pinned |u_A - U| / U", pin))
    rows.append(("voltage source: Z_in vs current-source Z_in", max(abs(zin_v[k] - Z11[k]) / abs(Z11[k]) for k in f)))
    rows.append(("voltage source: u_B vs Z_BA U / Z_AA",
                 max(abs(h[("voltage", "B_top")][k] - Z21[k] * U / Z11[k]) / abs(Z21[k] * U / Z11[k]) for k in f)))

    # 2. mixed: voltage 100 V at A_top, 2 A at 30 degrees at B_top
    import cmath
    Ib = 2.0 * cmath.exp(1j * cmath.pi / 6)
    c = base()
    c["sources"] = [{"node": "A_top", "voltage": cx(U)},
                    {"node": "B_top", "current": cx(Ib.real, Ib.imag)}]
    h = r.harmonic(c)
    errs_a, errs_b = [], []
    for k in f:
        Ia = (U - Z12[k] * Ib) / Z11[k]
        ub = Z21[k] * Ia + Z22[k] * Ib
        errs_a.append(abs(h[("voltage", "A_top")][k] - U) / U)
        errs_b.append(abs(h[("voltage", "B_top")][k] - ub) / abs(ub))
    rows.append(("mixed V@A + I@B: pinned u_A", max(errs_a)))
    rows.append(("mixed V@A + I@B: u_B vs superposition", max(errs_b)))

    # 3. two-node current dipole A(+) / B(-), 1 A
    c = base()
    c["sources"] = [{"node": "A_top", "returnNode": "B_top", "current": cx(1.0)}]
    h = r.harmonic(c)
    zd = {k: Z11[k] + Z22[k] - Z21[k] - Z12[k] for k in f}
    ua, ub = h[("voltage", "A_top")], h[("voltage", "B_top")]
    rows.append(("current dipole: u_A - u_B vs Z11+Z22-2Z12", max(abs(ua[k] - ub[k] - zd[k]) / abs(zd[k]) for k in f)))
    rows.append(("current dipole: u_A vs Z11 - Z12", max(abs(ua[k] - (Z11[k] - Z12[k])) / abs(Z11[k] - Z12[k]) for k in f)))
    zin_d = h.get(("inputImpedance", ""))
    if zin_d:
        rows.append(("current dipole: reported Z_in vs dipole Z", max(abs(zin_d[k] - zd[k]) / abs(zd[k]) for k in f)))

    # 4. two-node delta-gap voltage source, 100 V
    c = base()
    c["sources"] = [{"node": "A_top", "returnNode": "B_top", "voltage": cx(U)}]
    h = r.harmonic(c)
    ua, ub = h[("voltage", "A_top")], h[("voltage", "B_top")]
    rows.append(("delta gap: pinned u_A - u_B vs U", max(abs(ua[k] - ub[k] - U) for k in f) / U))
    i_a = {k: U / zd[k] for k in f}
    rows.append(("delta gap: u_A vs (Z11 - Z12) U / Zd",
                 max(abs(ua[k] - (Z11[k] - Z12[k]) * i_a[k]) / abs((Z11[k] - Z12[k]) * i_a[k]) for k in f)))
    zin_g = h.get(("inputImpedance", ""))
    if zin_g:
        rows.append(("delta gap: reported Z_in vs dipole Z", max(abs(zin_g[k] - zd[k]) / abs(zd[k]) for k in f)))

    print("| Check | worst relative error over 10 Hz-1 MHz |")
    print("| --- | --- |")
    for name, e in rows:
        print(f"| {name} | {e:.1e} |")
    print(f"\nDC (10 Hz) values: Z_AA = {Z11[f[0]].real:.4f} ohm, Z_AB = {Z21[f[0]].real:.4f} ohm, "
          f"dipole Z = {zd[f[0]].real:.4f} ohm")
    return zd[f[0]].real, Z11[f[0]].real


def transient_checks(r, rd, ra):
    """rd: dipole DC resistance, ra: single-rod DC resistance (ohm)."""
    sig = {"observeNodes": OUT_NODES, "observeElectrodes": ["Rod_A_e1"],
           "nyquistHz": 1.0e5, "fftPoints": 512, "transform": "nlt"}

    def case(src, extra=None):
        c = base()
        c.pop("frequencies")
        s = dict(sig)
        s["sources"] = [src]
        s.update(extra or {})
        c["signal"] = s
        return c

    slow = {"waveform": "doubleExp", "imax": 100.0, "front": "f250_2500"}
    rows = []

    def span(series, frac=0.5):
        return slice(0, int(len(series) * frac))

    def rel(a, b, sl):
        peak = max(abs(x) for x in b[sl])
        return max(abs(x - y) for x, y in zip(a[sl], b[sl])) / peak

    # single-node voltage source: i = U(t)/R_AA(DC). Re Z_AA falls by ~0.3 % by
    # 1 kHz (soil skin depth 160 m there), so the error is the surge's bandwidth
    for tf, ny in ((2.0e-3, 5.0e3), (2.0e-4, 5.0e4), (2.5e-5, 5.0e5)):
        surge = {"waveform": "portela", "imax": 100.0, "alpha": 0.0, "tFront": tf,
                 "tTopEnd": 4 * tf, "tTailEnd": 16 * tf, "quantity": "voltage"}
        t = r.transient(case({"node": "A_top", **surge}, {"nyquistHz": ny}))
        u = t[("injectedCurrent", "A_top")]
        sl = span(u)
        rows.append((f"voltage source, {tf * 1e6:g} us front: i1 vs U(t)/R_AA(DC)",
                     rel(t[("i1", "Rod_A_e1")], [x / ra for x in u], sl)))
        rows.append((f"voltage source, {tf * 1e6:g} us front: pinned u_A vs U(t)",
                     rel(t[("voltage", "A_top")], u, sl)))

    # two-node voltage source
    t = r.transient(case({"node": "A_top", "returnNode": "B_top", "quantity": "voltage", **slow}))
    u = t[("injectedCurrent", "A_top")]
    i1 = t[("i1", "Rod_A_e1")]
    d = [a - b for a, b in zip(t[("voltage", "A_top")], t[("voltage", "B_top")])]
    rows.append(("delta gap, slow surge: i1 vs U(t)/R_dipole", rel(i1, [x / rd for x in u], sl)))
    rows.append(("delta gap, slow surge: pinned u_A - u_B vs U(t)", rel(d, u, sl)))

    # two-node current source
    t = r.transient(case({"node": "A_top", "returnNode": "B_top", **slow}))
    i = t[("injectedCurrent", "A_top")]
    d = [a - b for a, b in zip(t[("voltage", "A_top")], t[("voltage", "B_top")])]
    rows.append(("current dipole, slow surge: u_A - u_B vs R_dipole i(t)", rel(d, [rd * x for x in i], sl)))
    rows.append(("current dipole, slow surge: i1 vs i(t)", rel(t[("i1", "Rod_A_e1")], i, sl)))

    # fast surge with an FFT transform: the pinned difference follows the waveform
    fast = {"waveform": "doubleExp", "imax": 1000.0, "front": "f1_2_50"}
    for tr in ("fft", "nlt"):
        extra = {"transform": tr, "nyquistHz": 1.0e6, "fftPoints": 1024}
        t = r.transient(case({"node": "A_top", "returnNode": "B_top", "quantity": "voltage", **fast}, extra))
        u = t[("injectedCurrent", "A_top")]
        d = [a - b for a, b in zip(t[("voltage", "A_top")], t[("voltage", "B_top")])]
        n = len(u)
        rows.append((f"delta gap, 1.2/50 us surge, {tr.upper()}: pinned u_A - u_B vs U(t) (first half)",
                     rel(d, u, slice(0, n // 2))))

    print("| Check | worst error / peak, first half of the record |")
    print("| --- | --- |")
    for name, e in rows:
        print(f"| {name} | {e:.1e} |")


def main():
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    exe = os.path.abspath(sys.argv[1])
    workdir = sys.argv[2] if len(sys.argv) > 2 else tempfile.mkdtemp(prefix="tupa_sources_")
    os.makedirs(workdir, exist_ok=True)
    r = Runner(exe, workdir)
    print("## Harmonic\n")
    rd, ra = harmonic_checks(r)
    print("\n## Transient\n")
    transient_checks(r, rd, ra)


if __name__ == "__main__":
    main()
