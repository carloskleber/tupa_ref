#!/usr/bin/env python3
"""Validation of the lightning-induced voltage anchor (ROADMAP Phase 10d, ADR 0028).

Reproduces the tables of docs/validation/induced-validation.md with the
Fortran CLI. The reference is Nucci, Rachidi, Ianoz & Mazzetti (IEEE TEMC
35(1), 1993; references.md [75]): a 1 km, 10 m high single-wire line, matched
at both ends, over perfect ground, a subsequent stroke 50 m from its centre.

Cases are generated here (one function, `build_case`) so that every variant
differs from the base by an explicit parameter; the two base cases are the
committed `common/induced_nucci1993.json` (variant A: the matching resistor
sits at the wire end) and `common/induced_nucci1993_risers.json` (variant B:
vertical risers with the resistor at their foot, the boundary condition of the
reference). `--write-common` regenerates them.

Sections (run all, or pick with --only):

  base      variants A and B against the base row of Tables III-IV
  sweep     the trends of Table III: peak current, di/dt, return-stroke speed
  fig14     the formulation test: 500 m line, stroke on its prolongation
  conv      sensitivity: line/channel segmentation, time step, R', channel
            length, wire radius
  internal  matched-line Z_c, channel current attenuation
  lossy     the Rachidi 1996 lossy-ground variant (information only)

Usage (from the repo root; needs the Fortran executable and numpy):

    python3 docs/validation/induced_checks.py --fortran-bin PATH [--only base,sweep] [--write-common]
"""
import argparse
import csv
import json
import math
import subprocess
import tempfile
from pathlib import Path

import numpy as np

ROOT = Path(__file__).resolve().parents[2]
C = 299792458.0

# Reference values: Nucci et al. 1993, Tables III and IV (MTL unless noted)
REF = {
    "base": (66.0, 190.0, 0.5, 1.4),
    "Ip=4.6": (28.0, 105.0, 0.3, 1.1),
    "Ip=30": (133.0, 225.0, 0.7, 2.1),
    "didt=12": (50.0, 65.0, 0.8, 2.2),
    "didt=120": (72.0, 290.0, 0.3, 1.1),
    "v=0.6e8": (69.0, 120.0, 0.7, 2.2),
    "v=2e8": (61.0, 220.0, 0.3, 0.8),
}
TCS = (74.0, 260.0, 0.3, 1.2)


# ----------------------------------------------------------------------------
# Channel-base current: sum of Heidler terms (Nucci 1993 eq. 2-3)
# ----------------------------------------------------------------------------
BASE_TERMS = [  # (I0 [A], tau1 [s], tau2 [s], n): typical subsequent stroke, Table I
    (10.7e3, 0.25e-6, 2.5e-6, 2),
    (6.5e3, 2.1e-6, 230e-6, 2),
]


def heidler(t, terms):
    out = np.zeros_like(t)
    for i0, t1, t2, n in terms:
        eta = math.exp(-(t1 / t2) * (n * t2 / t1) ** (1.0 / n))
        x = (t / t1) ** n
        out += i0 / eta * x / (1.0 + x) * np.exp(-t / t2)
    return out


def current_features(terms):
    t = np.arange(0.0, 60e-6, 5e-9)
    i = heidler(t, terms)
    return i.max(), np.gradient(i, t).max()


def fit_terms(ip_target, didt_target, terms=BASE_TERMS):
    """Scale the amplitudes by a and the front time constants by b so that the
    peak is `ip_target` (A) and the maximum slope `didt_target` (A/s), the
    construction of the sweep cases (Nucci 1993 fits its own coefficients but
    does not print them). Newton on (log a, log b), numerical Jacobian."""
    x = np.array([math.log(ip_target / 12.0e3), math.log(ip_target / 12.0e3)])

    def f(x):
        a, b = math.exp(x[0]), math.exp(x[1])
        ip, dd = current_features([(i0 * a, t1 * b, t2, n) for i0, t1, t2, n in terms])
        return np.array([math.log(ip / ip_target), math.log(dd / didt_target)])

    for _ in range(30):
        r = f(x)
        if np.max(np.abs(r)) < 1e-7:
            break
        jac = np.zeros((2, 2))
        for k in range(2):
            dx = np.zeros(2)
            dx[k] = 1e-4
            jac[:, k] = (f(x + dx) - r) / 1e-4
        x = x - np.linalg.solve(jac, r)
    a, b = math.exp(x[0]), math.exp(x[1])
    return [(i0 * a, t1 * b, t2, n) for i0, t1, t2, n in terms]


# ----------------------------------------------------------------------------
# Case generation
# ----------------------------------------------------------------------------
def graded_breaks(length, first, growth, maxseg):
    """The segment boundaries of `gradedBreaks` (fortran/src/element/Channel.f90)."""
    seg, pos, out = first, 0.0, [0.0]
    k = 0
    while pos < length - 1e-12:
        seg = min(first * growth**k, maxseg)
        pos += seg
        out.append(pos)
        k += 1
    scale = length / out[-1]
    return [b * scale for b in out]


def zc_line(h, a):
    return 60.0 * math.acosh(h / a)


def build_case(variant="A", *, xs=-500.0, xe=500.0, ys=50.0, strike=(0.0, 0.0), h=10.0, a=5.0e-3,
               line_seg=10.0, terms=None, v=1.3e8, lam=1700.0, rprime=None, ch_len=3000.0, ch_radius=0.03,
               first=5.0, growth=1.15, maxseg=20.0, nyq=1.0e7, npts=1024, ground=None,
               observe_currents=False, sigma_wire=1.0e9, calibrate=True, title=None):
    """The base case of Nucci et al. 1993 and its variants. `variant` "A": the matching
    resistor is connected to the wire end node; "B": vertical risers of height h
    with the resistor at their foot. `rprime`: channel resistance per metre,
    default 2 Zc(v)/lam at z = 500 m (theory.md 4.6.1)."""
    terms = terms or BASE_TERMS
    if rprime is None:
        zc_ch = (C / v) * 60.0 * math.log(2.0 * 500.0 / ch_radius)
        rprime = round(2.0 * zc_ch / lam, 3)
    rmatch = round(zc_line(h, a), 3)
    n = int(round((xe - xs) / line_seg))
    nodes = [{"id": "Lw", "position": [xs, ys, h]}, {"id": "Le", "position": [xe, ys, h]}]
    elements = [{"type": "line", "id": "Line", "from": "Lw", "to": "Le", "radius": a, "segments": n, "material": "pec"}]
    if variant == "A":
        term_nodes = ["Lw", "Le"]
    else:
        nodes += [{"id": "Fw", "position": [xs, ys, 0.0]}, {"id": "Fe", "position": [xe, ys, 0.0]}]
        elements += [
            {"type": "line", "id": "Riser_w", "from": "Lw", "to": "Fw", "radius": a, "segments": 2, "material": "pec"},
            {"type": "line", "id": "Riser_e", "from": "Le", "to": "Fe", "radius": a, "segments": 2, "material": "pec"},
        ]
        term_nodes = ["Fw", "Fe"]
    elements += [
        {"type": "branch", "id": "Rw", "from": term_nodes[0], "resistance": rmatch},
        {"type": "branch", "id": "Re", "from": term_nodes[1], "resistance": rmatch},
        {"type": "channel", "id": "ch", "position": [strike[0], strike[1], 0.0], "length": ch_len,
         "radius": ch_radius, "speed": v, "resistance": rprime, "calibrate": calibrate,
         "firstSegment": first, "growth": growth, "maxSegment": maxseg},
    ]
    observe = term_nodes + ["Line_n%d" % (n // 2)]
    signal = {
        "waveform": "heidler",
        "terms": [{"i0": round(i0, 3), "n": float(nn), "tau1": float("%.6e" % t1), "tau2": float("%.6e" % t2)}
                  for i0, t1, t2, nn in terms],
        "sourceNode": "ch-base", "observeNodes": observe,
        "nyquistHz": nyq, "fftPoints": npts, "transform": "nlt",
    }
    if observe_currents:
        signal["observeElectrodes"] = ["ch_e1", "ch_e%d" % _segment_near(500.0, ch_len, first, growth, maxseg)]
    case = {
        "title": title or ("Nucci et al. 1993 base case: 1 km, 10 m high matched line, subsequent stroke 50 m "
                           "from its centre; variant %s (%s)" % (
                               variant, "resistor at the wire end" if variant == "A" else "risers, resistor at their foot")),
        "soil": {"conductivity": ground[0] if ground else 1.0, "permittivity": ground[1] if ground else 10.0,
                 "permeability": 1.0},
        "numerics": {"imageModel": "ideal" if ground is None else "frequency-dependent"},
        "nodes": nodes,
        "materials": [{"id": "pec", "epsilonr": 1.0, "mur": 1.0, "sigma": sigma_wire}],
        "elements": elements,
        "signal": signal,
    }
    return case


def _segment_near(z, length, first, growth, maxseg):
    b = graded_breaks(length, first, growth, maxseg)
    return int(np.searchsorted(b, z, side="left"))  # 1-based index of the segment containing z


# ----------------------------------------------------------------------------
# Running and measuring
# ----------------------------------------------------------------------------
def run(fortran_bin, case, workdir, name):
    path = Path(workdir) / (name + ".json")
    path.write_text(json.dumps(case, indent=1))
    subprocess.run([fortran_bin, "-q", str(path)], cwd=workdir, check=True, capture_output=True)
    data = {}
    for row in csv.DictReader(open(Path(workdir) / (name + "_transient_results.csv"))):
        data.setdefault((row["quantity"], row["id"]), []).append((float(row["time_s"]), float(row["value"])))
    return {k: np.array(v) for k, v in data.items()}


def metrics(series, tmax=12e-6):
    """Peak (kV), steepest rise (kV/us), 30-90 % rise time (us), time to half
    value (us, from the 50 % point of the front), and the most negative value (kV)."""
    t, v = series[:, 0], series[:, 1]
    m = t <= tmax
    t, v = t[m], v[m]
    ip = int(v.argmax())
    up = v[ip]
    dv = np.gradient(v, t)

    def cross(level, lo=1, hi=None):
        for k in range(lo, (hi or ip) + 1):
            if v[k - 1] < level <= v[k]:
                return t[k - 1] + (level - v[k - 1]) / (v[k] - v[k - 1]) * (t[k] - t[k - 1])
        return float("nan")

    t30, t90, t50f = cross(0.3 * up), cross(0.9 * up), cross(0.5 * up)
    t50 = float("nan")
    for k in range(ip + 1, len(v)):
        if v[k - 1] >= 0.5 * up > v[k]:
            t50 = t[k - 1] + (v[k - 1] - 0.5 * up) / (v[k - 1] - v[k]) * (t[k] - t[k - 1])
            break
    return {"Up": up / 1e3, "dUdt": dv.max() / 1e9, "t3090": (t90 - t30) * 1e6, "t50": (t50 - t50f) * 1e6,
            "Umin": v.min() / 1e3, "tpeak": t[ip] * 1e6}


def term_series(res, variant):
    return res[("voltage", "Lw" if variant == "A" else "Fw")]


def fmt_row(label, m, ref=None):
    s = "| %s | %.1f | %.0f | %.2f | %.2f | %.2f |" % (label, m["Up"], m["dUdt"], m["t3090"], m["t50"], m["Umin"])
    if ref:
        s += " %.0f / %.0f / %.1f / %.1f |" % ref
    else:
        s += " |"
    return s


HEADER = ("| Case | U_p (kV) | max dU/dt (kV/µs) | t30-90 (µs) | t50 (µs) | min (kV) | reference U_p / dU/dt / t30-90 / t50 |\n"
          "| --- | --- | --- | --- | --- | --- | --- |")


def dev(x, ref):
    return 100.0 * (x - ref) / ref


# ----------------------------------------------------------------------------
# Sections
# ----------------------------------------------------------------------------
def sec_base(fb, wd):
    print("\n### Base case (variants A and B)\n")
    print(HEADER)
    out = {}
    for var in "AB":
        res = run(fb, build_case(var), wd, "base_" + var)
        m = metrics(term_series(res, var))
        out[var] = m
        print(fmt_row("variant " + var, m, REF["base"]))
    print("\nDeviation from the MTL base row (peak / steepness): "
          + "; ".join("%s: %+.1f %% / %+.1f %%" % (v, dev(out[v]["Up"], 66.0), dev(out[v]["dUdt"], 190.0)) for v in "AB"))
    print("TCS row of the same paper: %.0f kV, %.0f kV/µs (MTL-TCS spread: 12 %% / 37 %%)." % TCS[:2])
    return out


def sec_sweep(fb, wd):
    print("\n### Trends of Table III (variant B; variant A in brackets for the peak)\n")
    print(HEADER)
    base_i = current_features(BASE_TERMS)
    cases = [
        ("Ip=4.6", dict(terms=fit_terms(4.6e3, base_i[1]))),
        ("base", dict()),
        ("Ip=30", dict(terms=fit_terms(30e3, base_i[1]))),
        ("didt=12", dict(terms=fit_terms(base_i[0], 12e9))),
        ("didt=120", dict(terms=fit_terms(base_i[0], 120e9))),
        ("v=0.6e8", dict(v=0.6e8)),
        ("v=2e8", dict(v=2.0e8, calibrate=False)),  # the calibration refuses this target (see the writeup)
    ]
    out = {}
    for name, kw in cases:
        resB = run(fb, build_case("B", **kw), wd, "sweep_B_" + name.replace("=", "").replace(".", "p"))
        resA = run(fb, build_case("A", **kw), wd, "sweep_A_" + name.replace("=", "").replace(".", "p"))
        mb, ma = metrics(term_series(resB, "B")), metrics(term_series(resA, "A"))
        out[name] = (mb, ma)
        ref = REF[name]
        row = fmt_row(name, mb, ref)
        print(row.replace("| %.1f |" % mb["Up"], "| %.1f (A: %.1f) |" % (mb["Up"], ma["Up"]), 1))
    return out


def sec_fig14(fb, wd):
    print("\n### Formulation test (Nucci 1993 Fig. 14): 500 m line, stroke on the prolongation, 50 m from the left end\n")
    print("| Variant | U_p at the left end (kV) | dU/dt (kV/µs) | min (kV) |\n| --- | --- | --- | --- |")
    out = {}
    for var in "AB":
        case = build_case(var, xs=0.0, xe=500.0, ys=0.0, strike=(-50.0, 0.0),
                          title="Nucci et al. 1993 Fig. 14: 500 m matched line, stroke on its prolongation 50 m from the left end")
        res = run(fb, case, wd, "fig14_" + var)
        m = metrics(term_series(res, var))
        out[var] = m
        print("| %s | %.1f | %.0f | %.2f |" % (var, m["Up"], m["dUdt"], m["Umin"]))
    print("\nReference: 33 kV (Agrawal and Taylor), 19 kV (the total-voltage pair B-1/B-2, called Rusck there).")
    return out


def sec_conv(fb, wd):
    print("\n### Sensitivity (variant B, peak and steepness at the terminations)\n")
    print("| Change | U_p (kV) | max dU/dt (kV/µs) | t30-90 (µs) |\n| --- | --- | --- | --- |")
    variants = [
        ("base (line 10 m, channel 5 m x1.15 <= 20 m, 3 km, R' = 2 Zc/lambda, 5 mm wire, dt = 50 ns)", {}),
        ("line segments 5 m", dict(line_seg=5.0)),
        ("line segments 20 m", dict(line_seg=20.0)),
        ("channel segments 2.5 m x1.1 <= 10 m", dict(first=2.5, growth=1.1, maxseg=10.0)),
        ("channel segments 10 m x1.2 <= 40 m", dict(first=10.0, growth=1.2, maxseg=40.0)),
        ("time step 100 ns (nyquist 5 MHz, 512 samples)", dict(nyq=5.0e6, npts=512)),
        ("channel resistance 1.0 ohm/m", dict(rprime=1.0)),
        ("channel resistance 2.5 ohm/m", dict(rprime=2.5)),
        ("channel resistance 0.5 ohm/m (literature value)", dict(rprime=0.5)),
        ("channel length 2 km", dict(ch_len=2000.0)),
        ("channel radius 1 cm", dict(ch_radius=0.01)),
        ("line wire radius 10 mm (Zc = 456 ohm)", dict(a=0.01)),
        ("decay constant lambda = 1.0 km (R' from 2 Zc/lambda)", dict(lam=1000.0)),
        ("channel without attenuation (R' = 0.05 ohm/m)", dict(rprime=0.05)),
    ]
    out = {}
    for label, kw in variants:
        res = run(fb, build_case("B", **kw), wd, "conv_%d" % len(out))
        m = metrics(term_series(res, "B"))
        out[label] = m
        print("| %s | %.1f | %.0f | %.2f |" % (label, m["Up"], m["dUdt"], m["t3090"]))
    return out


def sec_internal(fb, wd):
    print("\n### Internal checks\n")
    # 1. matched-line impedance: a current step into one end, the other end matched
    case = build_case("A")
    case["elements"] = [e for e in case["elements"] if e["type"] != "channel" and e["id"] != "Rw"]
    case["signal"].update({"sourceNode": "Lw", "waveform": "portela", "imax": 1000.0, "alpha": 0.0,
                           "tFront": 0.3e-6, "tTopEnd": 20.0e-6, "tTailEnd": 40.0e-6,
                           "observeNodes": ["Lw", "Le"], "nyquistHz": 1.0e7, "fftPoints": 1024})
    case["signal"].pop("terms")
    res = run(fb, case, wd, "zc_line")
    t, v = res[("voltage", "Lw")][:, 0], res[("voltage", "Lw")][:, 1]
    inj = [k for k in res if k[0] == "injectedCurrent"][0]
    cur = res[inj][:, 1]
    # window after the front; a matched far end sends no echo back, so the ratio
    # stays flat up to the end of the current (a mismatch would show from 2 l / c = 6.7 us)
    w = (t > 1.0e-6) & (t < 15.0e-6)
    zc_meas = float(np.mean(v[w] / cur[w]))
    zc_th = zc_line(10.0, 5.0e-3)
    print("- matched line, 1 kA step into the west end, east end 498 ohm: u/i over 1-15 µs = %.1f ohm "
          "(theory 60 acosh(h/a) = %.1f ohm, %+.1f %%)" % (zc_meas, zc_th, dev(zc_meas, zc_th)))
    # 2. channel current attenuation
    case = build_case("B", observe_currents=True)
    res = run(fb, case, wd, "attenuation")
    ids = case["signal"]["observeElectrodes"]
    z_top = 500.0
    cur = {}
    for k in ids:
        t = res[("i1", k)][:, 0]
        i1, i2 = res[("i1", k)][:, 1], res[("i2", k)][:, 1]
        cur[k] = np.abs(((i1 - i2) / 2.0)[t <= 12e-6]).max()
    b = graded_breaks(3000.0, 5.0, 1.15, 20.0)
    kz = int(ids[1][4:])
    zmid = 0.5 * (b[kz - 1] + b[kz])
    ratio = cur[ids[1]] / cur[ids[0]]
    z0 = 0.5 * (b[0] + b[1])
    print("- channel current: peak at z = %.0f m over peak at z = %.0f m = %.3f; MTL exp(-dz/lambda) = %.3f"
          % (zmid, z0, ratio, math.exp(-(zmid - z0) / 1700.0)))


def sec_lossy(fb, wd):
    print("\n### Lossy-ground variant (Rachidi et al. 1996): sigma = 0.001 S/m, eps_r = 10, variant B (information only)\n")
    print("| Ground | U_p (kV) | max dU/dt (kV/µs) | min (kV) |\n| --- | --- | --- | --- |")
    for label, g in (("perfect (ideal images)", None), ("sigma = 0.001 S/m, eps_r = 10 (Gamma(omega) images)", (1.0e-3, 10.0))):
        res = run(fb, build_case("B", ground=g), wd, "lossy_%d" % (g is not None))
        m = metrics(term_series(res, "B"))
        print("| %s | %.1f | %.0f | %.2f |" % (label, m["Up"], m["dUdt"], m["Umin"]))


SECTIONS = {"base": sec_base, "sweep": sec_sweep, "fig14": sec_fig14, "conv": sec_conv,
            "internal": sec_internal, "lossy": sec_lossy}


def write_common():
    for var, name in (("A", "induced_nucci1993"), ("B", "induced_nucci1993_risers")):
        p = ROOT / "common" / (name + ".json")
        p.write_text(json.dumps(build_case(var), indent=2) + "\n")
        print("wrote", p)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--fortran-bin", help="path to the Tupa executable")
    ap.add_argument("--only", default=",".join(SECTIONS), help="comma-separated sections")
    ap.add_argument("--write-common", action="store_true", help="regenerate the two common/ base cases and exit")
    args = ap.parse_args()
    if args.write_common:
        write_common()
        return
    if not args.fortran_bin:
        ap.error("--fortran-bin is required")
    with tempfile.TemporaryDirectory() as wd:
        for name in args.only.split(","):
            SECTIONS[name](str(Path(args.fortran_bin).resolve()), wd)


if __name__ == "__main__":
    main()
