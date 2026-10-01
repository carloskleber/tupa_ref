#!/usr/bin/env python3
"""Validation of the lightning-channel element (ROADMAP Phase 10b, ADR 0025).

Reproduces the tables of docs/validation/channel-validation.md with the
Fortran CLI:

1. the unloaded channel against Chen's analytic current (Baba & Rakov's
   configuration, `common/channel_unloaded.json`);
2. the speed of a channel loaded with L' = 2, 4, 8 uH/m against Table 3 of
   Baba & Rakov (2007), measured with the same metric;
3. the base impedance of the calibrated channel (`common/channel_loaded.json`)
   against the transmission-line estimate (c/v) 60 ln(2 v t / r0);
4. the calibration scale kappa for a few (radius, speed, segmentation) sets.

Usage (from the repo root; needs the Fortran executable and numpy):

    python3 docs/validation/channel_checks.py --fortran-bin PATH [--only chen|table3|zc|kappa]
"""
import argparse
import csv
import json
import re
import subprocess
import tempfile
from pathlib import Path

import numpy as np

ROOT = Path(__file__).resolve().parents[2]
C = 299792458.0
ETA = 120.0 * np.pi


def run(fortran_bin, case_path, cwd):
    subprocess.run([fortran_bin, "-q", str(case_path)], cwd=cwd, check=True, capture_output=True)
    return Path(cwd) / (Path(case_path).stem + "_transient_results.csv")


def read_series(csv_path):
    data = {}
    for row in csv.DictReader(open(csv_path)):
        data.setdefault((row["quantity"], row["id"]), []).append((float(row["time_s"]), float(row["value"])))
    return {k: (np.array([x[0] for x in v]), np.array([x[1] for x in v])) for k, v in data.items()}


def onset(t, i, t_direct, t_reflected, t_rise=1.0e-6):
    """Time where the 10-90 % tangent of the first front crosses the time axis
    (the metric of `mChannelCalibration::frontOnsetTime`, ADR 0025): peak in a
    window of 2 t_rise + 0.2 t_direct after the start (and before the top
    reflection), 90 % = first rise through its level, 10 % = last rise through
    its level before that."""
    t_cut = min(0.5 * (t_direct + max(t_reflected, t_direct)), t_direct + 2.0 * t_rise + 0.2 * t_direct)
    window = t <= t_cut
    peak = np.max(i[window])
    k_peak = int(np.argmax(i[window]))

    def rise(k, level):
        return i[k - 1] < level <= i[k]

    def interp(k, level):
        return t[k - 1] + (level - i[k - 1]) / (i[k] - i[k - 1]) * (t[k] - t[k - 1])

    k90 = next(k for k in range(1, k_peak + 1) if rise(k, 0.9 * peak))
    k10 = next(k for k in range(k90, 0, -1) if rise(k, 0.1 * peak))
    t10, t90 = interp(k10, 0.1 * peak), interp(k90, 0.9 * peak)
    return t10 - (t90 - t10) / 8.0


def chen_ramp(z, t, v0=5.0e6, rise=1.0e-6, a=0.23):
    """Chen's step response of a monopole over ground (twice the dipole value,
    Baba & Rakov), convolved with a ramp of height v0 and rise time `rise`."""
    dt = t[1] - t[0]
    tt = np.arange(0.0, t[-1] + dt, dt / 10)
    step = np.zeros_like(tt)
    m = C * tt > z + 1e-9
    arg = np.sqrt((C * tt[m]) ** 2 - z**2) / a
    step[m] = 2.0 * (2.0 / ETA) * np.arctan(np.pi / (2.0 * np.log(arg)))
    integral = np.cumsum(step) * (dt / 10)
    at = lambda x: np.interp(x, tt, integral, left=0.0)
    return v0 / rise * (at(t) - at(t - rise))


def check_chen(fortran_bin):
    print("## Unloaded channel vs Chen (common/channel_unloaded.json)")
    with tempfile.TemporaryDirectory() as tmp:
        s = read_series(run(fortran_bin, ROOT / "common" / "channel_unloaded.json", tmp))
    print("| height z (m) | window (us) | worst |I_TUPA - I_Chen| / peak (%) | peak TUPA / Chen (kA) |")
    print("| --- | --- | --- | --- |")
    for seg, z in (("ch_e1", 5.0), ("ch_e31", 305.0), ("ch_e61", 605.0), ("ch_e91", 905.0)):
        t, i = s[("i1", seg)]
        ref = chen_ramp(z, t)
        # after the front, before the top reflection (2 km channel: back at z after (4000 - z)/c)
        t0 = z / C + 2.0e-6
        t1 = (4000.0 - z) / C - 0.5e-6
        w = (t >= t0) & (t <= min(t1, 12e-6))
        err = np.max(np.abs(i[w] - ref[w])) / np.max(ref[w])
        print(f"| {z:.0f} | {t0 * 1e6:.1f}-{min(t1, 12e-6) * 1e6:.1f} | {100 * err:.2f} | "
              f"{np.max(i[(t <= t1)]) / 1e3:.2f} / {np.max(ref[(t <= t1)]) / 1e3:.2f} |")


def loaded_speed(fortran_bin, inductance_uh, seg, tmp):
    """Speed between z = 0 and z = 300 m of a 0.23 m wire loaded with L' (uH/m)."""
    length = 900.0
    case = {
        "title": "Baba and Rakov Table 3", "soil": {"conductivity": 0.01, "permittivity": 10, "permeability": 1},
        "numerics": {"imageModel": "ideal"},
        "elements": [{"type": "channel", "id": "ch", "position": [0, 0, 0], "length": length, "radius": 0.23,
                      "inductance": inductance_uh * 1e-6, "segments": int(length / seg)}],
        "signal": {"waveform": "portela", "imax": 1.0, "alpha": 0.0, "tFront": 1e-6, "tTopEnd": 1e3, "tTailEnd": 2e3,
                   "sourceNode": "ch-base", "observeNodes": ["ch-base"],
                   "observeElectrodes": ["ch_e1", f"ch_e{300 // seg + 1}"],
                   "nyquistHz": 5e6, "fftPoints": 512, "transform": "nlt"}}
    path = Path(tmp) / "table3.json"
    path.write_text(json.dumps(case))
    s = read_series(run(fortran_bin, path, tmp))
    z1, z2 = seg / 2, 300 + seg / 2
    t1, i1 = s[("i1", "ch_e1")]
    _, i2 = s[("i1", f"ch_e{300 // seg + 1}")]
    guess = 0.6 * C
    ta = onset(t1, i1, z1 / guess, (2 * length - z1) / guess)
    tb = onset(t1, i2, z2 / guess, (2 * length - z2) / guess)
    return (z2 - z1) / (tb - ta) / C


def check_table3(fortran_bin):
    print("## Loaded wire speed vs Baba and Rakov Table 3 (FDTD), r0 = 0.23 m")
    published = {2: 0.60, 4: 0.48, 8: 0.37}
    print("| L' (uH/m) | published (FDTD) | TUPA, 10 m segments | TUPA, 5 m segments | closed form, z = 150 m |")
    print("| --- | --- | --- | --- | --- |")
    with tempfile.TemporaryDirectory() as tmp:
        for l_uh, v_pub in published.items():
            v10 = loaded_speed(fortran_bin, l_uh, 10, tmp)
            v5 = loaded_speed(fortran_bin, l_uh, 5, tmp)
            l0 = 2e-7 * np.log(2 * 150.0 / 0.23)
            closed = 1.0 / np.sqrt(1.0 + l_uh * 1e-6 / l0)
            print(f"| {l_uh} | {v_pub:.2f}c | {v10:.3f}c | {v5:.3f}c | {closed:.3f}c |")


def check_zc():
    print("## Base impedance of the calibrated channel (common/channel_loaded_expected.csv)")
    s = read_series(ROOT / "common" / "channel_loaded_expected.csv")
    t, v = s[("voltage", "ch-base")]
    _, i = s[("i1", "ch_e1")]
    print("| t (us) | V/I at the base (ohm) | (c/v) 60 ln(2 v t / r0) (ohm) |")
    print("| --- | --- | --- |")
    for tu in (1.5, 2.0, 3.0, 4.0, 5.0):
        k = int(np.argmin(np.abs(t - tu * 1e-6)))
        est = (C / 1.5e8) * 60.0 * np.log(2 * 1.5e8 * t[k] / 0.03)
        print(f"| {tu:.1f} | {v[k] / i[k]:.0f} | {est:.0f} |")


def check_kappa(fortran_bin):
    print("## Calibration scale kappa (lossless calibration channel, 3 km channel, R' = 0.5 ohm/m)")
    print("| r0 (m) | target speed | segmentation | kappa | measured speed | L' first / last segment (uH/m) |")
    print("| --- | --- | --- | --- | --- | --- |")
    sets = (("graded 5-20 m", {"firstSegment": 5, "growth": 1.15, "maxSegment": 20}),
            ("uniform 20 m", {"maxSegment": 20}))
    with tempfile.TemporaryDirectory() as tmp:
        for r0 in (0.03, 0.23):
            for vf in (0.67, 0.5, 0.33):
                for name, seg in sets:
                    el = {"type": "channel", "id": "ch", "position": [0, 0, 0], "length": 3000, "radius": r0,
                          "speed": vf * C, "resistance": 0.5, "calibrate": True, **seg}
                    case = {"title": "calibration", "soil": {"conductivity": 0.01, "permittivity": 10, "permeability": 1},
                            "numerics": {"imageModel": "ideal"}, "elements": [el],
                            "signal": {"waveform": "portela", "imax": 1.0, "alpha": 0.0, "tFront": 1e-6,
                                       "tTopEnd": 1e3, "tTailEnd": 2e3, "sourceNode": "ch-base",
                                       "observeNodes": ["ch-base"], "nyquistHz": 1e6, "fftPoints": 16,
                                       "transform": "nlt"}}
                    path = Path(tmp) / "cal.json"
                    path.write_text(json.dumps(case))
                    try:
                        run(fortran_bin, path, tmp)
                    except subprocess.CalledProcessError:
                        print(f"| {r0} | {vf:.2f}c | {name} | refused (no credible calibration) | | |")
                        continue
                    txt = (Path(tmp) / "cal_transient_results.json").read_text()
                    m = re.search(r'"scale": ([0-9.E+-]+), "measuredSpeed": ([0-9.E+-]+), "inductance": \[([^\]]*)\]', txt)
                    ind = [float(x) for x in m.group(3).split(",")]
                    print(f"| {r0} | {vf:.2f}c | {name} | {float(m.group(1)):.3f} | {float(m.group(2)) / C:.4f}c | "
                          f"{ind[0] * 1e6:.2f} / {ind[-1] * 1e6:.2f} |")


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--fortran-bin", required=True)
    ap.add_argument("--only", choices=["chen", "table3", "zc", "kappa"])
    args = ap.parse_args()
    for name, fn in (("chen", lambda: check_chen(args.fortran_bin)), ("table3", lambda: check_table3(args.fortran_bin)),
                     ("zc", check_zc), ("kappa", lambda: check_kappa(args.fortran_bin))):
        if args.only in (None, name):
            fn()
            print()
