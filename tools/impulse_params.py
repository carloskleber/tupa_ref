#!/usr/bin/env python3
"""Lightning-impulse parameters (IEC 60060-1) of the voltages in a Tupa transient CSV.

    python3 tools/impulse_params.py fortran/results/marx_lab_2MV_transient_results.csv [--scale Dlv=500]

Per node: peak value, T1 = 1.67 (t90 - t30), virtual origin O1 = t30 - 0.3 T1
and T2 = time from O1 to the half-value on the tail.  Records produced with
the NLT grow towards the end (undamping by exp(c t)), so only the first
`--keep` fraction of the record is used (default 0.6).  Oscillation on the
front is not filtered: the k-factor of IEC 60060-1 is not applied.
"""
import argparse
import collections
import csv

ap = argparse.ArgumentParser()
ap.add_argument("csv")
ap.add_argument("--keep", type=float, default=0.6)
ap.add_argument("--scale", action="append", default=[], help="NODE=factor, e.g. Dlv=500")
a = ap.parse_args()
scale = {k: float(v) for k, v in (s.split("=") for s in a.scale)}

series = collections.defaultdict(lambda: ([], []))
for r in csv.DictReader(open(a.csv)):
    if r["quantity"] == "voltage":
        series[r["id"]][0].append(float(r["time_s"]))
        series[r["id"]][1].append(float(r["value"]))


def crossing(t, v, level, start=0, rising=True):
    for j in range(max(start, 1), len(v)):
        if (v[j] >= level) if rising else (v[j] <= level):
            return t[j - 1] + (level - v[j - 1]) / (v[j] - v[j - 1]) * (t[j] - t[j - 1])
    return None


print(f"{'node':8s}{'peak kV':>10s}{'t_peak us':>11s}{'T1 us':>8s}{'T2 us':>8s}")
for node, (t, v) in series.items():
    n = int(len(t) * a.keep)
    t, v = t[:n], [x * scale.get(node, 1.0) for x in v[:n]]
    sign = 1.0 if max(v) >= -min(v) else -1.0
    v = [sign * x for x in v]
    ip = max(range(len(v)), key=lambda j: v[j])
    pk = v[ip]
    t30, t90 = crossing(t, v, 0.3 * pk), crossing(t, v, 0.9 * pk)
    t50 = crossing(t, v, 0.5 * pk, ip, rising=False)
    if None in (t30, t90, t50):
        print(f"{node:8s}{pk / 1e3:10.1f}{t[ip] * 1e6:11.2f}{'-':>8s}{'-':>8s}")
        continue
    t1 = 1.67 * (t90 - t30)
    o1 = t30 - 0.3 * t1
    print(f"{node:8s}{sign * pk / 1e3:10.1f}{t[ip] * 1e6:11.2f}{t1 * 1e6:8.2f}{(t50 - o1) * 1e6:8.1f}")
