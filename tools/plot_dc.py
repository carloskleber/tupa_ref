#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.9"
# dependencies = ["matplotlib"]
# ///
"""Profile plot of a quasi-stationary (DC) solve along contiguous conductors.

    tools/plot_dc.py common/rod_dc.json fortran/results/rod_dc_results.csv [-o out.png] [--freq HZ]

Reads the case (topology) and the harmonic results CSV and draws, for each
contiguous chain of `line`/`catenary` segments (split at junctions and free
ends), ONE figure with three stacked panels sharing the arc-length axis:

  1. node voltage V (to remote earth) at the chain's nodes;
  2. longitudinal current I_l = (I1 - I2)/2 per segment (theory.md §2:
     I1, I2 are the end currents, positive into the segment);
  3. transversal (leakage) current I_t = I1 + I2 per segment, as bars.

The row set plotted is the one at the lowest frequency in the CSV (the DC
point of `frequencies.dc`), or the one closest to `--freq`. Needs the CSV to
carry `voltage`, `i1` and `i2` (the default when `outputs` is absent). Element
types other than `line`/`catenary` (mesh, channel, ...) are skipped.
"""
import argparse
import csv
import json
import math
import sys
from collections import defaultdict


def expand(case):
    """Segments [(id, nodeA, nodeB, length)] and node positions {id: xyz}."""
    pos = {n["id"]: tuple(n["position"]) for n in case["nodes"]}
    segs = []
    skipped = set()
    for el in case["elements"]:
        t = el.get("type")
        if t not in ("line", "catenary"):
            if t != "branch":
                skipped.add(t)
            continue
        n = int(el.get("segments", 1))
        a, b = el["from"], el["to"]
        pa, pb = pos[a], pos[b]
        names = [a] + [f"{el['id']}_n{k}" for k in range(1, n)] + [b]
        for k in range(1, n):
            f = k / n
            pos[names[k]] = tuple(pa[i] + f * (pb[i] - pa[i]) for i in range(3))
        for k in range(1, n + 1):
            segs.append((f"{el['id']}_e{k}", names[k - 1], names[k],
                         math.dist(pos[names[k - 1]], pos[names[k]])))
    return segs, pos, skipped


def chains(segs):
    """Maximal paths of segments, split where a node has degree != 2."""
    at = defaultdict(list)
    for i, (_, a, b, _) in enumerate(segs):
        at[a].append(i)
        at[b].append(i)
    used = set()
    out = []

    def walk(start, node):
        path = []  # (segment index, node entered from, node left to)
        i = start
        while True:
            used.add(i)
            _, a, b, _ = segs[i]
            nxt = b if node == a else a
            path.append((i, node, nxt))
            rest = [j for j in at[nxt] if j != i]
            if len(at[nxt]) != 2 or rest[0] in used:
                return path
            i, node = rest[0], nxt

    for i, (_, a, b, _) in enumerate(segs):  # open chains: start at a non-degree-2 node
        if i in used:
            continue
        for end in (a, b):
            if len(at[end]) != 2:
                out.append(walk(i, end))
                break
    for i, (_, a, _, _) in enumerate(segs):  # closed loops
        if i not in used:
            out.append(walk(i, a))
    return out


def read_results(path, freq):
    rows = []
    with open(path, newline="") as f:
        for r in csv.DictReader(f):
            rows.append((float(r["frequency_hz"]), r["quantity"], r["id"],
                         complex(float(r["re"]), float(r["im"]))))
    freqs = sorted({r[0] for r in rows})
    if not freqs:
        sys.exit("no rows in " + path)
    f0 = freqs[0] if freq is None else min(freqs, key=lambda x: abs(math.log(x / freq)))
    data = {(q, i): v for fr, q, i, v in rows if fr == f0}
    return f0, data


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("case")
    ap.add_argument("results")
    ap.add_argument("-o", "--output", help="image file (default: <results stem>_profile[_N].png)")
    ap.add_argument("--freq", type=float, help="frequency (Hz) to plot; default: the lowest in the CSV")
    ap.add_argument("--show", action="store_true", help="open an interactive window instead of only saving")
    args = ap.parse_args()

    import matplotlib
    if not args.show:
        matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    with open(args.case) as f:
        case = json.load(f)
    segs, pos, skipped = expand(case)
    if skipped:
        print("skipped element types:", ", ".join(sorted(skipped)), file=sys.stderr)
    f0, data = read_results(args.results, args.freq)
    for q in ("voltage", "i1", "i2"):
        if not any(k[0] == q for k in data):
            sys.exit(f"results carry no '{q}' rows; plot needs voltage, i1 and i2")

    stem = args.results.rsplit(".", 1)[0] + "_profile"
    chs = chains(segs)
    for n, ch in enumerate(chs, 1):
        # arc-length coordinate of the chain's nodes
        s = [0.0]
        for i, _, _ in ch:
            s.append(s[-1] + segs[i][3])
        nodes = [ch[0][1]] + [c[2] for c in ch]
        v = [data[("voltage", nd)].real for nd in nodes]
        i1 = [data[("i1", segs[i][0])] for i, _, _ in ch]
        i2 = [data[("i2", segs[i][0])] for i, _, _ in ch]
        # I1/I2 belong to the segment's (from, to) ends; when the chain walks
        # a segment against that direction, I_l changes sign (I_t does not)
        il, it = [], []
        for (i, a, _), c1, c2 in zip(ch, i1, i2):
            sign = 1.0 if a == segs[i][1] else -1.0
            il.append(sign * ((c1 - c2) / 2).real)
            it.append((c1 + c2).real)
        mid = [(s[k] + s[k + 1]) / 2 for k in range(len(ch))]
        width = [s[k + 1] - s[k] for k in range(len(ch))]

        fig, ax = plt.subplots(3, 1, sharex=True, figsize=(8, 8))
        ax[0].plot(s, v, "o-", ms=3)
        ax[0].set_ylabel("node voltage V (V)")
        ax[1].step(s, il + [il[-1]], where="post")
        ax[1].plot(mid, il, "o", ms=3)
        ax[1].set_ylabel("longitudinal I_l (A)")
        ax[2].bar(mid, it, width=width, edgecolor="k", linewidth=0.3)
        ax[2].set_ylabel("transversal I_t (A)")
        ax[2].set_xlabel("distance along chain (m)")
        ax[0].ticklabel_format(useOffset=False)
        for a in ax:
            a.grid(alpha=0.3)
        first, last = nodes[0], nodes[-1]
        dc = "DC" if f0 < 1e-2 else ""
        fig.suptitle(f"{case.get('title', '')}\n{dc} f = {f0:g} Hz — chain {n}/{len(chs)}: {first} → {last}"
                     f" (Σ I_t = {sum(it):.4g} A)", fontsize=9)
        fig.tight_layout()
        out = args.output if args.output and len(chs) == 1 else \
            (args.output.rsplit(".", 1)[0] + f"_{n}." + args.output.rsplit(".", 1)[-1] if args.output
             else (f"{stem}.png" if len(chs) == 1 else f"{stem}_{n}.png"))
        fig.savefig(out, dpi=130)
        print("wrote", out)
    if args.show:
        plt.show()


if __name__ == "__main__":
    main()
