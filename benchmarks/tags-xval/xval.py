#!/usr/bin/env python3
"""Cross-code validation of TUPA against TAGS (ROADMAP Phase 10 item 5, §7 P3).

Runs the harmonic `common/` cases through

  * TAGS (benchmarks/tags, mHEM kernels) via `tags_hem.c`, with both choices of
    the longitudinal image coefficient (`one`: TAGS default; `gamma`: Γ_t on
    both image parcels, the original Matlab / PRTL-mHEM choice) plus a
    variant restoring the sign of the image direction cosine (TAGS uses
    |cos θ|), and
  * TUPA (Fortran executable; `--image-model frequency-dependent` default and
    `ideal`),

and compares the driving-point impedance Zin(f) = V(source)/I over the sweep:
physical outputs only (docs/BENCHMARKS.md comparison policy).

The electrodes/nodes of each case are taken from the Rust implementation's
`--dump-structure`, so both codes see the identical discretisation (electrodes
are oriented towards +x/+y/+z for TAGS, whose Z_l uses |cos θ| — see
`export_tags_inputs`). Only homogeneous linear soil is supported (TAGS' own
dispersive model is a different fit of the same paper).

    python3 benchmarks/tags-xval/xval.py [--cases portela1997 rod ...]
        [--fortran-bin PATH] [--rust-bin PATH] [--tags-bin PATH] [--keep DIR]

Needs: a built TAGS driver (`make -C benchmarks/tags-xval`), the Fortran CLI and
the Rust CLI (`cargo build --release`). Writes `xval_results.csv` and
`xval_summary.md` into `--out` (default: this directory's `out/`).
"""
import argparse
import csv
import json
import math
import os
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.normpath(os.path.join(HERE, "..", ".."))
COMMON = os.path.join(ROOT, "common")
DEFAULT_CASES = ["portela1997", "rod", "grid", "grcev_fig12_l10_rho300",
                 "grcev_fig12_l100_rho300", "lima_fig6"]


def freq_axis(spec):
    n = int(round(spec["pointsPerDecade"] * math.log10(spec["max"] / spec["min"]))) + 1
    a, b = math.log10(spec["min"]), math.log10(spec["max"])
    return [10 ** (a + (b - a) * k / (n - 1)) for k in range(n)]


def export_tags_inputs(case_json, rust_bin, directory, freqs):
    dump = subprocess.run([rust_bin, "--dump-structure", case_json], check=True,
                          capture_output=True, text=True).stdout
    nodes, pos, electrodes = [], {}, []
    for line in dump.splitlines():
        f = line.split(",")
        if f[0] == "node":
            pos[int(f[1])] = (f[3], f[4], f[5])
            nodes.append(pos[int(f[1])])
        elif f[0] == "electrode":
            a, b = pos[int(f[3])], pos[int(f[4])]
            # TAGS assembles Z_l with |cos θ| (theory.md §9.6) where TUPA uses the
            # signed cos θ, so the two agree only if no pair of segments is
            # anti-parallel. Orient every electrode towards +x/+y/+z (first
            # non-zero direction component positive): exact for the axis-aligned
            # geometries of this comparison, where the physics does not depend
            # on the reference direction of a segment.
            d = [float(q) - float(p) for p, q in zip(a, b)]
            lead = next((c for c in d if abs(c) > 1e-12), 0.0)
            if lead < 0:
                a, b = b, a
            electrodes.append(", ".join([*a, *b, f[5]]))
    with open(os.path.join(directory, "nodes.csv"), "w") as fh:
        fh.write("\n".join(", ".join(n) for n in nodes) + "\n")
    with open(os.path.join(directory, "electrodes.csv"), "w") as fh:
        fh.write("\n".join(electrodes) + "\n")
    with open(os.path.join(directory, "frequencies.csv"), "w") as fh:
        fh.write("\n".join("%.12e" % f for f in freqs) + "\n")
    return {line.split(",")[2]: int(line.split(",")[1]) for line in dump.splitlines()
            if line.startswith("node,")}


def run_tags(tags_bin, sigma, epsr, inj, ref_l, directory, signed=False):
    subprocess.run([tags_bin, repr(sigma), repr(epsr), str(inj), ref_l, directory] + (["signed"] if signed else []),
                   check=True, capture_output=True)
    with open(os.path.join(directory, "zh.csv")) as fh:
        return [complex(float(r[0]), float(r[1])) for r in csv.reader(fh)]


def run_tupa(fortran_bin, case_json, source_node, image_model, workdir):
    # Same study with the output block narrowed to the source-node voltage
    # (the case's own `outputs` may select other quantities, and no transient)
    spec = json.load(open(case_json))
    spec["outputs"] = {"nodes": [source_node], "quantities": ["voltage"]}
    spec.pop("signal", None)
    base = "xval_" + os.path.splitext(os.path.basename(case_json))[0]
    narrowed = os.path.join(workdir, base + ".json")
    json.dump(spec, open(narrowed, "w"))
    subprocess.run([fortran_bin, "-q", "--image-model", image_model, narrowed], check=True,
                   cwd=workdir, capture_output=True)
    z = {}
    with open(os.path.join(workdir, base + "_results.csv")) as fh:
        for r in csv.reader(fh):
            if r[1] == "voltage" and r[2] == source_node:
                z[float(r[0])] = complex(float(r[3]), float(r[4]))
    return [z[k] for k in sorted(z)]


def compare(a, b):
    """Max |ΔZ|/|Z| (%) and max |Δφ| (deg) of a against b, per band."""
    out = {}
    for name, hi in (("<=100 kHz", 1e5), ("<=1 MHz", 1e6), ("all", math.inf)):
        sel = [(x, y, f) for (x, y, f) in zip(a, b, FREQS) if f <= hi]
        mod = max(abs(abs(x) - abs(y)) / abs(y) for x, y, _ in sel) * 100
        ph = max(abs(math.degrees(math.atan2((x / y).imag, (x / y).real))) for x, y, _ in sel)
        out[name] = (mod, ph)
    return out


def main():
    global FREQS
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--cases", nargs="+", default=DEFAULT_CASES)
    ap.add_argument("--fortran-bin")
    ap.add_argument("--rust-bin", default=os.path.join(ROOT, "rust", "target", "release", "tupa"))
    ap.add_argument("--tags-bin", default=os.path.join(HERE, "tags_hem"))
    ap.add_argument("--out", default=os.path.join(HERE, "out"))
    args = ap.parse_args()
    if not args.fortran_bin:
        sys.exit("--fortran-bin PATH is required (the executable built by fortran/build.sh)")
    os.makedirs(args.out, exist_ok=True)

    rows = []
    summary = ["| Case | Segments | TUPÃ (Γ(ω)) vs TAGS (Γ_ℓ = Γ_t) | … with the image cosine signed in TAGS | TUPÃ (Γ(ω)) vs TAGS (Γ_ℓ = 1) | TUPÃ (ideal) vs TAGS (Γ_ℓ = Γ_t) |",
               "| --- | --- | --- | --- | --- | --- |"]
    for case in args.cases:
        case_json = os.path.join(COMMON, case + ".json")
        spec = json.load(open(case_json))
        if spec.get("soil", {}).get("type", "linear") != "linear":
            print(f"{case}: skipped (non-linear soil)")
            continue
        FREQS = freq_axis(spec["frequencies"])
        src = spec["sources"][0]["node"]
        soil = spec["soil"]
        with tempfile.TemporaryDirectory() as d:
            ids = export_tags_inputs(case_json, args.rust_bin, d, FREQS)
            n_el = sum(1 for _ in open(os.path.join(d, "electrodes.csv")))
            tags_g = run_tags(args.tags_bin, soil["conductivity"], soil["permittivity"], ids[src], "gamma", d)
            tags_1 = run_tags(args.tags_bin, soil["conductivity"], soil["permittivity"], ids[src], "one", d)
            tags_s = run_tags(args.tags_bin, soil["conductivity"], soil["permittivity"], ids[src], "gamma", d,
                              signed=True)
            tupa_fd = run_tupa(args.fortran_bin, case_json, src, "frequency-dependent", d)
            tupa_id = run_tupa(args.fortran_bin, case_json, src, "ideal", d)
        for k, f in enumerate(FREQS):
            rows.append([case, f, *(v[k] for v in (tupa_fd, tupa_id, tags_g, tags_1, tags_s))])
        c1, c2, c3, c4 = (compare(tupa_fd, tags_g), compare(tupa_fd, tags_1), compare(tupa_id, tags_g),
                          compare(tupa_fd, tags_s))
        fmt = lambda c: "; ".join(f"{b}: {m:.2g} % / {p:.2g}°" for b, (m, p) in c.items())
        summary.append(f"| `{case}` | {n_el} | {fmt(c1)} | {fmt(c4)} | {fmt(c2)} | {fmt(c3)} |")
        print(f"{case}: {n_el} electrodes, {len(FREQS)} frequencies\n  vs TAGS(Γ_ℓ=Γ_t): {fmt(c1)}\n"
              f"  vs TAGS(Γ_ℓ=Γ_t, signed image cos): {fmt(c4)}\n"
              f"  vs TAGS(Γ_ℓ=1):   {fmt(c2)}\n  ideal vs TAGS(Γ_ℓ=Γ_t): {fmt(c3)}")

    with open(os.path.join(args.out, "xval_results.csv"), "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["case", "frequency_hz", "tupa_fd_re", "tupa_fd_im", "tupa_ideal_re", "tupa_ideal_im",
                    "tags_gamma_re", "tags_gamma_im", "tags_one_re", "tags_one_im",
                    "tags_signed_re", "tags_signed_im"])
        for r in rows:
            w.writerow([r[0], "%.8e" % r[1]] + ["%.10e" % x for z in r[2:] for x in (z.real, z.imag)])
    with open(os.path.join(args.out, "xval_summary.md"), "w") as fh:
        fh.write("\n".join(summary) + "\n")


if __name__ == "__main__":
    main()
