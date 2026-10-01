#!/usr/bin/env python3
"""Cross-implementation benchmark: Fortran vs Rust vs Julia on the `common/` cases.

Runs every (case, implementation) pair REPEATS times in a fresh output
directory, records wall-clock time, and plots the results side by side.
Missing toolchains are skipped with a message; a failing run is recorded as
FAILED and does not stop the benchmark.

    python3 benchmarks/cross-impl/bench.py                    # everything available
    python3 benchmarks/cross-impl/bench.py --impl rust julia  # subset
    python3 benchmarks/cross-impl/bench.py --cases portela1997 grid --repeats 5
    python3 benchmarks/cross-impl/bench.py --plots-only --results <results/dir>

Needs: Python >= 3.9. Plots additionally need numpy + matplotlib (the run and
the timing table work without them).  See README.md in this directory.
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import glob
import json
import os
import platform
import shutil
import statistics
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
COMMON = ROOT / "common"

# Main base cases: the three golden fixtures, three published-paper cases
# (harmonic) and two transients.
DEFAULT_CASES = [
    "portela1997", "rod", "grid",
    "grcev_fig12_l10_rho300", "silva2025_rho300", "lima_fig6",
    "portela1997_transient", "silva2025_rho300_transient",
]
IMPLS = ["fortran", "rust", "julia"]
LABEL = {"fortran": "Fortran", "rust": "Rust", "julia": "Julia"}
# categorical slots 1-3 of the validated reference palette (blue, orange, aqua)
COLOR = {"fortran": "#2a78d6", "rust": "#eb6834", "julia": "#1baf7a"}
MARKER = {"fortran": "o", "rust": "s", "julia": "^"}
STYLE = {"fortran": "-", "rust": "--", "julia": ":"}

FORTRAN_FLAGS = ("-O3 -funroll-loops -ffast-math -finline-functions -ftree-vectorize "
                 "-march=native -mtune=native -fopenmp -fno-range-check -ffree-line-length-none")


def say(msg: str) -> None:
    print(msg, flush=True)


def is_transient(case: str) -> bool:
    return "signal" in json.loads((COMMON / f"{case}.json").read_text())


# ----------------------------------------------------------------------------
# Toolchain preparation
# ----------------------------------------------------------------------------

def prepare_rust(args) -> list[str] | None:
    if args.rust_bin:
        return [os.path.abspath(args.rust_bin), "-q"]
    if not shutil.which("cargo"):
        say("[rust]    cargo not found -> skipped (install from https://rustup.rs)")
        return None
    say("[rust]    cargo build --release ...")
    r = subprocess.run(["cargo", "build", "--release", "--manifest-path", str(ROOT / "rust/Cargo.toml")])
    exe = ROOT / "rust/target/release/tupa"
    if r.returncode or not exe.exists():
        say("[rust]    build failed -> skipped")
        return None
    return [str(exe), "-q"]


def prepare_fortran(args) -> list[str] | None:
    if args.fortran_bin:
        return [os.path.abspath(args.fortran_bin), "-q"]
    if not shutil.which("fpm") or not shutil.which("gfortran"):
        say("[fortran] fpm/gfortran not found -> skipped (or pass --fortran-bin)")
        return None
    env = dict(os.environ)
    env["LIBRARY_PATH"] = f"{Path.home()}/.local/lib:{env.get('LIBRARY_PATH', '')}"
    if not args.skip_fortran_build:
        say("[fortran] building SLATEC + TUPA (fortran/build.sh; first run is slow) ...")
        if subprocess.run(["bash", str(ROOT / "fortran/build.sh")], env=env).returncode:
            say("[fortran] fortran/build.sh failed -> skipped")
            return None
        # build.sh uses its own (-march=native ...) flags; nothing more to do
    exes = [p for p in glob.glob(str(ROOT / "fortran/build/*/app/*"))
            if os.access(p, os.X_OK) and os.path.isfile(p) and not p.endswith((".o", ".mod"))]
    if not exes:
        say("[fortran] no executable under fortran/build/*/app -> skipped (pass --fortran-bin)")
        return None
    return [sorted(exes, key=os.path.getmtime)[-1], "-q"]


def prepare_julia(args) -> list[str] | None:
    jl = args.julia or shutil.which("julia")
    if not jl:
        say("[julia]   julia not found -> skipped (https://julialang.org/downloads)")
        return None
    say("[julia]   instantiating project ...")
    r = subprocess.run([jl, f"--project={ROOT / 'julia'}", "-e", "using Pkg; Pkg.instantiate()"])
    if r.returncode:
        say("[julia]   Pkg.instantiate failed -> skipped")
        return None
    return [jl, f"--project={ROOT / 'julia'}", "-O2", str(HERE / "run_julia.jl"), str(ROOT)]


# ----------------------------------------------------------------------------
# Running
# ----------------------------------------------------------------------------

FORTRAN_THREADS = "1"  # set from --fortran-threads; serial by default (ROADMAP Phase 10 item 4)


def run_once(impl: str, cmd: list[str], case: str, outdir: Path) -> dict:
    outdir.mkdir(parents=True, exist_ok=True)
    if impl == "julia":
        full = cmd + [str(outdir), case]
    else:
        full = cmd + [str(COMMON / f"{case}.json")]
    env = dict(os.environ)
    if impl == "fortran":
        # the Fortran frequency sweep is threaded in `-fopenmp` builds: time it
        # serially unless asked otherwise, as the Rust and Julia drivers are
        env["OMP_NUM_THREADS"] = FORTRAN_THREADS
    t0 = time.perf_counter()
    p = subprocess.run(full, cwd=outdir, capture_output=True, text=True, env=env)
    wall = time.perf_counter() - t0
    row = {"impl": impl, "case": case, "wall_s": wall, "ok": p.returncode == 0,
           "load_s": "", "cold_s": "", "warm_s": ""}
    if p.returncode:
        row["error"] = (p.stderr or p.stdout).strip().splitlines()[-1:] or ["?"]
        row["error"] = row["error"][0]
    if impl == "julia":
        for line in p.stdout.splitlines():
            if line.startswith("TIMING"):
                kv = dict(x.split("=") for x in line.split()[1:])
                row.update(load_s=float(kv["load"]), cold_s=float(kv["cold"]), warm_s=float(kv["warm"]))
    return row


def run_all(args, results: Path) -> list[dict]:
    prep = {"rust": prepare_rust, "fortran": prepare_fortran, "julia": prepare_julia}
    cmds = {}
    for impl in args.impl:
        c = prep[impl](args)
        if c:
            cmds[impl] = c
    if not cmds:
        sys.exit("No implementation available.")
    rows = []
    for case in args.cases:
        for impl, cmd in cmds.items():
            for rep in range(args.repeats):
                out = results / "raw" / impl / case / f"rep{rep}"
                r = run_once(impl, cmd, case, out)
                r["rep"] = rep
                rows.append(r)
                say(f"  {case:32s} {LABEL[impl]:8s} rep{rep}: "
                    + (f"{r['wall_s']:8.3f} s" if r["ok"] else f"FAILED ({r.get('error')})"))
                if not r["ok"]:
                    break
    return rows


def write_timings(rows, results: Path) -> None:
    fields = ["impl", "case", "rep", "ok", "wall_s", "load_s", "cold_s", "warm_s", "error"]
    with open(results / "timings.csv", "w", newline="") as f:
        w = csv.DictWriter(f, fields, extrasaction="ignore")
        w.writeheader()
        w.writerows(rows)


def read_timings(results: Path) -> list[dict]:
    rows = []
    for r in csv.DictReader(open(results / "timings.csv")):
        r["ok"] = r["ok"] == "True"
        for k in ("wall_s", "load_s", "cold_s", "warm_s"):
            r[k] = float(r[k]) if r[k] not in ("", None) else None
        rows.append(r)
    return rows


# ----------------------------------------------------------------------------
# Reading results
# ----------------------------------------------------------------------------

def load_curve(impl: str, case: str, results: Path):
    """Return (x, y): |Zin| vs frequency (harmonic, from the results JSON's
    derived.inputImpedance) or v(t) of the first observed node (transient)."""
    import numpy as np
    base = results / "raw" / impl / case / "rep0"
    spec = json.loads((COMMON / f"{case}.json").read_text())
    if is_transient(case):
        files = list(base.glob("*_transient_results.csv"))
        if not files:
            return None
        node = spec["signal"]["observeNodes"][0]
        t, v = [], []
        for r in csv.DictReader(open(files[0])):
            if r["quantity"] == "voltage" and r["id"] == node:
                t.append(float(r["time_s"])); v.append(float(r["value"]))
        return np.array(t), np.array(v)
    files = [f for f in base.glob("*_results.json") if "transient" not in f.name]
    if not files:
        return None
    d = json.loads(files[0].read_text())
    z = np.array([complex(v["re"], v["im"]) for v in d["derived"]["inputImpedance"]])
    return np.array(d["frequencies"]), np.abs(z)


# ----------------------------------------------------------------------------
# Plots
# ----------------------------------------------------------------------------

def style_axes(ax):
    ax.grid(True, color="#d9d8d3", linewidth=0.6, alpha=0.8)
    ax.set_axisbelow(True)
    for s in ("top", "right"):
        ax.spines[s].set_visible(False)
    for s in ("left", "bottom"):
        ax.spines[s].set_color("#9a9994")
    ax.tick_params(colors="#52514e", labelsize=8)


def median_wall(rows, impl, case):
    v = [r["wall_s"] for r in rows if r["impl"] == impl and r["case"] == case and r["ok"]]
    return statistics.median(v) if v else None


def make_plots(rows, cases, impls, results: Path) -> list[str]:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.ticker import FuncFormatter
    import numpy as np

    plt.rcParams.update({"font.family": "sans-serif", "axes.titlesize": 10,
                         "axes.labelsize": 9, "axes.edgecolor": "#9a9994"})
    curves = {(i, c): load_curve(i, c, results) for c in cases for i in impls}
    ref_impl = next((i for i in ("fortran", "rust", "julia") if i in impls), None)
    summary = []

    # --- 1. result overlays with deviation panel -----------------------------
    n = len(cases)
    cols = 2
    rows_n = (n + cols - 1) // cols
    fig = plt.figure(figsize=(12, 3.9 * rows_n), constrained_layout=True)
    outer = fig.add_gridspec(rows_n, cols)
    for k, case in enumerate(cases):
        inner = outer[k // cols, k % cols].subgridspec(2, 1, height_ratios=[3, 1], hspace=0.08)
        ax = fig.add_subplot(inner[0]); axd = fig.add_subplot(inner[1], sharex=ax)
        tr = is_transient(case)
        ref = curves.get((ref_impl, case))
        for impl in impls:
            c = curves.get((impl, case))
            if c is None:
                continue
            x, y = c
            if tr:
                x, y_plot = x * 1e6, y / 1e3
            else:
                y_plot = y
            ax.plot(x, y_plot, STYLE[impl], color=COLOR[impl], lw=2, label=LABEL[impl], zorder=3)
            if ref is not None and impl != ref_impl and len(c[1]) == len(ref[1]):
                if tr:
                    dev = (c[1] - ref[1]) / np.max(np.abs(ref[1])) * 100
                else:
                    dev = (c[1] - ref[1]) / ref[1] * 100
                axd.plot(x, dev, STYLE[impl], color=COLOR[impl], lw=1.5)
                summary.append((case, impl, float(np.max(np.abs(dev)))))
        ax.set_title(case + ("  (GPR)" if tr else "  (|Zin|)"), loc="left", fontweight="bold")
        ax.set_ylabel("GPR (kV)" if tr else "|Zin| (Ω)")
        if not tr:
            ax.set_xscale("log"); ax.set_yscale("log")
        axd.axhline(0, color="#52514e", lw=0.8)
        axd.set_ylabel(f"Δ vs {LABEL[ref_impl]} (%)" + (" of peak" if tr else ""), fontsize=8)
        axd.set_xlabel("time (µs)" if tr else "frequency (Hz)")
        if not tr:
            axd.set_xscale("log")
        plt.setp(ax.get_xticklabels(), visible=False)
        style_axes(ax); style_axes(axd)
        if not tr:
            ax.yaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:g}"))
            ax.yaxis.set_minor_formatter(FuncFormatter(lambda v, _: f"{v:g}"))
        if k == 0:
            ax.legend(frameon=False, fontsize=8, loc="best")
    p1 = results / "results_overlay.png"
    fig.savefig(p1, dpi=150); fig.savefig(results / "results_overlay.svg"); plt.close(fig)

    # --- 2. timing ------------------------------------------------------------
    fig, ax = plt.subplots(figsize=(11, 4.6), constrained_layout=True)
    w = 0.8 / max(len(impls), 1)
    x0 = np.arange(len(cases))
    for j, impl in enumerate(impls):
        vals = [median_wall(rows, impl, c) for c in cases]
        xs = x0 + (j - (len(impls) - 1) / 2) * w
        h = [v if v is not None else np.nan for v in vals]
        ax.bar(xs, h, w * 0.92, color=COLOR[impl], label=LABEL[impl], zorder=3)
        for xv, v in zip(xs, vals):
            if v is not None:
                ax.text(xv, v * 1.08, f"{v:.2g}", ha="center", va="bottom", fontsize=7, color="#52514e")
    if "julia" in impls:  # warm (post-JIT) solve time as a marker
        for c_i, c in enumerate(cases):
            v = [r["warm_s"] for r in rows if r["impl"] == "julia" and r["case"] == c and r["ok"] and r["warm_s"]]
            if v:
                j = impls.index("julia")
                xv = c_i + (j - (len(impls) - 1) / 2) * w
                ax.plot([xv - w * 0.46, xv + w * 0.46], [statistics.median(v)] * 2, color="#0b0b0b", lw=2, zorder=4)
        ax.plot([], [], color="#0b0b0b", lw=2, label="Julia, warm (no JIT)")
    ax.set_yscale("log"); ax.set_ylabel("median wall time (s, log)")
    vals_all = [median_wall(rows, i, c) for i in impls for c in cases]
    vals_all = [v for v in vals_all if v]
    if vals_all:
        ax.set_ylim(bottom=min(vals_all) / 4, top=max(vals_all) * 2.5)  # log bars: lengths are ratios
    ax.set_xticks(x0); ax.set_xticklabels([c.replace("_", "\n", 1) for c in cases], fontsize=8)
    ax.set_title("Wall-clock per run (process start to exit, incl. JSON I/O)", loc="left", fontweight="bold")
    style_axes(ax); ax.legend(frameon=False, fontsize=8, ncol=len(impls) + 1)
    ax.yaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:g}"))
    ax.yaxis.set_minor_formatter(FuncFormatter(lambda v, _: ""))
    p2 = results / "timings.png"
    fig.savefig(p2, dpi=150); fig.savefig(results / "timings.svg"); plt.close(fig)
    return [str(p1), str(p2)], summary


# ----------------------------------------------------------------------------
# Report
# ----------------------------------------------------------------------------

def write_summary(rows, cases, impls, results: Path, dev_summary, repeats) -> None:
    lines = ["# Cross-implementation benchmark", "",
             f"- date: {dt.datetime.now().isoformat(timespec='seconds')}",
             f"- machine: {platform.platform()}, {platform.processor() or platform.machine()}, "
             f"{os.cpu_count()} logical CPUs",
             f"- repeats per cell: {repeats} (median shown); wall time = process start to exit",
             "", "## Wall time (s, median)", "",
             "| case | " + " | ".join(LABEL[i] for i in impls) + (" | Julia warm" if "julia" in impls else "") + " |",
             "| --- | " + " | ".join("---:" for _ in impls) + (" | ---:" if "julia" in impls else "") + " |"]
    for c in cases:
        cells = []
        for i in impls:
            v = median_wall(rows, i, c)
            cells.append(f"{v:.3f}" if v is not None else "n/a")
        if "julia" in impls:
            w = [r["warm_s"] for r in rows if r["impl"] == "julia" and r["case"] == c and r["ok"] and r["warm_s"]]
            cells.append(f"{statistics.median(w):.3f}" if w else "n/a")
        lines.append(f"| {c} | " + " | ".join(cells) + " |")
    if dev_summary:
        lines += ["", "## Max deviation from the reference implementation", "",
                  "|Zin| relative (harmonic) / fraction of peak (transient), percent.", "",
                  "| case | implementation | max deviation (%) |", "| --- | --- | ---: |"]
        lines += [f"| {c} | {LABEL[i]} | {d:.4g} |" for c, i, d in dev_summary]
    failed = [r for r in rows if not r["ok"]]
    if failed:
        lines += ["", "## Failures", ""] + [f"- {r['impl']} / {r['case']}: {r.get('error')}" for r in failed]
    (results / "summary.md").write_text("\n".join(lines) + "\n")
    say("\n" + "\n".join(lines))


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--impl", nargs="+", choices=IMPLS, default=IMPLS)
    ap.add_argument("--cases", nargs="+", default=DEFAULT_CASES, help="case names in common/ (without .json)")
    ap.add_argument("--repeats", type=int, default=3)
    ap.add_argument("--results", type=Path, help="output directory (default: results/<timestamp>)")
    ap.add_argument("--plots-only", action="store_true", help="re-plot an existing --results directory")
    ap.add_argument("--rust-bin"); ap.add_argument("--fortran-bin"); ap.add_argument("--julia")
    ap.add_argument("--skip-fortran-build", action="store_true")
    ap.add_argument("--fortran-threads", default="1",
                    help="OMP_NUM_THREADS for the Fortran runs (default 1: the sweep is threaded since Phase 10)")
    args = ap.parse_args()
    global FORTRAN_THREADS
    FORTRAN_THREADS = str(args.fortran_threads)

    if args.plots_only:
        if not args.results:
            sys.exit("--plots-only needs --results DIR")
        results = args.results
        rows = read_timings(results)
    else:
        results = args.results or HERE / "results" / dt.datetime.now().strftime("%Y%m%d-%H%M%S")
        results.mkdir(parents=True, exist_ok=True)
        rows = run_all(args, results)
        write_timings(rows, results)
    cases = [c for c in args.cases if any(r["case"] == c for r in rows)]
    impls = [i for i in IMPLS if any(r["impl"] == i for r in rows)]
    dev = []
    try:
        _, dev = make_plots(rows, cases, impls, results)
    except ImportError:
        say("numpy/matplotlib missing -> plots skipped (pip install numpy matplotlib; "
            "then re-run with --plots-only --results DIR)")
    write_summary(rows, cases, impls, results, dev, args.repeats)
    say(f"\nResults in {results}")


if __name__ == "__main__":
    main()
