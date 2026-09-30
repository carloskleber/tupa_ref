# Cross-implementation benchmark (Fortran · Rust · Julia)

Runs the main base cases of [`common/`](../../common/README.md) with each
implementation, clocks every run, and plots the results side by side with
the deviation from a reference implementation.

| Case | Kind | Why |
| --- | --- | --- |
| `portela1997`, `rod`, `grid` | harmonic | the three golden fixtures |
| `grcev_fig12_l10_rho300` | harmonic | published full-wave comparison (40 segments, 101 points) |
| `silva2025_rho300` | harmonic | dispersive soil (`alipio-visacro`), 128 points |
| `lima_fig6` | harmonic | 5-electrode tower footing, 150 points |
| `portela1997_transient` | transient | double exponential, 1 MHz Nyquist |
| `silva2025_rho300_transient` | transient | 7-term Heidler, 4096-point FFT |

Change the set with `--cases` (any name in `common/`, without `.json`).

## What you need

| Tool | For | Notes |
| --- | --- | --- |
| Python ≥ 3.9 | driver | plots also need `pip install numpy matplotlib` (without them you still get `timings.csv` and `summary.md`; re-plot later with `--plots-only`) |
| Rust (`cargo`, https://rustup.rs) | Rust | built by the script (`cargo build --release`) |
| `gfortran`, `fpm` ≥ 0.10, LAPACK/BLAS, git | Fortran | the script calls `fortran/build.sh`, which fetches the SLATEC submodule and builds everything — see [fortran/README.md](../../fortran/README.md) |
| Julia ≥ 1.10 (https://julialang.org/downloads) | Julia | the script runs `Pkg.instantiate()` for `julia/` (first run downloads packages, incl. Plots) |

Any implementation that is missing is skipped with a message; the others run.

## Run

From the repository root (Linux, macOS, WSL):

```bash
git submodule update --init            # once, for the Fortran/SLATEC build
python3 -m pip install numpy matplotlib

python3 benchmarks/cross-impl/bench.py                       # all cases, all tools, 3 repeats
python3 benchmarks/cross-impl/bench.py --repeats 5
python3 benchmarks/cross-impl/bench.py --impl rust julia     # subset
python3 benchmarks/cross-impl/bench.py --cases portela1997 grid
```

Useful options:

| Option | Meaning |
| --- | --- |
| `--fortran-bin PATH` | use an existing executable instead of calling `fortran/build.sh` (must accept `-q <case.json>` and write `*_results.json` / `*_transient_results.csv` to the current directory — the `app/main.f90` program does) |
| `--skip-fortran-build` | reuse `fortran/build/*/app/*` from an earlier build |
| `--rust-bin PATH` | use a given Rust binary |
| `--julia PATH` | Julia executable (default: `julia` on `PATH`) |
| `--results DIR` | output directory (default `benchmarks/cross-impl/results/<timestamp>/`, git-ignored) |
| `--plots-only --results DIR` | regenerate plots and summary from a finished run |

Windows: use WSL (the Fortran build chain is Unix-oriented). Rust and Julia
alone also work natively: `py benchmarks\cross-impl\bench.py --impl rust julia`.

## Output (`results/<timestamp>/`)

| File | Content |
| --- | --- |
| `results_overlay.png/.svg` | per case: \|Zin\|(f) or GPR(t) of every implementation, and below it the deviation from the reference (Fortran if present, else Rust; % of \|Zin\|, or % of the peak for transients) |
| `timings.png/.svg` | median wall time per case and implementation (log scale); for Julia a black tick marks the *warm* time |
| `timings.csv` | every run: `impl,case,rep,ok,wall_s,load_s,cold_s,warm_s,error` |
| `summary.md` | machine info, timing table, deviation table, failures |
| `raw/<impl>/<case>/rep<n>/` | the files each run wrote |

## How to read the timings

- **Wall time** = from process launch to exit, measured by the driver,
  including JSON parsing, assembly, solve and writing result files. It is
  the number a user sees, and it is what the bar chart shows.
- **Julia** pays start-up and JIT compilation in every process (loading
  `Plots` alone takes seconds), so its wall time is dominated by that for
  small cases. `run_julia.jl` therefore also times, in-process, `load` (read
  + assemble), `cold` (first solve, includes JIT) and `warm` (a second solve
  of a freshly loaded study, no JIT). The *warm* value is the fair
  per-solve figure for long-lived Julia sessions; report both.
- Cases of a few milliseconds are dominated by process start; compare the
  larger cases (`lima_fig6`, `silva2025_*`) for solver speed.
- Fortran is built by `fortran/build.sh` with `-O3 -march=native -fopenmp
  -ffast-math`; Rust by `cargo build --release` (default FP semantics,
  single-threaded). For a quiet measurement close other programs and, if
  you care about OpenMP effects, compare `OMP_NUM_THREADS=1` with the
  default: `OMP_NUM_THREADS=1 python3 benchmarks/cross-impl/bench.py ...`.
- The Julia port uses a fixed 64×64 midpoint quadrature and Fortran/Rust an
  adaptive Gauss–Kronrod rule, so they do different amounts of work per
  segment pair; speed ratios are between *implementations as shipped*, not
  between languages on identical algorithms.

## Expected deviations

Fortran and Rust agree to ~1e-6 (golden-fixture tolerance). Julia differs
by ≲ 0.1–0.2 % in \|Z\| because of its quadrature
([docs/validation/tupa-vs-mhem.md](../../docs/validation/tupa-vs-mhem.md));
the `silva2025_*` and `lima_fig6` cases have no published Julia figures, so
larger deviations there are worth a look, not necessarily a bug.

## Notes

- Julia's harmonic driver (`run_julia.jl`) writes `*_results.json` with
  `frequencies` and `derived.inputImpedance` only; transients write the tidy
  CSV for the first observed node. The plots read exactly those.
- A failing run (non-zero exit) is recorded in `summary.md` and does not
  stop the benchmark.
- Run status of this script: verified end to end with Rust (and with a
  second executable standing in for Fortran to exercise the multi-
  implementation plots). The Fortran build step and `run_julia.jl` were
  **not** executed by the author of this script (no Julia, and no working
  fpm/SLATEC build in that environment) — if either fails on your machine,
  the error text is in `summary.md` / the console; please report it.
