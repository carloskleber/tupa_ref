# Tupa - Fortran version

The reference implementation (modern Fortran 2008+, FPM). Project-level
documentation lives in [../docs/](../docs/README.md); conventions in
[../docs/CONVENTIONS.md](../docs/CONVENTIONS.md).

**Compilers**: developed against the latest gfortran; the code is kept
ifx-compatible.

## Recommended setup

### Windows

* Install VS Code;
* Install WSL and a Linux distribution (for the next scripts, I assume you chose Ubuntu);
* Open the Linux distro;
* Install the following packages:

```bash
sudo apt update
sudo apt install gfortran
curl -LsSf https://astral.sh/uv/install.sh | sh
uv tool install fpm
uv tool install fortls
```

* `git clone` from inside your `home/username/` folder;
* `cd tupa_ref/fortran`;
* `code .`;
* Install the recommended "VS Code Server for Linux";
* run `build.sh` (assuming Gfortran) to:
  * get and compile the [SLATEC lib](https://github.com/carloskleber/slatec);
  * Compile the main project with full optimization.
* Run the provided examples with `fpm run --example`.

### Linux

* Go directly to the bash procedure, install fpm and fortls.

#### Arch

Look for `gcc-fortran`

## Building and testing without build.sh

`build.sh` installs SLATEC to `~/.local/lib`, which is not on the default
linker search path. Plain `fpm` invocations therefore need:

```bash
export LIBRARY_PATH=$HOME/.local/lib:$LIBRARY_PATH
fpm build
fpm test
```

The SLATEC checkout that `build.sh` clones into `fortran/slatec/` is the
canonical copy (author's fork) and may be fine-tuned in place.

On a toolchain where the plain command does not build or link (verified
2026-10-01 with gfortran 13, fpm 0.12, Ubuntu, shared LAPACK), the tests need
long source lines allowed, OpenMP for the threaded sweep, and LAPACK kept on
the link line after SLATEC (`D1MACH` calls `DLAMCH`):

```bash
export LIBRARY_PATH=$HOME/.local/lib:$LIBRARY_PATH
fpm test --profile release \
  --flag "-ffree-line-length-none -fno-range-check -fopenmp" \
  --link-flag "-Wl,--no-as-needed -llapack -lblas"
```

All 18 test programs pass under it (2026-10-01). `test_parallel` checks that a
sweep is bit-identical for 1 and N threads; run the suite a few times with
`OMP_NUM_THREADS=4` after touching `runSweep` or anything it calls.

**Test runtimes**: `test_mesh` and `test_assemble` finish in seconds in any
profile. `test_geometry` and `test_impedance` are quadrature-heavy and only
practical under `--profile release` — in the default debug profile they run
for many minutes (see [../docs/ROADMAP.md](../docs/ROADMAP.md) §5). There is
no hosted CI; a local `fpm build && fpm test` is the merge gate.

## Profiling

Since ROADMAP Phase 10 the geometry quadrature is the single-integral
`geometryFactor1D` in `Impedance.f90` (8× cheaper than the nested 2-D
`TWODQ` path, which remains the oracle and runs under `--kernel double`), and
for a real-sized study the per-frequency `ZGESV` dominates: on the 585-unknown
`portelaMesh` the LU is ≈ 100 % of a frequency. Profile what you actually run —
the `test_geometry`/`test_impedance` runtime note above is about the 2-D
oracle. To profile:

```bash
export LIBRARY_PATH=$HOME/.local/lib:$LIBRARY_PATH
fpm build --profile release --flag "-g"
perf record --call-graph dwarf ./build/gfortran_*/app/Tupa ../common/rod_air.json
perf report
```

`perf` (Linux, `linux-tools`/`perf` package) needs no special compiler flags
beyond `-g` for symbol names, and works on the optimized `release` binary —
profiling the `debug` profile mostly just tells you debug builds are slow.
If `perf_event_paranoid` blocks unprivileged use and you'd rather not
change it, `gprof` is a lower-resolution fallback that doesn't need it:

```bash
fpm build --profile release --flag "-pg"
./build/gfortran_*/app/Tupa ../common/rod_air.json
gprof ./build/gfortran_*/app/Tupa gmon.out | less
```

For exact call counts and a navigable call graph (much slower to run),
`valgrind --tool=callgrind ./build/gfortran_*/app/Tupa ../common/rod_air.json`
then open the resulting `callgrind.out.*` in `kcachegrind`.

## Running Tupa

The standalone solver executable (`app/main.f90`, package name `Tupa` in
`fpm.toml`) takes a single JSON study file
([common/README.md](../common/README.md) schema) and runs it end to end.

```bash
fpm run -- ../common/portela1997.json
```

This is `fpm run` for the *default* (only) executable, passing the JSON
path after `--` as its command-line argument. The command is identical on
Linux and inside WSL on Windows — there is no separate native-Windows
build (see "Recommended setup" above). Once built, the compiled binary can
also be run directly, e.g.
`./build/gfortran_*/app/Tupa ../common/portela1997.json`, or install it to
a stable path with `fpm install` and invoke it as `Tupa <study.json>`.

`-v`/`--verbose` and `-q`/`--quiet` may be passed alongside (or in place of)
the study path, in any order, e.g. `fpm run -- -q ../common/portela1997.json`.
`-q` suppresses the routine report/summary output; errors and warnings
(e.g. an unrecognised element type) still print regardless of verbosity
(`mVerbosity`, [ARCHITECTURE.md](../docs/ARCHITECTURE.md) §5).

These flags control the numerics (a study's own `numerics` block, where it
states one, takes precedence over the flag; [ADR 0024](../docs/adr/0024-phase10-numerics.md)):

* `--kernel single|double` selects the geometry-factor quadrature: the mHEM
  single integral (default, `mImpedance%geometryFactor1D`) or the nested
  2-D quadrature (`geometryFactor2D`, the test oracle).
* `--image-model frequency-dependent|ideal` selects the image reflection
  coefficient: `Γ(ω)` (default) or the ideal ±1 limit (the pre-Phase-10
  behaviour).
* `--threads <n>` sets the threads of the frequency loop (needs a
  `-fopenmp` build such as `build.sh`'s; otherwise `OMP_NUM_THREADS`). Results
  do not depend on the count. With a threaded BLAS use one BLAS thread per
  solve (`OPENBLAS_NUM_THREADS=1`).
* `--epsrel <value>` sets the relative-error factor of the adaptive
  Gauss–Kronrod quadrature (`mImpedance%geometryFactor1D`/`geometryFactor2D`);
  default `1.0e-6`. Looser values (e.g. `--epsrel 1e-4`) trade accuracy for
  speed.
* `--no-cache` disables the quadrature memo table (`mGeometryCache`),
  which otherwise reuses the geometry factor of congruent segment pairs
  (same segment lengths and cross endpoint distances, e.g. translated
  copies in a regular grid) instead of re-integrating them. Mainly useful
  for benchmarking the cache itself. `-v` prints per-build hit/miss
  statistics.

`main` always discretises the structure and prints a report (node/material/
element list, each element's generated electrode-segment IDs). If — and
only if — the case file also carries `sources`/`frequencies` (ADR 0013,
like `portela1997.json`/`rod.json`/`grid.json`/`rod_air.json`), it
additionally runs the frequency sweep and writes
`<basename>_results.csv`/`.json` (tidy CSV + ADR 0012 JSON,
`mResultsWriter`) into the *current working directory* — e.g. running from
`fortran/` writes `fortran/portela1997_results.csv`. A case with an `observation` block
(ADR 0027, e.g. `grid_safety.json`) also writes
`<basename>_potentials.csv`/`.json` — surface potentials, GPR, touch and step
voltages. A structure-only case
(`buried_conductor_short.json`/`buried_conductor_long.json`) stops after
the report; there is nothing to sweep, and no output files are written.
`outputs.nodes`/`electrodes`/`quantities`, if present in the case file,
filter what gets written, same as `runStudyFromFile` (see
[common/README.md](../common/README.md)).

A `signal.signals` list (ADR 0026) writes the multi-signal variant of the
transient files (`signals` array in the JSON, a `signal` column in the CSV);
`mTransient%transientResponseSignals` is the driver (`independent = .true.`),
`transientResponseSources` its superposition wrapper. The transfer functions of
every distinct terminal come from one factorisation per frequency
(`tStudy%runSweepUnits`), so several signals on one node cost one solve.

**`Electrodes: None` in a report**: `study%report()` only shows real
`..._e1`/`..._n1` electrode/node IDs *after* the structure has been
discretised (`assembleStructure`, run either directly for a
structure-only case or via `runSweep` for a sweep case). Calling
`report()` before that point — e.g. from custom code that calls
`loadStudy` then `report()` directly, skipping assembly — prints
`Electrodes: None` for every element, because the element hasn't been
split into segments yet.

Bundled Fortran demo programs (hand-written studies, not JSON-driven) are
run with `fpm run --example <name>`:

| Example | What it does |
| --- | --- |
| `example1` | Smallest smoke case: 2 m buried conductor, single frequency |
| `example2` | Two collinear buried conductors, structure-only (no sweep) |
| `example3` | Portela-1997-parameter conductor, frequency sweep printed as a table |
| `example4` | Same case as `example3`, driven through `runSweep` end to end, writing `example4_results.csv`/`.json` |

## Code documentation

Using [FORD](https://forddocs.readthedocs.io/en/stable/), installed as a `uv`
tool (see "Recommended setup" above) — no separate venv needed, and this
works the same on Windows (inside WSL) and Linux:

```bash
uv tool install ford --with lxml
ford Tupa.md
```

After that the docs can be accessed in [doc/index.html](doc/index.html).