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

**Test runtimes**: `test_mesh` and `test_assemble` finish in seconds in any
profile. `test_geometry` and `test_impedance` are quadrature-heavy and only
practical under `--profile release` — in the default debug profile they run
for many minutes (see [../docs/ROADMAP.md](../docs/ROADMAP.md) §5). There is
no hosted CI; a local `fpm build && fpm test` is the merge gate.

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

`main` always discretises the structure and prints a report (node/material/
element list, each element's generated electrode-segment IDs). If — and
only if — the case file also carries `sources`/`frequencies` (ADR 0013,
like `portela1997.json`/`rod.json`/`grid.json`/`rod_air.json`), it
additionally runs the frequency sweep and writes
`<basename>_results.csv`/`.json` (tidy CSV + ADR 0012 JSON,
`mResultsWriter`) into the *current working directory* — e.g. running from
`fortran/` writes `fortran/portela1997_results.csv`. A structure-only case
(`buried_conductor_short.json`/`buried_conductor_long.json`) stops after
the report; there is nothing to sweep, and no output files are written.
`outputs.nodes`/`electrodes`/`quantities`, if present in the case file,
filter what gets written, same as `runStudyFromFile` (see
[common/README.md](../common/README.md)).

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