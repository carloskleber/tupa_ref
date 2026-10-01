# TUPÃ — Julia implementation

Native Julia implementation of the TUPÃ Hybrid Electromagnetic Model (HEM).
It implements the public contract — JSON schema v1 and the
[`common/`](../common/README.md) cases — and, like the
[Rust port](../rust/README.md), mirrors the Fortran reference module by
module, so a reviewer can audit kernel against kernel. Fortran stays the
implementation of record; Julia is a third conforming implementation and
the interactive/scripting route (REPL, notebooks, parameter studies).

Contributed by acslima as a compact prototype (2026-09-29,
[#1](https://github.com/carloskleber/tupa_ref/pull/1)) and realigned with
the Fortran/Rust code on 2026-09-30 ([History](#history)).

Dependencies: `JSON`, `SpecialFunctions` (AMOS `zbesi` Bessel functions —
the same routine as SLATEC `ZBESI` in the Fortran code — and `erfc`),
`PrecompileTools`, and the standard libraries `LinearAlgebra` (LAPACK
`zgetrf`/`zgetrs`, the two halves of the Fortran `ZGESV`) and `Printf`.
`Plots` is optional (`--plot`, see below). Julia ≥ 1.10.

## Recommended setup

Install Julia with [juliaup](https://github.com/JuliaLang/juliaup) — the
official installer, which also keeps it up to date:

- **Windows**: `winget install --name Julia --id 9NJNWW8PVKMN -e -s msstore`
  (native Windows works; WSL as for Fortran works too);
- **Linux/macOS**: `curl -fsSL https://install.julialang.org | sh`
  (Arch: the `julia` package also works).

Then, once, from `tupa_ref/julia`:

```sh
julia --project=. -e 'using Pkg; Pkg.instantiate()'
```

This downloads the dependencies and precompiles the package, including a
small compile workload, so later runs start in under a second. For
editing, VS Code with the Julia extension is the usual choice.

## Build, run, test

There is no build step. From `julia/` (the launcher activates the project
itself, so it can be called from anywhere):

```sh
julia bin/tupa.jl ../common/portela1997.json              # writes portela1997_results.{csv,json}
julia bin/tupa.jl ../common/portela1997_transient.json    # writes *_transient_results.{csv,json}
julia bin/tupa.jl -v --epsrel 1e-8 --no-cache ../common/grid.json
julia bin/tupa.jl --dump-structure ../common/portelaMesh.json   # nodes/electrodes, no physics
```

Options — the Fortran executable's, the Rust port's two extras, and one
Julia-only flag (`--threads` is not offered: sweeps are serial here):

| Option | Meaning |
| --- | --- |
| `-v`, `--verbose` / `-q`, `--quiet` | verbosity levels (`mVerbosity`); errors always print |
| `--epsrel <x>` | relative-error factor of the geometry-factor quadrature, default `1e-6` |
| `--no-cache` | disable the geometry-factor memo table (`mGeometryCache`) |
| `--kernel single\|double` | geometry-factor quadrature of the pairs without a closed form: the single integral (default) or the nested 2-D one (ADR 0024); a study's `numerics.kernel` overrides it |
| `--image-model frequency-dependent\|ideal` | image reflection coefficient of studies that do not state one: `Γ(ω)` (default) or the ideal `±1` (ADR 0024); a study's `numerics.imageModel` overrides it |
| `--dump-structure` | print the assembled nodes/electrodes and stop (byte-identical to the Rust dump) |
| `--output-dir <dir>` | where result files go (default: current directory) |
| `--plot` | also write `<case>_transient_plot.png` (Julia only; needs `Plots`) |

Result files are named like the Fortran ones (`<case>_results.csv|json`,
`<case>_transient_results.csv|json`), use the same `ES16.8` number layout,
and honour `outputs` filtering, so the GUI and the validation scripts read
them unchanged. The CLI runs LAPACK single-threaded unless
`OPENBLAS_NUM_THREADS` is set: the systems are small enough that threads
rarely pay, and they oversubscribe badly next to other processes (a 674×674
LU went from 17 ms to 380 ms on a loaded machine).

`--plot` is implemented by a package extension that loads when `Plots` is
available in the default environment (it is deliberately not a dependency:
it is heavy, and plotting results is the GUI's job, ADR 0011):

```sh
julia -e 'using Pkg; Pkg.add("Plots")'      # once, into the default environment
julia bin/tupa.jl --plot ../common/portela1997_transient.json
```

Tests — the local merge gate (no hosted CI, ADR 0018):

```sh
julia --project=. -e 'using Pkg; Pkg.test()'
```

## Library use

```julia
using Tupa
case = validate_study_references!(load_study("../common/portela1997.json"))
run_sweep!(case.study, case.freq_hz, case.sources)
zin = input_impedance(case.study, "Node_1")            # Vector{ComplexF64}
write("out.csv", results_csv(case.study; quantities = ["voltage"]))

case = validate_study_references!(load_study("../common/portela1997_transient.json"))
r = transient_response(case.study, case.transient)       # r.t, r.node_responses, …
```

Studies can also be built in code (`Structure`, `add_node!`,
`add_material!`, `add_element!(st, Line(...))`, `Study`, `Source`) — see
`test/physics.jl`. `run_from_file(path; options = RunOptions(...))` is the
CLI without the argument parsing.

## Layout (Julia file ↔ Fortran module ↔ Rust module)

| `src/` | Fortran | Rust | Role |
| --- | --- | --- | --- |
| `Ctes`, `Error`, `Verbosity` | `Ctes`, `Error`, `Verbosity` | `ctes`, `error`, `verbosity` | constants, `TupaError`, CLI levels |
| `Material` | `Material` | `material` | linear / Portela / Alipio–Visacro media, `W(ω)`, `γ` |
| `Node`, `Electrode`, `Structure`, `element/{Element,Line,Mesh}` | `Node`, `Electrode`, `Structure`, `element/*` | `node`, `electrode`, `structure`, `element/*` | object model and assembly (identical discretised IDs) |
| `Geometry`, `GeometryCache` | `Geometry`, `GeometryCache` | `geometry`, `geometry_cache` | `g(a,b)`, image terms, distances, cosines, congruence cache |
| `Impedance` | `Impedance` | `impedance`, `bessel` | adaptive GK 7/15 (`dqag_k15`, `twodq`), internal impedance |
| `Mesh` | `Mesh` | `mesh`, `linalg` | topology, `Zeq` assembly, LU with multiple RHS |
| `Study`, `Result` | `Study`, `Result` | `study`, `result` | preparation, fill, sweep, voltage sources (ADR 0016) |
| `ResultsWriter` | `ResultsWriter` | `results_writer` | CSV/JSON, `ES16.8` number format |
| `Signal`, `Fft`, `Transient` | `Signal`, `Fft`, `Transient` | `signal`, `fft`, `transient`, `special` | waveforms, radix-2 FFT, transient driver |
| `JsonParser` | `JsonParser`, `Tupa` | `json` | typed schema reader, `validate_study_references!` |
| `Tupa.jl` (`run_from_file`, `main`), `bin/tupa.jl` | `Tupa`, `app/main.f90` | `lib.rs`, `main.rs` | driver and CLI |
| `ext/TupaPlotsExt.jl` | — | — | optional transient plot |

Deliberate differences from the Rust port, all on the side of the Fortran
code: LAPACK LU instead of an in-repo LU; AMOS Bessel functions
(`SpecialFunctions`) instead of a series/asymptotic subset, so the
tubular conductor (Phase 12 item 2) needs no new special-function code;
`erfc` from `SpecialFunctions`. As in the Rust port, a present JSON value of
the wrong type is an error (the Fortran reader yields 0).

## Conformance status

> **ROADMAP Phase 10 (ADR 0024) — met 2026-10-01** (single-integral
> geometry kernel, `Γ(ω)` image coefficients, `numerics` block with
> `maxSegmentLength`). The three regenerated harmonic fixtures,
> `portela1997_ideal` and the harmonic half of `portelaMesh` now run at
> 1e-6 (table below). The frequency-level threading of Phase 10 item 4 is
> not ported (sweeps stay serial). Phases 9 and 10b still lag (see "Follow-along
> rule").

Measured on 2026-10-01 (Linux, Julia 1.13.1, AMD Ryzen 5 8500G). Rule of
the golden fixtures: 1e-6 relative on the row scale `max(1e-6, |re|, |im|)`,
rows keyed by `(frequency_hz, quantity, id)` (`test/conformance.jl`, same as
`rust/tests/conformance.rs`).

| Case | Reference | Worst deviation |
| --- | --- | --- |
| `portela1997` | `portela1997_expected.csv` | 5e-17 — **match** |
| `rod` | `rod_expected.csv` | 1e-17 — **match** |
| `grid` | `grid_expected.csv` | 6e-8 — **match** (fixture in pre-ADR 0020 electrode order; keyed comparison) |
| `portela1997_transient_signals` | `portela1997_transient_signals_expected.csv` (ADR 0026) | 1e-6 — **match** (transient shape with the `signal` column) |
| `portela1997_transient_{interpolated,hann,hann_time,multi,nlt}` | `*_expected.csv` (ROADMAP Phase 9) | **lag** — not implemented; the loader rejects the Phase 9 `signal` fields (below) |

**Cross-check on every runnable `common/` case** (re-measured 2026-10-01
with the Phase 10 defaults), against fresh runs of the Fortran
(`fpm --profile release` via `build.sh`, `OMP_NUM_THREADS=1`) and Rust
(`cargo build --release`) executables. Harmonic: worst row deviation under
the 1e-6 row rule; transient: max |Δ| over the peak of the quantity (voltage
or current) in the file. "0" means the `ES16.8` files are identical to the
last printed digit.

| Case | vs Fortran | vs Rust |
| --- | --- | --- |
| `counterpoise_3` | 0 | 2e-08 |
| `grcev_fig12_l{10,100}_rho{30,300,3000}` (6 cases) | 0 | 0 |
| `grid` | 2e-10 | 3e-10 |
| `horizontal_vertical_mesh` | 0 | 5e-10 |
| `lima_fig6`, `lima_fig7_case10`, `lima_fig7_case11` | 0 | 0 |
| `line_rod` | 1e-10 | 4e-10 |
| `linha0` | 2e-10 | 5e-10 |
| `linha1` (ADR 0023, `portela` waveform) | 3e-10 | 6e-10 |
| `linha2` | 4e-09 | 5e-09 |
| `linha3` | 1e-03 ² | 4e-03 ² |
| `linha4` (ADR 0023, `catenary`) | 1e-05 ³ | 1e-05 ³ |
| `linha5`, `linha5a` | 5e-05, 1e-04 | 4e-05, 1e-04 |
| `poljak_fig4` | 0 | 0 |
| `portela1997`, `portela1997_ideal` | 2e-17, 3e-16 | 1e-16, 3e-16 |
| `rod`, `rod_air` | 3e-17, 1e-08 | 2e-17, 1e-08 |
| `silva2025_rho{100,300,1000,2400}` | 0 | 0 |
| `torre0` | 2e-09 | 4e-09 |
| `torre1` | 6e-08 | 4e-08 |
| `torre2` | 5e-06 ³ | 5e-05 ³ |
| transients: `linha0`, `linha1`, `linha2` | 2e-09, 5e-10, 5e-07 | 2e-10, 2e-10, 4e-07 |
| transients: `linha3`, `linha4`, `linha5`, `linha5a` | 5e-06, 2e-06, 2e-06, 7e-07 | 7e-06, 2e-06, 3e-06, 1e-06 |
| transients: `portela1997_transient`, `silva2025_rho{100,300,1000,2400}` | ≤ 3e-15, ≤ 1e-11 | ≤ 4e-16, 0 |
| transients: `torre0`, `torre1`, `torre2` | 2e-09, 3e-09, 9e-11 | 2e-09, 3e-09, 9e-10 |

`portelaMesh` is not in the table: its `signal` block uses the Phase 9
`transferFunction`, which the loader refuses, so Julia cannot run the case
whole. Its harmonic sweep (case text without that field) matches the golden
fixture at 1.2e-13 (`test/conformance.jl`).

² `linha3` (310 segments, a floating 100 m telephone wire beside the
shield wire) is sensitive to the quadrature tolerance at low frequency, not
a Julia discrepancy: the deviation falls from ~1e-3 at 100 Hz to 1e-9 above
1 MHz, Julia at `--epsrel 1e-6` vs `1e-9` differs from itself by 5.4e-4,
and Fortran vs Rust (both at 1e-6) differ by 4.6e-3.

³ Worst rows are currents that are zero by symmetry (a scale of 1e-6, the
rule's floor, so round-off counts as 1e-3 relative); rows with a scale above
1e-3 agree to 1e-5 or better. Transient deviations are against the peak of the
quantity (voltage or current) in the file: against a series' *own* peak,
series that are ~1e-13 of it show O(1) round-off. (`linha4` was last compared
before the ADR 0017 finding 8 fix: its numbers are now current.)

Structure dumps (`--dump-structure`) are byte-identical to the Rust dump on
`portelaMesh`, `lima_fig6`, `rod_air` and `counterpoise_3`.

On the Grcev ℓ = 10 m cases the realigned port moves by 0.09 % (mean |Z|)
from the prototype — the prototype's documented distance from Fortran —
so the figures in
[`docs/validation/tupa-vs-mhem.md`](../docs/validation/tupa-vs-mhem.md),
which were measured with the prototype, now apply to Fortran and Julia
alike (ROADMAP Phase 8J item 5 regenerates them).

### Not yet covered

- No golden fixture exists for transient, voltage-source, `mesh`, `portela`
  or `alipio-visacro` cases (ROADMAP Phase 8 item 1, Fortran side); those
  paths are checked by the cross-check above and by ported unit and
  consistency tests (`test/physics.jl`), not against fixtures.
- Only one frequency point runs at a time, single-threaded (the Fortran
  sweep is threaded since Phase 10 item 4; the port is optional, with one
  BLAS thread per task — see ROADMAP Phase 8J).
- The GUI has not been pointed at Julia output files yet (they are
  byte-compatible with the Rust files it reads).

See ROADMAP Phase 8J for the remaining items.

## Follow-along rule

Like the Rust port (ROADMAP Phase 8 item 10): each contract change (schema,
`common/` case, default numerics) carries a Julia item; lags are recorded in
the conformance table above.

- **ROADMAP Phase 9** (ADR 0015 amendment 2026-09-30) — **lagging.** The
  session that implemented Phase 9 in Fortran and Rust had no Julia
  toolchain (the Julia download and package hosts were not reachable), so
  the port does not implement `signal.sources`, the `sine` waveform,
  `window`, `transferFunction` or `transform`/`nltDamping`. So that a
  Phase 9 case can never silently run as a plain-FFT transient,
  `load_signal` (`src/JsonParser.jl`) raises a `TupaError` naming the field
  when any of `sources`, `window`, `transform`, `nltDamping` or
  `transferFunction` is present; `test/runtests.jl` lists the five Phase 9
  cases in `PHASE9_LAG` (the fixture guard accepts them, and
  `test/physics.jl` checks the loader refuses them). These test edits were
  made without running Julia; the suite has since been run (2026-10-01) and
  passes. Porting guide: `rust/src/transient.rs` (same structure as the Fortran
  `mTransient`, ~300 lines), plus the `admittance_laplace`/
  `calc_param_laplace`/`internal_impedance_laplace` counterparts; remove
  names from `PHASE9_LAG` as their fixtures pass. The Phase 10 grid case
  `portelaMesh` (its `signal` uses `transferFunction`) is in `PHASE9_LAG` too;
  `test/conformance.jl` runs its harmonic fixture and `test/physics.jl` loads
  its structure with that field removed.
- **ADR 0026** (independent transient signals) — **implemented** for shared and
  per-entry nodes: `transient_signals` solves one sweep per distinct node (the
  port has no multi-right-hand-side path, so it is slower than the Fortran/Rust
  drivers on several nodes), `transient_signals_csv/json`. `returnNode`/`quantity`
  on a `signals` entry are refused (Phase 10b lag). The fixture
  `portela1997_transient_signals` is run by `test/conformance.jl` at 1e-6.
- **ROADMAP Phase 11** ([ADR 0027](../docs/adr/0027-observation-potentials-and-safety-outputs.md),
  grounding-safety outputs) — **lagging.** `load_study_string` refuses the
  `observation` block with a `TupaError` naming ROADMAP Phase 11, so a case
  never silently runs without its potentials. The case `grid_safety` is in
  `PHASE11_LAG_CASES` and its two fixtures (`grid_safety`,
  `grid_safety_potentials`) in `PHASE11_LAG_FIXTURES` (`test/runtests.jl`).
  The port is short (≈ 150 lines in `rust/src/potentials.rs`: closed-form
  segment factor, medium constants per frequency, one pass over the segments)
  and needs only the stored sweep.
- **ROADMAP Phase 10b** ([ADR 0025](../docs/adr/0025-lightning-channel-and-two-node-sources.md)) —
  **lagging** (it also needs Phase 9: the channel fixtures use the NLT). `load_study_string` refuses the `channel` element,
  `sources[].returnNode` and `signal.returnNode`/`quantity` with a `TupaError`
  naming ROADMAP Phase 10b, so a case never silently runs without its channel
  or return node. The four cases `channel_{unloaded,loaded,tower,tower_gap}`
  are in `PHASE10B_LAG_CASES` and their six fixtures in
  `PHASE10B_LAG_FIXTURES` (`test/runtests.jl`); the suite passes with them
  skipped. Porting guide (all in `rust/src`): `element/channel.rs` →
  `Element/Channel.jl`; `channel_calibration.rs`; the loaded-electrode
  internal impedance in `study.rs`; `injection_patterns` and the dipole
  constraint of `solve_with_voltage_sources`.
- **ROADMAP Phase 10** ([ADR 0024](../docs/adr/0024-phase10-numerics.md)) —
  **ported 2026-10-01**, items 1–3 and 6 (item 4, the threaded sweep, is not
  ported). `geometry_factor_1d` (`src/Impedance.jl`) is the default kernel,
  switched by `GeometryOptions.kernel` (`:single`/`:double`; CLI `--kernel`,
  `numerics.kernel`); `image_coefficients` and the `gamma_air`/`gamma_soil`
  fields of `MediumConstants` (`src/Mesh.jl`) give the `Γ(ω)` images, with
  `:ideal` as the pre-Phase-10 pin (`--image-model`, `numerics.imageModel`);
  the `numerics` block, optional `segments` and `maxSegmentLength` are read by
  `load_numerics`/`load_study_string`, and a study's own choices override the
  process defaults (`Study.kernel`, `Study.image_model`,
  `Study.default_image_model`). The fixtures `portela1997`, `rod`, `grid`,
  `portela1997_ideal` and the harmonic half of `portelaMesh` pass at 1e-6
  (worst 2.4e-10, i.e. the CSV's own rounding); with
  `kernel = :double` + `imageModel = :ideal` the same cases sit 3e-4 to 2e-3 from
  the fixtures, which is what the new defaults changed. Unit tests: kernel vs
  the 2-D oracle on four pairs and the T-junction end-point singularity,
  `Γ` against the closed form and its two limits, the `numerics` block and
  segment counts, and that a study's choices reach the solver
  (`test/unit.jl`).
- **Known issue shared with the reference (`numerics.maxSegmentLength` on a
  `mesh`).** All three implementations size a mesh's bars as
  `lengthX/rowsX` and `lengthY/rowsY`, but the bars span `rows − 1`
  intervals (`assemble!` places the main nodes at `length/(rows − 1)`), so
  the target is undershot: with `rowsX = 3, rowsY = 2, lengthX = 8,
  lengthY = 4` and a 1 m target the longest electrode is 2.67 m. Julia
  mirrors Fortran/Rust deliberately so the three stay identical; no fixture
  sets a target on a mesh, and `line`/`catenary` are exact. Fix all three
  together (and add a mesh case to `test_segmentation.f90`).

## History

The contributed prototype (single file, ~200 lines) solved harmonic and
transient studies and brought the ADR 0021 anti-alias filter, but used a
fixed 64×64 midpoint rule for the geometry factors (~0.09 % from Fortran)
and lacked voltage sources, `outputs` filtering, the sweep writers and
reference validation. The 2026-09-30 realignment:

- split the code into one file per Fortran module and ported the Fortran
  algorithms line by line (adaptive `dqag_k15`, closed-form parallel
  pairs, congruence cache, radix-2 FFT, voltage-source superposition,
  reference validation, results writers, CLI options);
- aligned names and behaviour with the Fortran/Rust code: mesh bars are
  `<mesh>-<rrcc>-<rrcc>` (were `<mesh>_x<i>_<j>`), the transient CSV is
  the tidy `time_s,quantity,id,value` layout (was one column per series),
  a transient case also writes the JSON results, μ₀ = 4π·10⁻⁷ exactly;
- replaced `JSON3` (deprecated) with `JSON`, dropped `FFTW`, and made
  `Plots` an optional extension behind `--plot`;
- renamed the API (`solve_frequency!`, `run_sweep!(study, freqs, sources)`,
  `TransientSpec`, `tukey_antialias_filter`, …), mirroring the Fortran
  procedure names.
