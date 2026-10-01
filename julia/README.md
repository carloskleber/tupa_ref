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
Julia-only flag:

| Option | Meaning |
| --- | --- |
| `-v`, `--verbose` / `-q`, `--quiet` | verbosity levels (`mVerbosity`); errors always print |
| `--epsrel <x>` | relative-error factor of the geometry-factor quadrature, default `1e-6` |
| `--no-cache` | disable the geometry-factor memo table (`mGeometryCache`) |
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

> **ROADMAP Phase 10 (2026-10-01) — this port lags.** The Fortran and Rust
> defaults changed (single-integral geometry kernel, `Γ(ω)` image
> coefficients) and every harmonic golden fixture was regenerated; the Julia
> port still computes the pre-Phase-10 numerics (nested 2-D quadrature, ideal
> `Γ = ±1` images). Its results therefore now differ from the fixtures and
> from Fortran by the Phase 10 effects: up to ~2e-3 relative at 1 MHz on a
> buried conductor, growing as f², and ~1e-7 from the kernel
> ([ADR 0024](../docs/adr/0024-phase10-numerics.md)). The tables below are
> the **2026-09-30** measurements, valid up to that date; `test/runtests.jl`
> lists the three regenerated harmonic fixtures in `PHASE10_LAG_FIXTURES`
> (not run) and `portela1997_ideal` in `PHASE10_LAG_CASES`. See the Phase 10
> bullet under "Follow-along rule" for what to port.

Measured on 2026-09-30 (Linux, Julia 1.13.1, AMD Ryzen 5 8500G). Rule of
the golden fixtures: 1e-6 relative on the row scale `max(1e-6, |re|, |im|)`,
rows keyed by `(frequency_hz, quantity, id)` (`test/conformance.jl`, same as
`rust/tests/conformance.rs`).

| Case | Reference | Worst deviation |
| --- | --- | --- |
| `portela1997` | `portela1997_expected.csv` | 5e-17 — **match** |
| `rod` | `rod_expected.csv` | 1e-17 — **match** |
| `grid` | `grid_expected.csv` | 6e-8 — **match** (fixture in pre-ADR 0020 electrode order; keyed comparison) |
| `portela1997_transient_{interpolated,hann,hann_time,multi,nlt}` | `*_expected.csv` (ROADMAP Phase 9) | **lag** — not implemented; the loader rejects the Phase 9 `signal` fields (below) |

**Cross-check on every runnable `common/` case** (harmonic: same row rule;
transient: max |Δ| over the series peak), against fresh runs of the
Fortran (`fpm --profile release`, `OMP_NUM_THREADS=1`) and Rust
(`cargo build --release`) executables. "0" means the `ES16.8` files are
identical to the last printed digit.

| Case | vs Fortran | vs Rust |
| --- | --- | --- |
| `counterpoise_3` | 0 | 1e-08 |
| `grcev_fig12_l100_rho30` | 0 | 0 |
| `grcev_fig12_l100_rho300` | 0 | 0 |
| `grcev_fig12_l100_rho3000` | 0 | 0 |
| `grcev_fig12_l10_rho30` | 0 | 0 |
| `grcev_fig12_l10_rho300` | 0 | 0 |
| `grcev_fig12_l10_rho3000` | 0 | 0 |
| `grid` | 5e-10 | 3e-10 |
| `horizontal_vertical_mesh` | 0 | 0 |
| `linha1` (ADR 0023, `portela` waveform) | 3e-10 | 4e-10 |
| `linha4` (ADR 0023, `catenary`; measured before the ADR 0017 finding 8 fix) | 1e-05 | 1e-05 |
| `lima_fig6` | 0 | 0 |
| `lima_fig7_case10` | 0 | 0 |
| `lima_fig7_case11` | 0 | 0 |
| `line_rod` | 1e-10 | 8e-10 |
| `poljak_fig4` | 0 | 0 |
| `portela1997` | 8e-17 | 3e-17 |
| `portelaMesh` | 2e-06 ¹ | 7e-07 ¹ |
| `rod` | 3e-17 | 3e-17 |
| `rod_air` | 1e-08 | 2e-08 |
| `silva2025_rho100` | 0 | 0 |
| `silva2025_rho1000` | 0 | 0 |
| `silva2025_rho2400` | 0 | 0 |
| `silva2025_rho300` | 0 | 0 |
| `portela1997_transient` | 3e-16 | 2e-15 |
| `portelaMesh` (transient) | 3e-11 | 2e-11 |
| `silva2025_rho1000_transient` | 0 | 0 |
| `silva2025_rho100_transient` | 0 | 0 |
| `silva2025_rho2400_transient` | 0 | 0 |
| `silva2025_rho300_transient` | 1e-11 | 1e-11 |

¹ Two of 23 985 rows: `i2` of the last segment at the free grid corner
`m-0404` at 10 MHz, a physically zero current of ~1e-12 A, where the
rule's 1e-6 floor becomes a 1e-12 A absolute tolerance — round-off, not
physics. Rust fails the same two rows against Fortran (1.2e-6); every other
row is within 1e-7.

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
  made without running Julia — run `Pkg.test()` before relying on them.
  Porting guide: `rust/src/transient.rs` (same structure as the Fortran
  `mTransient`, ~300 lines), plus the `admittance_laplace`/
  `calc_param_laplace`/`internal_impedance_laplace` counterparts; remove
  names from `PHASE9_LAG` as their fixtures pass. The Phase 10 grid case
  `portelaMesh` (its `signal` uses `transferFunction`) is in `PHASE9_LAG` too;
  `test/physics.jl` loads its structure with that field removed.
- **ROADMAP Phase 10** ([ADR 0024](../docs/adr/0024-phase10-numerics.md)) —
  **lagging**, for the same reason (no Julia toolchain reachable: the egress
  policy denied `julialang.org`). `load_study_string` raises a `TupaError`
  naming `numerics (ROADMAP Phase 10)` when the block is present, so a
  case never silently runs with the wrong kernel or image model. The edits
  to `src/JsonParser.jl` and the three test files were made **without
  running Julia** — run `Pkg.test()` before relying on them. Porting guide
  (all in `rust/src`, ~150 lines): `impedance::geometry_factor_1d` →
  `Impedance.jl` and the kernel switch in `mutual_geometry_factor`;
  `mesh::ImageModel`, `MediumConstants::set_image_coefficients` and the
  complex image factor in `calc_z_self`/`calc_z_mutual` → `Mesh.jl`
  (note the ideal ±1 → `gamma_soil`/`gamma_air` change in the multiplication
  order, `s * fpropi * gi`); the `numerics` block, optional `segments` and
  `maxSegmentLength` → `JsonParser.jl`. Then regenerate the cross-check table
  below and the Julia columns of
  [`docs/validation/tupa-vs-mhem.md`](../docs/validation/tupa-vs-mhem.md),
  and remove the `PHASE10_LAG_*` lists.

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
