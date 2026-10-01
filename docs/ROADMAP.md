# TUPÃ — Roadmap

Reference electromagnetic transient solver (HEM / Method of Moments).
Fortran implementation first; object model and test cases shared with the
Julia port (Phase 8J, `julia/`) and the Rust implementation (Phase 8, `rust/`; see
[ADR 0002](adr/0002-language-agnostic-object-model.md)).

This roadmap supersedes the earlier `implementation-plan.md` /
`IMPLEMENTATION_PLAN.md`. It is based on a side-by-side analysis of this
repository against the legacy (private) implementations and the theory
consolidated in [theory.md](theory.md). Since July 2026 the **original
Matlab code** (the dissertation implementation) is the **model reference of
record** — the re-inspection findings live in
[ADR 0017](adr/0017-legacy-reinspection-findings.md) (§8 below is a stub),
and the author-interview decisions of 2026-07-05 in
[ADR 0018](adr/0018-author-interview-decisions-2026-07.md) (§9 stub).

**MVP scope**: tower-footing grounding under lightning. Full transmission
lines and substation grids are the follow-on application tier (ADR 0018).
The project's primary role is a **scientifically citable reference
implementation**; usability as an engineering tool is secondary.

---

## 1. Current state

| Area | State |
| --- | --- |
| Object model | `tStudy → tStructure → tElement/tMaterial → tNode/tElectrode`, `tMesh`; FORD-documented |
| Geometry layer | `Geometry.f90`: mean/image distances, direct+image geometry factors (`g`, closed-form `g_self`; mHEM single-integral quadrature by default since Phase 10, the nested 2-D path selectable as oracle), direction cosines; quadrature cache |
| Impedance fill | ADR 0009 interface: `calcZSelf`/`calcZMutual` apply all theory factors internally — image parcels carry the frequency-dependent `Γ(ω)` by default since Phase 10 (ideal ±1 selectable); solid-conductor Bessel internal impedance (SLATEC `ZBESI`) |
| Solver | Augmented `Zeq` assembly + `ZGESV` (ADR 0003); multi-RHS variant for superposition (ADR 0016); frequencies solved in parallel on thread-private meshes, bit-identical to serial (Phase 10 item 4) |
| Sources | Current injections at named nodes (ADR 0010); ideal voltage sources via unit-injection superposition, mixable with current sources (ADR 0016); several simultaneous transient injections, each with its own waveform (Phase 9 item 4) |
| Materials | `tLinear`, `tPortelaSoil` (ADR 0007), `tVisacroAlipioSoil` (mean set, theory.md §7); air hardcoded to vacuum (ADR 0019) |
| Sweep & results | `runSweep` (OpenMP over frequencies) + `tResult` storage, `inputImpedance`/`maxVoltageMagnitude`; CSV/JSON writers (ADR 0012) with `outputs` filtering |
| Time domain | `mSignal` (Heidler — legacy 6-term [38] and standard parametrised form [37, 39]; double-exp ± Jones; Portela concave front; switched-on sine), tail taper, in-repo FFT (ADR 0014), transfer-function transient driver (`mTransient`) with opt-in scan-fed (pchip-interpolated) transfer function, half-Hann window (spectral or time placement), multiple injections and Numerical Laplace Transform (Phase 9, ADR 0015 amendment 2026-09-30) |
| JSON I/O | json-fortran parser (ADR 0006, superseded-in-place 2026-08-01); schema v1: structure + `sources`/`frequencies`/`outputs` (ADR 0013) + `signal` (ADR 0015) + voltage sources/Heidler terms (ADR 0016/0015 amendment) + `mesh` composite element (ADR 0020) + optional `signal.antialiasStart` (ADR 0021) + `catenary` element and `portela` waveform (ADR 0023) + `signal.sources`/`window`/`transferFunction`/`transform`/`nltDamping` and the `sine` waveform (ADR 0015 amendment 2026-09-30); the optional `numerics` block (`kernel`, `imageModel`, `maxSegmentLength`) and optional `segments` (ADR 0024); pre-run reference validation (`validateStudyReferences`) and CLI verbosity levels |
| Cases & tests | `common/` regression fixtures (golden; four harmonic — `portela1997`, `rod`, `grid`, `portela1997_ideal` — five transient since Phase 9, and the 32 × 32 m `portelaMesh` harmonic + scan-fed transient since Phase 10; **all regenerated once for Phase 10**) plus ten cases imported from the legacy Matlab case library (`linha*.json`, `torre*.json`, ADR 0023), 18 test programs under `fpm test --profile release` — all green (re-run 2026-10-01, gfortran 13; `test_impedance`'s coincident-segment divergence check is defeated by `-ffast-math`, as before). Plain `fpm test` needs `-ffree-line-length-none` and, on shared LAPACK, `--no-as-needed -llapack -lblas` (fortran/README.md) |
| Validation | [`docs/validation/`](validation/README.md): digitized published-curve comparisons — Grcev et al. 2018 Fig. 12 (6 cases), Lima et al. 2020 Figs. 6/7, Poljak & Doric 2006 Fig. 4, Silva et al. 2025 Figs. 3/4 (harmonic + transient) — accepted as the release-bar oracle (§4) |; since Phase 10 also the cross-code check against TAGS ([`validation/tags-xval.md`](validation/tags-xval.md): ≤ 0.3 % below 1 MHz on six cases) and the effect of the `Γ(ω)` default on every comparison ([`phase10-image-model.md`](validation/phase10-image-model.md))
| GUI | Python/PySide6 view-only module (`gui/`, ADR 0011): study tree, 3-D view, results/transient plots |
| Other implementations | **Julia port (`julia/`, contributed 2026-09-29, realigned 2026-09-30, Phase 8J)** — module-by-module mirror of the Fortran code, golden fixtures met at 1e-6 and every runnable `common/` case within 1e-6 of Rust/Fortran (bar two round-off rows Rust shares); **Rust port (`rust/`, 2026-09-30, [ADR 0022](adr/0022-rust-implementation.md))** — harmonic conformance met on the three golden fixtures at 1e-6, transient path implemented, Phase 8 item 1 (Fortran fixture widening) still open; **Phase 10 update 2026-10-01: Rust implements the whole phase and matches every regenerated fixture (worst 1.3e-10 harmonic, 1.5e-8 NLT); Julia ported items 1–3 and 6 the same day** (single-integral kernel, Γ(ω) images, `numerics` block; the five harmonic fixtures it can run match at 1e-6, worst 2.4e-10; the threaded sweep stays unported) — it still lags on Phase 9 and Phase 10b |

The original gap analysis (nine numbered gaps between this repository and
the legacy pipeline) is fully resolved as of Phases 0–6; the historically
notable items are recorded where they now belong:

- self-geometry-factor bug in *both* legacies (was item 8) —
  [ADR 0017](adr/0017-legacy-reinspection-findings.md) finding 2;
- parallel-segment closed form wrong for opposite-direction pairs of
  unequal length, inherited from the Matlab by all three ports (found
  2026-09-30 while importing the legacy tower cases) — ADR 0017 finding 8;
- `tStructure%air` never populated → NaN for any electrode in air (was
  item 9) — [ADR 0019](adr/0019-air-medium-hardcoded-vacuum.md);
- C-interop leftovers, sign conventions, stub `assemble`, missing
  sweep/sources/outputs — closed by Phases 0–3 below.

---

## 2. Guiding principles

- **Theory doc is normative** — code conforms to [theory.md](theory.md);
  papers are mapped through its §2 conventions table (ADR 0008).
- **Correctness before performance** — every physics routine lands with a test
  against an analytical value or a published curve.
- **Reference quality** — prefer the simple, auditable path (full `Zeq` solve,
  ADR 0003) over optimisations until validated.
- **Language-agnostic contracts** — JSON schema + `common/` cases are the
  cross-implementation interface (ADR 0002, 0006).

---

## 3. Phases

Item numbers are stable — code comments cite them ("ROADMAP Phase 3
item 4").

### Phase 0 — Convention audit and cleanup — **done**

1. Sign conventions audited against theory.md §2/§5/§6: propagation constant
   `√(jωμ(σ+jωε))`, decaying `e^{−γR}`, air-image longitudinal sign "−";
   pinned by `test_mesh.f90` (ADR 0008).
2. Mesh module naturalised: 1-based indices, `initMesh` (no pointer), dead
   C-interop code deleted.
3. Test-runner bugs fixed along the way (`check.f90` counter accumulation,
   bogus `TWODQ` argument).

### Phase 1 — Geometry layer — **done**

1. `tLine%assemble`: node/material resolution, internal nodes, electrode
   registration (`test_assemble.f90`).
2. `Geometry.f90`: mean/image distances, `g(a,b)` by Gauss–Kronrod
   quadrature, closed-form `g_self` (legacy formula confirmed wrong —
   ADR 0017 finding 2), direction cosines; mixed-media pairs skipped as a
   perf hint only (the zeroing decision stays in `Mesh.f90`, ADR 0005).
3. Solid-conductor internal impedance (SLATEC `ZBESI`); tubular deferred to
   Phase 12 item 2.
4. Exit criterion met: geometry matrices match independent numerical
   integration to 1e−6 relative (`test_geometry.f90`).

### Phase 2 — End-to-end single-frequency solve — **done (DC-limit scope)**

1. `tStudy%run` wired: assemble → topology → geometry (cached once) →
   per-frequency fill → `calcFreq2` → inject → solve. Fill interface per
   ADR 0009 (motivated by the C++ call-site bug, ADR 0017 finding 3).
2. Current-source injection at named nodes (ADR 0010).
3. Validation: DC limit vs Sunde/Dwight (15 %), low-frequency plateau,
   passivity across 10 Hz–1 MHz (`test_solve.f90`). **Full Portela-1997
   curve match not attempted** — no tabulated data exists (theory.md §9.2);
   §7 P3 (done, Phase 10 item 5) supplied an independent executable oracle
   instead: TAGS agrees on this conductor to 0.03 %.

### Phase 3 — Frequency sweep, results, output — **done (items 1–3)**

1. `logFrequencyAxis` default log-spaced axis.
2. `runSweep` result storage + `inputImpedance`/`maxVoltageMagnitude`
   queries; `tResult` simplified to plain-copy accessors.
3. CSV (tidy/long) and JSON (ADR 0012) writers; `example4.f90` end to end.
4. **Superseded**: OpenMP on the geometry fill loop — it was blocked on
   `mImpedance` reentrancy (module-level procedure pointers + `COMMON
   /params/`); a determinism test pins the write pattern
   (`test_geometry.f90`). Replaced by frequency-level parallelism
   (§7 P6, **done** in Phase 10 item 4): the per-frequency LU is ≈ 100 % of
   the cost, so the fill loop is not worth threading.

### Phase 4 — Dispersive soil — **done**

1. `tPortelaSoil` per ADR 0007 (ω₀ = 2π·1 MHz); `tMaterial%admittance`
   deferred function, shared `calcPropagationConstant`; `calcParamW`.
2. DC-limit convergence to `tLinear`, passivity, formula pinned at ω₀
   (`test_material.f90`). Curve match: same data gap as Phase 2.

### Phase 5 — JSON I/O and common cases — **done**

1. Input schema v1 frozen (ADR 0013: `sources`/`frequencies`/`outputs`);
   Fortran reader (`loadStudy` optional arguments, `runStudyFromFile`);
   write-time output filtering. Discretised-ID gotcha documented in
   `common/README.md`.
2. `common/` cases: `portela1997`, `rod`, `grid` (+ later `rod_air`,
   `silva2025_rho*`) with golden `_expected.csv` fixtures diffed by
   `test_common_cases.f90` (1e-6 relative; independent passivity check).
   Grid kept to one mesh — non-parallel pairs cost ~1–2 s each in 2-D
   quadrature at the time; the larger grid arrived with §7 P1 (Phase 10
   items 1 and 6, `portelaMesh`).
3. Parser stayed within the ADR 0006 minimal subset.

### Phase 6 — Sources and time domain — **done**

1. `mSignal`: Heidler (legacy 6-term set) and double exponential
   (`f1_2_5`/`f1_2_50`/`f1_2_200`/`f250_2500`, optional Jones front).
   Remaining legacy waveforms (single exp, impulse/step, Portela concave,
   sine) ported as needed.
2. FFT transient driver (`mTransient`): tail taper, one-sided spectrum,
   unit-current transfer function, conjugate-symmetric IFFT; DC bin
   replaced by `freqZeroHz` (ADR 0019 singularity). In-repo
   double-precision radix-2 FFT (ADR 0014). Schema + transient results
   shape in ADR 0015; `portela1997_transient.json`.
3. Validation is internal-consistency only (slow-surge GPR tracks the
   validated low-frequency `|Zin|` within 25 %, `test_transient.f90`) —
   same data gap as Phases 2/4.

### Phase 7 — Sources, signals and composite elements — **done**

Closed 2026-09-30. Phase 7 had grown into an unordered backlog ("more
elements, input functions and outputs"); the items that landed stay here
under stable numbers, and the remainder was prioritised into Phases 9–15
(mapping table below).

1. **Heidler function** — standard parametrised form (Heidler 1985 [37];
   IEC 62305-1 [39] parameter sets): `newHeidlerSignalTerms` (arbitrary
   terms, analytic η peak correction, optional legacy-style `imax`
   rescale); JSON `signal.terms` (ADR 0015 amendment). Done 2026-07-17.
2. **Voltage source** — ideal voltage sources converted to equivalent
   current injections by unit-injection superposition in the study layer
   (ADR 0016, implementing ADR 0010); mixed voltage+current source sets
   supported; JSON `sources[].voltage`; `inputImpedance` uses per-frequency
   effective currents. Done 2026-07-17.
3. **Grid/mesh generator element** — ADR 0020: JSON `"type": "mesh"`,
   `mElementMesh`/`tMeshElement`, composite element emitting `tLine` bars
   on a rectangular pattern. Running a frequency sweep over a real-sized
   grid was impractical until Phase 10 item 1 (P1 kernel) —
   `common/portelaMesh.json` shipped structure-only for that reason and
   has carried a sweep, a transient and golden fixtures since Phase 10
   item 6.
   Done 2026-07-31.
4. **Optional anti-alias filter for transient synthesis** — ADR 0021:
   frequency-domain Tukey roll-off, JSON `signal.antialiasStart`, off by
   default (golden fixtures unchanged). Contributed by acslima together
   with the Julia port (§4). Done 2026-09-29.

Where the former Phase 7 items went:

| Former Phase 7 item | Now |
| --- | --- |
| Transient driver fed by the harmonic scan | Phase 9 item 1 — done 2026-09-30 |
| Windowing (Hanning first) | Phase 9 item 2 — done 2026-09-30 |
| Portela concave-front signal | Phase 9 item 3 — done 2026-09-30 |
| Multiple injections (transient) | Phase 9 item 4 — done 2026-09-30 |
| Numerical Laplace Transform (§7 P4) | Phase 9 item 5 — done 2026-09-30 |
| GPR, touch and step voltage (§7 P7) | Phase 11 items 1–2 |
| `tCircumference` (grounding rings) | Phase 12 item 1 |
| Tubular conductor | Phase 12 item 2 |
| Series RLC element | Phase 12 item 3 |
| `tCatenary` | Phase 13 item 1 — done 2026-09-30 |
| Generic internal impedance models (OPGW) | Phase 13 item 2 |
| Insulated conductor | Phase 13 item 3 |
| Multipolar cables | Phase 13 item 4 |
| Mutual impedance between segments in different media | Phase 14 item 1 |
| Lightning discharge channel | Phase 10b (in air) — done 2026-10-01; coupling to buried electrodes: Phase 14 item 2 |
| Multi-layer soil and reflection-coefficient images | Phase 15 item 1 |

### Prioritisation of Phases 8–15

Phase numbers give priority order. The order was set on 2026-09-30 by
these rules, applied in turn (one exception, 2026-10-01: the lightning
channel in air was pulled ahead of Phase 11 by author decision and is
numbered 10b, rule 6 notwithstanding):

1. **Second implementation first.** The project's role is a citable
   reference (ADR 0018); an independent implementation passing the public
   contract is worth more to that role than any single feature, and the
   port is cheapest while the contract is small (≈ 6 k lines of Fortran
   today). The Julia prototype (§4) showed the contract is portable.
2. **Decided and small before open-ended.** Items whose design was settled
   in the author Q&As (2026-07-17, 2026-08-02) and rated **S** go first.
3. **MVP before application tier.** Tower-footing grounding under
   lightning (ADR 0018) before full-line/substation features.
4. **Dependencies.** P1 (1-D kernel) before anything that needs large
   grids or surface maps; P2 (Γ(ω) images) strictly before mixed-media
   coupling.
5. **Change the golden fixtures once.** Items that alter default numerics
   (P1, P2) are grouped in Phase 10 so `common/` fixtures are regenerated
   in one step, not per feature.
6. **New theory and object-model work (L) last.**

Phase 8 lives in its own tree (`rust/`) and targets a frozen contract tag
(Phase 8 item 1), so Fortran work on Phase 9 may overlap it. Phase 10 item 2
(Γ(ω), which regenerates every golden fixture) waited for Milestone 8a, so
the Rust port proved conformance on the pre-P2 fixtures first (met
2026-09-30); Phase 10 then moved both implementations together (done
2026-10-01).

Scoping decisions from the author Q&As are in *italics*; effort is rated
S/M/L (S ≈ days, M ≈ a focused week-scale task, L = new theory or
object-model work) from the 2026-07-17 legacy survey (registered findings
in theory.md §3.1, §4.3, §5, §6).

### Phase 8 — Second implementation (Rust) — **in progress (items 2–8 implemented; item 1 open)**

**Goal.** An independent Rust implementation of the public contract (JSON
schema v1 + `common/` cases, ADR 0002/0018) that reproduces every golden
fixture at the same 1e-6 relative tolerance as Fortran. Fortran stays the
implementation of record; Rust is the conformance cross-check and a
second, Fortran-toolchain-free distribution route (`cargo build` instead
of fpm + LAPACK/BLAS + the SLATEC clone, [DISTRIBUTION.md](DISTRIBUTION.md)).
Originally Python was proposed, but Python is dedicated to the GUI side
(ADR 0011); Rust gives compiled speed on the same cases, memory safety and
a single static binary.

**Scope.** In: everything schema v1 exercises at the conformance tag —
`line` and `mesh` elements; `linear`, `portela` and `alipio-visacro`
soils; vacuum air (ADR 0019); solid-conductor internal impedance; current
and voltage sources; frequency axes, sweep, `outputs` filtering; CSV and
JSON results; the full transient path (signals, tail taper, FFT,
anti-alias filter). Out: the GUI (it already reads the shared results
schema), parallelism, and Phase 9+ features until the follow-along rule
(item 10) picks them up.

**Design decisions** — recorded in [ADR 0022](adr/0022-rust-implementation.md)
(which also lists the deviations from this proposal: in-repo LU instead of
`faer`, series + Hankel Bessel subset instead of an AMOS port, no FFI
features):

| Topic | Proposal |
| --- | --- |
| Location | Top-level `rust/` Cargo package (library `tupa` + binary `tupa`), sibling of `fortran/`, `julia/`, `gui/` ([CONVENTIONS.md](CONVENTIONS.md) language separation) |
| Toolchain | Stable Rust, edition 2024, MSRV pinned in `Cargo.toml`; `#![forbid(unsafe_code)]` in the default build |
| Structure | Rust modules map 1:1 onto the Fortran modules (`ctes`, `error`, `verbosity`, `material`, `node`, `electrode`, `element` with `element::line`/`element::mesh`, `structure`, `geometry`, `geometry_cache`, `impedance`, `mesh`, `study`, `result`, `results_writer`, `signal`, `fft`, `transient`, and `json` for `mJsonParser` plus the `tupa` loader), so a reviewer can audit kernel against kernel ([ARCHITECTURE.md](ARCHITECTURE.md) §2) |
| Numbers | `f64` and `num_complex::Complex64` throughout (ADR 0018 precision row) |
| Quadrature | Line-by-line port of `dqag_k15` (adaptive Gauss–Kronrod 7/15) with the same tolerances and subdivision strategy — not a crate. The Julia prototype's fixed 64×64 midpoint rule sits ~0.09 % from Fortran, three orders above the golden tolerance; only the same rule can meet 1e-6 |
| Bessel functions | Internal impedance needs I₀/I₁ of complex argument (K₀/K₁ follow with the tubular conductor, Phase 12 item 2). Pure-Rust port of the required AMOS subset, including the Fortran code's large-argument ratio branch, pinned against SLATEC `ZBESI` tables. FFI to SLATEC allowed only behind an opt-in cargo feature for bit-level comparison tests |
| Linear solve | Dense complex LU with partial pivoting and multiple right-hand sides (ADR 0003, ADR 0016): `faer` (pure Rust) by default; LAPACK `zgesv` bindings behind an opt-in feature for comparison |
| FFT | Port the in-repo radix-2 FFT (ADR 0014, ~100 lines) so the transient path matches Fortran's operation order; general-purpose FFT crates are not used on the reference path |
| JSON | `serde` + `serde_json` with typed schema structs; defaults, unknown-key handling and the reference checks of `validateStudyReferences` mirror the Fortran loader exactly |
| Errors | `Result<_, TupaError>`; no panics on user input; messages follow the Fortran `raiseError` texts where practical |
| Dependencies | Minimal: `serde`, `serde_json`, `num-complex`, `faer` — everything else in-repo, in the spirit of ADR 0006 |
| Gate | Local `cargo fmt --check && cargo clippy -- -D warnings && cargo test --release` before merging — no hosted CI (ADR 0018) |
| License | GPL-3.0-or-later, as the Fortran package |

**Items** (numbered for citation; effort in brackets):

1. **Contract freeze and fixture widening** (Fortran side) — **S–M**. **Status: open** (no Fortran toolchain was available when the Rust port was written; the Rust tree targets the three existing fixtures, [ADR 0022](adr/0022-rust-implementation.md)). A finding for this item: `grid_expected.csv` predated the FIFO order fix of ADR 0020 (electrodes listed `Line_4…Line_1`), so the positional comparison of `test_common_cases.f90` failed (2026-09-30, gfortran 13); **fixed 2026-10-01** — every fixture was regenerated for Phase 10, so the file is in declaration order and the Fortran suite is green. The other parts of this item (widened fixtures, structure dump, tag) stay open; Phase 10 added fixtures for the ideal-image path (`portela1997_ideal`) and the 32 × 32 m grid (`portelaMesh`). Phase 9 added the transient-fixture harness this item needs (`compareTransientCase` in `test_common_cases.f90`, `check_transient_case` in `rust/tests/conformance.rs`; tolerance rule in the ADR 0015 amendment 2026-09-30) and five transient fixtures; `portela1997_transient` itself is still to be added.
   Only three cases carry golden fixtures today (`portela1997`, `rod`,
   `grid` — all harmonic, all `linear` soil, current sources only), so
   "passes every `common/` case" is currently a weak bar. Add
   `_expected.csv` fixtures, wired into `test_common_cases.f90`, for: one
   transient case (`portela1997_transient`), one voltage-source case (new,
   small), one `mesh`-element case (new, a single small cell), one
   `portela` and one `alipio-visacro` soil case (reduced-size derivatives
   of existing cases to keep test time low), and `rod_air` once its
   air-side physics is signed off (common/README). Add a CLI/debug dump of
   the discretised nodes and electrodes (IDs, coordinates, radii, media)
   so assembly can be compared before any physics. Write the ADR; cut the
   conformance tag (proposed **v0.6.0**).
2. **Crate scaffold and conformance harness** — **S**. **Status: done** — `rust/tests/conformance.rs` (keyed rows, 1e-6, passivity, guard for new fixtures). `rust/` package,
   gate commands, and first of all an integration test that walks
   `common/*_expected.csv`, runs the matching case and diffs at 1e-6
   relative, plus the independent passivity check of
   `test_common_cases.f90`. Written first so progress is measured case by
   case from red to green.
3. **Schema reader and validation** — **S–M**. **Status: done** — `rust/src/json.rs`; all 29 `common/*.json` load, validate and assemble (`tests/physics.rs`). Deviation: a present value of the wrong JSON type is an error rather than 0. Schema v1 as frozen by
   ADR 0013/0015 plus the 0016, 0020 and 0021 additions; pre-run reference
   validation; ADR 0013 frequency-axis rule
   (`round(ppd·log10(fmax/fmin)) + 1`); CLI verbosity levels. Test: every
   `common/*.json` loads; negative cases mirror the Fortran rejections.
4. **Object model and assembly** — **M**. **Status: done** — identical discretised IDs, FIFO element order; `--dump-structure` prints the assembled nodes/electrodes (the Fortran-side dump is part of item 1). Materials (`linear`,
   `portela` per ADR 0007, `alipio-visacro` mean set, vacuum air);
   `tLine` discretisation and the `mesh` composite element in FIFO element
   order (ADR 0020); node/electrode registration with **identical
   discretised IDs** (the common/README gotcha — outputs and sources name
   generated nodes). Test: item 1's dump matches for every case.
5. **Numerical kernels** — **M**. **Status: done** — line-by-line `dqag_k15`; Bessel subset is series + Hankel asymptotics rather than an AMOS port (ADR 0022). Geometry layer (mean/image distances,
   direction cosines, closed-form `g_self`, adaptive GK 7/15 `g(a,b)`,
   parallel-pair cache), propagation constant and `calcParamW`,
   solid-conductor internal impedance, `calcZSelf`/`calcZMutual` with all
   theory factors inside (ADR 0009). Tests: port the pins of
   `test_geometry.f90` (1e-6 vs independent integration) and
   `test_mesh.f90` (sign conventions, ADR 0008).
6. **System assembly and solve** — **M**. **Status: done** — in-repo LU with `izamax`-style pivoting (no `faer`, ADR 0022). Topology matrices, augmented
   `Zeq` (ADR 0003), multi-RHS LU; current injections at named nodes
   (ADR 0010); voltage sources by unit-injection superposition (ADR 0016).
   Tests: port `test_solve.f90` (DC limit vs Sunde/Dwight, low-frequency
   plateau, passivity) and `test_material.f90`.
7. **Sweep, results and writers** — **S–M**. **Status: done** — CLI parity plus `--output-dir`. **Milestone 8a met** on `portela1997`, `rod`, `grid` (1e-6). `logFrequencyAxis`,
   `runSweep`, `inputImpedance`, `maxVoltageMagnitude`; tidy CSV and
   results JSON (ADR 0012) with `outputs` filtering (ADR 0013); CLI
   parity with the Fortran executable (same arguments, same output file
   names). → **Milestone 8a: harmonic conformance.**
8. **Time domain** — **M**. **Status: implemented; Milestone 8b not formally closed** — ported unit and consistency tests pass, but no golden transient fixture exists until item 1. `mSignal` (Heidler legacy 6-term set and
   parametrised form with η, double exponential ± Jones front), tail
   taper, radix-2 FFT, transfer-function transient driver with the
   `freqZeroHz` DC-bin replacement (ADR 0019), anti-alias filter
   (ADR 0021), transient results JSON (ADR 0015). Tests: port
   `test_signal.f90`, `test_fft.f90`, `test_transient.f90`.
   → **Milestone 8b: full conformance.**
9. **Cross-implementation report** — **S**. **Status: partial** — Rust vs Julia/mHEM on Grcev ℓ = 10 m matches the published Fortran numbers (see `rust/README.md`); `docs/validation/fortran-vs-rust.md` and the wall-time table still need Fortran outputs.
   `docs/validation/fortran-vs-rust.md`: every runnable `common/` case,
   including those without golden fixtures (Grcev, Lima, Poljak, Silva),
   Rust vs Fortran, plus the three-way Fortran/Rust/Julia comparison on
   the Grcev ℓ = 10 m cases and `portela1997_transient` (extending
   [validation/tupa-vs-mhem.md](validation/tupa-vs-mhem.md)); wall-time
   table on the same cases; the GUI opens Rust results unchanged.
10. **Follow-along rule** — policy, from Milestone 8b on. Each later phase
    that changes the contract (schema, `common/` case, default numerics)
    carries a Rust item, per the ADR 0002 order (theory → schema/case →
    implementations). A phase closes when Rust passes its new or changed
    fixtures, or the lag is recorded in a conformance table in
    `rust/README.md`. First applied to Phase 9 (2026-09-30: Rust passes
    the five new transient fixtures); then Phase 10 (2026-10-01: Rust
    implements the single-integral kernel, the Γ(ω) images, the `numerics`
    block and the optional `segments`, and passes every regenerated fixture
    plus the two new ones; the threaded sweep is Fortran-only — Phase 8
    excludes parallelism).

**Risks.**

- *Adaptive-quadrature branching.* An identical algorithm can still
  flip a subdivision decision near its threshold under a different
  floating-point summation order, giving differences up to the local error
  estimate. Any case outside 1e-6 is investigated before the tolerance is
  touched; a tolerance change needs an ADR.
- *Hidden loader behaviour.* Defaults and generated IDs are only
  discoverable by comparison — hence item 1's assembly dump.
- *Moving target.* Fortran Phase 9 runs in parallel; the frozen tag and
  the item 10 rule keep the target fixed.

**Exit criteria.** `cargo test --release` green on every golden fixture
at the conformance tag (1e-6 relative) and on the ported unit pins; the
item 9 report published; [README.md](../README.md),
[common/README.md](../common/README.md) and ARCHITECTURE.md updated to
describe two conforming implementations. Overall effort **L**; most of it
sits in the contract surface (validation, IDs, writers, filtering) and in
matching numerics to 1e-6, not in the physics core — the Julia prototype
covers that core in a few hundred lines.

### Phase 8J — Julia port as a third conforming implementation — **in progress (items 1–4 done)**

**Goal.** Bring the contributed Julia prototype (§4) to the same bar as
Phase 8: the public contract implemented module by module after the
Fortran code, every golden fixture reproduced at 1e-6, CLI and output
files interchangeable with Fortran/Rust. It runs off the critical path
and alongside Phase 8 (it shares item 1's fixtures and item 9's report).
Its niche is interactive and scripted use (REPL, notebooks, parameter
studies); the design choices lean on the Fortran code where the Rust port
had to re-implement libraries: LAPACK `zgetrf`/`zgetrs` (= `ZGESV`),
AMOS Bessel functions (`SpecialFunctions`, = SLATEC `ZBESI`), `JSON`.
Layout and status: [julia/README.md](../julia/README.md).

**Items:**

1. **Module-by-module realignment** — **M**. **Status: done 2026-09-30.**
   One file per Fortran module (`julia/src/`), line-by-line ports of
   `dqag_k15`/`TWODQ`, the closed-form parallel pairs, the congruence
   cache, the radix-2 FFT (ADR 0014), voltage sources by superposition
   (ADR 0016), `validateStudyReferences`, the CSV/JSON writers with
   `outputs` filtering (ADR 0012/0013/0015) and the CLI options
   (`--epsrel`, `--no-cache`, `--dump-structure`, `--output-dir`). Mesh
   bar IDs, the tidy transient CSV and μ₀ now match the other codes.
   `JSON3` (deprecated upstream) replaced by `JSON`; `FFTW` dropped;
   `Plots` demoted to an optional extension (`--plot`); a precompile
   workload brings a CLI run of a small case to ~0.8 s.
2. **Harmonic conformance (Milestone 8J-a)** — **S**. **Status: met
   2026-09-30** — `julia/test/conformance.jl` (keyed rows, 1e-6,
   passivity): `portela1997` and `rod` to ~1e-17, `grid` to 6e-8.
3. **Ported unit and physics tests** — **S**. **Status: done** —
   `julia/test/unit.jl`, `physics.jl` (the Rust ports of `test_geometry`,
   `test_impedance`, `test_fft`, `test_signal`, `test_solve`,
   `test_sweep`, `test_transient`, `test_mesh_element`,
   `test_validation`); 166 checks, ~3 s.
4. **Cross-check on every runnable case** — **S**. **Status: done
   2026-09-30** — all 28 runnable `common/` outputs (22 sweeps, 6
   transients) within 1e-6 of fresh Rust and Fortran runs, most identical
   to the printed digit; the only exceptions are two round-off rows of
   `portelaMesh` (~1e-12 A free-end currents at 10 MHz, 1.9e-6 vs Fortran),
   which Rust fails the same way (1.2e-6). Assembly dumps byte-identical
   to Rust. Table in `julia/README.md`. Finding for Phase 8 item 1: the
   1e-6 row floor is an absolute 1e-12 tolerance on near-zero currents —
   a `portelaMesh` fixture would need a larger floor or those rows
   filtered.
5. **Validation writeup refresh** — **S**. **Status: open.** The Julia
   figures in [validation/tupa-vs-mhem.md](validation/tupa-vs-mhem.md) and
   `validation/julia-grcev-l10-results.csv` were measured with the
   prototype (fixed 64×64 midpoint rule, ~0.09 % from Fortran). Re-run
   `docs/validation/run_all.sh` (it calls
   `julia/comparison/run_tupa_grcev_l10.jl`, already ported to the new
   API), regenerate the metrics and restate the Fortran–Julia row (now
   expected ≲ 1e-6) and the transient paragraph.
6. **Transient and widened-fixture conformance (Milestone 8J-b)** — **S**,
   blocked on Phase 8 item 1. When the transient, voltage-source, `mesh`
   and dispersive-soil fixtures land, add them to
   `julia/test/conformance.jl` (its "every golden fixture has a test"
   guard fails until then) and to the README table.
7. **Three-way report** — **S**, with Phase 8 item 9: add the Julia column
   to `docs/validation/fortran-vs-rust.md` (all runnable cases and the
   wall-time table); run `benchmarks/cross-impl/bench.py` with all three
   (its `run_julia.jl` now writes the standard result files).
8. **GUI check** — **S**. Open Julia sweep and transient result files in
   the GUI (ADR 0011); they are byte-compatible with the Rust files, so
   this is a confirmation, not new work.
9. **Follow-along rule** — policy, as Phase 8 item 10: every contract
   change carries a Julia item; lags go into the `julia/README.md`
   conformance table. First instance: Phase 9 — **lagging** (no Julia
   toolchain in the session that implemented it); the loader rejects the
   Phase 9 `signal` fields with an explicit error instead of ignoring them,
   and the five Phase 9 fixtures are listed as lag in `julia/test/`
   (`PHASE9_LAG`). Second instance: Phase 10 — **lagging**, same reason
   (the Julia host was denied by the egress policy again): the loader
   refused the `numerics` block and the regenerated harmonic fixtures were
   skipped. **Ported 2026-10-01** with Julia 1.13 (`geometry_factor_1d`,
   `image_coefficients`, `load_numerics`; the threaded sweep not ported):
   `portela1997`, `rod`, `grid`, `portela1997_ideal` and the harmonic half
   of `portelaMesh` run in `Pkg.test()` (239 checks) at 1e-6; the Phase 9
   transient fixtures and `portelaMesh_transient` stay lag. Found on the
   way: the mesh segment target undershoots (ADR 0024 §8), open in all
   three implementations (`julia/README.md`).

**Optional, not required for conformance:** multi-threading over
frequencies (`Threads.@threads`, with one BLAS thread per task — the
Julia form of Phase 10 item 4/§7 P6); registering the package or shipping
a `juliac`/PackageCompiler binary (DISTRIBUTION.md); a
Documenter.jl API page from the existing docstrings.

**Exit criteria.** `Pkg.test()` green on every golden fixture at the
conformance tag (items 2 and 6), item 5's writeup refreshed and item 7's
report including Julia; then README.md and common/README.md describe
three conforming implementations.

### Phase 9 — Transient pipeline completion — **done 2026-09-30**

All five items touch `mTransient`/`mSignal` and the `signal` block; items
1, 2, 4 and 5 landed together under one ADR 0015 amendment
([ADR 0015, amendment 2026-09-30](adr/0015-time-domain-signal-schema.md#amendment-2026-09-30--transient-pipeline-completion-roadmap-phase-9)),
which also settles theory.md §8 open questions 1–3. Defaults stay as
before, so no golden fixture changes. Of the remaining legacy waveforms
the sine came along with item 4; single exponential and impulse/step are
ported when a case needs them. Follow-along: Rust implements the whole
phase and reproduces its fixtures (worst 1.5e-8 under the fixture rule);
Julia lags and rejects the new fields explicitly (julia/README.md
conformance table). Checks and measured tolerances:
[validation/phase9-transient-options.md](validation/phase9-transient-options.md).

1. **Transient driver fed by the harmonic scan** (interpolated transfer
   function) — **S** — **done 2026-09-30** (`signal.transferFunction:
   "interpolated"`; `mTransient::pchipInterpolate`, a port of Matlab
   `pchip`; loader rejects a scan axis that does not span
   [`freqZeroHz`, `nyquistHz`]; within 1.7e-5 (driving point) / 2.7e-5
   (remote node) of peak of the per-bin solve on the
   `silva2025_*_transient` cases, 12–15× faster; fixture
   `portela1997_transient_interpolated`). Today `transientResponse` solves the system at
   every FFT bin (N/2 + 1 solves), so the transient dominates run time
   even where the harmonic sweep itself is fast. The original Matlab
   already ships the remedy as its default mode (`TODA_FREQ` off): solve
   H(ω) only on a reduced scan grid (`freq_log` — log-spaced points with
   the low end raised to the linear-bin floor, first point `FREQ_ZERO`),
   then interpolate onto the FFT bins (`imitancia.m`, complex `pchip`
   with extrapolation) before the usual inverse FFT. *Decided
   (2026-08-02): the scan grid is the existing `frequencies` block —
   with `signal.transferFunction: "interpolated"` the case's
   `frequencies` axis is solved and interpolated onto the FFT bins;
   `"full"` (default — golden fixtures unchanged) keeps today's per-bin
   solve.* Unlike the legacy, no extrapolation: the loader must reject a
   scan axis that does not span [`freqZeroHz`, `nyquistHz`]. Validate by
   comparing both paths on the `silva2025_*_transient` cases; needs an
   interpolation routine (pchip, componentwise on Re/Im, matching the
   Matlab).
2. **Windowing, Hanning first** — **S** — **done 2026-09-30**
   (`signal.window: { "type": "hann", "placement": "spectral" | "time" }`,
   falling half of a Hann window; spectral Hann = the ADR 0021 Tukey
   filter in the s → 0 limit, the two fields multiply; fixtures
   `portela1997_transient_hann`, `_hann_time`). *Decided (2026-08-02): both
   placements, selectable in the file* — a `signal.window` option choosing
   the window function and where it acts: (a) spectral data window applied
   to the one-sided H·X product before the inverse transform (Gibbs
   suppression, the NLT-style filter — theory.md §8), or (b) time-domain
   window on the sampled excitation record. Default stays "none"; the erfc
   tail taper (`tailTaper`) keeps its separate record-truncation role and
   default, and the ADR 0021 anti-alias filter stays a separate option.
3. **Portela concave-front signal** — **S** — **done 2026-09-30**
   (`signal.waveform: "portela"`, fields `imax`/`alpha`/`tFront`/`tTopEnd`/
   `tTailEnd`, in Fortran, Rust and Julia;
   [ADR 0023](adr/0023-legacy-case-import.md)). Faithful port of the legacy
   `sinais.Portela`/`impulso.m` waveform: concave exponential front
   i(t) = I·(e^(αt/t₁) − 1)/(e^α − 1) up to the front time t₁, flat top
   at I until t₂, linear decay to zero at t₃ (formula in theory.md §8);
   JSON `signal.waveform: "portela"` with English parameter names (peak,
   alpha = front-inclination factor, front/top-end/tail-end times); cite
   Portela 1997 [1] as the usage context and Portela's 1982 course text
   [67] as the front law's source. Accept α < 0: [67] uses it for
   subsequent strokes (convex front), which settles theory.md §8 open
   question 4.
4. **Multiple injections in the transient pipeline** — **S–M** — **done
   2026-09-30** (`signal.sources[]`, each a node plus a full waveform;
   new `sine` waveform; one unit-current sweep per source, Σ_k H_k·X_k;
   results JSON gains `sources[]`, CSV sums per node; fixture
   `portela1997_transient_multi`, a differential ±30 kA impulse plus a
   sine). Current injections only — voltage sources in the transient
   and the multi-RHS per-frequency solve are left open. The
   harmonic side already handles simultaneous mixed sources (ADR 0016);
   remaining work is per-source spectra × per-source transfer functions,
   superposed (linear), and the schema (e.g. three-phase sine emulating
   line voltage plus an impulse injection). Survey note: the legacy also
   supports a *differential* (±1 two-node) injection pattern worth
   carrying along.
5. **Numerical Laplace Transform option** (§7 P4) — **M** — **done
   2026-09-30** (`signal.transform: "nlt"`, `nltDamping` default
   ln(N²)/T; Laplace-domain `admittanceLaplace`/`calcParamLaplace`/
   `internalImpedanceLaplace`, the real-ω path untouched; rejected together
   with `"interpolated"`; 6–10× closer than the FFT to a long-record
   reference over the first half of a short record, degrading towards
   the record end as e^{ct} — theory.md §8; fixture
   `portela1997_transient_nlt`). *Opt-in
   first: `signal.transform: "fft"` (default) `| "nlt"`; flip the default
   only after P3 cross-validation (Phase 10 item 5; **outcome 2026-10-01: not
   flipped** — ADR 0024 §7), keeping golden
   fixtures stable.* Driver-only change (theory.md §8); refs: Gómez &
   Uribe [17], TAGS as executable reference. Reuses the item 2 spectral
   window. Own ADR 0015 amendment.

**Exit criteria.** Scan-fed and full transients agree on the
`silva2025_*_transient` cases within a tolerance stated in the amendment;
one new transient golden fixture per new option; existing fixtures
unchanged. **Met 2026-09-30:** stated tolerance 1e-4 of the series peak,
measured ≤ 2.7e-5; five new fixtures; the three harmonic fixtures pass as
before (`grid` keeps its pre-existing Phase 8 item 1 drift) and existing
transient outputs are unchanged (byte-identical, bar last-digit round-off
on a few near-zero samples of `portela1997_transient`).

### Phase 10 — Reference kernel, image model and performance — **done 2026-10-01**

The §7 proposals that change default numerics or unblock larger cases,
grouped so the golden fixtures are regenerated once (rule 5). The decisions,
measurements and the `numerics` schema are in
[ADR 0024](adr/0024-phase10-numerics.md); fixtures, tests and the Rust
counterparts landed together. Results: [validation/tags-xval.md](validation/tags-xval.md),
[validation/phase10-image-model.md](validation/phase10-image-model.md). The
Fortran package is **0.6.0** ([CHANGELOG.md](../CHANGELOG.md) records the
changed defaults).

1. **mHEM single-integral kernel** (§7 P1) — **S** — **done 2026-10-01**.
   `mImpedance::geometryFactor1D`, the default for `g(a,b)` off the closed
   forms (`numerics.kernel: "single"`; `"double"` and `--kernel double` give
   the nested 2-D path, kept as the test oracle). Re-entrant; `epsrel`
   unscaled, `epsabs = 0`; cancellation-free `r1 + r2 − lb`; clamp for the
   touching-end-point singularity written without NaN comparisons (the
   release build is `-ffast-math`). 8× faster than the 2-D path on random
   non-parallel pairs, 7.9e-8 apart; a T junction resolved to 2e-7 of the
   exact value. Tolerances revisited (§6). `test_geometry.f90` pins the
   tolerance sweeps to the 2-D kernel and adds the 1-D vs 2-D oracle, T
   junction and mirror-image cases; Rust `geometry_factor_1d`.
2. **Frequency-dependent image reflection coefficient** (§7 P2) — **S** —
   **done 2026-10-01**. `Γ(ω) = (W_own − W_other)/(W_own + W_other)` on the
   image parcels of both `Z_t` and `Z_ℓ` (original Matlab default, ADR 0017
   finding 1) from `tMesh%gammaAir`/`gammaSoil`, set in `calcParamW`/
   `calcParamLaplace` (so the NLT path has it too); ideal ±1 remains
   (`numerics.imageModel: "ideal"`, `--image-model ideal`) with its own
   fixture `portela1997_ideal`. Every golden fixture regenerated (all eight
   plus the two new ones); the change is 1.8e-3 at 1 MHz on the Portela
   conductor, f²-scaling, and under 2 points of MAPE against every published
   curve. `grid_expected.csv` thereby left its pre-ADR-0020 order. The
   release is **0.6.0** (CHANGELOG.md).
3. **Criteria-based segmentation target** (§7 P8) — **S** — **done
   2026-10-01**. `numerics.maxSegmentLength`: `max(segments,
   ⌈length/target⌉)` per `line`/`catenary`/`mesh` element (arc length for a
   catenary, longest bar for a mesh), `segments` optional; the λ/10 default
   stays the user's to apply (theory.md §4.1). `test_segmentation.f90`: the
   counts, and a coarse-vs-fine convergence test (|Zin| of the 10 m conductor
   at 5 / 2.5 / 1 / 0.5 m targets vs 0.25 m: 0.7 / 0.26 / 0.03 / 0.08 % at
   1 kHz, 9.1 / 1.8 / 0.007 / 0.13 % at 1 MHz; the error is not monotone below
   1 m — the 0.5 m one exceeds the 1 m one — and no convergence-sign claim is
   made).
4. **Frequency-level parallelism** (§7 P6) — **M** — **done 2026-10-01**.
   `runSweep` solves frequencies in an OpenMP loop on thread-private meshes
   (`solveAtFrequency`; the study is read-only), bit-identical for any
   thread count (`test_parallel.f90`, current and voltage sources). Measured
   against the fill-loop alternative: the LU is ≈ 100 % of a frequency's
   cost (585 unknowns), so the fill-loop OpenMP is **superseded** and not
   written. 3.4× on 4 threads (`portelaMesh`, 41 frequencies, 5.2 s → 1.5 s).
   Found on the way: SLATEC's `D1MACH` lazy init races (flag set before the
   table is filled) → `warmUpMachineConstants` before the parallel region.
   `--threads`, `OMP_NUM_THREADS`. Rust and Julia stay serial.
5. **Cross-code validation against TAGS** (§7 P3) — **M** — **done
   2026-10-01**, [validation/tags-xval.md](validation/tags-xval.md);
   driver and runner in `benchmarks/tags-xval/`. Six cases (conductor, rod,
   loop, two Grcev electrodes, tower footing): ≤ 0.3 % below 1 MHz once
   TAGS' `|cos θ|` convention is accounted for (found: it makes a loop
   oriented around itself differ by 4 % at 100 kHz, and the image of a
   vertical electrode by ~1.6 % at 1 MHz); the passivity violation of the
   mHEM factorisation near 7–9 MHz is shared by both codes. **Outcome for the
   Phase 9 item 5 NLT default: not flipped** (ADR 0024 §7) — the comparison
   is harmonic; a TAGS time-domain case is the open follow-up.
6. **Larger `common/` grid case** — **S** — **done 2026-10-01**.
   `portelaMesh.json` (32 × 32 m, 185 nodes, 200 electrodes): 21 harmonic
   frequencies and a 512-sample scan-fed transient in ≈ 5 s serial, with
   golden fixtures `portelaMesh_expected.csv` and
   `portelaMesh_transient_expected.csv` (`compareMeshGridCase`, Rust
   `portela_mesh_*`).

**Follow-along.** Rust implements items 1–3 and 6 and matches every fixture
(worst 1.3e-10 harmonic, 1.5e-8 NLT, 3e-12 other transients); Julia
followed on 2026-10-01 for items 1–3 and 6 (item 8J-9, `julia/README.md`).

**Exit criteria — met.** Fixtures regenerated once under P1 + P2 and matched
by Rust; P3 writeup in `docs/validation/`; a real-sized grid sweep runs in
seconds, not hours (the 200-electrode `portelaMesh`: 5.2 s serial for 41
frequencies, 1.5 s on four threads).

### Phase 10b — Lightning channel in air

Moved ahead of Phase 11 on 2026-10-01 (author decision), splitting the
former Phase 14 item 2. It is numbered 10b rather than 11 so that the
Phase 11–15 cross-references in ADRs and the other implementations'
READMEs stay valid. Everything here lives in air: with cross-media
coupling neglected (theory.md §5) the channel needs nothing from Phase 14
item 1, and it changes no buried-electrode output (GPR, touch and step
voltages) until that item exists. Design basis: theory.md §4.5 (new,
2026-09-30 review of Cooray [74] and Silveira [45]); the legacy provides
only the geometry generator `canal.m`.

1. **Channel element with series loading** — **M** — **done
   2026-10-01** ([ADR 0025](adr/0025-lightning-channel-and-two-node-sources.md)).
   *Decided (2026-07-17 Q&A): antenna-model route, with distributed series
   impedance calibrated so the channel propagation matches a return-stroke
   speed prescribed in the JSON.* The `"channel"` element: strike node (or a
   free-standing position), length, incidence and azimuth, radius, `speed`
   (number or `v(z)` profile) or explicit `inductance`, `R'` (number or
   piecewise profile), segments uniform or graded from the foot with a
   bounded adjacent-length ratio (`canal.m`'s spacing is not ported). A chain
   of ordinary air segments with `z_ch = R' + jωL'` in the internal-impedance
   slot (`tElectrode%loaded`); `L'` from the closed form, and with
   `calibrate: true` **calibrated** on the lossless channel alone above
   ideal ground with the 10–90 % tangent metric (`mChannelCalibration`); the
   calibrated scale is recorded in the results (`channels` block). The
   calibrated `L'` lies within 18 % of the closed form on the cases tried
   (scale 0.82–1.05, docs/validation/channel-validation.md). Fortran and
   Rust conform to the four new fixtures (the calibrated one included);
   **Julia lags** (its loader refuses the element, `returnNode` and
   `quantity`; it has followed Phase 10 but not Phase 9).
2. **Channel excitation** — **S–M** — **done 2026-10-01** (ADR 0025).
   Two-node sources: `returnNode` on `sources[]` and `signal`/
   `signal.sources[]` entries — a current dipole (`+I` at the object node,
   `−I` at `<channel>-base`, no kernel change, ADR 0010) or a delta-gap
   voltage source (unit pattern the ±1 dipole, constraint on `u_a − u_b`,
   amending ADR 0016); transient sources take `quantity: "voltage"`.
   Channel cases run on the NLT path (`channel_*.json`). A cloud capacitance
   at the top (the low-frequency limit in theory.md §4.5) was not added: no
   case needed it.
3. **Validation** — **S–M** — **done 2026-10-01 except the Ishii case**,
   [validation/channel-validation.md](validation/channel-validation.md),
   script `docs/validation/channel_checks.py`. Chen's analytic current on
   a vertical cylinder over perfect ground (Baba & Rakov's configuration, run
   with `imageModel: "ideal"`): within 1.5 % of the peak; loaded-channel speed
   against Table 3 of [44] (0.23 m wire, 2/4/8 µH/m): 0.35–0.58c against
   0.37–0.60c; base impedance against the TL estimate. **Open**: the Ishii
   reduced-scale induced-voltage case as reproduced by Pokharel et al.
   (all-air geometry) — the measured waveform exists here only as a
   description, so it needs a digitised figure. Fixtures:
   `channel_unloaded`, `channel_loaded`, `channel_tower`, `channel_tower_gap`.

**Exit criteria — met (2026-10-01).** A tower strike runs end to end
(`channel_tower*.json`: harmonic and transient, current and voltage
sources); Fortran and Rust agree on every fixture; the channel reproduces
Chen's unloaded current and Baba & Rakov's loaded speeds.

Out of scope here: fields from the channel (Phase 11 item 1 supplies the
post-processing), induced-voltage work over lossy ground (post-MVP), and
the HEM option of an enlarged transversal radius to lower the channel
impedance (theory.md §4.5, optional extension).

### Phase 11 — Grounding-safety outputs

The headline engineering output for the MVP application (tower-footing
grounding); placed after Phase 10 because surface maps over real footings
need the P1 kernel's speed and Γ(ω)-consistent potentials.

1. **Field/potential post-processing** (§7 P7) — **M**. Scalar
   potential, electric field and path voltages at arbitrary points from
   the solved I_t/I_ℓ, including image contributions; prioritise `tResult`
   subtypes from the Matlab output-class inventory (ADR 0017 finding 6).
2. **GPR, touch and step voltage** — **M**. *Both input forms from the
   start: explicit observation-points array plus an optional auto
   surface-grid block.* Formula and legacy/TAGS correlation in theory.md
   §3.1; ADR 0012 results-schema extension. *Decided (2026-07-17 Q&A):
   legacy-geometric definitions (touch = max |ψ − u_node| on a 1 m
   circle, step = ψ difference at 1 m spacing), citing IEEE Std 80 [42]
   as normative context; body-circuit / surface-layer derating factors
   stay out of the solver.* Single-frequency first; transient maps reuse
   the Phase 9 driver.

### Phase 12 — Tower-footing electrode library

Elements and materials that complete the typical tower-footing
geometries (rods, counterpoises, rings, lumped branches).

1. **`tCircumference` (grounding rings)** — **S**. Legacy `Anel.m`:
   circle in an arbitrary plane (centre + normal vector + rotation),
   ≥ 3 straight segments, closed loop (exercises loop topology);
   single-medium constraint enforced.
2. **Tubular conductor** (metallic pipes) — **S**. Schelkunoff I/K
   formula in theory.md §4.3 [40]; element = `tLine` + wall thickness
   (legacy `Tubo.m`); extends `mImpedance` with SLATEC `ZBESK` alongside
   `ZBESI` (scaled variants for large arguments).
3. **Series RLC element** — **M**. *Series form only first
   (R + jωL + 1/(jωC) two-terminal, Matlab-style non-coupling lumped
   element); parallel later if a case needs it.* Legacy mechanism (extra
   non-electromagnetic branch, Z(ω) on the Z_ℓ diagonal, zeroed Z_t row)
   in theory.md §6, including the nonsingularity check and DC pin a port
   must add.
4. **Remaining dispersive-soil parameter sets** (§7 P5) — **S–M**.
   *Relatively conservative*/*conservative* Alipio–Visacro sets and
   `tLongmireSmithSoil` (Longmire & Smith [15] per Cavka et al. [16]).

### Phase 13 — Line and substation tier: conductors and cables

Application-tier features (ADR 0018). `tCatenary` is cheap and may be
pulled forward if a tower-footing case needs shield wires.

1. **`tCatenary`** — **S** — **done 2026-09-30** (pulled forward for the
   legacy `linha4` case; [ADR 0023](adr/0023-legacy-case-import.md)). *Matlab-faithful port, discretised into
   straight segments like `tLine`.* The legacy "catenary" (`Catenaria.m`)
   is a **parabolic** sag profile (z ∝ x², sag parameter at midspan,
   uniform plan spacing), plus a 3-node variant — Matlab-faithful and
   parabolic approximation coincide. Pure element-assembly work.
   Implemented as JSON `"catenary"` (theory.md §4.4) in Fortran, Rust and
   Julia; the 3-node variant (`catenaria3`) waits for a case that needs it.
2. **Generic internal impedance models (e.g. OPGW)** — **M**. JSON
   database, alternatively referenced from the material property of
   elements. *Decided (2026-07-17 Q&A): entries carry
   frequency-tabulated R(f), X(f) (measured/datasheet data), interpolated
   at solve time* — captures stranding/steel-core effects the equivalent
   tube misses; extrapolation limits validated and flagged. (The legacy
   ACSR spreadsheet is candidate seed data; the C++ bundle/L-profile
   models remain a possible catalogue kind later.) Where no measured
   data exists, conductor subdivision (de Arizon & Dommel [73]) can
   compute R(f), X(f) tables for stranded or non-circular cross-sections
   offline.
3. **Insulated conductor** — **M–L**. The legacy branch is an
   acknowledged placeholder (drops soil conduction, ignores the coating;
   flagged TODO in the legacy code) — do **not** port it; implement
   Sunde's coating admittance in series with the bare-conductor soil
   leakage [41] (theory.md §4.3).
4. **Multipolar cables** — **L**. Internal representation by
   impedance/admittance matrix; object-model change (multi-conductor
   element); refs: Ametani cable constants [43], Schelkunoff [40];
   PRTL-mHEM's tubular bundles are a partial analogue.

### Phase 14 — Air–soil coupling

1. **Mutual impedance between segments in different media** — **L**.
   No legacy implementation to port (unfinished body, ADR 0017); the
   candidate quasi-static transmission-coefficient route is in theory.md
   §5 [35], worked out for cross-medium potentials in Salari [66]
   App. B; validate on `rod_air`-class cases. *Decided (2026-07-17 Q&A):
   strictly after P2 (Phase 10 item 2) — both touch the same interface
   machinery and P2 restores reference behaviour first.*
2. **Channel coupling to buried electrodes** — **M**. The channel
   itself moved to Phase 10b (2026-10-01). What remains is its
   interaction with electrodes in soil, which needs item 1: a channel
   above buried electrodes then affects GPR, touch and step voltages
   beyond the injected current. Also the post-MVP routes the channel
   opens: induced voltages ([45,46]; lossy-ground coupling via Norton's
   approximation) and the LEMP term plain EMT analysis misses [52].

### Phase 15 — Multi-layer soil

1. **Multi-layer soil and reflection-coefficient images** — **L**. Lifts
   the §5 single-interface premise; route: layered-earth Green's
   functions via quasi-static complex images (Li, Chen & Wang [33]; Sunde
   [41] background). Explicitly out of MVP scope (theory.md §5, §10.1).

### Legacy element inventory

The Matlab reference's full element inventory, for the record: straight
lines (three variants), ring, grid, cable, catenary, lightning-channel
element, helicoidal rod, tube, conduit, lattice and crossarm placeholders
(block/cube/pyramid/tetrahedron), lumped series-RLC "impedance" elements
(extra unknowns that do not couple electromagnetically — their `Zt` row is
zeroed), and insulated buried cables (leakage through jωε only;
placeholder theory, flagged TODO in the legacy code itself). The C++ adds
bundle and L-profile (lattice-member) internal impedances and a
shielded-wire segment.

---

## 4. Milestones

- **Phases 0–6**: met (see §3) — end-to-end harmonic sweep and FFT
  transient from a JSON case file, dispersive soils, regression fixtures.
- **Release bar — met (2026-08-02 decision)**: ADR 0018's "0.1.0" bar
  asked for one validated physics case; the
  [`docs/validation/`](validation/README.md) published-curve campaign
  (Grcev, Lima ×2, Poljak, Silva ×2 — six writeups, harmonic and
  transient) is accepted as the executable-oracle substitute (ADR 0018
  postscript). **v0.5.0** (tagged 2026-07-31, "first release") is the
  first public release. The Portela-curve case itself remains unmatched
  for lack of tabulated data (theory.md §9.2); §7 P3 (TAGS
  cross-validation) was run in Phase 10 item 5 as the independent oracle
  ([validation/tags-xval.md](validation/tags-xval.md)) — it matches this
  conductor to 0.03 %.
- **Julia port — contributed prototype — met (2026-09-29)**: a compact
  native Julia implementation (`julia/`) contributed by acslima and merged
  in [carloskleber/tupa_ref#1](https://github.com/carloskleber/tupa_ref/pull/1)
  — the first implementation of the contract other than Fortran, and the
  first external code contribution. It reads the shared `common/` JSON and
  solves harmonic and transient studies (lines and meshes;
  linear/Portela/Alipio–Visacro soils; internal impedance;
  `signal.antialiasStart`). Results: ≤ 0.17 % in |Z| from Fortran on the
  Grcev Fig. 12 ℓ = 10 m cases, and under 8·10⁻⁴ of peak in v(t) and
  i₁/i₂(t) on `portela1997_transient.json`
  ([validation/tupa-vs-mhem.md](validation/tupa-vs-mhem.md), which also
  compares the TAGS mHEM prototype). The same contribution brought the
  ADR 0021 anti-alias filter (Phase 7 item 4) and the `run_all.sh`
  validation driver with PDF output. What it established: the object
  model and schema port cleanly (ADR 0002), and a different quadrature
  rule alone costs ~0.09 % — which fixed the Phase 8 quadrature decision.
  As contributed it was **not conforming**: fixed 64×64 midpoint
  quadrature for the geometry factors, no voltage sources (ADR 0016), no
  `outputs` filtering (ADR 0013), no results JSON/CSV for sweeps.
- **Milestone 8J-a — Julia harmonic conformance** (**met** 2026-09-30):
  Phase 8J items 1–4 realigned the port module by module with the Fortran
  code; the three golden fixtures pass at 1e-6 and every runnable
  `common/` case is within 1e-6 of Fortran and Rust (bar two round-off
  rows of `portelaMesh` that Rust shares). **Milestone 8J-b**
  (transient and widened fixtures) waits on Phase 8 item 1, like 8b.
- **Milestone 8a — Rust harmonic conformance** (**met** 2026-09-30 on the
  three existing golden fixtures; item 1's widened fixtures would
  strengthen it): Phase 8 items 2–7.
- **Milestone 8b — Rust full conformance** (**implemented, not closed**):
  Phase 8 item 8 is coded and self-consistent; closing it needs item 1's
  transient/voltage/mesh/dispersive-soil fixtures so the claim is measured
  against Fortran numbers, not asserted.
- **Phase 10 — met (2026-10-01)**: single-integral kernel, `Γ(ω)` images,
  segment-length target, threaded sweep, TAGS cross-validation and the 32 × 32 m
  grid fixtures; all golden fixtures regenerated once and matched by Rust;
  Fortran package version **0.6.0**.
- **Phase 10b — met (2026-10-01)**: lightning channel element in air
  (series-loaded, graded, speed-calibrated), two-node current and voltage
  sources, four `common/` fixtures matched by Rust, validation against Chen
  and Baba & Rakov (the Ishii case is open); ADR 0025; Fortran package **0.7.0**.
- **Next engineering steps**: Phase 8 item 1 (the rest — widened fixtures, the
  structure dump and the conformance tag, which now falls on the Phase 10
  fixture set; it closes Milestone 8b and gives the Julia port a sharper
  target), then Phase 8 item 9 (report); Phase 8J follow-along for Phases 9
  and 10b (Julia: Phase 10 done 2026-10-01 on the 1.13 toolchain; Phase 9
  — transfer-function/NLT transients — is next there, and Phase 10b follows it); then Phase 11.

---

## 5. Testing strategy

| Layer | Where | What |
| --- | --- | --- |
| Unit | `fortran/test/` (Rust: `rust/tests/`, Phase 8; Julia: `julia/test/`, Phase 8J) | quadrature vs closed forms; sign/decay pins; Bessel `Z_int` vs tables; dispersion DC limit; waveforms |
| Integration | `fortran/test/` | end-to-end DC resistance; sweep/transient consistency; reciprocity, passivity; voltage-source superposition |
| Reference | `common/` | JSON in → CSV out, golden diff at 1e-6; shared across languages (fixture coverage widened by Phase 8 item 1) |
| Published curves | `docs/validation/` | digitized-figure comparisons (Grcev, Lima, Poljak, Silva) with regenerable plots + per-case xlsx data |
| Benchmarks | `benchmarks/` (proposed) | TAGS and PRTL-mHEM as git submodules; cross-code runs per [BENCHMARKS.md](BENCHMARKS.md) |

There is **no hosted CI** (ADR 0018): the gate is a local
`fpm build && fpm test` before merging (for `rust/`, the Phase 8 cargo gate; for `julia/`, `Pkg.test()`). Practical caveats:

- **Run the slow suites under `--profile release`** (as `build.sh` builds):
  in the debug profile the quadrature-heavy suites were effectively
  un-runnable (`test_geometry`'s 10×10 fill exceeded 9 minutes, measured
  2026-07-05, with the 2-D kernel; the single-integral default has not been
  re-measured in debug). The whole release suite takes about 6 s (2026-10-01).
- A cold `fpm test` pays several minutes of stdlib compilation; keep the
  build cache.
- A non-parallel segment pair costs ≈ 3 µs in the default single-integral
  kernel and ≈ 23 µs in the 2-D oracle (release, random pairs, measured
  2026-10-01); the ~1–2 s per pair recorded 2026-07-10 predates the
  Gauss-weight fix of theory.md §4.2. The cost of a case is now the
  per-frequency LU, ∝ (n_n + 2 n_s)³: a 200-electrode grid sweeps in
  seconds, a 300-electrode legacy line with a full-FFT transient in minutes.
  Frequencies run in parallel under `-fopenmp`.

---

## 6. Open decisions

| Decision | Status |
| --- | --- |
| Soil dispersion model | **Implemented** — ADR 0007 (`tPortelaSoil`, ω₀ = 2π·1 MHz), Phase 4; `tVisacroAlipioSoil` mean set (§7 P5) |
| Voltage-source handling | **Implemented** — current-injection equivalents (ADR 0010) by unit-injection superposition (ADR 0016), Phase 7 item 2 |
| Impedance-fill interface | **Implemented** — theory factors inside `calcZ*` (ADR 0009) |
| FFT dependency | **Implemented** — in-repo double-precision radix-2 FFT (ADR 0014); opt-in NLT on top (§7 P4, Phase 9 item 5) |
| JSON schema v1 | **Implemented (Fortran, GUI)** — ADR 0013 + 0015 (+ 0016/0020/0021 additions, 0015 Phase 9 amendment); parser migrated to json-fortran (ADR 0006 update); Rust reader implemented (`rust/src/json.rs`, ADR 0022) |
| Reduced `Z_g` solver | Deferred optimisation (ADR 0003) |
| Rust port dependencies and conformance tolerance | **Decided** — [ADR 0022](adr/0022-rust-implementation.md): in-repo GK 7/15 and FFT ports, in-repo LU (not `faer`), series + Hankel Bessel subset; 1e-6 relative kept |
| GUI module | **Decided** — Python/PySide6/Qt3D, view-only v1 (ADR 0011) |
| Results JSON schema | **Frozen** — ADR 0012 (harmonic) and ADR 0015 (transient) |
| Quadrature tolerances | **Revisited 2026-10-01** (Phase 10 item 1, ADR 0024 §1): the 2-D oracle keeps the dissertation-era values (`errrel = min(la,lb)·10⁻⁶`, `maxint = 500`); the single-integral default takes `epsrel = 10⁻⁶` unscaled, `epsabs = 0`, `maxint = 500` |
| Binary results format | Deferred — stay on JSON/CSV (ADR 0012); HDF5 is the leading candidate vs ADR 0006's zero-dependency philosophy. Revisit once a real case is actually too large for JSON. |

---

## 7. Proposals from the open-source HEM comparison (July 2026)

Three companion open-source codes were inspected side by side with this
repository (see "Related open-source implementations" in
[references.md](references.md)): **TAGS** (pedrohnv, C99), **PRTL-mHEM**
(VitorLima1990, Python), **PRTL** (acslima, Wolfram/CDF).

Headline finding: TUPÃ's geometry-factor separation (theory.md §4.1, from
the 2003 dissertation) is the same optimisation published as **mHEM** by
Lima et al. [11] and used by TAGS/PRTL-mHEM — the core design is
independently validated in the literature. TAGS's closed-form self integral
is identical to theory.md's `g_self`, confirming the Phase 1 fix.

### P1 — mHEM single-integral kernel for `g(a,b)` (low effort) — Phase 10 item 1 — **done 2026-10-01**

Replace the default 2-D Gauss–Kronrod evaluation with the 1-D form of
theory.md §4.2 (inner integral in closed form). Same quantity, cheaper and
better conditioned for close segments; keep the 2-D path as the test
oracle. Unblocks larger `common/` grid cases (§5).

*Outcome:* 8× faster, 7.9e-8 from the 2-D path; the larger grid is
`common/portelaMesh.json` (ADR 0024 §1).

### P2 — Frequency-dependent image reflection coefficient (low effort) — Phase 10 item 2 — **done 2026-10-01**

`Γ_t(ω) = (W_soil − jωε₀)/(W_soil + jωε₀)`, `Γ_ℓ = 1` (theory.md §5) as
the default for buried conductors. **The original Matlab already does this
as its default mode** (ADR 0017 finding 1) — this restores reference
behaviour, it does not extend it. The ideal ±1 table remains as the
low-frequency limit and test pin. Grcev-grid MHz-range behaviour needs it.

*Outcome:* implemented with Γ on both image parcels (Matlab and PRTL-mHEM;
TAGS keeps Γ_ℓ = 1, a ≤ 0.2 % difference below 1 MHz on the Phase 10
comparison cases); ideal images selectable (ADR 0024 §2).

### P3 — Cross-code validation against TAGS (medium effort) — Phase 10 item 5 — **done 2026-10-01**

Run the Phase 2 buried conductor (later the Grcev grid) through both codes
and compare input impedance over the sweep — the executable oracle the
0.1.0 bar needs (§4). Compare physical outputs only (conventions differ
internally — theory.md §9.6, ADR 0017 finding 5).

*Outcome:* [validation/tags-xval.md](validation/tags-xval.md).

### P4 — Numerical Laplace Transform for the time domain (medium effort) — Phase 9 item 5 — **done 2026-09-30**

NLT (`s = c + jω`, damping `c ≈ ln(N²)/T`, window filter — Gómez & Uribe
[17]) as TAGS and PRTL use. Physics kernels untouched (already complex);
only the sweep driver and inverse transform change. Plain FFT is the
`c = 0` special case and remains for tests.

### P5 — Concretise the dispersive-soil subtypes — remainder in Phase 12 item 4

`tVisacroAlipioSoil` per Alipio & Visacro 2014 [14]: **done (2026-07-16,
mean parameter set)** — see ADR 0007's "Exercised" note and
`test_material.f90`; the JSON `soil.type` selector reaches all three soil
models. Open: *relatively conservative*/*conservative* parameter sets and
`tLongmireSmithSoil` (Longmire & Smith [15] per Cavka et al. [16]).

### P6 — Parallelise over frequencies, not the matrix fill — Phase 10 item 4 — **done 2026-10-01**

TAGS multithreads the frequency loop and pins BLAS to one thread;
frequencies are embarrassingly parallel and TUPÃ's loop has the same shape
once geometry factors are cached. Measure both, but expect this to
supersede the fill-loop OpenMP pencilled in Phase 3 item 4
([CONVENTIONS.md](CONVENTIONS.md) records the current default).

*Outcome:* both measured; the LU is ≈ 100 % of a frequency, the fill-loop
OpenMP is superseded and the frequency loop is threaded (ADR 0024 §4).

### P7 — Field/potential post-processing (new feature) — Phase 11

Scalar potential, electric field and path voltages at arbitrary points
from the solved `I_t`/`I_ℓ` (touch and step voltages, GPR profiles), as
TAGS and the Matlab reference both provide — use the Matlab output-class
inventory (ADR 0017 finding 6) to prioritise `tResult` subtypes. Scheduled
together with GPR, touch and step voltage as Phase 11.

### P8 — Criteria-based segmentation defaults (low effort) — Phase 10 item 3 — **done 2026-10-01**

Schroeder, Moura & Machado [19]: segments up to ~1000·r₀ stay within
engineering accuracy — far coarser than 10·r₀, >30× faster. Keep λ/10 as
the default `assemble` bound (theory.md §4.1) but expose the
segment-length target per study, validated by a coarse-vs-fine convergence
test.

*Outcome:* `numerics.maxSegmentLength` (ADR 0024 §3).

### Explicitly *not* proposed

- TAGS's symmetric `(u, I_ℓ, I_t)` block system — equivalent to the ported
  `(u, i₁, i₂)` form; consistency-check option only (the Matlab carries it
  as "método 5", ADR 0017 finding 4).
- The PRTL transmission-line performance chain — out of scope for the
  grounding-solver milestone; revisit after Phase 8.
- Complex images (Kuhar et al. [20]) — only relevant above a few MHz and
  only after P2 is in and validated (theory.md §5/§10.1).
- Time-domain HEM (HEM-TD; origin [47], refined in [21]) — pays off only for
  nonlinear phenomena, which TUPÃ excludes by design (theory.md §8); the
  direct TL+FDTD time-domain route [48] confirms the same trade-off.
- Rational-model/FDNE export for EMT programs [26, 27] — a future *output
  format*, not solver work; revisit when users ask for EMT integration.
  The cheapest first step would write the port impedance matrix as
  Touchstone Z-parameters [71]. External vector-fitting tools [69,70]
  could then do the fitting and passivity enforcement, with nothing
  in-repo.

---

## 8. Findings from re-inspecting the legacy implementations (July 2026)

Moved to [ADR 0017](adr/0017-legacy-reinspection-findings.md) (finding
numbers preserved): Γ(ω) images are original Matlab behaviour (1); self
geometry factor bug lineage (2); C++-only call-site bug (3); all three
solver layouts in the Matlab (4); convention mixing gates cross-validation
(5); feature inventory (6); legacy input formats (7).

---

## 9. Author-interview decisions (2026-07-05)

Moved to [ADR 0018](adr/0018-author-interview-decisions-2026-07.md):
scope, project role, convention authority, sources, tolerances,
stable/fluid modules, public contract, compilers, no-hosted-CI, release
process (0.1.0 bar), SLATEC, benchmarks, validation-data status,
precision, error handling.
