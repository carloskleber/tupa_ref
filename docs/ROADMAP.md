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
| Impedance fill | ADR 0009 interface: `calcZSelf`/`calcZMutual` apply all theory factors internally — image parcels carry the frequency-dependent `Γ(ω)` by default since Phase 10 (ideal ±1 selectable); solid-conductor Bessel internal impedance (native `mBessel` since 2026-10-29, ADR 0031) or a declared cross-section entry (`crossSections`, ADR 0030); embedded `lumped` R-L-C in a `line` segment (Phase 10e) |
| Solver | Augmented `Zeq` assembly + `ZGESV` (ADR 0003); multi-RHS variant for superposition (ADR 0016); frequencies solved in parallel on thread-private meshes, bit-identical to serial (Phase 10 item 4) |
| Sources | Current injections at named nodes (ADR 0010); ideal voltage sources via unit-injection superposition, mixable with current sources (ADR 0016); several simultaneous transient injections, each with its own waveform (Phase 9 item 4) |
| Materials | `tLinear`, `tPortelaSoil` (ADR 0007), `tVisacroAlipioSoil` (mean set, theory.md §7); air hardcoded to vacuum (ADR 0019); `tCrossSection` (`poleResidue` / `table` internal-impedance entries, ADR 0030, Fortran only) |
| Elements | `line`, `catenary`, `mesh` (ADR 0020), `channel` (ADR 0025), `branch` (ADR 0028), `plane` wire-grid rectangle (Phase 10e) |
| Sweep & results | `runSweep` (OpenMP over frequencies) + `tResult` storage, `inputImpedance`/`maxVoltageMagnitude`; CSV/JSON writers (ADR 0012) with `outputs` filtering; surface potentials, GPR, touch and step voltage from the solved currents (`observation` block, `mPotentials`, `<case>_potentials.*`, Phase 11, ADR 0027) |
| Time domain | `mSignal` (Heidler — legacy 6-term [38] and standard parametrised form [37, 39]; double-exp ± Jones; Portela concave front; switched-on sine), tail taper, in-repo FFT (ADR 0014), transfer-function transient driver (`mTransient`) with opt-in scan-fed (pchip-interpolated) transfer function, half-Hann window (spectral or time placement), multiple injections and Numerical Laplace Transform (Phase 9, ADR 0015 amendment 2026-09-30) |
| JSON I/O | json-fortran parser (ADR 0006, superseded-in-place 2026-08-01); schema v1: structure + `sources`/`frequencies`/`outputs` (ADR 0013) + `signal` (ADR 0015) + voltage sources/Heidler terms (ADR 0016/0015 amendment) + `mesh` composite element (ADR 0020) + optional `signal.antialiasStart` (ADR 0021) + optional `observation` block (ADR 0027) + `catenary` element and `portela` waveform (ADR 0023) + `signal.sources`/`window`/`transferFunction`/`transform`/`nltDamping` and the `sine` waveform (ADR 0015 amendment 2026-09-30); the optional `numerics` block (`kernel`, `imageModel`, `maxSegmentLength`, `crossMedium` — ADR 0029) and optional `segments` (ADR 0024); pre-run reference validation (`validateStudyReferences`) and CLI verbosity levels |
| Cases & tests | `common/` regression fixtures (golden; four harmonic — `portela1997`, `rod`, `grid`, `portela1997_ideal` — five transient since Phase 9, and the 32 × 32 m `portelaMesh` harmonic + scan-fed transient since Phase 10; **all regenerated once for Phase 10**; `grid_safety` harmonic + observation since Phase 11) plus ten cases imported from the legacy Matlab case library (`linha*.json`, `torre*.json`, ADR 0023), 19 test programs under `fpm test --profile release` — all green except `test_common_cases` on the `channel_loaded` fixture, which predates the case's later change to 2048 samples and fails in Fortran and Rust alike until regenerated (re-run 2026-10-01, gfortran 13; `test_impedance`'s coincident-segment divergence check is defeated by `-ffast-math`, as before). Plain `fpm test` needs `-ffree-line-length-none` and, on shared LAPACK, `--no-as-needed -llapack -lblas` (fortran/README.md) |
| Validation | [`docs/validation/`](validation/README.md): digitized published-curve comparisons — Grcev et al. 2018 Fig. 12 (6 cases), Lima et al. 2020 Figs. 6/7, Poljak & Doric 2006 Fig. 4, Silva et al. 2025 Figs. 3/4 (harmonic + transient) — accepted as the release-bar oracle (§4) |; since Phase 10 also the cross-code check against TAGS ([`validation/tags-xval.md`](validation/tags-xval.md): ≤ 0.3 % below 1 MHz on six cases) and the effect of the `Γ(ω)` default on every comparison ([`phase10-image-model.md`](validation/phase10-image-model.md))
| GUI | Python/PySide6 view-only module (`gui/`, ADR 0011): study tree, 3-D view, results/transient plots |
| Other implementations | **Julia port (`julia/`, contributed 2026-09-29, realigned 2026-09-30, Phase 8J)** — module-by-module mirror of the Fortran code, golden fixtures met at 1e-6 and every runnable `common/` case within 1e-6 of Rust/Fortran (bar two round-off rows Rust shares); **Rust port (`rust/`, 2026-09-30, [ADR 0022](adr/0022-rust-implementation.md))** — harmonic conformance met on the three golden fixtures at 1e-6, transient path implemented, Phase 8 item 1 (Fortran fixture widening) still open; **Phase 10 update 2026-10-01: Rust implements the whole phase and matches every regenerated fixture (worst 1.3e-10 harmonic, 1.5e-8 NLT); Julia ported items 1–3 and 6 the same day** (single-integral kernel, Γ(ω) images, `numerics` block; the five harmonic fixtures it can run match at 1e-6, worst 2.4e-10; the threaded sweep stays unported) — it still lags on Phase 9 and Phase 10b |

History (what was done, decisions, lessons) lives in the
[engineering log](engineering_log.md); this roadmap keeps the state and the
**future plan**. The early gap analysis (nine gaps against the legacy pipeline)
is closed — see log entries P0–P3 and lessons L-08.

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
item 4"). A phase that is finished is a single line pointing to its entry in
the [engineering log](engineering_log.md), which keeps the original item
numbers; only open work is detailed here.

### Closed phases

| Phase | Subject | Closed | Log entry |
| --- | --- | --- | --- |
| 0 | Convention audit and cleanup | 2026-07 | P0 |
| 1 | Geometry layer | 2026-07 | P1 |
| 2 | End-to-end single-frequency solve | 2026-07 | P2 |
| 3 | Frequency sweep, results, output | 2026-07 | P3 |
| 4 | Dispersive soil | 2026-07 | P4 |
| 5 | JSON I/O and common cases | 2026-07 | P5 |
| 6 | Sources and time domain | 2026-07 | P6 |
| 7 | Sources, signals and composite elements | 2026-09-30 | P7 |
| 9 | Transient pipeline completion | 2026-09-30 | P9 |
| 10 | Reference kernel, image model, performance (v0.6.0) | 2026-10-01 | P10 |
| 10c | Independent transient signals | 2026-10-01 | P10c |
| 12.3 | Series RLC (= `branch`, pulled into 10d) | 2026-10-12 | P10d |
| 13.1 | `catenary` | 2026-09-30 | P13 item 1 |

### Phase 8 — Rust implementation — **items 2–8 done; items 1 and 9 open**

Delivered and decided: log entry P8 ([ADR 0022](adr/0022-rust-implementation.md)).

1. **Contract freeze and fixture widening** (Fortran side) — **S–M**,
   **open**. The Phase 9 and 10 fixtures widened the set (five transient
   fixtures, `portela1997_ideal`, `portelaMesh*`, `grid_safety*`, `channel_*`),
   and `compareTransientCase` exists. Still to do: `portela1997_transient`
   itself; a small voltage-source case; a single-cell `mesh` case; reduced
   `portela` and `alipio-visacro` soil cases; `rod_air` once its air-side
   physics is signed off; the Fortran-side structure dump; **cut the
   conformance tag** (proposed v0.6.0, now on the Phase 10 fixture set).
   This closes Milestone 8b and gives the Julia port a sharper target.
   Write the ADR. Regenerate the stale `channel_loaded` fixture (log L-13).
9. **Cross-implementation report** — **S**, **partial**. `docs/validation/
   fortran-vs-rust.md`: every runnable `common/` case, Rust vs Fortran, plus the
   three-way comparison on the Grcev ℓ = 10 m cases and `portela1997_transient`
   (extending [validation/tupa-vs-mhem.md](validation/tupa-vs-mhem.md)) and a
   wall-time table; the GUI opens Rust results unchanged. Rust vs Julia/mHEM on
   Grcev ℓ = 10 m already matches the published Fortran numbers
   (`rust/README.md`).
10. **Follow-along rule** (policy): every later phase that changes the contract
    carries a Rust item (ADR 0002 order: theory → schema/case →
    implementations); a phase closes when Rust passes its fixtures or the lag is
    recorded in the conformance table in `rust/README.md`. **Current lag:**
    `branch`, `ring`, `plane`, `lumped`, the Marx waveforms, the `channel` cross
    medium switch, `crossSections` (Rust parity).

**Risks.** Adaptive-quadrature branching flips under a different summation
order (investigate any case outside 1e-6 before touching the tolerance; a
change needs an ADR, L-10); hidden loader behaviour (hence the assembly dump);
Fortran moving target (the frozen tag).

**Exit criteria.** `cargo test --release` green on every golden fixture at the
conformance tag and on the ported unit pins; item 9 report published;
README.md, common/README.md and ARCHITECTURE.md describe two conforming
implementations.

### Phase 8J — Julia port — **items 1–4 done; items 5–8 open**

Delivered and decided: log entry P8J. Layout: [julia/README.md](../julia/README.md).

5. **Validation writeup refresh** — **S**, open. The Julia figures in
   [validation/tupa-vs-mhem.md](validation/tupa-vs-mhem.md) and
   `validation/julia-grcev-l10-results.csv` were measured with the prototype
   (fixed 64×64 midpoint rule). Re-run `docs/validation/run_all.sh`,
   regenerate the metrics, restate the Fortran–Julia row (now expected
   ≲ 1e-6) and the transient paragraph.
6. **Transient and widened-fixture conformance (Milestone 8J-b)** — **S**,
   blocked on Phase 8 item 1. Add the transient, voltage-source, `mesh` and
   dispersive-soil fixtures to `julia/test/conformance.jl` (its "every golden
   fixture has a test" guard fails until then) and to the README table.
7. **Three-way report** — **S**, with Phase 8 item 9: add the Julia column to
   `fortran-vs-rust.md` and run `benchmarks/cross-impl/bench.py` with all three.
8. **GUI check** — **S**: open Julia sweep and transient result files in the GUI
   (they are byte-compatible with the Rust files; a confirmation).
9. **Follow-along rule** — as Phase 8 item 10. **Current lag:** Phase 9
   (transfer-function/NLT transients; `PHASE9_LAG`), 10b, 10d, 10e, 11,
   12 item 1, 14. The loader rejects unsupported fields with an explicit error.

Optional: multi-threading over frequencies (`Threads.@threads`, one BLAS thread
per task), registering the package or a `juliac` binary, a Documenter.jl page.

**Exit criteria.** `Pkg.test()` green on every golden fixture at the
conformance tag; item 5 refreshed; item 7 report including Julia.

### Phase 9 — open remainders

Delivered: log entry P9. Left open: voltage sources in the transient driver and
the multi-RHS per-frequency solve (current injections only today); the single
exponential and impulse/step waveforms, ported when a case needs them.

### Phase 10 — open remainders

Delivered: log entry P10. Left open: a TAGS **time-domain** comparison, which is
the precondition for revisiting the NLT default (not flipped, ADR 0024 §7); the
mesh segment target undershoots in all three implementations (ADR 0024 §8).

### Phase 10b — Lightning channel — open remainder

Delivered: log entry P10b. **Open:** the Ishii reduced-scale induced-voltage
case as reproduced by Pokharel et al. (all-air geometry) — the measured waveform
exists here only as a description, so it needs a digitised figure; the Julia
port. Out of scope: field vectors from the channel (Phase 11 remainder),
induced voltages over lossy ground (Phase 10d), the HEM option of an enlarged
transversal radius (theory.md §4.5, optional).

### Phase 10d — Induced voltages — open remainder

Delivered: log entry P10d ([ADR 0028](adr/0028-induced-voltages-and-lumped-branch.md)).

4. **Open remainder**: a total-voltage option for harmonic output and for
   incident sources other than the loaded channel; the lossy-ground
   horizontal-field correction (Norton / Cooray–Rubinstein, Phase 14 item 2
   territory — Cooray–Rubinstein is the first target, Tossani et al. [89]); a
   branch-current output; the Rust and Julia ports.
5. **Candidates, unscheduled** (2026-10-15 library review, theory.md §4.6.3,
   §9 item 9): a measured anchor from field or scale-model data (Rubinstein
   [83], Barker [84], Piantini [93]); conductive coupling through the line's
   grounding against Napolitano [92] (needs buried electrodes but no Phase 14
   work); an inclined-channel case against Natsui [91]; a sagged-line
   induced-voltage case against Stracqualursi [96].

### Phase 10e — Impulse laboratory — open remainder

Delivered: log entry P10e ([marx_generator.md](marx_generator.md)).

3. **Branch current in the transient driver** — API only; expose it in JSON and
   the writers (also the Phase 10d item 4 remainder).
- A charged embedded capacitor (series EMF in a `lumped` segment).
- Golden fixtures for the laboratory cases (`marx_lab_2MV*`, `labuat_impulse*`),
  once the near-term items below settle the LabUAT amplitudes.
- Rust and Julia follow-along: their loaders refuse `plane`, `lumped` and the
  new waveforms.

### Phase 11 — Grounding-safety outputs — open remainder

Delivered: log entry P11 ([ADR 0027](adr/0027-observation-potentials-and-safety-outputs.md)).
**Open:** the electric field vector (`−∇ψ − jωA`) and path voltages along an
arbitrary route (the legacy `camposdif`/`calctensoes` line integral); transient
maps — the potential is linear in the solved currents, so a per-site transfer
function through the Phase 9 driver is the natural next step; showing the
potentials in the GUI; the Julia port.

### Phase 12 — Tower-footing electrode library

1. **`ring`** — done 2026-10-30 (log P12 item 1); open: Rust and Julia loaders.
2. **Tubular conductor** (metallic pipes, steel poles) — **S**.
   *Re-scoped 2026-10-29 (author): a tube is a **cross-section entry**
   ([ADR 0030](adr/0030-cross-section-internal-impedance.md)), not a new
   element and not a wall-thickness field on `tLine`.* A pipe or pole is a
   `line`/`catenary` that names a tube entry in `crossSection`.
   - **Entry.** A `poleResidue` fit of the Schelkunoff external-return
     formula (theory.md §4.3 [40]) with `equivalentRadius = r₀`. The
     internal impedance of a tube is a diffusion function, so the v0
     real-pole Foster form is enough. Multi-sided (8/12/16-sided) poles use
     the logarithmic capacity of the polygon as `r_eq` and a 2-D solve for
     `Z_int` (ADR 0030 §1).
   - **Magnetic steel.** Linear, with a constant μr per entry (ADR 0030 §3
     forbids rescaling across μr). Entries come as a **μr family** (for
     example μr = 1, 50, 200, 500, 1000), so a study can show the
     sensitivity band. Saturation under lightning current is outside the
     linear frequency-domain solver. It is stated as a limitation, and the
     family brackets it.
   - **Public scope.** `common/cross_sections/` ships a few example tubes
     (one steel pole section across the μr family, one thin-wall pipe) as
     fixtures, and a test against the closed form. The engineering pole
     series is not part of the reference.
   - **Code.** `mBessel` gains `K₀/K₁` (ADR 0031 open point 4). They are
     needed for the test oracle (fit vs Schelkunoff), not on the solve path.
   - **Tapered poles.** A chain of `line` elements, each with its own entry.
     The short-segment check of ADR 0030 §6 applies: a Ø 600 mm section needs
     segments ≥ 1.2 m.
   - **Not covered.** Current *inside* the tube (a down-lead run inside a
     hollow pole) breaks the external-return assumption. That is the
     shielded case of Phase 13 item 4: the pole acts as the armour and the
     down-lead as the inner conductor.
3. **Series RLC element** — done as Phase 10d item 1.
4. **Remaining dispersive-soil parameter sets** (§7 P5) — **S–M**.
   *Relatively conservative*/*conservative* Alipio–Visacro sets and
   `tLongmireSmithSoil` (Longmire & Smith [15] per Cavka et al. [16]).


### Phase 13 — Line and substation tier: conductors and cables

Application-tier features (ADR 0018). Item 1 (`catenary`) is closed (log).

2. **Generic internal impedance models (OPGW)** — Fortran done 2026-10-29
   (log P13 item 2, [ADR 0030](adr/0030-cross-section-internal-impedance.md)).
   **Open:** Rust and Julia parity; an NLT transient comparison test; complex
   pole pairs, for stranded and steel-cored conductors with resonances; the
   **`matrix` kind** for multi-conductor sections (item 4); the **`sheet` kind**
   (item 5); tube entries (Phase 12 item 2). Where no measured data exists,
   conductor subdivision (de Arizon & Dommel [73]) can compute R(f), X(f) tables
   offline; the legacy ACSR spreadsheet is candidate seed data and the C++
   bundle/L-profile models a possible catalogue kind.
3. **Coaxial encasement layer — insulated, concrete-encased and
   semiconducting coatings** — **M–L**. *Fortran done 2026-11-05 for `line`
   and `catenary` (theory.md §4.3, `test_coating`: validation items 1, 2 and
   the capacitive limit of 3). **Open:** `mesh` bars, Rust and Julia parity,
   a comparison with Sunde's propagation constant (item 3 proper) and the
   measured-benefit trend (item 4); the concrete-foundation
   equivalent-cylinder helper.* *Widened 2026-10-29 (author):
   one mechanism covers an insulated wire, a steel bar inside concrete
   (rebar, foundation cage, Ufer electrode) and a semiconducting layer.
   It is staged as a thin coaxial layer here, and a finite volume later
   (Phase 15 item 2).* The legacy insulated-cable branch is an acknowledged
   placeholder (it drops soil conduction and ignores the coating; flagged
   TODO in the legacy code), so do **not** port it.
   - **Model.** A layer of outer radius `r_c` and medium (σ_c, ε_c) around a
     conductor of radius `r₀` (or `r_eq`). Its radial admittance per metre,
     `y_c = 2π(σ_c + sε_c)/ln(r_c/r₀)`, sits **in series** with the leakage
     path (Sunde's coated buried wire [41], theory.md §4.3). The leakage
     current of a segment crosses the layer before it enters the medium.
     - Only the self term changes: `Z_t(a,a) += 1/(y_c·l)`, normalised as
       in theory.md §6. This needs checking against the `Z_t` scaling there
       before coding.
     - The external transversal terms use `r_c`, because the leakage
       emerges at the layer surface. `Z_ℓ` keeps `r₀`: the layer is μ₀ and
       the current stays in the conductor.
     - Mutual terms are unchanged. That is the thin-layer assumption
       (`r_c ≪ l`, and `r_c` ≪ the distance to other segments).
   - **Material.** Any `tMaterial` with an admittance, so a dispersive
     model is accepted for concrete as for soil. The Laplace path follows.
   - **Schema** (proposed). An element field
     `coating: { "outerRadius": …, "material": "<id>" }` on `line`,
     `catenary` and `mesh` bars. A concrete foundation with a rebar cage
     takes the cage as an equivalent cylinder, `r_eq = (n·r·R^{n−1})^{1/n}`
     (n bars of radius r on a circle of radius R, the bundle GMR), inside a
     layer of the foundation radius.
   - **Parameter ranges.**
     - Insulation: σ_c ≈ 0, ε_r 2.3–4. Purely capacitive.
     - Concrete: σ_c roughly 10⁻³–5·10⁻² S/m depending on moisture, ε_r
       about 5–15, dispersive. Concrete is more conductive than resistive
       soil, which is the reason encasement lowers footing resistance
       (Ufer [115], Fagan & Lee [116], Wiener [122]).
     - Semiconducting screens and ground-enhancing compounds [110, 117]
       fall in between.
   - **Validity.** Thin layer only. Warn when `r_c > l/4`, alongside the
     ADR 0030 short-segment check. Pad and pile footings of 0.5–1.5 m
     radius violate the assumption and belong to Phase 15 item 2.
   - **Validation.**
     1. Identity: a layer with the surrounding medium's own (σ, ε) must
        reproduce the bare conductor, to O((r_c/l)²).
     2. DC: an encased rod against the two-shell closed form
        `ln(r_c/r₀)/(2πσ_c L) + R_rod(r_c, σ_soil)`.
     3. Insulated wire: Sunde's propagation constant for a coated buried
        wire [41].
     4. Trend check against the measured encasement benefit [115, 116, 122].
   - **Reuse.** The same layer forms the jacket on a cable armour (item 4).
4. **Insulated and multipolar cables (armoured)** — **L**. *Elaborated
   2026-10-29 (author).* Target types, all three in scope: single-core
   with a metallic screen; three screened cores in a common steel-wire
   armour (MV land and submarine); LV/control multicore cables with an
   overall screen or armour (substation EMC, induced voltages). An
   insulated wire with no metallic layer is item 3 alone.
   - **Element.** A `cable` element follows a path (straight or catenary
     chain) and names a cross-section entry of the **`matrix` kind**. The
     entry lists the terminals (for example `core1…core3`, `screen1…screen3`,
     `armour`) and carries:
     - (a) the outer metallic layer's `r_eq` and outer-surface `Z_int`,
       with external return (Schelkunoff tube [40]; helical steel-wire
       armour as an equivalent tube in a μr family [111]);
     - (b) the interior series matrix `Z_in(s)` (n×n, conductors referenced
       to the armour, loop formulation [43, 109]), as a common-pole
       pole–residue matrix;
     - (c) the interior shunt matrix `Y_in(s) = G + sC` (constant in v0;
       semiconducting screens later [110]);
     - (d) the armour **transfer impedance** `z_T(s)`, the mutual surface
       impedance between the armour's outer and inner faces [40, 114].
   - **Exterior.** Only the outermost metallic layer couples
     electromagnetically. The armour is an **ordinary HEM electrode**
     carrying the cable's net current `I_net` (radius `r_eq`, internal
     impedance from (a), jacket as an item 3 coating). No geometry factor
     involves the interior conductors.
   - **Interior, per segment and co-located** (decided). Each armour
     segment of length `l` carries an internal π-section with the internal
     loop currents `I_k` (conductor k out, armour inner face back):

         ΔV_net = (Z_HEM + l·z_ao)·I_net − l·z_T·Σ_k I_k
         ΔV_k   = l·Σ_j Z_in,kj·I_j − l·z_T·I_net          (v_k = u_k − u_armour)

     with `l·Y_in/2` at each end node between the internal potentials
     `v_k`. The coupling is symmetric, so reciprocity holds. Signs and
     loop-to-conductor transforms are to be fixed against Ametani [43]
     before coding. For a thick steel armour `z_T → 0` at high frequency,
     and the interior decouples (shielding). At low frequency it does not,
     and that is the case for induced voltages on control cables.
   - **Unknowns.** Per cable node, n internal potentials. Per cable
     segment, n internal currents. They are non-electromagnetic unknowns
     stamped like the `branch` element (ADR 0028), so the interior adds no
     fill cost, only LU size.
   - **Ports.** At each cable end and at declared joints, a `ports` map
     ties every terminal to a structure node: an electrode node (tower or
     substation grounding), a `branch` node (linear terminations, surge
     capacitances), or the next cable element. An unconnected terminal
     floats. Screen bonding (solid, single-point) is a mapping, and
     cross-bonding is a permuted mapping at a joint. Absolute terminal
     potential is `u_armour + v_k`. The cable's (n+1)-port matrix at its
     ends is an output, exportable as Touchstone Z-parameters (§7, EMT
     note).
   - **Entry sources.** Coaxial layers are closed-form [43, 40, 109].
     Eccentric cores and proximity effects need a numerical 2-D solve:
     MoM-SO [107, 108], FEM [113], or subdivision [73]. Pipe-type or
     armour formulations follow [43]. Fits use matrix vector fitting with
     a matrix passivity check (eigenvalues of `Re Z_in ≥ 0`). The `matrix`
     kind amends ADR 0030 (new ADR).
   - **Stages.**
     - 4a: single-core screened, analytic entries. The oracle is the
       exact-TL chain of the same `Z_in`, `Y_in`, and Ametani/EMTP cable
       constants [43, 112].
     - 4b: three-core armoured, numerical matrix entry, armour μr family
       [111].
     - 4c: LV/control multicore, where exterior coupling through `z_T`
       dominates.
     - Each stage gets one `common/` case with a golden fixture.
   - **Validation.**
     1. Degenerate: an unexcited interior with `z_T = 0` reproduces a
        coated conductor of the armour radius.
     2. Per-segment vs exact-TL interior on 4a, converging with
        segmentation.
     3. Screen voltage and current of a buried single-core cable against
        published dispersive-soil cable transients.
     4. Reciprocity and passivity over the sweep.
   - **Not in scope here.** An exact-TL fast path for the interior, which
     is a performance kernel. PRTL-mHEM's tubular bundles are a partial
     analogue.
5. **Sheet cross-section for the `plane` element (steel walls)** — **M**.
   *Added 2026-10-29 (author).* The `plane` element (Phase 10e) gives every
   bar a round-wire radius. For a metal wall, each bar stands for a strip of
   width `d` of a **continuous** sheet, where `d` is the cell size across
   the bar. An isolated-strip entry would add edge crowding, which a
   continuous sheet does not have.
   - **Entry.** A new `sheet` kind stores the per-square surface impedance
     of a plate of thickness `t`, σ and μr:
     `Z_s(s) = (k/2σ)·coth(k t/2)`, `k = √(sμσ)`. That is field on both
     faces: the DC limit is `1/(σt)`, and at high frequency the two faces
     act in parallel. A one-sided variant covers the interior of a closed
     enclosure.
   - **Use.** A plane bar gets `Z_int' = Z_s/d`. Non-square cells give the
     u and v bars different `d`. The entry is a diffusion function, so it
     fits in the v0 real-pole form, or it can be evaluated analytically.
     Choosing between the two is open.
   - **Magnetic steel.** A μr family, as for the tubes (Phase 12 item 2).
   - **Bar radius (open).** Ludwig's equal-area rule `a = d/2π` [119] or
     the strip conformal radius `d/4` (ADR 0030 §1). Settle it against a
     solid-plate 3-D reference (BEM/FEM) on driving-point impedance and
     shielding. ADR 0030's `Z_int` definition must be restated for the
     sheet: a per-square impedance with no GMD correction.
   - **Validation.** The plate surface impedance against the closed form,
     the plate against the reference solve, and `marx_lab_2MV` rerun with
     steel-sheet walls to measure the difference from bare wire-grid walls
     [118, 119].


### Phase 14 — Air–soil coupling — Fortran done 2026-10-19 (log P14)

**Open:** Rust and Julia ports (their loaders refuse `numerics.crossMedium`);
flipping the default after review; a published reference to validate against
(the checks so far are analytic limits and continuity, not literature curves);
lossy-ground induced voltages and LEMP.

### Phase 15 — Multi-layer soil

1. **Multi-layer soil and reflection-coefficient images** — **L**. Lifts
   the §5 single-interface premise; route: layered-earth Green's
   functions via quasi-static complex images (Li, Chen & Wang [33]; Sunde
   [41] background). Explicitly out of MVP scope (theory.md §5, §10.1).
   Time-domain and full-wave counterparts for cross-checking: Fan & Li [103]
   and Nazari et al. [104].
2. **Finite conductive and dielectric volumes (concrete footings,
   backfill)** — **L**, research. *Added 2026-10-29 (author), stage 2 of
   Phase 13 item 3.* A concrete pad or pile is a bounded region of a third
   medium, not a thin coaxial layer. Candidate routes:
   - (a) a volume integral equation, with the conduction and polarisation
     currents of the inclusion as extra unknowns;
   - (b) equivalent sources or images for canonical shapes (a cylinder,
     a hemisphere at the surface);
   - (c) coupling to an external FEM solve through a port admittance.

   Validate against FEM on encased-rod and pile cases, and require the
   thin-layer model (Phase 13 item 3) as the limit of a slender inclusion.
   Shares interface machinery with item 1 and with the faceted-terrain idea
   (layered and inhomogeneous media).


### Maintenance M1 — Remove SLATEC — mostly done (log M1)

**Open:** step 8 (per-conductor `Z_int` cache), step 10 (the decision on the
`|ρ| > 500 → ratio = 1` cut-off, 1e-3 relative error) and the 40× parity run.
`K₀/K₁` follow with Phase 12 item 2.

### Near-term work (2026-11)

Raised by the LabUAT investigation (log INV-2026-11-02):

1. **Tame the NLT record end in the LabUAT cases** — *done 2026-11-03 for the solver (ADR 0032, `signal.riseTime` + `nltDamping` at c·T ≈ 6; `labuat_impulse_noarc` 4096 points, 4 min, finite and monotone to the last sample, peak 4.64 MV); still open: usable-fraction field/warning, GUI marker, Rust/Julia port, golden fixtures.* Original item: The 1.6e10 V peaks are an
   end-of-window artefact (about the last 25 % of the record; amplitude ∝ N²;
   the first ~60 % is physical, peak ≈ 4.8 MV — log INV-2026-11-02). Try the
   spectral Hann window (`signal.window`), a smaller `nltDamping` and a longer
   window cropped to its usable part; state the usable fraction in
   `marx_generator.md` and in the results (a field or a warning), and consider a
   GUI marker for the unreliable tail. Then add golden fixtures for the
   laboratory cases (Phase 10e).
2. **Audit the NaN-dependent code.** `ieee_is_nan` alone did not rescue the
   closed-form fallback under `-ffinite-math-only`; find every NaN/Inf-based
   guard (`Geometry.f90`, `Bessel.f90`, `Channel*`) and make it independent of
   the flag, or document why the flag is required. Add a regression test that
   runs under the release flags (the golden suite is run with `-O2`, L-02).
3. **ifx portability.** Replace `__has_include` in `Version.F90` (generate the
   file or use a plain include) so `fpm --compiler ifx` builds the repo as is.
4. **Make `build.sh` executable** (`chmod +x`) and decide whether the fast-math
   release path needs `-fno-finite-math-only` documented in DISTRIBUTION.md.

---

## 4. Milestones and next steps

Milestones met (8a, 8J-a, release bar, Phases 9–11 and the 10x series) are in
the log, "Milestones and releases". Open: **8b** (Rust full conformance — needs
Phase 8 item 1) and **8J-b** (Julia transient/widened fixtures).

**Next engineering steps:** the near-term list above; then Phase 8 item 1
(widened fixtures, structure dump, conformance tag) and item 9 (report); Phase 8J
follow-along (Julia Phase 9, then 10b, then 11); Phase 12 items 2 and 4 with
transient potential maps as the Phase 11 remainder. Conductor and cable track
(2026-10-29, on top of ADR 0030): tube entries (Phase 12 item 2) → coaxial
encasement layer (Phase 13 item 3) → `sheet` kind for planes (Phase 13 item 5) →
the `matrix` kind and the `cable` element, stages 4a–4c (Phase 13 item 4) →
finite volumes (Phase 15 item 2, research).

Prioritisation rules (2026-09-30) that still apply: second implementations
before features; decided-and-small before open-ended; MVP before application
tier; dependencies (P1 before large grids, Γ(ω) before mixed-media coupling);
change the golden fixtures once; new theory and object-model work last.
Effort is rated S/M/L (S ≈ days, M ≈ a focused week-scale task, L = new theory
or object-model work).

---

## 5. Testing strategy

| Layer | Where | What |
| --- | --- | --- |
| Unit | `fortran/test/` (Rust: `rust/tests/`; Julia: `julia/test/`) | quadrature vs closed forms; sign/decay pins; Bessel `Z_int` vs tables; dispersion DC limit; waveforms |
| Integration | `fortran/test/` | end-to-end DC resistance; sweep/transient consistency; reciprocity, passivity; voltage-source superposition |
| Reference | `common/` | JSON in → CSV out, golden diff at 1e-6; shared across languages |
| Published curves | `docs/validation/` | digitized-figure comparisons (Grcev, Lima, Poljak, Silva) with regenerable plots + per-case xlsx data |
| Benchmarks | `benchmarks/` | TAGS and PRTL-mHEM; cross-code runs per [BENCHMARKS.md](BENCHMARKS.md) |

There is **no hosted CI** (ADR 0018): the gate is a local `fpm build && fpm test`
before merging (`rust/`: the cargo gate; `julia/`: `Pkg.test()`). Run the slow
suites under `--profile release`; validate solver changes also with
`-O2 -fopenmp` and, for numerics, a second compiler (log L-02, L-14). Cost of a
case is the per-frequency LU, ∝ (n_n + 2 n_s)³: a 200-electrode grid sweeps in
seconds, a 300-electrode legacy line with a full-FFT transient in minutes;
frequencies run in parallel under `-fopenmp`.

---

## 6. Open decisions

| Decision | Status |
| --- | --- |
| Reduced `Z_g` solver | Deferred optimisation (ADR 0003) |
| NLT as the transient default | Not flipped (ADR 0024 §7); revisit after a TAGS time-domain comparison |
| Binary results format | Deferred — stay on JSON/CSV (ADR 0012); HDF5 is the leading candidate vs ADR 0006's zero-dependency philosophy. Revisit once a real case is too large for JSON |
| `|ρ| > 500` Bessel cut-off | Open (M1 step 10) |
| Plane-bar radius (`d/2π` vs `d/4`) | Open (Phase 13 item 5) |
| Release flags and NaN semantics | `-fno-finite-math-only` kept; audit pending (near-term item 2) |

The resolved decisions of the former table (soil dispersion, voltage sources,
fill interface, FFT, JSON schema, Rust dependencies, GUI technology, results
schemas, quadrature tolerances) are in the log's decision register.

---

## 7. Proposals from the open-source HEM comparison — status

Code and ADRs cite these as "ROADMAP §7 Pn". The proposals came from comparing
TAGS (C99), PRTL-mHEM (Python) and PRTL (Wolfram) with this repository; the
headline finding was that TUPÃ's geometry-factor separation (theory.md §4.1) is
the optimisation published as mHEM [11], and TAGS's closed-form self integral
equals `g_self`.

| # | Proposal | Status | Where |
| --- | --- | --- | --- |
| P1 | mHEM single-integral kernel for `g(a,b)` | done 2026-10-01 | Phase 10 item 1, log P10 |
| P2 | Frequency-dependent image coefficient `Γ(ω)` | done 2026-10-01 | Phase 10 item 2, log P10 |
| P3 | Cross-code validation against TAGS | done 2026-10-01 | Phase 10 item 5, [tags-xval.md](validation/tags-xval.md) |
| P4 | Numerical Laplace Transform | done 2026-09-30 | Phase 9 item 5, log P9 |
| P5 | Dispersive-soil subtypes | mean set done 2026-07-16; remainder open | Phase 12 item 4 |
| P6 | Parallelise over frequencies | done 2026-10-01 | Phase 10 item 4, log P10 |
| P7 | Field/potential post-processing | scalar potential done | Phase 11, log P11 |
| P8 | Criteria-based segmentation | done 2026-10-01 | Phase 10 item 3, log P10 |

Explicitly *not* proposed:

- TAGS's symmetric `(u, I_ℓ, I_t)` block system — equivalent to the ported
  `(u, i₁, i₂)` form; a consistency-check option only (the Matlab carries it as
  "método 5", ADR 0017 finding 4).
- The PRTL transmission-line performance chain — out of scope for the
  grounding-solver milestone.
- Complex images (Kuhar et al. [20]) — only relevant above a few MHz (theory.md
  §5/§10.1).
- Time-domain HEM (HEM-TD; [47], [21]) — pays off only for nonlinear phenomena,
  which TUPÃ excludes by design (theory.md §8); the TL+FDTD route [48] confirms
  the same trade-off.
- Rational-model/FDNE export for EMT programs [26, 27] — a future *output
  format*, not solver work. The cheapest first step would write the port
  impedance matrix as Touchstone Z-parameters [71]; external vector-fitting
  tools [69, 70] could then do the fitting and passivity enforcement.

---

## 8. Legacy findings and 9. Author-interview decisions

Moved to [ADR 0017](adr/0017-legacy-reinspection-findings.md) and
[ADR 0018](adr/0018-author-interview-decisions-2026-07.md); the legacy element
inventory is in the log ("Legacy and project decisions").
