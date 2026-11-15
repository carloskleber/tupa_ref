# TUPÃ — Roadmap

Reference electromagnetic transient solver (HEM / Method of Moments).
Fortran implementation first; object model and test cases shared with the
Rust implementation (`rust/`) and the Julia port (`julia/`)
([ADR 0002](adr/0002-language-agnostic-object-model.md)).

This document holds two things: the **current state** (§1) and the **plan**
(§3 onward). It holds no history. What was done, when, with what evidence and
what was learned is in the [engineering log](engineering_log.md); why
something was decided is in the [ADRs](adr/); the model is in
[theory.md](theory.md). When an item here is finished, it is removed from
this file and recorded in the log.

**MVP scope**: tower-footing grounding under lightning. Full transmission
lines and substation grids are the follow-on application tier
([ADR 0018](adr/0018-author-interview-decisions-2026-07.md)). The project's
primary role is a **scientifically citable reference implementation**;
usability as an engineering tool is secondary. The original Matlab code is
the model reference of record
([ADR 0017](adr/0017-legacy-reinspection-findings.md)).

---

## 1. Current state

| Area | State |
| --- | --- |
| Object model | `tStudy → tStructure → tElement/tMaterial → tNode/tElectrode`, `tMesh`; FORD-documented |
| Geometry layer | mean and image distances, direct and image geometry factors (single-integral quadrature by default, nested 2-D path selectable as oracle, closed forms for self and parallel pairs), direction cosines, quadrature cache (theory.md §4.1–4.2) |
| Impedance fill | all theory factors applied inside `calcZSelf`/`calcZMutual` (ADR 0009); image terms with `Γ(ω)` by default, ideal ±1 selectable (ADR 0024); internal impedance from the solid-conductor Bessel formula (native `mBessel`, ADR 0031) or a declared cross-section entry (ADR 0030); `lumped` R-L-C in a `line` segment; `coating` layer on `line`/`catenary`; opt-in cross-medium coupling (ADR 0029) |
| Solver | augmented `Zeq` assembly + `ZGESV` (ADR 0003); multi-RHS for superposition (ADR 0016); frequencies in parallel, bit-identical to serial; quasi-stationary DC point (`frequencies.dc`) |
| Sources | current injections at named nodes (ADR 0010); two-node sources (ADR 0025); ideal voltage sources by superposition (ADR 0016); several transient injections or independent signals (ADR 0026) |
| Materials | `tLinear`, `tPortelaSoil` (ADR 0007), `tVisacroAlipioSoil` (mean set); air is vacuum (ADR 0019); `tCrossSection` entries (`poleResidue`, `table`) |
| Elements | `line`, `catenary`, `mesh` (ADR 0020), `channel` (ADR 0025), `branch` (ADR 0028), `plane` wire grid, `ring` |
| Sweep and results | `runSweep` + `tResult`; CSV/JSON writers with `outputs` filtering (ADR 0012); surface potentials, GPR, touch and step voltage (`observation`, ADR 0027) |
| Time domain | waveforms (Heidler, double exponential ± Jones, Portela, sine, step, `branchNorton`), `riseTime` smoothing (ADR 0032), tail taper, band-edge filter (ADR 0021), Hann window, per-bin or scan-fed transfer function, FFT (ADR 0014) or NLT (theory.md §8) |
| Input format | JSON schema v1, described in [common/README.md](../common/README.md); pre-run reference validation; CLI verbosity levels |
| Cases and tests | golden fixtures in `common/` (harmonic, transient, potentials) compared at 1e-6; cases imported from the legacy library (ADR 0023); the Fortran test programs run under `fpm test --profile release`. Known failures are listed in §3 "Maintenance" |
| Validation | [validation/](validation/README.md): digitised published curves (Grcev, Lima, Poljak, Silva), the TAGS cross-code check, the channel and induced-voltage anchors; status in [BENCHMARKS.md](BENCHMARKS.md) |
| GUI | Python/PySide6 view-only module (`gui/`, ADR 0011): study tree, 3-D view, results, transient and DC-profile plots |
| Rust | conforming on the golden fixtures through Phases 9–11 and 10b; lag list in Phase 8 item 10 and [rust/README.md](../rust/README.md) |
| Julia | conforming on the harmonic fixtures and the Phase 10 kernel; lag list in Phase 8J item 9 and [julia/README.md](../julia/README.md) |

Which language implements which feature is stated once, in the conformance
tables of [rust/README.md](../rust/README.md) and
[julia/README.md](../julia/README.md). "Fortran only" below means the other
loaders refuse the input with an explicit error (log L-12).

---

## 2. Guiding principles

- **Theory doc is normative** — code conforms to [theory.md](theory.md);
  papers are mapped through its §2 conventions (ADR 0008).
- **Correctness before performance** — every physics routine lands with a test
  against an analytical value or a published curve.
- **Reference quality** — prefer the simple, auditable path (full `Zeq` solve,
  ADR 0003) over optimisations until validated.
- **Language-agnostic contracts** — JSON schema + `common/` cases are the
  cross-implementation interface (ADR 0002, 0006).
- **One home per kind of information** — model in the theory, state and plan
  here, history and lessons in the log, decisions in the ADRs (log L-18).

---

## 3. Phases

Item numbers are stable — code comments cite them ("ROADMAP Phase 3
item 4"). A finished phase or item appears here only as a pointer to its
entry in the [engineering log](engineering_log.md), which keeps the original
item numbers. Only open work is detailed. Effort is rated S/M/L (S ≈ days,
M ≈ a focused week-scale task, L = new theory or object-model work).

### Closed phases and items

| Phase | Subject | Log entry |
| --- | --- | --- |
| 0 | Convention audit and cleanup | P0 |
| 1 | Geometry layer | P1 |
| 2 | End-to-end single-frequency solve | P2 |
| 3 | Frequency sweep, results, output | P3 |
| 4 | Dispersive soil | P4 |
| 5 | JSON I/O and common cases | P5 |
| 6 | Sources and time domain | P6 |
| 7 | Sources, signals and composite elements | P7 |
| 9 | Transient pipeline completion | P9 |
| 10 | Reference kernel, image model, performance | P10 |
| 10c | Independent transient signals | P10c |
| 12 item 1 | `ring` element (Fortran) | P12 item 1 |
| 12 item 3 | Series RLC (= `branch`) | P10d |
| 13 item 1 | `catenary` | P13 item 1 |
| 13 item 2 | Cross-section internal impedance (Fortran) | P13 item 2 |
| 13 item 3 | Coating layer on `line`/`catenary` (Fortran) | P13 item 3 |
| — | Quasi-stationary DC point (Fortran, GUI) | DC-2026-11-05 |
| — | NLT record end with step-like sources (Fortran) | INV-2026-11-03 |
| — | Flag-independent geometry guards, portable version include | M2 |

### Phase 8 — Rust implementation — **items 2–8 done; items 1 and 9 open**

Delivered and decided: log entry P8 ([ADR 0022](adr/0022-rust-implementation.md)).

1. **Contract freeze and fixture widening** (Fortran side) — **S–M**,
   **open**. The Phase 9 and 10 fixtures widened the set (five transient
   fixtures, `portela1997_ideal`, `portelaMesh*`, `grid_safety*`, `channel_*`),
   and `compareTransientCase` exists. Still to do: `portela1997_transient`
   itself; a small voltage-source case; a single-cell `mesh` case; reduced
   `portela` and `alipio-visacro` soil cases; `rod_air` once its air-side
   physics is signed off; the Fortran-side structure dump; **cut the
   conformance tag** (on the current fixture set).
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

Voltage sources in the transient driver and the multi-RHS per-frequency solve
(current injections only today); the single exponential and impulse
waveforms, ported when a case needs them.

### Phase 10 — open remainders

A TAGS **time-domain** comparison, which is the precondition for revisiting
the NLT default (ADR 0024 §7); the mesh segment target undershoots in all
three implementations (ADR 0024 §8).

### Phase 10b — Lightning channel — open remainder

The Ishii reduced-scale induced-voltage case as reproduced by Pokharel et al.
(all-air geometry): it needs the measured waveform as a digitised figure. The
Julia port. Optional: the HEM option of an enlarged transversal radius
(theory.md §4.5).

### Phase 10d — Induced voltages — open remainder

4. A total-voltage option for harmonic output and for incident sources other
   than the loaded channel; the lossy-ground horizontal-field correction
   (Cooray–Rubinstein first, Tossani et al. [89]; theory.md §4.6.4); a
   branch-current output; the Rust and Julia ports.
5. **Candidates, unscheduled** (theory.md §4.6.3, §9 item 9):
   - a measured anchor from field or scale-model data: Rubinstein [83],
     Barker [84], Piantini [93], or the Tampa line of Master et al.
     [129, 130] with the validations reviewed by Paolone et al. [133];
   - **a strike to a tall object beside the line** against Baba & Rakov
     [131]: same wire height and matched termination as the Nucci case, a
     100 m object, tabulated FDTD and Rusck values. Tower, channel and line
     are one system, so it needs no new code, only a case and a comparison
     script — **S**, the cheapest next anchor;
   - an inclined-channel case against Natsui [91] and the closed forms of
     Matsubara & Sekioka [132] (mind the factor 2 in their eq. (36),
     references.md);
   - conductive coupling through the line's grounding against Napolitano
     [92] (needs buried electrodes and the cross-medium option);
   - a sagged-line induced-voltage case against Stracqualursi [96].

### Phase 10e — Impulse laboratory — open remainder

3. **Branch current in the transient driver** — API only; expose it in JSON
   and the writers (also the Phase 10d item 4 remainder).
- A charged embedded capacitor (series EMF in a `lumped` segment).
- Golden fixtures for the laboratory cases (`marx_lab_2MV*`,
  `labuat_impulse*`), now that the record-end artefact is controlled (log
  INV-2026-11-03); they need `riseTime` in the ports first, or Fortran-only
  fixtures.
- Rust and Julia follow-along: `plane`, `lumped`, the `step` and
  `branchNorton` waveforms, `riseTime`.

### Phase 11 — Grounding-safety outputs — open remainder

The electric field vector (`−∇ψ − jωA`) and path voltages along an arbitrary
route (the legacy `camposdif`/`calctensoes` line integral); transient maps —
the potential is linear in the solved currents, so a per-site transfer
function through the transient driver is the natural next step (theory.md
§3.1); showing the potentials in the GUI; the Julia port.

### Phase 12 — Tower-footing electrode library

1. **`ring`** — Fortran and GUI done; open: Rust and Julia loaders.
2. **Tubular conductor** (metallic pipes, steel poles) — **S**.
   A tube is a **cross-section entry**
   ([ADR 0030](adr/0030-cross-section-internal-impedance.md), theory.md
   §4.3), not a new element and not a wall-thickness field on `tLine`. A
   pipe or pole is a
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
   `tLongmireSmithSoil` (Longmire & Smith [15] per Cavka et al. [16];
   theory.md §7.4).

### Phase 13 — Line and substation tier: conductors and cables

Application-tier features (ADR 0018).

2. **Generic internal impedance models** — Fortran done (theory.md §4.3,
   ADR 0030). **Open:** Rust and Julia parity; an NLT transient comparison
   test; complex pole pairs, for stranded and steel-cored conductors with
   resonances; the **`matrix` kind** for multi-conductor sections (item 4);
   the **`sheet` kind** (item 5); tube entries (Phase 12 item 2). Where no
   measured data exists, conductor subdivision (de Arizon & Dommel [73]) can
   compute R(f), X(f) tables offline; the legacy ACSR spreadsheet is
   candidate seed data and the C++ bundle/L-profile models a possible
   catalogue kind.
3. **Coaxial encasement layer** — Fortran done for `line` and `catenary`
   (model in theory.md §4.3). **Open:**
   - `mesh` bars; Rust and Julia parity;
   - comparison with Sunde's propagation constant for a coated buried wire
     [41];
   - a trend check against the measured encasement benefit [115, 116, 122];
   - the concrete-foundation helper: a rebar cage as an equivalent cylinder,
     `r_eq = (n·r·R^(n−1))^(1/n)` (n bars of radius r on a circle of radius
     R), inside a layer of the foundation radius;
   - parameter guidance in `common/README.md`: insulation (σ_c ≈ 0, ε_r
     2.3–4), concrete (σ_c roughly 10⁻³–5·10⁻² S/m with moisture, ε_r about
     5–15, dispersive; more conductive than resistive soil, which is why
     encasement lowers footing resistance [115, 116, 122]), semiconducting
     screens and ground-enhancing compounds [110, 117].
   The same layer forms the jacket on a cable armour (item 4). Pad and pile
   footings of 0.5–1.5 m radius violate the thin-layer assumption and belong
   to Phase 15 item 2.
4. **Insulated and multipolar cables (armoured)** — **L**. Target types, all three in scope: single-core
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
   The `plane` element (Phase 10e) gives every
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

### Phase 14 — Air–soil coupling

Fortran done, opt-in (theory.md §5.1, ADR 0029). **Open:** Rust and Julia
ports; a published reference to validate against — the checks so far are
analytic limits and continuity. Candidate: a wire crossing the interface
against the NEC treatment of Burke & Miller [134]; flipping the default after
that; lossy-ground induced voltages and LEMP.

### Phase 15 — Multi-layer soil

1. **Multi-layer soil and reflection-coefficient images** — **L**. Lifts
   the §5 single-interface premise; route: layered-earth Green's
   functions via quasi-static complex images (Li, Chen & Wang [33]; Sunde
   [41] background). Explicitly out of MVP scope (theory.md §5, §10.1).
   Time-domain and full-wave counterparts for cross-checking: Fan & Li [103]
   and Nazari et al. [104]. The general layered-media Green's functions are
   reviewed by Michalski & Mosig [135].
2. **Finite conductive and dielectric volumes (concrete footings,
   backfill)** — **L**, research. Stage 2 of Phase 13
   item 3. A concrete pad or pile is a bounded region of a third
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

### Maintenance

- **M1 — Remove SLATEC**: open are step 8 (per-conductor `Z_int` cache),
  step 10 (the decision on the `|ρ| > 500 → ratio = 1` cut-off, 1e-3
  relative error) and the 40× parity run. `K₀/K₁` follow with Phase 12
  item 2.
- **Known test failures** (not regressions; see the log entries cited):
  - `test_common_cases` on `channel_loaded`: the fixture is older than the
    case (log L-13). Regenerate it with Phase 8 item 1.
  - Under the release flags of `build.sh` (fast-math), the harmonic fixtures
    of `channel_tower` and `channel_tower_gap` drift past 1e-6 and the
    `channel_loaded` transient differs (log L-02, M2). The gate runs the
    golden suite with `-O2 -fopenmp`. To decide: state that in
    `fortran/README.md` as the rule, or give those fixtures a documented
    tolerance (needs an ADR, log L-10).
  - `test_bessel` and `test_impedance` assert NaN/Inf results and cannot run
    under `-ffinite-math-only` (log L-01).
  - GUI: `test_load_portela_mesh_element` (title `portelaMesh` vs `m`).

### Near-term work

1. **Usable part of an NLT record.** The solver side is done (ADR 0032, log
   INV-2026-11-03). Open: report the usable fraction of the record (a field
   in the results or a warning when the tail grows), a GUI marker for the
   unreliable tail, and the Rust/Julia port of `riseTime` (their loaders
   ignore the field today and would silently return the unsmoothed result —
   make them refuse it, log L-12).
2. **Finish the NaN audit** (log M2). `Channel*` and the transient path under
   `-ffinite-math-only`; then decide whether `-fno-finite-math-only` stays a
   requirement or becomes a safeguard, and say so in DISTRIBUTION.md.
3. **Documentation follow-through** (log DOC-2026-11-14): code comments and
   ADR bodies still cite "theory.md §n" by number, which the theory index
   resolves; a FORD page that links the chapters is optional.

---

## 4. Milestones and next steps

Open milestones: **8b** (Rust full conformance — needs Phase 8 item 1) and
**8J-b** (Julia transient and widened fixtures). Milestones met are in the
log, "Milestones and releases".

**Next engineering steps:** the near-term list; Phase 8 item 1 (widened
fixtures, structure dump, conformance tag) and item 9 (report); Phase 8J
follow-along (Julia Phase 9, then 10b, then 11); the tall-object
induced-voltage case (Phase 10d item 5); Phase 12 items 2 and 4 with
transient potential maps as the Phase 11 remainder. Conductor and cable
track, on top of ADR 0030: tube entries (Phase 12 item 2) → remaining coating
items (Phase 13 item 3) → `sheet` kind for planes (Phase 13 item 5) → the
`matrix` kind and the `cable` element, stages 4a–4c (Phase 13 item 4) →
finite volumes (Phase 15 item 2, research).

Prioritisation rules: second implementations before features;
decided-and-small before open-ended; MVP before application tier;
dependencies first; change the golden fixtures once (log L-13); new theory
and object-model work last.

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

---

## 6. Open decisions

| Decision | Status |
| --- | --- |
| Reduced `Z_g` solver | Deferred optimisation (ADR 0003) |
| NLT as the transient default | Not flipped (ADR 0024 §7); revisit after a TAGS time-domain comparison |
| Default `nltDamping` (c·T = ln N² vs ≈ 6) | Unchanged (ADR 0032); a change moves every NLT fixture, so it goes with the next fixture regeneration |
| Cross-medium coupling as default | Opt-in (ADR 0029); revisit after a published comparison (Phase 14) |
| Binary results format | Deferred — stay on JSON/CSV (ADR 0012); HDF5 is the leading candidate vs ADR 0006's zero-dependency philosophy. Revisit once a real case is too large for JSON |
| `\|ρ\| > 500` Bessel cut-off | Open (M1 step 10) |
| Plane-bar radius (`d/2π` vs `d/4`) | Open (Phase 13 item 5) |
| Release flags and NaN semantics | `-fno-finite-math-only` kept; audit to finish (near-term item 2) |
| Gate flags for the golden suite | `-O2 -fopenmp` in practice; to be written down or replaced by a tolerance (Maintenance) |

Resolved decisions are in the log's decision register.

---

## 7. Proposals from the open-source HEM comparison — status

Code and ADRs cite these as "ROADMAP §7 Pn". The proposals came from comparing
TAGS (C99), PRTL-mHEM (Python) and PRTL (Wolfram) with this repository; the
headline finding was that TUPÃ's geometry-factor separation (theory.md §4.1) is
the optimisation published as mHEM [11], and TAGS's closed-form self integral
equals `g_self`.

| # | Proposal | Status | Where |
| --- | --- | --- | --- |
| P1 | mHEM single-integral kernel for `g(a,b)` | done | Phase 10 item 1, log P10 |
| P2 | Frequency-dependent image coefficient `Γ(ω)` | done | Phase 10 item 2, log P10 |
| P3 | Cross-code validation against TAGS | done | Phase 10 item 5, [tags-xval.md](validation/tags-xval.md) |
| P4 | Numerical Laplace Transform | done | Phase 9 item 5, log P9 |
| P5 | Dispersive-soil subtypes | mean set done; remainder open | Phase 12 item 4 |
| P6 | Parallelise over frequencies | done | Phase 10 item 4, log P10 |
| P7 | Field/potential post-processing | scalar potential done | Phase 11, log P11 |
| P8 | Criteria-based segmentation | done | Phase 10 item 3, log P10 |

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

---

## 8. Legacy findings and 9. Author-interview decisions

Moved to [ADR 0017](adr/0017-legacy-reinspection-findings.md) and
[ADR 0018](adr/0018-author-interview-decisions-2026-07.md); the legacy element
inventory is in the log ("Legacy and project decisions").
