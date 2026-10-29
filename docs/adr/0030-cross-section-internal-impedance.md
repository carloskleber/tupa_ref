# ADR 0030 — Cross-section internal-impedance entries (ROADMAP Phase 13 item 2)

- **Status**: Accepted — Fortran implemented 2026-10-29 (uncommitted); Rust and Julia open
- **Date**: 2026-10-29
- **Amends**: [ADR 0013](0013-input-schema-sources-frequencies-outputs.md)
  (element schema), [ADR 0012](0012-results-json-schema.md) (additive
  `warnings`); revises the 2026-07-17 Q&A decision recorded in ROADMAP
  Phase 13 item 2 ("entries carry frequency-tabulated R(f), X(f)")

## Context

The internal (skin-effect) impedance of a segment is today the solid
cylinder of theory.md §4.3, evaluated by SLATEC `ZBESI` from the segment's
`radius` and its `tLinear` material, or the channel loading of ADR 0025. It is
wrong for any conductor that is not a round homogeneous wire: strips, angle
sections, stranded or steel-cored cables. For a 200 × 0.5 mm copper strip the
equal-area cylinder overestimates |Z_int| by an order of magnitude at 1 kHz,
and misses the edge crowding that raises the high-frequency resistance by
about 2.2× over the uniform-current value (ROADMAP §2, mom side).

ROADMAP Phase 13 item 2 planned a database of tabulated R(f), X(f). Two
points were not settled then:

1. The solver evaluates `Z_int` at a **complex** frequency `s = c + jω` on the
   NLT path (`internalImpedanceLaplace`). A table cannot be evaluated off the
   `jω` axis.
2. The thin-wire kernel needs a radius for the *external* part, and today the
   same `radius` feeds both parts. A non-circular section has to say which
   radius the kernel uses, and `Z_int` must then be defined relative to it,
   or the two parts do not add up to the true total impedance.

The commercial side supplies the catalog and the tools that populate it
(`mom/data/cross_sections`, `mom/tools/import_comsol_zint.py`). This ADR fixes
only the **mechanism**: entry schema, lookup, evaluation and the definition.

## Decision

### 1. Definition: `Z_int` is relative to the equivalent radius

Each entry carries an **equivalent radius** `r_eq`, the *logarithmic
capacity* (conformal radius) of the section: the radius of the round wire
with the same external field in the perfect-conductor limit (`w/4` for a thin
strip, `0.5902 a` for a square of side `a`). The thin-wire kernel uses
`r_eq` as the segment radius. The entry's impedance is

    Z_int(ω) = Z_total(ω) − jω (μ0/2π) ln(R_b / r_eq)        (per metre)

where `Z_total` is the 2D solution of the section inside a coaxial return of
radius `R_b` (or, equivalently, with the external inductance
`μ0 ε0 / C'` of the same geometry subtracted, `C'` from an electrostatic
solve on the same mesh). Consequences, all checked on the first entries:

- circle: `r_eq = r0`, and `Z_int` is exactly the §4.3 Bessel result;
- DC: the internal inductance is `μ0/2π · ln(r_eq/GMD)`, with `GMD` the
  geometric mean distance of the section (23.7 nH/m for 200 × 0.5 mm, against
  0.26 nH/m for the energy-only "internal inductance" μ0 t/12w);
- high frequency: the external field is exact and edge crowding sits in the
  resistance of `Z_int`.

A conductor-only model (no air domain) is *not* a valid entry source for a
non-circular section: it forces uniform surface current and understates the
high-frequency resistance by the crowding factor.

### 2. Entry schema (`tupa.crossSection`, draft v0)

```json
{ "schema": "tupa.crossSection/0-draft",
  "id": "strip_Cu_200x0.5",
  "kind": "poleResidue",                  // or "table"
  "geometry": { "type": "rectangle", "width": 0.2, "thickness": 5e-4, "filletRadius": 1e-4 },
  "equivalentRadius": 0.0503792,          // m, logarithmic capacity (§1)
  "reference": { "sigma": 5.998e7, "mur": 1.0 },
  "band": { "fMin": 1.0, "fMax": 2.51e7 },          // Hz, fitted/tabulated range
  "model": { "d": ..., "e": ..., "poles": [...], "residues": [...] },
  "dc": { "resistance": 1.667e-4, "internalInductance": 2.33e-8 } }
```

`geometry`, `provenance`, `fit`, `checks` and `description` are informative;
the solver reads `id`, `kind`, `equivalentRadius`, `reference`, `band`,
`model` and `dc`.

- **`poleResidue`**: per-metre `Z(s) = d + e·s + Σ c_k/(s − a_k)`. v0 allows
  only real poles, `a_k < 0`, and requires `Re Z(jω) ≥ 0` for all ω (the
  Foster RL form `c_k = −r_k p_k < 0`, `r_k, e ≥ 0`, guarantees it). The
  loader rejects a positive pole and warns if `d + Σ Re(c_k/(jω − a_k)) < 0`
  on a log grid over `band`. It is evaluated exactly at any `s`, including
  the NLT's `c + jω`.
- **`table`**: `"model": { "f": [...], "R": [...], "X": [...] }` (Ω/m),
  interpolated linearly in `log f` (the transient driver's `pchip` lives in
  a module that depends on the study, so it cannot be reused here). Valid on the `jω` axis only:
  evaluating it at complex `s` is an error (`Z_int` of a `table` entry needs
  `transient.method` ≠ `nlt`; the loader says so). Below `fMin` the entry is
  held at its lowest point as `R = R(fMin)`, `L = X(fMin)/ω(fMin)`; above
  `fMax` it continues as `Z_last·√(f/fMax)` (phase kept).

### 3. Conductivity scaling

An entry is stored at `reference.sigma`, `reference.mur`. For a segment of
conductivity σ (same μr) the solver uses

    Z_int(s; σ) = (σ0/σ) · Z_ref(s · σ/σ0)

(valid because the per-metre impedance of a fixed shape at fixed μr is
`σ⁻¹ F(ωσ)`). One entry therefore covers copper, aluminium or any non-magnetic
conductor of that shape. A different `mur` is an error: magnetic conductors
need their own entry.

### 4. Input schema

A top-level optional `crossSections` array. Items are either an inline entry
or `{ "file": "<path>" }` (relative to the case file) naming a database file:
one entry, or `{ "entries": [ ... ] }`. Ids must be unique over the case.

An element that today carries `radius` and `material` may instead carry

    "crossSection": "strip_Cu_200x0.5"      // id
    "material": "copper"                    // optional: its sigma rescales the entry

`radius` is then forbidden (the radius is `r_eq`); `material`, if present, must
be `tLinear` and supplies σ for §3 (its `mur` must equal `reference.mur`); if
absent the segment uses `reference.sigma`. Applies to `line`, `catenary` and
the bars of `mesh`. A `channel` element never takes a cross-section (ADR 0025).

### 5. Object model and dispatch

`tCrossSection` **extends `tMaterial`** (`fortran/src/CrossSection.f90`) and is
stored in the structure's material list under its id, so elements reach it
through the existing material lookup and `tElectrode` needs no new field; the
loader (`readConductor`) sets the element radius to `r_eq`. A cross-section
element that also names a `material` gets a derived copy registered as
`<entry>@<material>` carrying that material's σ. (Rust: an enum variant of the
conductor model.) `segmentInternalImpedance[Laplace]` dispatch order:
`loaded` (channel) → `tCrossSection` → `tLinear` cylinder. The profile contributes `l · Z_int`
exactly where the cylinder term does, so the self impedance
`Zlong(i,i) = … + zint` is unchanged in form. `checkConductorMaterials`
learns the third case, and a `table` entry under the NLT is rejected there,
before any parallel region.

### 6. Out-of-band and thin-wire checks (warnings, not errors)

- If a requested frequency lies outside `[fMin, fMax]`, the entry is still
  evaluated (the rational model is pinned to the exact DC resistance and has a
  passive high-frequency tail) and a warning naming the entry and the range is
  recorded once per run, in the log and in the results JSON `warnings` array
  (ADR 0012, additive). This replaces the "hard flag" wording of the July
  decision.
- The thin-wire kernel needs a segment much longer than the radius. A
  cross-section segment with `l < 4 r_eq` raises one aggregated warning per
  entry (`r_eq ≈ 50 mm` for a 200 mm strip, so segments below 0.2 m are
  flagged). Applied to cross-sections only: the same check on ordinary
  cylinders would warn on existing cases (a 0.3 m tower member with 1 m
  segments) whose results are pinned.

## Consequences

- Positive: any section computable in 2D (strip, angle, tube, stranded
  conductor, bundle equivalent) enters the solver without new physics code;
  the NLT works for it; one kernel (vector fitting) is shared with the future
  EMT export; the cylinder stays the default and the degenerate case.
- Positive: the commercial catalog and importer need no upstream change beyond
  this schema.
- Negative: `Z_int` is only meaningful with its `r_eq`; an entry cannot be
  used with a user-set radius. The mismatch is prevented by forbidding
  `radius` with `crossSection`.
- Negative: v0 real poles give fits of 8–11 poles for 0.1–0.5 % over seven
  decades; complex pole pairs (stranded or magnetic conductors with
  resonances) are a later, backward-compatible schema extension.
- Rust and Julia implementations follow in a separate parity item; Fortran
  (and the DLL, which exposes it through the case JSON) comes first.

## Validation

1. A `poleResidue` entry fitted to the round-wire COMSOL solve (r0 = 10 mm,
   11 poles) must reproduce the `ZBESI` cylinder: 4.1e-4 (data) and 6.8e-4
   (stored fit at the data points; 1.1e-3 on a dense grid) maximum relative
   error over 1 Hz–25 MHz. Implemented as `test_cross_section`: `Z_int` within
   1.5e-3, and a full sweep with a `crossSection` element within 1e-4 of the
   `radius`+`material` case (the internal impedance is small against the
   soil term). An NLT transient comparison is still to add.
2. σ scaling: the same entry used at σ and 2σ must equal the entry fitted at
   2σ with frequencies halved (analytic round-wire entry, tolerance 1e-6).
3. DC pin: `Re Z(s→0)·l` equals `l/(σA)`; `Im Z/ω → internalInductance`.
4. Strip: edge-crowding limit against an independent boundary-element solve
   (agreement 0.3 % at 15.8 MHz for 0.1 mm fillets, importer report) and the
   Antonini–Orlandi–Paul rectangular-conductor results once acquired.
5. Negative tests: positive pole, `table` under NLT, `radius` with
   `crossSection`, unknown id, `mur` mismatch, unknown file.

## Open points

- Public test fixture: `common/cross_sections/wire_Cu_r10mm.json` (round wire
  from the COMSOL chain, validated against the cylinder); the strip catalog
  stays in the commercial package.
- Complex-pole extension and a matrix (multi-conductor) kind for bundles
  (ROADMAP §2 mom side) are not decided here.
