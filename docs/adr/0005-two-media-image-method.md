# ADR 0005 — Two half-space media via ideal images; no cross-media coupling

- **Status**: Accepted
- **Date**: 2026-07-03

## Context

TUPÃ models air (z > 0) and soil (z < 0) separated by a plane interface. The
rigorous treatment uses Sommerfeld integrals or frequency-dependent
reflection/transmission coefficients relating image sources (Portela [1]
§2.4). The original code implemented the simpler ideal-image limits.

> **Correction (2026-07-05).** The last sentence is true only of the **C++
> port**. The original Matlab (now the model reference of record)
> implements the frequency-dependent reflection coefficient Γ(ω) as its
> *default* mode — equal-permeability Fresnel form, applied to both the
> transversal and longitudinal image parcels — with ideal images behind a
> `SOLO_IDEAL` switch; the C++ dropped Γ(ω). The decision below stands for
> the reference core's first milestone, but Γ(ω) is promoted from "out of
> scope" to a planned restoration of reference behaviour (implementation
> plan P2, theory.md §5). Cross-media coupling remains neglected in all
> generations of the code.

## Decision

Represent the interface by **ideal images** (theory.md §5):

- both segments in soil → image contributions **add**;
- both segments in air → image contributions **subtract**;
- segments in different media → mutual coupling **neglected** (zero).

Multi-layer soil and frequency-dependent reflection coefficients are out of
scope for the reference core; the object model must not preclude them (the
`Material`/medium abstraction and the per-pair image terms are the extension
points).

## Consequences

- Matches the original implementation and the published validation cases, and
  keeps the per-pair cost at two geometry factors (direct + image).
- Accuracy degrades for conductors very close to the interface and for strong
  air↔soil coupling problems (e.g. tower + counterpoise interaction through
  the surface); this limitation must be stated in results and revisited when
  implementing multi-zone soil (planned dispersive-soil `tMaterial` subtypes,
  e.g. `tPortelaSoil`/`tLongmireSmithSoil` — ADR 0007).
- The sign rules are a frequent bug source; they are pinned by unit tests
  against the DC image formulas (theory.md §9.1).

## Amendment 2026-10-01 — Γ(ω) is the default (ROADMAP Phase 10 item 2)

The decision above (ideal images) is now the **low-frequency limit**, no
longer the default: image parcels of both `Z_t` and `Z_ℓ` carry the
frequency-dependent coefficient `Γ(ω) = (W_own − W_other)/(W_own + W_other)`
(+1 → soil, −1 → air in the limit), restoring the original Matlab's default
mode as the 2026-07-05 correction planned. Ideal images remain selectable
(`numerics.imageModel: "ideal"`, CLI `--image-model ideal`) and keep a
golden fixture. Cross-media coupling remains neglected. Details, size of the
change and its validation: [ADR 0024](0024-phase10-numerics.md) §2,
[validation/phase10-image-model.md](../validation/phase10-image-model.md),
[validation/tags-xval.md](../validation/tags-xval.md).
