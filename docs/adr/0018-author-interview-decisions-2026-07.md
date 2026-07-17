# ADR 0018 — Author-interview decisions (2026-07-05)

- **Status**: Accepted (decision record; moved here from ROADMAP.md §9 when
  the roadmap was tidied, 2026-07-17)
- **Date**: 2026-07-05

## Context

Recorded from the documentation-reconstruction interview with the author.
Where a decision changed a document or the code, the change is already
applied and referenced. Other documents cite this record as "ROADMAP §9"
(the roadmap keeps a stub section pointing here) or "ADR 0018".

## Decisions

| Topic | Decision |
| --- | --- |
| Application scope | MVP: tower-footing grounding. Full application tier: complete lines or substations. |
| Project role | Scientifically citable reference implementation (docs tone, validation rigour follow from this). |
| Convention authority | theory.md remains normative with the **engineering convention** (`e^{+jωt}`); Portela's papers (physics convention) are mapped through the §2 conjugation table, never followed directly. |
| Validated models | **None yet** (as of 2026-07-05) — no end-to-end physics validation has run; only unit-level convention pins. |
| Soil dispersion | ADR 0007 accepted: `tPortelaSoil` first, Lima–Portela [31] parametrisation, ω₀ = 2π·1 MHz. |
| Proposals P1/P2/P4 | Confirmed as written (mHEM 1-D kernel; Γ(ω) images; NLT) — ROADMAP §7. |
| Sources | Current-injection equivalents (ADR 0010; mechanism later concretised by ADR 0016). |
| Fill interface | Theory factors inside `calcZSelf`/`calcZMutual` (ADR 0009); `current1/current2` documented as end currents `i₁`/`i₂`. |
| Quadrature tolerances | Dissertation-era values (`errrel = min(la,lb)·10⁻⁶`, `maxint = 500`) kept for now, open to revision — revisit with the P1 mHEM kernel. |
| Stable vs fluid modules | Stable: `mMesh` conventions, `mGeometry`, `mImpedance`. Fluid: `tResult`, `mJsonParser`, `tStudy%run`. |
| Public contract | JSON schema + `common/` cases only; all Fortran module APIs are internal and changeable. |
| Compilers | Latest gfortran; keep ifx-compatible. |
| CI | No hosted CI (no GitHub Actions); local `fpm build && fpm test` gate (ROADMAP §5). |
| Release process | Proposed (delegated): semver; **0.1.0 = validated Phase 2 milestone** (Portela-curve case within tolerance); annotated git tags + a CHANGELOG; no package-registry publishing planned. Note: only the DC-limit check is executable today — the Portela-curve tolerance check needs P3 (TAGS cross-validation) or real tabulated data first. |
| SLATEC | The **cloned** `fortran/slatec/` checkout (from the author's fork, fetched by `build.sh`) is canonical and may be fine-tuned in place. |
| Benchmarks | TAGS and PRTL-mHEM to be added as git submodules under `benchmarks/` (see [BENCHMARKS.md](../BENCHMARKS.md)); they are the executable validation oracles until curated reference datasets arrive. |
| Validation data | No tabulated data from Portela 1997 exists — only the equations; further validation references to be supplied by the author. Visacro & Soares 2005 [5] has no usable comparison data. No legacy-output fixtures for now. |
| Precision | `dp` kind added to `mCtes`; new code uses it, legacy `kind=8` migrates gradually. |
| Error handling | All `error stop` converted to feh (`raiseError`), including the JSON parser and the CLI entry point. |
| Housekeeping | `check.f90` stub test and `calcBase` placeholder deleted (done). |

## Consequences

- These decisions are standing project policy until superseded by a later
  ADR; the roadmap references them instead of restating them.
