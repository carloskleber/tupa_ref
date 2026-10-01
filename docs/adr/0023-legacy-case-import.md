# ADR 0023 — Legacy case import: `catenary` element, `portela` waveform, legacy `common/` cases

- **Status**: Accepted
- **Date**: 2026-09-30

## Context

The original Matlab implementation (the model reference of record) ships a
library of study cases as `.est` (structure) and `.caso` (study) text files.
The author asked for ten of them to be imported into `common/`: the line
cases `linha0`–`linha5`, `linha5a` and the tower cases `torre0`–`torre2`.
They cover what no existing `common/` case does: structures in air (shield
wires, towers, a lightning channel as a plain conductor) coupled to short
grounding electrodes, driven by the legacy transient waveform.

Three things in them had no JSON counterpart:

1. **Catenary spans** (`catenarian`, used by `linha4`). This is ROADMAP
   Phase 13 item 1 (`tCatenary`), pulled forward as the roadmap allows
   ("cheap and may be pulled forward if a tower-footing case needs shield
   wires").
2. **Portela's surge** (`sinal` `portela`, all ten cases) and the linear
   `rampa` (`linha0`). The Portela surge is ROADMAP Phase 9 item 3, already
   specified in theory.md §8.
3. **Lattice keywords** `cubo` (a hexahedron's 12 edges; `torre0`, `torre1`)
   and `piramide` (a square pyramid's 8 edges; `torre1`). In the current
   Matlab these element classes are empty placeholders. The reader branches
   for them are left over from the earlier procedural version and no longer
   run: they refer to variables that no longer exist.

Several legacy outputs also have no counterpart yet: field points (`cem`,
Phase 11), path voltages from `.pot` files (`v`, Phase 11) and entries of
the impedance matrices (`zl`/`zt`).

## Decision

### `catenary` element (schema v1 addition)

```json
{ "type": "catenary", "id": "E13", "from": "N13", "to": "N16", "sag": 5.0,
  "radius": 0.005, "segments": 20, "material": "steel" }
```

The fields are those of `line`, plus `sag`, the downward displacement at
midspan in metres. The chain nodes follow the parabolic profile of
theory.md §4.4:
`p_k = P1 + s(P2 - P1) - 4·sag·s(1 - s)·ẑ`, `s = k/n`. For end nodes at
equal heights this is the legacy `Catenaria.m` geometry exactly. For unequal
heights the legacy takes every internal node's height from `P1`, which makes
a step at the far end; the chord form here stays continuous. A profile that
crosses `z = 0` is rejected, because an element must stay in one medium
(ADR 0005). Generated IDs follow the `line` scheme (`<id>_n<k>`,
`<id>_e<k>`).

Implementation: in Fortran, `tCatenary` extends `tLine` and overrides only
the type-bound `nodePosition`. `assembleLine` is shared, and it must be
called with the polymorphic `this`, not the parent component
(`this%tLine`), whose dynamic type would bind the straight profile.
`test_assemble` pins this. Rust and Julia pass the sag to a shared
line-assembly routine. The 3-node legacy variant (`catenaria3`) is not
ported until a case needs it.

### `portela` waveform (ADR 0015 schema addition)

```json
"signal": { "waveform": "portela", "imax": 1000.0, "alpha": 2.0,
            "tFront": 1e-6, "tTopEnd": 20e-6, "tTailEnd": 100e-6, ... }
```

These are the English names asked for by ROADMAP Phase 9 item 3: `imax`
(peak, A), `alpha` (front inclination factor; α < 0 allowed), and the ends
of the front, flat top and tail (s). The formula and interval boundaries
are those of theory.md §8 and the legacy `impulso.m`. The front is
evaluated as `expm1(α t/t₁)/expm1(α)`, and `alpha: 0` is exactly the linear
ramp, which is how the legacy `rampa` is imported. Fortran has no `expm1`
intrinsic, so `mSignal` carries a Kahan-corrected one. The loader requires
`0 < tFront <= tTopEnd < tTailEnd`.

### Lattice keywords expand to `line` elements at import

`cubo`/`piramide` get no JSON element type. The importer expands them into
one `line` per edge and reproduces two legacy reader rules that change the
element list:

- an edge joining two nodes that are already connected by an earlier
  element is skipped (legacy `isLigado`), and it does not take an element
  number;
- an edge whose segments would be shorter than the keyword's `lmin` is
  re-segmented to `ceil(length/lmin)` segments (legacy `RetaMin.m`).

The legacy connection orders are 1-2, 1-4, 2-3, 4-3, 5-6, 5-8, 6-7, 8-7,
1-5, 2-6, 3-7, 4-8 (cube) and 1-2, 1-4, 2-3, 4-3, 1-5, 2-5, 3-5, 4-5
(pyramid). A native lattice element can come later with the tower models,
if one is wanted.

### Import mapping (`tools/legacy_import.py`)

The converter follows the Matlab readers and writes `common/<name>.json`, keeping the legacy case name.

- **IDs.** Node `k` becomes `N<k>`. Element number `k` becomes `E<k>`, in
  the legacy numbering that the legacy `il`/`it` outputs refer to. That
  numbering counts only elements actually added, as described above.
- **Materials.** The letters map to the Matlab `Estrutura` table:
  `a` aluminum, `c` copper, `f` iron (μr 1000), `s` steel (μr 100),
  `z` zinc, `w` copperweld.
- **Soil.** `solo σ εr μr` becomes a linear soil. `solo_freq σ0 α kr μr`
  becomes `"type": "portela"`. The legacy `kr` is referenced to ω₀ = 1 rad/s
  and is converted to the ADR 0007 form as `kr' = kr·tan(πα/2)·(2π·10⁶)^α`.
- **Signal.** A case with several `sinal` entries becomes a `signals` list
  ([ADR 0026](0026-independent-transient-signals.md)), all at the first
  entry's node as in the Matlab solver, so the legacy line cases keep their
  three fronts (1, 2 and 10 µs; before 2026-10-01 the importer kept only the
  first entry and running another meant editing `tFront`). `nyquistHz` is the legacy `freq_*` maximum, `fftPoints` is
  `num_pontos_fft`, and `freqZeroHz` is `freq_zero` when the case sets it.
- **Observations.** `func_tran` `u`/`deltau` nodes become `observeNodes`.
  `il`/`it` elements become `observeElectrodes` on the element's first
  segment, which is the segment the legacy reports. The same lists fill
  `outputs`.
- **Harmonic sweep.** A 1 A injection at the signal's node is added.
  `freq_log fmax n` keeps the legacy n log-spaced points over
  [fmax·10⁻⁵, fmax]. `freq_lin` becomes 20 points per decade over
  [fmax·10⁻⁴, fmax], since the schema is log-only (ADR 0013).
- **Not carried over**, each reported by the converter: `cem`, `v`, `zl`,
  `zt` outputs; the legacy's Γ(ω) images (`torre*` do
  not set `solo_ideal` — TUPÃ had ideal images only until Phase 10 item 2,
  which restored them as the default on 2026-10-01 (ADR 0024));
  a non-vacuum air (`torre2`'s σ = 10⁻¹⁰ S/m, ADR 0019).

Legacy segment lengths are kept as they are, even where they exceed the
λ/10 bound at the Nyquist frequency (e.g. 30 m shield-wire segments at
10 MHz). The point of the import is the legacy study, not a re-meshed one.

## Consequences

- Schema v1 gains one element type and one waveform. Fortran, Rust and Julia
  implement both (follow-along rule, ROADMAP Phase 8 item 10 / 8J item 9),
  and every implementation loads, validates and assembles the new cases in
  its "every `common/*.json`" test.
- The ten cases have **no `_expected.csv` fixtures**. No legacy output files
  exist to compare against, and fixtures from this implementation would
  only pin its current numerics. They run end to end (run times in
  `common/README.md`) and are the natural first targets once the Phase 9
  item 1 interpolated transient and the Phase 10 Γ(ω) images land.
- Legacy quirks surfaced by the import, recorded here rather than fixed in
  the data:
  - `linha4.est` comments out the `non` header of its phase-conductor node
    block, so that conductor never existed in the legacy runs either. The
    imported case is a shield wire with catenary spans and an indirect
    strike; its phase-conductor output is commented out as well.
  - The `torre*` cases omit the `sinal` count line: the current Matlab
    reads zero signals, and the evident intent is one. Their `freq_max`
    keyword is not read by any Matlab version.
  - `linha1` asks for `deltau` over three node pairs, which the current
    Matlab rejects (exactly two nodes are allowed). The nodes are observed
    individually instead.
- Checking the imported tower cases for passivity exposed a geometry-factor
  bug inherited from the Matlab: the closed form for parallel segments was
  wrong for opposite-direction pairs of unequal length. It is fixed in all
  three implementations ([ADR 0017](0017-legacy-reinspection-findings.md)
  finding 8). Before the fix, `torre2` showed a negative input inductance.
  After the fix, Re(Zin) < 0 remains only at isolated points near the top
  of some sweeps: `linha2` above 5 MHz (15 of 500 points), `linha3` above
  9 MHz (2 of 800), `linha4` from 1.4 MHz (24 of 500), `torre0` above
  0.8 MHz (8 of 81) and `torre2` at 2 MHz (1 of 10). Many of these lie where
  the kept legacy segments are long against the wavelength. They are not
  investigated further here.
- The GUI skips `catenary` elements with a warning (GUI_SDD G1 renders
  `line`/`mesh` only). Drawing one is a follow-up.
