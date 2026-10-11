# Surface potentials, GPR, touch and step voltage — validation

Checks behind ROADMAP Phase 11 ([ADR 0027](../adr/0027-observation-potentials-and-safety-outputs.md),
theory.md §3.1): the `observation` block and the `<case>_potentials.*` files.
These outputs are post-processing of a solved sweep, so the interesting
question is not the solver but whether the potential field it implies is the
right one. No published curve exists for them in this repository; the oracles
are closed forms that hold in the low-frequency limit. Regenerate every number
below with

```sh
python3 docs/validation/phase11_checks.py <path to the tupa executable>
```

(Fortran `build.sh` binary or `rust/target/release/tupa`; same tables to the
digits shown.)

## A note on the frequency

The kernel carries the soil's quasi-static attenuation `exp(-γR)`,
γ = √(jωμ(σ + jωε)). At 10 Hz in 100 Ω·m soil the skin depth is 1.6 km, so the
potential 400 m from a rod is already 22 % below the static value — the model
being right, the static closed forms being wrong at that range. The
comparisons with static formulas below are therefore made at **10 mHz** (skin
depth 50 km), where the attenuation is under 1 % at 400 m. (An earlier pass at
10 Hz showed the errors growing as exactly `e^{-r/δ}` with distance, which is
how this was identified.)

## 1. Flush vertical rod

3 m rod, radius 10 mm, top at the soil surface, 12 segments, ρ = 100 Ω·m,
1 A at the top, 10 mHz.

**GPR.** 32.288 V, against Dwight's rod resistance
ρ/(2πL)·(ln(4L/a) − 1) = 32.309 Ω: **−0.06 %**.

**Surface potential.** The reference is the line source of uniform leakage
along the rod, ψ(r) = ρI/(2πL)·asinh(L/r) (the closed integral of the
half-space point-source field). It is exact in the far field; near the rod the
real leakage is not uniform (it concentrates towards the tip), which TUPÃ
solves for and the closed form does not.

| r (m) | TUPÃ ψ (V) | line source ψ (V) | error |
| --- | --- | --- | --- |
| 0.5 | 12.764 | 13.219 | −3.44 % |
| 1 | 9.402 | 9.647 | −2.54 % |
| 2 | 6.245 | 6.338 | −1.47 % |
| 5 | 3.005 | 3.018 | −0.42 % |
| 10 | 1.566 | 1.569 | −0.14 % |
| 20 | 0.792 | 0.793 | −0.07 % |
| 50 | 0.318 | 0.318 | −0.10 % |
| 100 | 0.159 | 0.159 | −0.20 % |
| 400 | 0.039 | 0.040 | −0.79 % |

Beyond ~5 m the error is under 0.5 % and is the remaining attenuation; inside
2 m it is the non-uniform leakage, growing towards the rod, as expected.

**Touch voltage** (36 points on a 1 m circle round the rod, `touch` site):
22.886 V against GPR − ψ_line(1 m) = 22.641 V (**+1.1 %**); the `gpr` row of
the same site is the node voltage, 32.288 V (identical to the `voltage`
output by construction).

**Step voltage** (`steps`, 1 m stride along the surface):

| pair (m) | TUPÃ (V) | line source (V) | error |
| --- | --- | --- | --- |
| 1 – 2 | 3.1569 | 3.3087 | −4.6 % |
| 2 – 3 | 1.6121 | 1.6626 | −3.0 % |
| 5 – 6 | 0.4600 | 0.4648 | −1.0 % |
| 10 – 11 | 0.1386 | 0.1391 | −0.3 % |

Step voltage is a difference of two potentials, so the near-rod relative error
is larger than that of ψ itself; the absolute difference is 0.15 V at the
worst pair, on a GPR of 32 V.

## 2. 16 × 16 m grid

The `grid_safety` grid (3 × 3 bars of 16 m, 0.5 m deep, 5 mm radius, ρ = 200 Ω·m)
without its riser, 1 kA injected at a corner, 10 mHz.

| Quantity | TUPÃ | Reference | |
| --- | --- | --- | --- |
| Grid resistance | 6.403 Ω | equal-area disc ρ/(4r) = 5.539 Ω | the lower bound for a grid of this footprint: TUPÃ is above it, as it must be |
| | | Laurent–Niemann ρ/(4r) + ρ/L_T = 7.622 Ω | −16.0 % |
| | | Sverak (IEEE Std 80 [42] form) = 7.331 Ω | −12.7 % |

The two empirical formulas are conservative for a grid this small (two bays
per side, 96 m of conductor), where they are not meant to be tight: they sit
above TUPÃ, and the plate bound sits below. This is a plausibility bracket, not
an anchor — a sharper one would need a published value for the same grid
(BENCHMARKS.md anchor 5, still open).

| Point | TUPÃ ψ (V) | ψ / GPR | point source ρI/(2πd), d from the grid centre | error |
| --- | --- | --- | --- | --- |
| grid centre | 5860 | 0.915 | — | — |
| (40, 0) m | 980 | 0.153 | 965 V | +1.6 % |
| (200, 0) m | 165.3 | 0.026 | 165.6 V | −0.2 % |

The far point reproduces the point-source field of the injected current to
0.2 %; at 33 m the 16 m grid is not yet a point source, which is the +1.6 %.

## 3. Potential beside a conductor vs its node voltages

A point 10 mm (one conductor radius) from the axis at the middle of a
buried-rod segment must see that segment's voltage, the same consistency the
solved system has (theory.md §3.1). Rod 3 m long, 0.5–3.5 m deep, 6 segments;
the node voltages of the segment's two ends averaged.

| f (Hz) | ψ beside the segment | mean of its node voltages | difference |
| --- | --- | --- | --- |
| 10 | 30.723 − 0.009j | 30.817 − 0.010j | 0.31 % |
| 10³ | 30.639 − 0.094j | 30.728 − 0.099j | 0.29 % |
| 10⁴ | 30.436 − 0.296j | 30.515 − 0.311j | 0.26 % |
| 10⁵ | 29.802 − 0.932j | 29.847 − 0.978j | 0.22 % |
| 10⁶ | 27.972 − 3.097j | 27.908 − 3.225j | 0.51 % |

Within 0.5 % at every frequency including the MHz band, where the soil
attenuation and the Γ(ω) image coefficient both act on the potential point
and the solved voltage alike.

## 4. Golden fixture and cross-implementation

`common/grid_safety.json` (tower riser, 13 × 13 surface grid with step map,
touch site and step pair, four frequencies): 1372 rows of
`grid_safety_potentials_expected.csv`; the Fortran executable and the Rust
executable both reproduce the file to a worst difference of 1.8e-8 and 1.9e-8
of the peak of the quantity (the golden rule is 1e-6).

## 5. Not covered

- **Transient maps.** The block is harmonic only, an error beside a
  transient-only case (ADR 0027 "Limits"); GPR over time is already compared
  with a published curve by [silva2025-fig4.md](silva2025-fig4.md), but that is
  the node voltage, not the new surface potentials.
- **A published touch/step case.** No reference with tabulated surface
  potentials is in this repository yet; the closed forms above bracket the
  physics only for the single rod and the point-source far field.
- **IEEE Std 80 derating** (body circuit, surface layer) is deliberately
  outside the solver (ADR 0018, ADR 0027).
- **Julia and the GUI.** The Julia port refuses the block rather than skipping it; the GUI ignores it.
