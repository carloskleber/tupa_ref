# Coated pipeline near a line: fault and lightning study (ROADMAP Phase 14 item 2)

A first application study of a buried, coated steel pipe near an overhead line,
limited to what the solver can do before the earth-return term of Phase 14
item 1.3 lands: **conductive** coupling from a faulted tower footing at
power frequency, and the full lightning transient of a stroke to the tower.
Run 2026-11-22 with the release build (`fortran/build.sh`), 12 threads: about
6 s per fault case, about 6 min per lightning case. Every number below comes
from [pipeline_checks.py](pipeline_checks.py); nothing was computed by hand.

```
python3 docs/validation/pipeline_checks.py --fortran-bin fortran/bin/Tupa [--only fault,lightning,conv] [--write-common]
```

Needs numpy only. `--write-common` regenerates the two base cases
`common/pipeline_fault_60hz.json` and `common/pipeline_lightning.json`.

This is a study, not a validation against a published curve: the anchors are a
closed-form footing resistance, an internal consistency check of the coating
voltage, and the order of magnitude of the published fault cases of Ribeiro et
al. (see [Reference points](#reference-points)).

## Case

One cross-section; x runs along the line and the pipe.

| Part | Model |
| --- | --- |
| Soil | ρ = 1000 Ω·m, εr 10, homogeneous (two-layer soil is Phase 15) |
| Aerial conductor | Shield-wire stand-in, the ACSR Grosbeak equivalent of `aerial_buried_cross` (r = 12.6 mm), 30 m high, x = 0 to 1000 m, 5 m segments |
| Tower | At x = 500 m: a 0.3 m-radius steel conductor from the aerial conductor down to z = 0, a 0.5 m copper link, then a four-arm star counterpoise, 30 m arms of 70 mm² copper 0.5 m deep. The +y arm points at the pipe, so its tip is 20 m from the pipe axis |
| Pipe | DN 300 (OD 323.9 mm), steel σ = 5e6 S/m, μr 100/300/1000, axis 1.5 m deep at y = 50 m, x = 0 to 1000 m, 2 m segments |
| Coating | 3LPE, 3 mm, εr 2.3, σ = t/r_c from the specific coating resistance r_c = 1e4 (aged), 1e5, 1e7 Ω·m² (new) |
| Pipe ends | `isolated` (insulating joints, nothing attached) or `earthed`: a 20 m bare 70 mm² copper wire from each end, running away from the line to 1 m depth |
| Coupling | `numerics.crossMedium: "transmission"` (ADR 0029), and off for comparison |

**Fault study.** A phase-to-tower fault: 10 kA at 50/60 Hz injected at the
tower top against remote earth. The aerial conductor ends in 10 Ω to remote
earth at each end, standing for the rest of the shield-wire and tower ladder.
Results scale linearly with the fault current. The phase conductor that carries
the fault current along the line is absent, because its inductive coupling to
the pipe is exactly the earth-return term the model lacks (theory.md §5.1).

**Lightning study.** A first stroke to the tower top: one-term Heidler,
31 kA peak, τ1 = 1.8 µs, τ2 = 95 µs, n = 10. The aerial conductor is matched at
both ends (508.2 Ω = 60 acosh(h/a)), so the two spans behave as infinite lines.
NLT, 1024 samples to 2 MHz (256 µs). Peaks are read on the first 100 µs, before
the record-end artefact of the NLT (theory.md §8).

**Coating voltage.** In the model the leakage current I_t = i1 + i2 of a coated
segment crosses the layer impedance z_c = ln(r_c/r₀)/(2π(σ_c + sε_c)·l) in
series with the soil path (theory.md §4.3, `fillCoatedSelf`). The voltage across
the coating, pipe minus the soil just outside it, is therefore I_t·z_c exactly.
The fault study evaluates that phasor. The lightning study integrates the
leakage current of each segment through the layer's G ∥ C,
C dV/dt + G V = I_t (trapezoidal rule).

## Fault results (60 Hz, 10 kA at the tower)

Footing: 1.92 kA (19 % of the fault current) enters the soil through the
counterpoise. The rest leaves along the aerial conductor. The footing GPR is
40.6 kV and the footing resistance 21.09 Ω.

| Ends | r_c (Ω·m²) | Pipe potential (kV) | Coating voltage, max (kV), at x ≈ 500 m | Touch at a fitting at x = 500 m (kV) | Touch at the ends (kV) |
| --- | --- | --- | --- | --- | --- |
| isolated | 1e4 | 1.71 | 4.03 | 4.32 | 1.17 |
| isolated | 1e5 | 1.72 | 4.57 | 4.75 | 1.24 |
| isolated | 1e7 | 1.72 | 4.64 | 4.80 | 1.25 |
| earthed | 1e4 | 1.45 | 4.25 | 4.55 | 0.66 |
| earthed | 1e5 | 0.82 | **5.44** | 5.62 | 0.26 |
| earthed | 1e7 | 0.48 | **5.87** | 6.03 | 0.09 |

The soil potential on the coating surface peaks at 6.3 kV opposite the
+y arm. The pipe potential is uniform to 0.1 % over the 1 km: at 60 Hz the
steel is an equipotential next to the coating resistance. "Touch" is the
`observation.touch` output: the pipe node voltage minus the mean surface
potential on a 1 m circle around it, the hand-to-feet voltage at a test post or
valve.

- **Earthing the ends raises the coating stress near the tower.** The bare
  terminations hold the pipe near remote earth, so the full soil potential
  rise appears across the coating: 5.44 kV against 4.57 kV isolated
  (r_c = 1e5). With a new coating (1e7) the pipe drains even less current
  through its own coating and sits lower still, giving 5.87 kV. Above the
  5 kV of NBR 16563-1 in both cases; isolated, it stays below. Earthing lowers
  the touch voltage at the ends (0.26 kV against 1.24 kV). This is the
  usual trade-off of AC mitigation at a GPR zone.
- **μr has no effect** (under 0.1 % between 100, 300 and 1000): the pipe's
  longitudinal impedance does not matter for conductive coupling over 1 km.
  It will matter for the inductive part.
- **The air–soil coupling has no effect** on this conductive case (identical
  to the printed precision with `crossMedium` off): the tower current is
  orthogonal to the pipe, and the aerial current's inductive coupling to the
  pipe vanishes at 60 Hz in the present model (Phase 14 item 1.3).
- **Coating voltage and touch voltage agree** to a few per cent at the GPR
  zone (5.44 kV against 5.62 kV), as Ribeiro et al. note for an outcrop without
  an AC-decoupled earthing.

**Consistency check.** The soil potential at 51 observation points on the
coating surface (ψ at r_c, line side, every 20 m) subtracted from the pipe
voltage reproduces I_t·z_c to 0.5 % at the GPR zone and 1–2 % along most of
the pipe. The worst point is at the pipe end, 7 % (219 V against 236 V), where
the observation point at x = 0 is compared with the first segment's midpoint at
x = 1 m.

## Lightning results (31 kA first stroke to the tower)

Tower top 1.55 MV, footing GPR 556 kV.

| Variant | Pipe potential, peak at x = 500 m (kV) | at the ends (kV) | Coating voltage, peak at x = 500 m (kV) | time of that peak (µs) | Coating voltage, peak at x = 9 m (kV) |
| --- | --- | --- | --- | --- | --- |
| earthed, r_c 1e5, μr 300 (base) | 28.6 | 16.5 | **50.2** | 48 | 12.3 |
| base, `crossMedium` off | 28.0 | 16.5 | 50.2 | 48 | 12.3 |
| isolated ends | 28.6 | 20.0 | 50.2 | 48 | 17.4 |
| r_c 1e4 (aged) | 28.7 | 14.8 | 43.4 | 44 | 9.9 |
| r_c 1e7 (new) | 28.6 | 16.8 | 51.1 | 49 | 12.6 |

Time series of the base case (kV):

| t (µs) | footing GPR | pipe at x = 500 m | pipe end | coating at x = 499 m |
| --- | --- | --- | --- | --- |
| 2 | 430 | 7.7 | 0.0 | 0.0 |
| 4 | 545 | 25.7 | −1.3 | −1.8 |
| 10 | 538 | 24.8 | 0.0 | −15.9 |
| 20 | 493 | 15.4 | 1.0 | −35.1 |
| 48 | 374 | 3.6 | 15.2 | −50.2 |
| 100 | 221 | 5.5 | 6.9 | −33.8 |

(The coating voltage is pipe minus soil; negative means the soil is above the
pipe.)

- **The coating stress peaks late, on the tail, not on the front.** On the
  front the pipe follows the soil next to it: its coating capacitance is
  7 nF/m, so at 4 µs the mid-pipe potential is 25.7 kV and the coating sees
  1.8 kV. The charge then spreads along the pipe, which behaves as a slow
  lossy line (C′ ≈ 7 nF/m; it equalises over the 500 m to each end in about
  40 µs), and the pipe falls back while the soil around it follows the
  footing current. At the peak, 50 kV at 48 µs, the soil next to the pipe
  is at about 54 kV (pipe 3.6 kV plus 50.2 kV across the coating). That is
  close to the quasi-static value: the 60 Hz ratio of the soil potential on
  the coating to the GPR (6.3/40.6), applied to the 374 kV GPR at 48 µs,
  gives 58 kV.
- **The coating stress is set by the stroke's tail and charge** more than by
  its peak or steepness. A shorter tail (subsequent stroke) would give much
  less; a long-tail first stroke more.
- **Pipe ends do not matter near the tower** within 100 µs: earthed or
  isolated give the same 50.2 kV at x = 500 m. They change the far end, where
  isolated ends float higher (17.4 kV against 12.3 kV of coating voltage).
- **An aged coating limits the stress** (43 kV at r_c = 1e4): its
  time constant ε/σ = 68 µs now bleeds the charge within the event. For 1e5
  and above, τ ≥ 0.68 ms and the coating is a capacitor.
- **The air–soil coupling** moves the pipe potential near the tower by 2.4 %
  (28.6 against 28.0 kV) and the coating voltage by under 0.1 %. The
  aerial-to-pipe path is small next to conductive coupling through 20 m of
  soil from the counterpoise.

**Record length.** The base case on a record twice as long (2048 samples,
512 µs, same Nyquist) gives the same tower-top voltage, footing GPR and
coating-voltage peaks to better than 0.01 %, and the same peak time (48.0 µs),
so the 256 µs record and the 100 µs reading window are adequate.

## Reference points

- **Footing resistance.** Dwight's four-point-star formula (arm 30 m, wire
  radius 4.72 mm, 0.5 m deep; via Sunde [10]) gives 21.78 Ω. The model gives
  21.09 Ω at 60 Hz, −3.2 %, with the tower and the pipe present.
- **Ribeiro et al.** (XXV SNPTEE 2019, GLT/34; not yet in references.md)
  study a 500 kV line crossing and paralleling a pipeline, with the same footing
  type (four 90 m radial counterpoises, 2500 Ω·m, 20.4 Ω; Dwight gives 20.6 Ω
  for 4.72 mm wire 0.5 m deep). Their phase-to-ground faults at a tower near the
  pipe give coating voltages of 2.6 to 10.7 kV against the 5 kV limit of
  NBR 16563-1, with conductive and inductive coupling together. The 4–6 kV
  here, conductive only, for 1.9 kA in the footing, is the same order. Their
  profiles are figures only, and the case data is incomplete, so this is a
  plausibility point, not a comparison.

## What this study does not cover

- **Inductive (earth-return) coupling at 60 Hz** from the faulted phase and
  the shield-wire return current: Phase 14 item 1.3. A real fault adds it
  to the conductive part, and over a long parallel it often dominates. The
  fault numbers above are a lower bound for that reason, and only for the
  conductive share.
- **Soil ionisation and arcing.** The model is linear. A 556 kV GPR on 30 m
  arms in 1000 Ω·m ionises the soil near the arms (lower GPR) and can arc
  towards the pipe; the 20 m from the arm tip to the pipe is likely out of
  arc reach for 31 kA, but that is not checked here.
- **Impulse withstand of the coating.** The 5 kV of NBR 16563-1 is a
  power-frequency fault criterion. The impulse withstand of 3LPE is a
  different number, to source before comparing the 50 kV with it.
- **AC corrosion** (steady-state current density at holidays, ISO 18086): needs
  the operating-current inductive coupling, item 1.3.
- **DC decouplers, two-layer soil, crossings at an angle**: ROADMAP Phase 14
  item 2 and Phase 15.
