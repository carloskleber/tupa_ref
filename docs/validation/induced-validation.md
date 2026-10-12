# Lightning-induced voltages: the Nucci et al. base case (ROADMAP Phase 10d)

Validation of the induced-voltage anchor of theory.md §4.6.5 / §9 item 9
([ADR 0028](../adr/0028-induced-voltages-and-lumped-branch.md)). Run
2026-10-12 with the Fortran reference (`-O2 -fopenmp`), 12 threads, about
25 s per case. Every number below comes from
[induced_checks.py](induced_checks.py); nothing was computed by hand.

```
python3 docs/validation/induced_checks.py --fortran-bin <Tupa executable> [--only base,sweep,fig14,conv,interior,internal,lossy]
```

Needs numpy only. The full run takes about 15 minutes.

## Reference and method

Reference: Nucci, Rachidi, Ianoz & Mazzetti, IEEE Trans. EMC 35(1), 1993
(references.md [75]): a 1 km, 10 m high single wire, lossless, matched at both
ends, over perfect ground. The stroke is 50 m from the line centre. The channel
current is the two-term Heidler subsequent stroke (12.01 kA, 40.08 kA/µs,
recomputed in theory.md §4.6.1), the return-stroke model is MTL with
$v = 1.3\times10^8$ m/s and $\lambda$ = 1.7 km. The reference values are the
paper's Tables III and IV and its Fig. 14, which are tabulated; no curve is
digitised.

TUPÃ model: the channel of §4.5 (free-standing at the origin, 3 km, 3 cm
radius, `speed: 1.3e8` calibrated, $R'$ = 2 Z_c/λ = 1.7 Ω/m), the line a 5 mm
wire with 10 m segments, a 497.6 Ω `branch` at each end ($= 60\cosh^{-1}(h/a)$;
the paper does not state the wire radius, and its companion [78] gives about
500 Ω for the same line). NLT, 1024 samples to 10 MHz (dt = 50 ns). The metrics
are measured on the first 12 µs: the peak, the steepest sample-to-sample slope,
the 30–90 % rise and the time from the 50 % point of the front to the 50 % point
of the tail.

- **Variant A** (decided baseline): each resistor connects the wire end node to
  remote earth. This is the Rusck boundary condition without the riser term
  (theory.md §4.6.3). Observed at the wire end.
- **Variant B**: a vertical wire from the line end down to the ground, resistor at
  its foot — the reference's boundary condition, and the one the pass criteria
  gate. Observed at the foot (`Fw`, `Fe`; the voltage across the resistor).

The termination observables are the nodal scalar potential $\varphi$ (variant A, at
the wire end) and the voltage at the foot of the riser (variant B). An interior node
reports $\varphi$ by default, which differs from the paper's total voltage by the
$\int \partial A_z^i/\partial t\,dz$ term; `signal.totalVoltage` adds it (section
[Total voltage along the line](#total-voltage-along-the-line)). The committed
variant B case sets it.

## Base case

| Case | U_p (kV) | max dU/dt (kV/µs) | t30–90 (µs) | t50 (µs) | min (kV) | reference U_p / dU/dt / t30–90 / t50 |
| --- | --- | --- | --- | --- | --- | --- |
| variant A | 60.4 | 150 | 0.30 | 1.58 | −0.99 | 66 / 190 / 0.5 / 1.4 |
| variant B | 62.9 | 154 | 0.30 | 1.61 | −0.94 | 66 / 190 / 0.5 / 1.4 |

Against the MTL base row: A −8.5 % in peak and −21.0 % in steepness; B −4.6 %
and −18.8 %. The MTL–TCS spread in the same paper (74 kV, 260 kV/µs against
66 kV, 190 kV/µs) is 12 % and 37 %; both variants are inside it. The waveform
is unipolar and positive; the rise is faster than the reference's (0.30 against
0.5 µs) and the tail a little longer.

## Trends (Table III)

Variant B; variant A's peak in brackets. The current for each row is the base
two-term Heidler current with the amplitudes scaled by $a$ and the front time
constants by $b$, solved to the peak and maximum slope of the row (the paper
fits its own coefficients and does not print them).

| Case | U_p (kV) | max dU/dt (kV/µs) | t30–90 (µs) | t50 (µs) | min (kV) | reference U_p / dU/dt / t30–90 / t50 |
| --- | --- | --- | --- | --- | --- | --- |
| I_p = 4.6 kA | 24.7 (A: 24.1) | 71 | 0.29 | 1.56 | −1.06 | 28 / 105 / 0.3 / 1.1 |
| base | 62.9 (A: 60.4) | 154 | 0.30 | 1.61 | −0.94 | 66 / 190 / 0.5 / 1.4 |
| I_p = 30 kA | 139.6 (A: 133.7) | 209 | 0.48 | 1.81 | −0.32 | 133 / 225 / 0.7 / 2.1 |
| di/dt = 12 kA/µs | 52.2 (A: 49.9) | 63 | 0.59 | 2.08 | −2.36 | 50 / 65 / 0.8 / 2.2 |
| di/dt = 120 kA/µs | 65.9 (A: 65.3) | 188 | 0.30 | 1.47 | −2.96 | 72 / 290 / 0.3 / 1.1 |
| v = 0.6×10⁸ m/s | 65.6 (A: 64.5) | 100 | 0.50 | 2.64 | −0.46 | 69 / 120 / 0.7 / 2.2 |
| v = 2×10⁸ m/s | 58.8 (A: 55.1) | 178 | 0.24 | 1.30 | −1.54 | 61 / 220 / 0.3 / 0.8 |

All trends have the reference's direction and ratio. The peak follows $I_p$
(24.7 → 62.9 → 139.6 kV), depends weakly on $di/dt$ (52 → 66 kV for a factor 10)
and falls slightly with the speed; the steepness follows $di/dt$ and the speed.
The steepness is 17–19 % under the reference at the base current and 35 % under
at 120 kA/µs, the edge of the 37 % allowance. The 30–90 % rise is within
0.2 µs of the paper's in every row but is faster in the base row (0.30 against
0.5 µs).

At $v = 2\times10^8$ m/s the calibration of theory.md §4.5 refuses the target
("measured speed does not respond to the loading") for the 3 cm channel, so that
row runs with the closed-form loading ($\kappa$ = 1). This is a limitation of the
calibration metric at speeds near $c$, not of the induced-voltage model; the
row is within 4 % of the paper's peak all the same.

## Formulation test (Fig. 14)

500 m line, stroke on the line's prolongation 50 m from the left end (current,
speed and channel as the base case).

| Variant | U_p at the left end (kV) | dU/dt (kV/µs) | min (kV) |
| --- | --- | --- | --- |
| A | 15.8 | 35 | −0.04 |
| B | 32.4 | 102 | −0.08 |

Reference: 33 kV (Agrawal and Taylor), 19 kV for the total-voltage pair B-1/B-2
that [75] calls Rusck's and [77] attributes to Chowdhuri and Gross.
**Variant B gives the Agrawal value (−1.8 %).** Variant A gives 15.8 kV, about
half: with the stroke on the line's axis the vertical field at the near end is
large, the riser term that A omits is as large as the rest of the voltage, and
the base case happens to be a benign geometry where the term is small. The
agreement of A with the 19 kV value is not a reproduction of the
Chowdhuri–Gross omission (that omission is a missing magnetic source term, not a
missing riser), and should not be read as one. **Conclusion for the project:
variant B is the termination to use; A is correct only as the $h_{riser} \to 0$
limit.**

## Total voltage along the line

The paper's Fig. 9 gives the total voltage at three points of the base line: the
terminations, 250 m from the end and the centre. Only the terminations are
tabulated (Table III and Fig. 10), so the other two rows are read off the figure
and are good to a few kV. Variant B, run with `"totalVoltage": true`
(theory.md §4.6.3: $u = \varphi + s\int_0^h A_z^i\,dz$, the incident potential of
the channel only):

| Point | Fig. 9 total (kV, from the figure) | scalar potential $\varphi$ (kV) | total voltage $u$ (kV) | $u$: max dU/dt (kV/µs) |
| --- | --- | --- | --- | --- |
| termination (foot Fw) | 66 | 62.9 | 62.9 | 154 |
| riser top Lw | - | 58.0 | 62.9 | 154 |
| 250 m from the end | 74 | 58.9 | 68.1 | 168 |
| centre | 100 | 51.3 | 81.8 | 237 |

The scalar potential alone is 20 % (250 m) and 49 % (centre) below the figure; the
total voltage is 8 % and 18 % below. The deficit grows towards the centre, the
point closest to the stroke, where the return-stroke model matters most (a loaded
wire with a solved current against a prescribed MTL current); the termination
deficit (−4.6 %) and the front-steepness deficit (−19 %) are of the same order.
The cause was not isolated here and no tolerance is claimed for these two points.

**Riser identity** (an internal check that needs no reference): in the paper's
transmission-line model the total voltage at the top of a riser equals the
resistor voltage at its foot. In the HEM the riser is a wire of its own, and the
two differ by that wire's inductive drop. The total voltage at the riser top
$L_w$ must therefore track the foot voltage up to $L_{riser}\,di/dt$. It does: the
peak is 62.9 kV at both points, and the largest difference over the first 12 µs is
5.0 kV, on the rising front, where a 10 m riser of about 13 µH carrying
$di/dt \approx 0.3$ A/ns predicts about 4 kV. The scalar potential at the same node is 58.0 kV.

Unit test (`fortran/test/test_incident.f90`): the drop $s\int A_z^i dz$ against a
double Simpson quadrature of the retarded potential (400 points along the
channel segment, 100 along the path), from the currents of the same solve, at 10 kHz,
300 kHz and 1 MHz: the difference is 0.03 % of the largest drop.

## Sensitivity (variant B)

| Change | U_p (kV) | max dU/dt (kV/µs) | t30–90 (µs) |
| --- | --- | --- | --- |
| base (line 10 m, channel 5 m ×1.15 ≤ 20 m, 3 km, R' = 2 Z_c/λ, 5 mm wire, dt = 50 ns) | 62.9 | 154 | 0.30 |
| line segments 5 m | 63.1 | 157 | 0.30 |
| line segments 20 m | 62.6 | 150 | 0.32 |
| channel segments 2.5 m ×1.1 ≤ 10 m | 63.1 | 154 | 0.30 |
| channel segments 10 m ×1.2 ≤ 40 m | 61.8 | 162 | 0.29 |
| time step 100 ns (5 MHz, 512 samples) | 62.7 | 149 | 0.31 |
| channel resistance 1.0 Ω/m | 63.3 | 155 | 0.30 |
| channel resistance 2.5 Ω/m | 62.6 | 153 | 0.30 |
| channel resistance 0.5 Ω/m (literature value) | 63.5 | 156 | 0.30 |
| channel length 2 km | 63.0 | 154 | 0.30 |
| channel radius 1 cm | 63.3 | 157 | 0.30 |
| line wire radius 10 mm (Z_c = 456 Ω) | 63.0 | 154 | 0.30 |
| decay constant λ = 1.0 km | 62.4 | 153 | 0.30 |
| channel without attenuation (R' = 0.05 Ω/m) | 63.7 | 156 | 0.30 |

Nothing moves the peak by more than 2.5 % or the steepness by more than 5 %.
The channel attenuation, the segment sizes and the wire radius are not what
separates the result from the reference, so the remaining −19 % in steepness is
the difference between the loaded-wire and the MTL return-stroke models (and the
sampling of a 0.3 µs rise at 50 ns).

## Internal checks

- A 1 kA step into the west end of the matched line (no channel, east end
  497.6 Ω): $u/i$ between 1 and 15 µs is **497.5 Ω**, against
  $60\cosh^{-1}(h/a)$ = 497.6 Ω. It stays flat past the 6.7 µs at which an echo
  from a mismatched far end would arrive: the termination is matched.
- Channel current: the peak longitudinal current at z = 491 m is 0.705 of the
  base value (MTL: $e^{-488/1700}$ = 0.750; −6 %) with $R'$ = 1.7 Ω/m.
- Unit tests of the branch element (`fortran/test/test_branch.f90`): series and
  shunt resistors against the closed forms to 1e-9, the full $R + j\omega L +
  1/(j\omega C)$ at 10 Hz–1 MHz, and the same at a complex frequency.

## Lossy ground (information only)

The line of Rachidi et al. 1996 [78] — σ = 0.001 S/m, ε_r = 10 — with the $\Gamma(\omega)$ images
of theory.md §5, variant B:

| Ground | U_p (kV) | max dU/dt (kV/µs) | min (kV) |
| --- | --- | --- | --- |
| perfect (ideal images) | 62.9 | 154 | −0.94 |
| σ = 0.001 S/m, ε_r = 10 | 69.1 | 217 | −2.47 |

The paper reports that this ground changes the voltage's size, shape and
polarity and shows it as a figure only (no tabulated values), so no number can be
compared. TUPÃ moves the peak by +10 % and the steepness by +41 %, and deepens
the negative lobe, but does not invert the polarity. The wave effect behind the
Cooray–Rubinstein correction is not a quasi-static one (theory.md §4.6.4), so
this is where the model's claim stops: perfect ground.

## What this does and does not show

- The HEM, with a loaded channel and a resistor-terminated line, reproduces the
  induced voltage of the MTL + Agrawal procedure at the terminations to within
  the spread between return-stroke models, for the base case, the Table III
  trends and the formulation test, without a field-to-line coupling model.
- It does so only with the riser (variant B) in the formulation test. The
  decided baseline A is adequate for the base case and not for an on-axis stroke.
- Interior voltages follow the paper's Fig. 9 in shape, polarity and ordering
  (the total grows from the terminations to the centre), but the centre is 18 % low;
  they are compared against figure readings only, not tabulated values.
- The channel is a different return-stroke model from MTL, so a residual of the
  order of the MTL–TCS spread is expected, and the steepness is the quantity
  that carries it.
- Lossy ground, corona and nonlinear terminations are outside the claim.
