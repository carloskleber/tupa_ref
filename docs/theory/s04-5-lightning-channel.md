# 4.5 The lightning-channel element

[Theory index](../theory.md) · previous: [§4 Impedances](s04-impedances.md) · next: [§4.6 Induced voltages](s04-6-induced-voltages.md)

> **Summary.** The return-stroke channel is a vertical chain of ordinary
> segments in air. A bare wire would carry the current wave at the speed of
> light, while a real return stroke travels at one third to one half of it.
> Extra series inductance per metre slows the wave to a prescribed speed, and
> a series resistance damps it. Nothing else in the model changes.

Decisions: [ADR 0025](../adr/0025-lightning-channel-and-two-node-sources.md);
measured results: [validation/channel-validation.md](../validation/channel-validation.md).

**Why a loading is needed.** On a thin vertical wire the current wave is
guided by the wire and travels at $c$, whatever the wire radius. Optical
observations of return strokes give $c/3$ to $c/2$ near the ground [125],
because
the real channel is surrounded by a wide sheath of corona charge that raises
its capacitance, and its core is a resistive plasma. A transmission line of
inductance $L$ and capacitance $C$ per metre has $v = 1/\sqrt{LC}$, so the
wave can be slowed by raising $L$, raising $C$, or both.

The channel is a chain of ordinary segments in air. Of everything in §4.1–§5,
only one term changes: a per-unit-length series impedance on the $Z_\ell$
diagonal. Main sources: Baba & Rakov's review [44] and its book-length
update [74, ch. 8], the antenna-model chapter [74, ch. 9], and the
HEM-specific treatment in [45].

**Representation.** Baba & Rakov sort electromagnetic return-stroke models
into six channel representations [74, ch. 8]. Five of them exist only to
slow the current wave from $c$ to the observed return-stroke speed
(typically $c/3$ to $c/2$ near the ground). The decided route is their
*type 2*, a wire loaded by extra distributed series inductance. The other
types do not fit this model:

- Types 3 and 5, and the HEM variant of [45] §4.6.1.5 that scales
  $\varepsilon_r$ and $\mu_r$ around the channel, change the medium around
  the wire. In TUPÃ the air medium is vacuum and is shared by every air
  segment (ADR 0019). A fictitious upper medium would also enter
  $\Gamma(\omega)$ (§5): the frequency-domain antenna model of [74, ch. 9]
  computes its image coefficient with the fictitious $\varepsilon_1$.
- Type 4 (a dielectric coating) needs relative permittivities in the
  hundreds and distorts the radiated fields.
- Type 6 is a fictitious two-wire line.

The HEM offers a native alternative that the antenna models cannot: the
transversal and longitudinal radii are independent. Giving $Z_t$ an enlarged
"corona-sheath" radius while $Z_\ell$ keeps the core radius raises the
capacitance per unit length. On a 1 cm core, [45] measured $0.60c$, $0.58c$
and $0.48c$ for sheath radii of 2, 4 and 8 m. Under the TEM estimate below,
scaling $L$ by a factor $a$ and $C$ by a factor $b$ gives
$v = c/\sqrt{ab}$ and $Z_c = Z_{c0}\sqrt{a/b}$. Inductive loading alone
raises $Z_c$ and the sheath alone lowers it, so using both would let one
case fit a target speed and a target channel impedance. This is an optional
extension; series loading is the decided route.

**Loading term.** The channel puts $z_{ch} = R' + j\omega L'$ in the
internal-impedance slot of §4.3:
$Z_{\text{int}}(a) = z_{ch}\,l_a$. The slot is *replaced*, not added to:
the channel wire is a perfect conductor, so it has no skin-effect term of
its own, and the Fortran skips the Bessel evaluation for loaded segments.
$Z_t$ and all off-diagonal terms are
unchanged, so the loading slows only the transmission-line-mode current.
The antenna-mode part still travels at $c$, so the loaded channel shows
more current dispersion than a dielectric-slowed one, with a weak
precursor at $c$ [74, ch. 9].

**Calibration.** The TEM transmission-line estimate gives the loading
directly [74, chs. 9–10]:

$$L'(z) = \frac{1}{v^2(z)\,C_0(z)} - L_0(z) = L_0(z)\left(\frac{c^2}{v^2} - 1\right),
\qquad L_0 = \frac{\mu_0}{2\pi}\ln\frac{2z}{r_0},\quad C_0 = \frac{2\pi\varepsilon_0}{\ln(2z/r_0)}$$

Take $z$ as the segment-midpoint height above the ground. For
$r_0 = 3$ cm at $z = 500$ m, $L_0 \approx 2.1$ µH/m. This gives
$L' \approx 6.2$ µH/m for $v = c/2$ and 17 µH/m for $c/3$, inside the
1–20 µH/m range used in the literature. A height-dependent $v(z)$, such as
the exponential profile of [74, ch. 9], gives $L'(z)$ by the same formula.

The estimate is a *starting value*, not the final loading. Against the
published full-wave cases (the type-2 rows of Table 8.1 and the FDTD case of
[74, ch. 8], and the ATIL-F case of [74, ch. 9]), it predicts speeds up to
$0.12c$ higher than reported. Equivalently, it overestimates the loading:
11 µH/m from the formula against the 8 µH/m found by trial for
$1.3\times10^8$ m/s. So the element should calibrate $L'$. Run the channel
alone above ideal ground, measure the speed, and adjust (speed falls
monotonically as $L'$ grows). Do this once per (radius, speed, segmentation)
set, and record the calibrated value in the results. The implementation
calibrates a scale factor $\kappa$ on the closed form,
$L'(z) = \kappa\,L_0(z)(c^2/v^2 - 1)$, on the lossless wire (below); it
lands within a few per cent of the closed form for the cases tried
(validation/channel-validation.md).

The speed metric must be fixed in advance. Use Baba & Rakov's: track the
point where the 10–90 % tangent of the front crosses the time axis. Do not
track the onset: the antenna-mode precursor makes a low-threshold tracking
point read close to $c$ [74, ch. 9]. The implementation
(`mChannelCalibration`) excites the channel alone above ideal ground with a
1 µs current ramp, solved by NLT (a steady final value is its native case).
It takes the tangent crossing time of the segment current at $0.2L$ and
$0.8L$ of the calibration length $L = \min(L_{ch}, 2.5\ \text{km})$, and
defines the speed as the height difference over the time difference. The
iteration stops at 0.2 % speed error, accepts the best evaluation within
0.6 % (the metric jitters by about 0.5 %, one time sample plus the NLT
ripple), and rejects $\kappa$ outside $[0.25, 4]$ as a sign that the metric
has locked onto something other than the wave. For a channel with a speed
profile, the target is the harmonic-mean speed between the two heights.
Two practical points from the implementation. First, calibrate the **lossless** wire
($R' = 0$): with resistance the front turns convex and slowly rising, the
tangent metric stops being a property of the wire (Baba & Rakov report speeds
above $c$ for $R' > 2\ \Omega/\text{m}$), and in a trial with $R' = 1\
\Omega/\text{m}$ the measured speed was not even a monotonic function of the
loading. $R'$ afterwards only damps the calibrated wave. Second, the
calibrated value absorbs the discretisation's numerical dispersion, so it is
valid for the segmentation it was found with.

**Channel impedance.** For loaded wires,
$Z_c \approx (c/v)\cdot 60\ln(2z/r_0)$, which is 1.2–1.9 kΩ for
$v = c/2 \ldots c/3$. That lies inside the 0.6–2.5 kΩ range inferred from
current measurements along the Ostankino tower [74, ch. 8]. So the channel
needs no lumped resistor between itself and the struck object.
Dielectric-type representations lower $Z_c$ to 0.2–0.3 kΩ and need one of
several hundred ohms.

**Resistance.** In the literature, inductive loading comes with
$R' \approx 0.5$–1 Ω/m. Part of its job is to damp spurious
high-frequency oscillations. Physical estimates are about 0.035 Ω/m behind
the front and 3.5 Ω/m ahead of it [74, ch. 8; 45]. For a low-loss line
$\alpha \approx R'/(2Z_c)$, and loading raises $Z_c$, so a heavier load
needs a larger $R'$ for the same attenuation. The attenuation formula
printed in [74, ch. 9] drops the $\beta^2$ term and should not be reused.

A stepped profile reproduces all five benchmark features of measured
fields [74, ch. 8]: higher $R'$ in the bottom 0.5 km, about 0.65–1 Ω/m up
to 7.5 km, and 10 Ω/m above. A piecewise $R'$ costs nothing in this slot,
so the element should accept one. Time-varying channel resistance (arc and
hydrodynamic models) and voltage-dependent corona capacitance
[74, ch. 10] fall outside the linearity premise (§10), as soil ionisation
does.

**Excitation.** The channel's lowest node is a separate node from the
strike point. A series ideal current source between the two nodes is then
just a pair of injections: $+I$ at the strike node and $-I$ at the channel
base. The right-hand side sums to zero and ADR 0010 is unchanged. For
strikes to flat ground, current- and voltage-source excitation give
identical channel currents [74, ch. 8].

An ideal current source has infinite internal impedance, though. It
isolates the channel from waves reflected up a struck object, which is
unphysical once those reflections return within the observation window. A
tower 30–60 m tall gives round trips of 0.2–0.4 µs, comparable to
subsequent-stroke fronts. For those cases, use a series voltage source (a
delta gap) between the two nodes. ADR 0016 generalises to this case: the
unit pattern becomes the ±1 dipole, and the constraint applies to
$u_a - u_b$. Both forms are implemented as two-node sources
(`returnNode`, ADR 0025). A channel standing alone above ideal ground has no
object to connect to; its source acts against remote earth.

**Low-frequency limit.** A channel connected only through the source is an
isolated capacitor as $\omega \to 0$. Its potentials go as
$I/(j\omega C_{ch})$, which in time is a step to $Q/C_{ch}$ for the
transferred charge $Q$. Air nodes near the channel pick up the same $1/\omega$
term through the mutual $Z_t$ (electrostatic induction). These values are
*changes* relative to the leader-charged state before the stroke: the
superposition picture of the lumped-excitation models [74, ch. 10]. A
nonzero final value is the case the FFT path's DC-substitute bin
(§8, ADR 0019) handles worst and the NLT path handles natively. So channel
cases should default to NLT, or close the top with a cloud termination. The
discharge-type models use 0.01–1 µF [74, ch. 10].

**Length, top end and segmentation.**

- *Top reflection.* The wave reflected from the open top reaches the base
  at $2L_{ch}/v$, which is 40 µs for 3 km at $c/2$. Choose $L_{ch}$ so this
  falls outside the observation window, or raise $R'$ near the top.
- *Segment length.* The §4.1 $\lambda/10$ rule applies with the *loaded*
  wavelength $v/f$, which is 1.5 m at 10 MHz for $v = c/2$. The channel
  then dominates the unknown count. Published antenna models segment more
  coarsely: 10 m up to 10 MHz [74, ch. 8], or 3.25 m on a $\lambda/4$
  criterion [74, ch. 9]. A channel-specific rule needs a convergence
  study.
- *Grading.* Grade the segments finer toward the strike point, as
  the earlier code [3] does. A purely geometric spacing is not enough: for
  20 segments it ends with a top segment of 1 % of the length next to one of
  about 21 %. Cap the ratio between adjacent segment lengths.
- *Thin-wire bound.* Published equivalent radii run from 1 cm to 0.7 m,
  chosen to set $Z_c$. The §4.1 thin-wire bound ($l \gtrsim 10\,r_0$)
  limits how fine the base segments can go.

**Scope.** With cross-media coupling neglected (the default, §5), the
channel couples only to other air segments: towers, shield wires,
conductors, and the overhead lines of §4.6 whose induced voltages it causes.
For a strike at ground level, buried-electrode outputs then see the channel
only through the source current, which a current source fixes, so GPR, touch
and step voltages do not feel the channel's field. The transmitted-field
option of §5.1 adds that coupling.

**Validation anchors.**

1. *Unloaded channel* (needs only the voltage source). Chen's closed-form
   current along a perfectly conducting vertical cylinder over perfect
   ground, driven by a step voltage. Baba & Rakov's setup: 2 km,
   $r_0 = 0.23$ m, 10 m segments, 5 MV ramp with a 1 µs rise. Time-domain
   MoM, NEC-2 and FDTD all agree with Chen on it [44; 74, ch. 8]. TUPÃ runs
   it with `imageModel: "ideal"` and a zero-length source at the base
   (Chen's own premise) — `common/channel_unloaded.json`. This case tests the
   antenna-mode attenuation that TL models miss. On a lossless wire that
   attenuation comes from field scattering on a conductor of nonzero radius:
   the field has a non-TEM structure, and the pulse loses amplitude with
   height while its tail grows, so the charge transferred stays the same.
   A nonuniform line whose $Z_c$ rises with height reproduces it [100].
   **Result**: the segment
   currents follow Chen's to 1.3–1.5 % of the peak over the window after the
   front (heights 5–905 m), the peak itself within 2.5 %; the discrepancy is
   of the order the cited codes show against each other.
2. *Loaded channel.* Speed against Table 3 of Baba & Rakov [44] (FDTD,
   $r_0 = 0.23$ m, $L' = 2, 4, 8\ \mu\text{H/m}$: $0.60c$, $0.48c$,
   $0.37c$); $Z_c$ against the transmission-line estimate and the
   0.6–2.5 kΩ range. **Result**: $0.57$–$0.58c$, $0.45$–$0.46c$,
   $0.35$–$0.36c$ — within $0.03c$, a little below the FDTD values (the
   source there is a 10 m lumped one). The base impedance of the calibrated
   3 km channel (3 cm, $c/2$, $R' = 0.5\ \Omega$/m) is 12 % below
   $(c/v)\,60\ln(2vt/r_0)$ at 1.5 µs and within 5 % of it from 3 µs on:
   the estimate neglects the front's finite rise and $R'$, which matter
   early. Details in [validation/channel-validation.md](../validation/channel-validation.md).
3. *Induced voltages.* Two cases, both with every segment in air. The
   reduced-scale experiment of Ishii et al., as reproduced by Pokharel et al.
   with NEC-2: a 25 m wire over 0.06 S/m ground, channel at 6 µH/m and
   0.5 Ω/m, $v \approx 0.43c$ [74, ch. 8], with [46] as the HEM counterpart.
   This comparison is open: it needs the measured waveform of the original
   experiment as a digitised figure. The Nucci et al. base case of
   §4.6.5, a 1 km line 10 m high over perfect ground with tabulated reference
   values [75], needs no digitised figure; it is the induced-voltage anchor
   (§9 item 9).

## Check yourself

- With $L_0$ and $C_0$ of the calibration formula, verify that
  $1/\sqrt{L_0 C_0} = c$ at any height, and that adding $L'$ gives the
  stated speed.
- Why does series inductance slow the wave while leaving the radiated
  precursor at $c$? (The loading acts on the current guided along the wire;
  the field radiated by the source region propagates through the air.)
- Estimate the round-trip time of the wave in a 3 km channel at $c/2$, and
  the longest observation window it allows.
