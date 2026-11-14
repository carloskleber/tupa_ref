# 9. Validation anchors

[Theory index](../theory.md) · previous: [§8 Transient response](s08-transient.md) · next: [§10 Positioning](s10-positioning.md)

> **Summary.** A model of this kind is trusted through a ladder of checks:
> analytic limits, internal consistency, agreement with an independent code,
> and agreement with published curves. This chapter lists the anchors and
> their tolerances. The item numbers are stable and cited elsewhere. Execution
> status and measured deviations are in [BENCHMARKS.md](../BENCHMARKS.md) and
> [validation/](../validation/README.md).

Every implementation must reproduce, within stated tolerance:

1. **DC limit, buried horizontal conductor** (length $l$, radius $r_0$, depth
   $h$, soil $\sigma$): grounding resistance from the classical image formula (Sunde/Dwight form, [10, 41])

   $$R = \frac{1}{2\pi \sigma l} \left[ \ln\left(\frac{2l}{r_0}\right) + \ln\left(\frac{l}{h}\right) - 2 + \frac{2h}{l} - \frac{h^2}{l^2} + \ldots \right]$$

   (Dwight's series with wire length $l$ and depth $h$; valid for
   $r_0 \ll h \ll l$) — the low-frequency asymptote of the full model.
   The [worked example](worked-example.md) reproduces this limit by hand.
   The unit test (`test_solve.f90`) compares $|Z_{in}(10\,\text{Hz})|$
   with the three-term truncation (through $-2$) at 15 % tolerance.
2. **Portela 1997 [2]** application curves: harmonic input impedance of a 10 m
   buried conductor, 0.5 m depth, $\sigma = 0.01\, \text{S/m}$, $\varepsilon_r \approx 10$, from 100 Hz to 1 MHz
   (project reference case; 5 % tolerance against tabulated data). No
   tabulated reference data exists for it, only the published equations and
   figures, so the case is not matched directly. Its oracles are the
   cross-code check (item 6) and the published curves of item 7
   ([ADR 0018](../adr/0018-author-interview-decisions-2026-07.md)).
3. **Visacro & Soares 2005 [5]**: formulation reference only. The paper
   carries no data usable for quantitative comparison, so it is not a data
   anchor. The underlying thesis [55] does document an
   experimental comparison (Ch. 5.2: insulator-string voltage waveforms vs.
   Ishii et al.'s 1991 low-amplitude injections on a real tower, valid to
   ~2 µs), but only as figures — same caveat as item 2, so the executable
   oracle remains the cross-code check (item 6).
4. **Internal consistency**: full $Z_{\text{eq}}$ solve vs. reduced $Z_g$ form;
   quadrature geometry factor vs. closed-form parallel/orthogonal formulas;
   reciprocity ($Z_t$, $Z_\ell$ symmetric); passivity ($\text{Re}\{Z_{\text{in}}\} \geq 0$).
5. **Grcev & Heimbach 1997 [18]**: harmonic impedance of square grounding
   grids — exercises many segments, right angles and the image terms; the
   TAGS examples reproduce it, enabling a three-way comparison.
6. **Cross-code check**: TAGS (references.md) is open source, builds locally,
   and accepts the same geometries — run identical cases and compare input
   impedance and node voltages. Compare *physical outputs only*, not raw
   matrices: TAGS assembles $Z_\ell$ with $|\cos\theta|$ and its own incidence
   conventions (a valid convention set paired with its solver, but different
   from §2), and its "immittance" system uses unknowns $(\mathbf{u}, I_\ell, I_t)$
   in a symmetric block layout rather than §6's $(\mathbf{u}, \mathbf{i}_1, \mathbf{i}_2)$.
   Result ([validation/tags-xval.md](../validation/tags-xval.md)):
   below 1 MHz the codes agree to 0.3 % or better once the $|\cos\theta|$
   convention is accounted for (a closed loop oriented around itself
   differs by 4 % at 100 kHz otherwise).
7. **Published-curve comparisons** ([validation/](../validation/README.md)) —
   the executable oracle accepted for the release bar
   ([ADR 0018](../adr/0018-author-interview-decisions-2026-07.md) postscript).
   Digitized figures, harmonic
   and transient: Grcev et al. [23] Fig. 12 (horizontal electrodes, 10 and
   100 m, 30–3000 Ω·m, against the paper's full-wave MoM); Lima et al. [11]
   Figs. 6 and 7 (counterpoise and square grids, 1000 Ω·m); Poljak &
   Doric [35] Fig. 4 (2 m vertical rod, 5400 Ω·m, to 100 MHz); Silva et
   al. [36] Figs. 3 and 4 (60 m electrode, Alipio–Visacro soil [14];
   transient GPR under the De Conti & Visacro double-peaked first-stroke
   current [38]). **Tolerance policy**: a digitized curve carries reading
   error of its own, largest on steep slopes and resonance nulls, so no
   single fixed tolerance applies; each writeup reports its point-by-point
   deviation and names its outliers. Current state: about ±7 % (Lima
   Fig. 7, below 4 MHz); ±10 % (Poljak); ±11.5 % excluding the
   slope-sensitive points on one resonance and two knees (Grcev); up to
   ~14 % on a mid-band knee (Silva Fig. 3, §4.1); about 5 % on the GPR
   humps (Silva Fig. 4); a systematic −13 to −17 % offset for Lima Fig. 6,
   whose geometry the paper only partly states. The 5 % of item 2 applies
   to tabulated data only.
8. **Lightning channel** (§4.5): Chen's analytic current
   on an unloaded cylinder over perfect ground, and the speed of an
   inductance-loaded wire against Baba & Rakov's Table 3 [44] —
   [validation/channel-validation.md](../validation/channel-validation.md).
9. **Lightning-induced voltages** (§4.6.5): the Nucci et
   al. base case [75] — a 1 km, 10 m high line matched at both ends, a
   subsequent stroke 50 m from its centre, perfect ground — against the
   tabulated peak and steepness of the MTL + Agrawal procedure, with the
   parametric trends of its Table III and the formulation test of its Fig. 14.
   The tolerance is the spread between return-stroke models in the same
   paper (12 % in peak, 37 % in steepness), because the loaded channel is a
   different return-stroke model from MTL. The anchor is met by the
   like-for-like variant (§4.6.5); results in
   [validation/induced-validation.md](../validation/induced-validation.md).
   Candidates for a measured anchor, not yet digitised: the Florida records
   of Rubinstein et al. [83] (distant strokes, wave-tilt field), the
   triggered-stroke data of Barker et al. [84] (145 m, 63 strokes) and the
   São Paulo reduced-scale networks [93].
10. **Element-level closed forms.** Each added mechanism is pinned by a limit
    that can be worked by hand: a lumped `branch` in series and in parallel
    with a grounded rod (§6); a coating of the soil's own medium, a conductive
    shell and an insulating shell (§4.3); a round-wire cross-section entry
    against the Bessel formula (§4.3); the cross-medium kernel for identical
    media (§5.1); the matched line's $Z_c = 60\ln(2h/a)$ (§4.6.3).

## How to read a comparison

- A tolerance is meaningful only with its oracle. A hand formula such as
  Dwight's is itself approximate (item 1 uses 15 %), a digitised curve
  carries reading error (item 7), and another code shares some of the same
  premises (item 6).
- Agreement of two implementations of *this* model to $10^{-6}$ (the golden
  fixtures of `common/`) shows that they implement the same equations. It
  says nothing about whether the equations describe the physics.
- A result that is finite and reproducible can still be wrong. Compare
  magnitudes with a hand estimate, and look at the whole time record before
  judging a transient (§8.6).
