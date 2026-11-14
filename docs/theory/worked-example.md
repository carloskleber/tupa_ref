# Worked example: a buried rod, by hand and with the solver

[Theory index](../theory.md) · uses §2 to §6

This example carries the smallest possible case through the whole method with
a pocket calculator, and then compares the numbers with the solver. The case
is `common/rod_dc.json`: a vertical copper rod 3 m long, radius 1 cm, with its
top 0.5 m below the surface of a soil of conductivity 0.01 S/m. A current of
1 A is injected at the top. The question is the grounding resistance, i.e. the
voltage of the rod at very low frequency.

| Quantity | Symbol | Value |
| --- | --- | --- |
| Rod length | $l$ | 3 m |
| Rod radius | $r_0$ | 0.01 m |
| Depth of the top | — | 0.5 m ($z$ from −0.5 to −3.5 m) |
| Soil conductivity | $\sigma$ | 0.01 S/m |
| Injected current | $i_e$ | 1 A at the top node |

## Step 1. One segment, two nodes

Take the whole rod as a single segment: $n_s = 1$, $n_n = 2$. The unknowns are
the two node voltages $u_1$ (top), $u_2$ (bottom) and the two end currents
$i_1$, $i_2$, both counted positive into the segment (§2).

At very low frequency the medium immittance is $W = \sigma$ (the displacement
current $j\omega\varepsilon$ is negligible), the propagation factor
$e^{-\gamma R}$ is 1, and the longitudinal impedance $Z_\ell$ is only the DC
resistance of 3 m of copper, about 0.16 mΩ. Against tens of ohms of leakage
path this is zero for practical purposes, so the rod is an equipotential:
$u_1 = u_2 = u$.

## Step 2. The self geometry factor

The potential that the rod's leakage current produces on its own surface needs
$g_{\text{self}}$ (§4.2):

$$g_{\text{self}} = 2\left[l\,\ln\frac{l + \sqrt{l^2 + r_0^2}}{r_0} - \sqrt{l^2 + r_0^2} + r_0\right]$$

With $l = 3$ and $r_0 = 0.01$: $\sqrt{l^2 + r_0^2} = 3.0000167$, the logarithm
is $\ln(600.0017) = 6.39693$, and

$$g_{\text{self}} = 2\,[\,19.19079 - 3.00002 + 0.01\,] = 32.4016\ \text{m}.$$

## Step 3. The image

The soil ends at $z = 0$. Air does not conduct, so no current crosses the
surface. The method of images enforces that by adding a mirror rod above the
surface, from $z = +0.5$ to $+3.5$ m, leaking the same current with the same
sign ($\Gamma = +1$ for a buried conductor at low frequency, §5).

The rod and its image lie on the same vertical line, separated by a gap
$d = 1$ m. For two collinear segments of equal length the double integral of
$1/R$ has a closed form:

$$g_i = \int_0^l\!\!\int_0^l \frac{dz\,dz'}{d + z + z'}
= (2l + d)\ln(2l + d) - 2(l + d)\ln(l + d) + d\ln d$$

With $l = 3$, $d = 1$: $g_i = 7\ln 7 - 8\ln 4 + 0 = 13.6214 - 11.0904 = 2.5310$ m.

## Step 4. The transversal impedance

Equation (§5) for the self term, with $c_E = 1/(4\pi\sigma)$:

$$Z_t = \frac{g_{\text{self}} + \Gamma\,g_i}{4\pi\sigma\,l^2}
= \frac{32.4016 + 2.5310}{4\pi \times 0.01 \times 9} = \frac{34.9326}{1.13097} = 30.887\ \Omega$$

Without the image (a rod in an unbounded soil) the result would be
$32.4016/1.13097 = 28.65\ \Omega$. The surface raises the resistance by 8 %,
because it removes half of the space the current could spread into.

## Step 5. The nodal system

The three statements of §6 for one segment:

1. Voltage drop: $u_1 - u_2 = Z_\ell\,(i_1 - i_2)/2 \approx 0$.
2. Mean potential from leakage: $(u_1 + u_2)/2 = Z_t\,(i_1 + i_2)$.
3. Kirchhoff at the nodes: $i_1 = 1$ A at the top (the injection), $i_2 = 0$
   at the bottom (nothing connected).

So $I_t = i_1 + i_2 = 1$ A leaks out, and $u = Z_t \times 1\ \text{A} = 30.887$ V.
The grounding resistance of the one-segment model is **30.887 Ω**.

## Step 6. Compare with the solver

Set `"segments": 1` in a copy of `common/rod_dc.json` and run it:

```
fortran/bin/Tupa rod_dc1.json
```

The first rows of `results/rod_dc1_results.csv` are the DC point (solved at
1 mHz, see §8):

| Quantity | By hand | Solver, 1 segment |
| --- | --- | --- |
| $u_1$ (top) | 30.8872 V | 30.88716 V |
| $u_2$ (bottom) | 30.8872 V | 30.88708 V |
| $i_1$, $i_2$ | 1, 0 A | 1, 0 A |

The two node voltages differ by 0.08 mV. That is the copper resistance
neglected in step 1: half of the 1 A flows, on average, through 0.16 mΩ.

## Step 7. Refine the mesh

The shipped case uses six segments of 0.5 m. Now the leakage is free to vary
along the rod:

| Segment (top to bottom) | 1 | 2 | 3 | 4 | 5 | 6 |
| --- | --- | --- | --- | --- | --- | --- |
| Leaked current $I_t$ (A) | 0.180 | 0.155 | 0.155 | 0.156 | 0.160 | 0.194 |

The rod voltage drops to 30.827 V, a resistance of **30.827 Ω**, 0.2 % below the one-segment
value. Three things are worth noticing.

- The leakage is larger at both ends. An end segment has neighbours on one
  side only, so less potential is induced on it by the others, and it must
  leak more to reach the same voltage. The bottom end leaks most, since the
  top end has the image close by.
- The one-segment model forces a uniform leakage. A forced distribution
  dissipates more than the natural one, so the coarse model overestimates the
  resistance. Refining the mesh lowers it.
- The change is small. Integral quantities such as the grounding resistance
  are insensitive to the details of the current distribution (§4.1), which is
  why coarse meshes are usable for them.

For orientation, the textbook formula for a rod of the same length with its
top *at* the surface, $R = (\ln(4l/r_0) - 1)/(2\pi\sigma l)$, gives 32.3 Ω.
Burying the top by 0.5 m lowers the resistance by about 4.6 %.

## Step 8. What changes with frequency

Repeat the hand calculation at frequency $f$ with three replacements:

- $\sigma \to W = \sigma + j\omega\varepsilon$ in $c_E$, which makes $Z_t$
  complex (the soil also stores charge);
- each $g$ is multiplied by $e^{-\gamma\bar R}$, with $\bar R = r_0$ for the
  self term and $\bar R = 2h = 4$ m for the image ($h = 2$ m is the depth of
  the rod's midpoint);
- $Z_\ell$ gains the inductive term $j\omega\mu/(4\pi)\,(g_{\text{self}}\,e^{-\gamma r_0} + \Gamma\,g_i\cos\theta_i\,e^{-\gamma\bar R_i})$
  and the skin-effect internal impedance (§4.3), so the rod stops being an
  equipotential. For this vertical rod the image current points the other
  way, $\cos\theta_i = -1$.

At 10 Hz the one-segment solver result is $30.8814 - j0.0058$ V, almost the
DC value. At 1 MHz the skin depth in this soil is about 5 m, the same order
as the rod, and the single segment violates the $\lambda/10$ rule of §4.1.
That is the regime the rest of the theory is about.

## Exercises

1. Recompute steps 2 to 4 for a horizontal conductor 10 m long, radius 5 mm,
   at 0.5 m depth in the same soil (the `portela1997` case). The image is now
   a *parallel* segment at distance $2h = 1$ m; use
   $g_i \approx 2\,[\,l\,\ln\frac{l + \sqrt{l^2 + (2h)^2}}{2h} - \sqrt{l^2 + (2h)^2} + 2h\,]$,
   the same expression as $g_{\text{self}}$ with $r_0$ replaced by $2h$.
   Compare with the Sunde/Dwight formula of §9.
2. Show that for $l \gg r_0$ the self term gives
   $Z_t \approx \dfrac{1}{2\pi\sigma l}\left[\ln\dfrac{2l}{r_0} - 1\right]$,
   the resistance of an isolated rod in an unbounded medium.
3. In step 7, why do segments 2 and 3 leak almost the same current while
   segments 1 and 6 differ? Predict what happens to the profile if the rod is
   buried 10 m deeper, and check with the solver.
4. Run `common/rod.json` (10 Hz to 1 MHz) and plot $|Z_{in}|$. At which
   frequency does it depart from the DC value by 5 %? Relate that frequency
   to the skin depth $1/\text{Re}\,\gamma$ in the soil.
