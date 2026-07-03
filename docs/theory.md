# TUPÃ — Theory Reference

This document states the electromagnetic model implemented by TUPÃ: the Hybrid
Electromagnetic Model (HEM), an application of the Method of Moments (MoM) to
lightning and grounding-system transients. It is the normative reference for
every implementation (Fortran, and future Python/Rust): where code and this
document disagree, one of them has a bug — and the discrepancy must be resolved
before the code is merged.

The formulation follows Portela [1] and the author's dissertation [3], with the
matrix assembly as used by Salari et al. [4] and validated against Visacro &
Soares [5]. See [references.md](references.md) for the full bibliography.

---

## 1. Model overview

A network of thin cylindrical conductors (in air and/or buried in soil) is
discretised into **segments** (electrodes) connected at **nodes**. Each segment
`j` carries:

- a **longitudinal current** flowing along its axis, represented by the currents
  at its two ends, `I₁(j)` and `I₂(j)`;
- a **transversal current** `Iₜ(j)` leaking from its lateral surface into the
  surrounding medium (conduction + displacement current), assumed uniformly
  distributed along the segment.

Each node `k` has a scalar potential (voltage to remote earth) `u(k)`.

The electromagnetic coupling between every pair of segments is condensed into
two full complex matrices at each angular frequency ω:

- **Zₜ** — transversal (nseg × nseg): mean scalar potential on segment *a* per
  unit transversal current injected by segment *b*;
- **Zₗ** — longitudinal (nseg × nseg): voltage drop along segment *a* per unit
  longitudinal current in segment *b*.

Circuit-type (Kirchhoff) relations between node voltages and segment currents
close the system, which is solved once per frequency. Time-domain responses are
obtained by inverse Fourier transform of the frequency sweep.

This is a frequency-domain, full-wave method restricted to thin-wire structures:
retardation and attenuation in dissipative media are kept, but the current
distribution within each segment is approximated as uniform (pulse basis
functions, point matching on averages — the classic MoM compromise, cf.
Harrington [6], Gibson [7]).

---

## 2. Conventions

Sign conventions are the single largest source of bugs in this class of code.
TUPÃ adopts **one** convention set; every routine must conform to it.

- **Time factor**: $e^{+j\omega t}$ (electrical engineering convention). A quantity
  $X$ means the phasor of $x(t) = \text{Re}\{X e^{j\omega t}\}$.
- **Medium immittance** (per volume element): $\sigma + j\omega\varepsilon$. All media are linear,
  isotropic, non-magnetic unless stated ($\mu = \mu_r\mu_0$).
- **Propagation constant**:

  $$\gamma = \sqrt{j\omega\mu (\sigma + j\omega\varepsilon)} = \alpha + j\beta, \quad \text{Re}\, \gamma \geq 0$$

  with the principal square root, so that the **propagation factor**

  $$F(R) = e^{-\gamma R}$$

  decays with distance ($\alpha > 0$ in any lossy medium) and delays the phase.
- **Geometry**: right-handed Cartesian axes, `z` up. The air–soil interface is
  the plane `z = 0`; air occupies `z > 0`, soil `z < 0`.
- **Segment end currents**: both $I_1$ and $I_2$ are positive **into** the
  segment (from the node toward the segment). Hence

  $$I_\ell = \frac{I_1 - I_2}{2} \quad \text{(mean longitudinal current)}$$
  $$I_t = I_1 + I_2 \quad \text{(total transversal / leakage current)}$$

> **Mapping to the literature.** Portela [1] and the dissertation [3] use the
> physics convention $e^{-i\omega t}$, in which the immittance appears as $\sigma - i\omega\varepsilon$,
> the wavenumber is $k = \sqrt{\mu\varepsilon\omega^2 + i\omega\mu\sigma}$ and the propagation factor is
> $e^{+ikR}$. The two notations are complex conjugates of each other:
> $\gamma = j \cdot \text{conj}(k)$, and $e^{-\gamma R} = \text{conj}(e^{+ikR})$. Results (impedance moduli,
> time-domain waveforms) are identical; phases are conjugated. Any code mixing
> the two conventions in a single expression is wrong.

---

## 3. Potentials of a segment source

For a homogeneous, lossy, unbounded medium and sinusoidal steady state, the
retarded potentials of the elementary sources are (Lorenz gauge):

**Point injected current** $I_t$ (total current, conduction + displacement):

$$\psi(r) = \frac{I_t}{4\pi(\sigma + j\omega\varepsilon)} \cdot \frac{e^{-\gamma r}}{r}$$

This replaces $q/\varepsilon$ of electrostatics by $I_t/(\sigma + j\omega\varepsilon)$ — the continuity
equation in a dissipative medium ties injected current to charge.

**Current element** $I \,d\ell$ directed along the unit vector $\mathbf{\hat{u}}$:

$$\mathbf{A}(r) = \frac{\mu I \,d\ell}{4\pi} \cdot \frac{e^{-\gamma r}}{r} \cdot \mathbf{\hat{u}}$$
$$\mathbf{E} = -j\omega\mathbf{A} - \nabla\psi$$

A thin cylindrical segment is treated as a line of such sources along its axis,
with the transversal current per unit length $i_t = I_t/l$ and the longitudinal
current $I_\ell$ both assumed **uniform** over the segment. Field points are taken
on the conductor surface (radius `r₀`), which regularises the self terms.

---

## 4. Mutual impedances

Let segment *a* have endpoints $A_1A_2$, length $l_a$, and segment *b* endpoints
$B_1B_2$, length $l_b$, both in the same medium. $R_{ab}$ is the distance between
the integration points on the two axes (or axis-to-surface for self terms).

**Transversal**: averaging the scalar potential produced by *b* over *a*, and
dividing by the total transversal current of *b*:

$$Z_t(a,b) = \frac{1}{4\pi l_a l_b (\sigma + j\omega\varepsilon)} \iint \frac{e^{-\gamma R_{ab}}}{R_{ab}} \, d\ell_a \, d\ell_b$$

**Longitudinal**: from the projection of the vector potential of *b* on *a*
(electric field $-j\omega\mathbf{A}$ integrated along *a*):

$$Z_\ell(a,b) = \frac{j\omega\mu}{4\pi} \iint \frac{e^{-\gamma R_{ab}}}{R_{ab}} \, (d\ell_a \cdot d\ell_b)$$

For straight segments $d\ell_a \cdot d\ell_b = \cos \theta_{ab} \, d\ell_a \, d\ell_b$ with $\theta_{ab}$ the
(constant) angle between the segment directions.

### 4.1 Geometry-factor separation

Evaluating the complex double integral at every frequency is expensive. TUPÃ
uses the separation introduced in [3]: write $R_{ab} = \bar{R}_{ab} + \Delta R$, where $\bar{R}_{ab}$
is the distance between segment midpoints. When the propagation factor varies
little over the segments ($|\gamma| \cdot \Delta R \ll 1$), it can be pulled out of the integral
at the mean distance:

$$Z_t(a,b) \approx \frac{e^{-\gamma\bar{R}_{ab}}}{4\pi l_a l_b (\sigma + j\omega\varepsilon)} \cdot g(a,b)$$
$$Z_\ell(a,b) \approx \frac{j\omega\mu e^{-\gamma\bar{R}_{ab}}}{4\pi} \cdot \cos \theta_{ab} \cdot g(a,b)$$

with the **geometry factor**, a purely real, frequency-independent quantity:

$$g(a,b) = \iint \frac{d\ell_a \, d\ell_b}{R_{ab}}$$

The matrices of geometry factors $G$, mean distances $\bar{R}$, direction cosines
$\cos \theta$, and inverse length products $1/(l_a l_b)$ are computed **once** per
geometry; the frequency loop then only evaluates medium constants and
propagation factors. This is the decisive optimisation over the plain HEM
(where the full integrals are redone per frequency, cf. [5]) and bounds the
segment length: the approximation requires segments short compared to the
wavelength in the medium (in practice $\lesssim \lambda/10$, a few metres for soil at 1 MHz).

### 4.2 Evaluating the geometry factor

- **General position**: adaptive 2-D quadrature (nested Gauss–Kronrod 7/15) of
  $1/R_{ab}$ over both segments. The integrand is smooth unless segments touch.
- **Parallel segments** and **orthogonal segments**: closed-form expressions
  exist (logarithms and arctangents of the corner distances); see [3, annex]
  for the derivation. Used both as fast paths and as quadrature test oracles.
- **Coincident (self) factor**: for a segment of length $l$ and radius $r_0$,
  integrating axis-to-surface:

  $$g_{\text{self}} = 2 \left[ l \ln\left(\frac{l + \sqrt{l^2 + r_0^2}}{r_0}\right) - \sqrt{l^2 + r_0^2} + r_0 \right]$$

  Note $\ln\left(\frac{l+h}{h-l}\right) = 2 \ln\left(\frac{l+h}{r_0}\right)$ for $h = \sqrt{l^2+r_0^2}$, which explains
  the equivalent forms found in the literature.

### 4.3 Self impedances

The self (diagonal) terms combine three effects:

$$Z_\ell(a,a) = Z_{\text{int}} + Z_{\text{ext}} + Z_{\text{interface}}$$
$$Z_t(a,a) = Z_{t,\text{ext}} + Z_{t,\text{interface}}$$

- $Z_{\text{ext}}$ uses the machinery of §4.1 with $g_{\text{self}}$ and $\bar{R} = r_0$ (field point
  on the conductor surface); $Z_{t,\text{ext}} = \frac{e^{-\gamma r_0} g_{\text{self}}}{4\pi l^2 (\sigma+j\omega\varepsilon)}$.
- $Z_{\text{interface}}$ is the image contribution (§5).
- $Z_{\text{int}}$ is the **internal impedance** (skin effect). For a solid cylindrical
  conductor of radius $r_0$, conductivity $\sigma_c$, permeability $\mu_c$:

  $$z_{\text{int}} = \frac{\sqrt{j\omega\mu_c/\sigma_c}}{2\pi r_0} \cdot \frac{I_0(\rho)}{I_1(\rho)}, \quad \rho = r_0 \sqrt{j\omega\mu_c \sigma_c}$$
  $$Z_{\text{int}} = z_{\text{int}} \cdot l$$

  with $I_0, I_1$ modified Bessel functions. For tubular conductors (inner
  radius $r_i$) the standard Schelkunoff expression with $I$ and $K$ Bessel
  functions applies; see [3].

---

## 5. The air–soil interface: image method

Two half-spaces (air: $\sigma \approx 0, \varepsilon_0$; soil: $\sigma_s, \varepsilon_s$) meet at $z = 0$. The
boundary condition is represented by **images**: each segment has a mirror
image reflected through $z = 0$, and every impedance term gains an image
contribution evaluated with the image geometry factor $g_i$, image mean distance
$\bar{R}_i$, and the propagation constant of the medium containing the *real*
segments:

$$Z_t(a,b) = c_E \cdot \left( e^{-\gamma\bar{R}} g \pm \Gamma_t e^{-\gamma\bar{R}_i} g_i \right) / (l_a l_b)$$
$$Z_\ell(a,b) = c_M \cdot \cos \theta \cdot e^{-\gamma\bar{R}} g \pm c_M \cdot \cos \theta_i \cdot \Gamma_\ell e^{-\gamma\bar{R}_i} g_i$$

where $c_E = \frac{1}{4\pi(\sigma+j\omega\varepsilon)}$, $c_M = \frac{j\omega\mu}{4\pi}$, and $\theta_i$ is the angle with the
image direction (the image of a segment reverses the sign of the z-component of
its direction vector).

In the current implementation the reflection coefficients are taken at their
ideal limits, which gives the sign rules:

| Configuration            | Transversal image | Longitudinal image |
| --- | --- | --- |
| Both segments in **soil** | `+` (add)        | `+` (add)          |
| Both segments in **air**  | `−` (subtract)   | `−` (subtract)     |
| Segments in different media | coupling neglected (set to 0) | idem |

Rationale: for buried conductors the air above is (nearly) non-conducting, so
the leakage current sees a "current mirror" of equal sign; for conductors in
air above a conducting soil, the soil approaches a potential boundary and the
image charge/current has opposite sign. The general treatment with
frequency-dependent Fresnel-type reflection coefficients $\Gamma(\omega)$ (Portela [1]
§2.4, [3]) is a planned refinement; the cross-media coupling (air segment ↔
buried segment) is second-order and is neglected, as in the original code.

For the **self** terms the "mutual with the own image" appears with distance
$\bar{R}_i = 2h$ (twice the depth/height of the segment centre).

---

## 6. Nodal system

With $n_n$ nodes and $n_s$ segments, define the incidence matrices (all sparse,
entries 0 elsewhere):

- **A** ($n_s \times n_n$): row $j$ has $-1$ at column $n_1(j)$, $+1$ at $n_2(j)$;
- **B** ($n_s \times n_n$): row $j$ has $-\frac{1}{2}$ at both $n_1(j)$ and $n_2(j)$;
- **C** ($n_n \times n_s$): column $j$ has $+1$ at row $n_1(j)$;
- **D** ($n_n \times n_s$): column $j$ has $+1$ at row $n_2(j)$.

Three physical statements close the model ($\mathbf{u}$ = node voltages, $\mathbf{i}_1, \mathbf{i}_2$ = end
currents, both into the segment; $\mathbf{i}_e$ = external currents injected at nodes):

1. **Longitudinal voltage drop**: $u(n_1) - u(n_2) = Z_\ell \cdot I_\ell$

   $$\mathbf{A} \mathbf{u} + \frac{1}{2} Z_\ell \mathbf{i}_1 - \frac{1}{2} Z_\ell \mathbf{i}_2 = 0$$

2. **Mean surface potential from leakage**: $\frac{u(n_1) + u(n_2)}{2} = Z_t \cdot I_t$

   $$\mathbf{B} \mathbf{u} + Z_t \mathbf{i}_1 + Z_t \mathbf{i}_2 = 0$$

3. **KCL at each node** (currents leave the node into the segments):

   $$\mathbf{C} \mathbf{i}_1 + \mathbf{D} \mathbf{i}_2 = \mathbf{i}_e$$

Stacked as one linear system $Z_{\text{eq}} \mathbf{x} = \mathbf{y}$ of dimension $n_n + 2n_s$:

$$\begin{bmatrix}
\mathbf{A} & \frac{1}{2}Z_\ell & -\frac{1}{2}Z_\ell \\
\mathbf{B} & Z_t & Z_t \\
\mathbf{0} & \mathbf{C} & \mathbf{D}
\end{bmatrix}
\begin{bmatrix} \mathbf{u} \\ \mathbf{i}_1 \\ \mathbf{i}_2 \end{bmatrix} =
\begin{bmatrix} \mathbf{0} \\ \mathbf{0} \\ \mathbf{i}_e \end{bmatrix}$$

solved by dense LU (LAPACK `ZGESV`) once per frequency. Voltage sources are
converted to equivalent current injections (or handled by constraint rows) —
see the implementation plan.

**Reduced form.** Eliminating $\mathbf{i}_1, \mathbf{i}_2$ yields the nodal admittance relation used
when only $\mathbf{u}$ is needed and $n_n \ll n_s$:

$$\mathbf{u} = Z_g \mathbf{i}_e, \quad Z_g = \left[ (\mathbf{D}-\mathbf{C}) Z_\ell^{-1} \mathbf{A} + \frac{1}{2} (\mathbf{C}-\mathbf{D}) Z_t^{-1} \mathbf{B} \right]^{-1}$$
$$\mathbf{i}_1 = -\left( Z_\ell^{-1}\mathbf{A} + \frac{1}{2} Z_t^{-1}\mathbf{B} \right) \mathbf{u}$$
$$\mathbf{i}_2 = \left(-Z_\ell^{-1}\mathbf{A} + \frac{1}{2} Z_t^{-1}\mathbf{B} \right) \mathbf{u}$$

This trades one $(n_n+2n_s)^2$ solve for two $n_s \times n_s$ solves plus a $n_n \times n_n$ solve
(cf. [1] eqs. 50–56, [4]). Both forms must give identical results — a useful
consistency test.

> **Sign caveat.** The literature differs in the direction assumed for $\mathbf{i}_2$
> (Portela [1] takes it *out* of the segment at node $n_2$, flipping signs in B,
> D and the $\mathbf{i}_2$ blocks). The table above is self-consistent with the
> both-into-segment convention of §2. Implementations must verify against the
> DC limit (§9), not against any single paper's sign table.

---

## 7. Frequency-dependent soil

Soil $\sigma$ and $\varepsilon$ are strongly frequency dependent between DC and a few MHz;
ignoring this materially changes impulse impedances. TUPÃ models the soil
immittance $W(\omega) = \sigma(\omega) + j\omega \varepsilon(\omega)$ as a minimum-phase-shift system: the real
and imaginary parts are Hilbert-transform pairs, so a power-law model fitted to
measured $\sigma(\omega)$ fixes $\omega\varepsilon(\omega)$ as well (Portela [1] §1):

$$W(\omega) = \sigma_0 + \Delta\sigma \cdot \left[ 1 + j \tan\left(\frac{\pi \alpha}{2}\right) \right] \cdot \left(\frac{\omega}{\omega_0}\right)^\alpha$$

with $\sigma_0$ the low-frequency conductivity, $\alpha \in (0,1)$ the dispersion exponent
and $\Delta\sigma$ the dispersion magnitude at the reference frequency $\omega_0$ (commonly
$2\pi \cdot 1\, \text{MHz}$). The parameters `alpha0` and `kr` carried by `tPortelaSoil` correspond
to this model (`kr` scaling the dispersive parcel, $\tan(\pi\alpha/2)$ tying the
imaginary part). Per [ADR 0007](adr/0007-soil-dispersion-model.md), `tMaterial` admits
several dispersive-soil subtypes side by side, each named after its original
reference — `tPortelaSoil` (implemented first, matches the validation curves),
`tLongmireSmithSoil` (a 13-term Debye expansion, an alternative parametrisation
targeted by lightning studies), `tVisacroAlipioSoil`, etc. All must reduce to the
constant-parameter (`tLinear`) medium as $\omega \to 0$.

---

## 8. Transient (time-domain) response

1. Sample the excitation waveform (e.g. Heidler or double-exponential lightning
   surge) and take its FFT.
2. Solve the frequency-domain system at the required frequencies; form the
   transfer function $H(\omega)$ between injected signal and each observed quantity.
3. Multiply and inverse-FFT.

Practical notes from [1] and [3]: 512–8192 frequencies in $[0, 1\, \text{MHz}]$ suffice
for lightning impulses; for large structures, the smooth behaviour of $H(\omega)$
allows computing a reduced set of frequencies and interpolating (analytic
fitting), drastically cutting run time. Logarithmic frequency spacing is the
project default for broadband sweeps.

---

## 9. Validation anchors

Every implementation must reproduce, within stated tolerance:

1. **DC limit, buried horizontal conductor** (length $l$, radius $r_0$, depth
   $h$, soil $\sigma$): grounding resistance from the classical image formula

   $$R = \frac{1}{2\pi \sigma l} \left[ \ln\left(\frac{2l}{r_0}\right) + \ln\left(\frac{2l}{2h}\right) - 2 + \ldots \right] \quad \text{(Sunde/Dwight form)}$$

   — the low-frequency asymptote of the full model.
2. **Portela 1997 [2]** application curves: harmonic input impedance of a 10 m
   buried conductor, 0.5 m depth, $\sigma = 0.01\, \text{S/m}$, $\varepsilon_r \approx 10$, from 100 Hz to 1 MHz
   (project reference test; 5 % tolerance).
3. **Visacro & Soares 2005 [5]**: HEM validation cases for grounding
   electrodes (harmonic impedance and impulse response).
4. **Internal consistency**: full $Z_{\text{eq}}$ solve vs. reduced $Z_g$ form;
   quadrature geometry factor vs. closed-form parallel/orthogonal formulas;
   reciprocity ($Z_t$, $Z_\ell$ symmetric); passivity ($\text{Re}\{Z_{\text{in}}\} \geq 0$).
