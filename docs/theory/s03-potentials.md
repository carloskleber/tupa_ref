# 3. Potentials of a segment source

[Theory index](../theory.md) · previous: [§2 Conventions](s02-conventions.md) · next: [§4 Impedances](s04-impedances.md)

> **Summary.** Two elementary sources generate every field in the model: a
> point that injects current into the medium (scalar potential $\psi$) and a
> short current element (vector potential $\mathbf{A}$). Both have the same
> spherical-wave kernel $e^{-\gamma r}/r$. A segment is a line of such sources
> with uniform strength.

## 3.0 From Maxwell's equations to the two kernels

Take a homogeneous, lossy, unbounded medium in sinusoidal steady state, with
an impressed source current density $\mathbf{J}_s$ (the current forced along
the wires). Maxwell's equations read

$$\nabla\times\mathbf{E} = -j\omega\mu\mathbf{H}, \qquad
\nabla\times\mathbf{H} = W\mathbf{E} + \mathbf{J}_s, \qquad W = \sigma + j\omega\varepsilon .$$

*Step 1, potentials.* Since $\nabla\cdot(\mu\mathbf{H}) = 0$, write
$\mu\mathbf{H} = \nabla\times\mathbf{A}$. Faraday's law then says that
$\mathbf{E} + j\omega\mathbf{A}$ has no curl, so it is a gradient:

$$\mathbf{E} = -j\omega\mathbf{A} - \nabla\psi .$$

The first term is the electric field induced by changing currents (it will
give $Z_\ell$). The second is the field of charges (it will give $Z_t$).

*Step 2, gauge.* $\mathbf{A}$ is fixed only up to a gradient. The Lorenz
choice for a lossy medium, $\nabla\cdot\mathbf{A} = -\mu W\psi$, decouples the
two potentials. Substituting into Ampère's law gives one Helmholtz equation
for each (cf. Stratton [8]):

$$\nabla^2\mathbf{A} - \gamma^2\mathbf{A} = -\mu\mathbf{J}_s, \qquad
\nabla^2\psi - \gamma^2\psi = \frac{\nabla\cdot\mathbf{J}_s}{W}, \qquad \gamma^2 = j\omega\mu W .$$

*Step 3, point sources.* The solution of $\nabla^2 G - \gamma^2 G = -\delta(\mathbf{r})$
that vanishes at infinity is $G = e^{-\gamma r}/(4\pi r)$. Where a source
current ends and pours a total current $I_t$ into the medium,
$-\nabla\cdot\mathbf{J}_s$ integrates to $I_t$. Hence:

**Point injected current** $I_t$ (total current, conduction plus displacement):

$$\psi(r) = \frac{I_t}{4\pi(\sigma + j\omega\varepsilon)} \cdot \frac{\exp(-\gamma r)}{r}$$

This is Coulomb's potential with $q/\varepsilon$ replaced by $I_t/W$. In a
conducting medium a steady current can flow out of a point without charge
piling up, and the potential is set by the current, not by a static charge.

**Current element** $I \,d\ell$ directed along the unit vector $\mathbf{\hat{u}}$:

$$\mathbf{A}(r) = \frac{\mu I \,d\ell}{4\pi} \cdot \frac{\exp(-\gamma r)}{r} \cdot \mathbf{\hat{u}}$$

*Step 4, segments.* A thin cylindrical segment is treated as a line of such
sources along its axis, with the transversal current per unit length
$i_t = I_t/l$ and the longitudinal current $I_\ell$ both assumed **uniform**
over the segment. Field points are taken on the conductor surface (radius
$r_0$), which keeps the self terms finite: the distance between a source
point on the axis and a field point on the surface never falls below $r_0$.

The uniform leakage is consistent with the gauge. For a filament,
$-\nabla\cdot\mathbf{J}_s$ is the rate at which the axial current decreases
along the wire, and a current that falls linearly from $I_1$ to $-I_2$ sheds
exactly $I_t/l$ per metre.

### Check yourself

- Set $\omega = 0$ in $\psi$. The result, $I_t/(4\pi\sigma r)$, is the
  potential of a point electrode in a conducting medium. Integrate
  $\sigma E_r$ over a sphere and recover $I_t$.
- Set $\sigma = 0$. With $I_t = j\omega q$ the formula becomes
  $q\,e^{-j\omega r/c}/(4\pi\varepsilon r)$, the retarded Coulomb potential.
- Verify by substitution that $e^{-\gamma r}/r$ satisfies
  $\nabla^2 G = \gamma^2 G$ for $r > 0$.

## 3.1 Observation-point potentials (GPR, touch and step voltages)

The same segment-source potential, evaluated at an arbitrary observation
point $P$ *off* the conductors, is pure post-processing of a solved study
(no new unknowns): with the solved transversal currents $I_{t,b} = i_{1,b} + i_{2,b}$,

$$\psi(P) = \sum_b \frac{I_{t,b}}{4\pi l_b (\sigma + j\omega\varepsilon)}
\left( g_b(P)\, e^{-\gamma R_b} + \Gamma\, g_{b,i}(P)\, e^{-\gamma R_{b,i}} \right)$$

where $g_b(P) = \int_b d\ell/R$ is the §4.1 geometry factor with the field
point at $P$, $R_b$/$R_{b,i}$ are the distances from $P$ to the midpoint of
the segment and of its mirror image, and $g_{b,i}$ is the factor of the image
segment. This is the $Z_t$ row of §4.1 with the field point moved off the
wire, so it is *consistent* with the solved system: a point on a conductor's
surface reproduces that conductor's voltage to the discretisation error. Two
rules fix the medium constants:

- **Medium of $P$**: $P$ is in soil if $z \le 0$ and in air above, the same
  midpoint rule as for segments (§2). $\sigma + j\omega\varepsilon$, $\gamma$
  and the image coefficient $\Gamma(\omega)$ (§5) are those of that medium.
  By default **only the segments in the same medium as $P$ contribute**, as in
  the impedance fill (§5). A surface point at $z = 0$ then sees only the
  buried electrodes; the air-side leakage of a tower's above-ground part is a
  displacement current, smaller than the soil leakage by about
  $\omega\varepsilon_0/\sigma$. With the cross-medium option of §5.1 the
  segments of the other medium contribute through the transmitted term.
- **Closed form**: for a straight segment $a \to b$ seen from $P$, with
  $s_1$, $s_2$ the axial coordinates of $P$ relative to the two ends and
  $\rho$ its distance to the axis, the factor is
  $g = \operatorname{asinh}(s_1/\rho) - \operatorname{asinh}(s_2/\rho)$
  (the form of §4.2 for an off-axis point, algebraically equal to the
  logarithm there but stable when $P$ is far along the axis). A point nearer
  to the axis than the conductor radius is moved to the surface
  ($\rho \to r_0$), the same regularisation as the self terms (§4.3).

The same quantity is computed by the author's earlier code [3], and TAGS
exposes it as potential, field and step-and-touch post-processing.

The outputs built on $\psi$ (ADR 0027):

- **Surface potential / GPR profile**: $\psi$ on a line or a rectangular grid
  of points, usually at $z = 0$;
- **ground potential rise (GPR)**: the solved node voltage $u_k$ of a
  designated node (potentials are referred to a remote earth);
- **touch voltage** (the definition of [3]): $\max_P |\psi(P) - u_k|$ over a
  circle of 36 points and 1 m radius in the horizontal plane around the node's
  $(x, y)$, at the soil surface — a purely geometric definition, without IEEE
  Std 80's body-circuit and surface-layer derating factors [42];
- **step voltage**: $|\psi(P_2) - \psi(P_1)|$ for two surface points a stride
  (1 m) apart [42]; as a *map*, the maximum over a set of azimuths of
  $|\psi(P) - \psi(P + 1\,\mathrm{m}\,\hat e_\theta)|$ at every grid point.

All are harmonic phasors (peak amplitudes follow the source's), evaluated at
each frequency of a harmonic sweep. A time-domain map would be the inverse
transform of these (§8); the potential is linear in the solved currents, so
it has a transfer function like any other output. The input format does not
offer that yet, and an `observation` block beside a transient-only study is
refused.

The observation-point geometry factor is a closed form for every position,
so no quadrature is involved and the output costs $O(n_{\text{sites}}\,
n_{\text{segments}}\,n_f)$ with no factorisation.

### Check yourself

- Put $P$ on the surface of a conductor. Which row of $Z_t$ does the formula
  reproduce, and why only "to the discretisation error"? (The $Z_t$ row
  averages the potential over the segment; $\psi(P)$ is its value at one
  point.)
- Far from a short electrode, $g_b(P) \to l_b/R$. Show that $\psi$ tends to
  the point-source potential of §3.0 with the total leaked current.
