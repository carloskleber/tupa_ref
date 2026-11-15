# 2. Conventions

[Theory index](../theory.md) · previous: [§1 Model overview](s01-model-overview.md) · next: [§3 Potentials](s03-potentials.md)

> **Summary.** One set of sign conventions applies to every formula and every
> routine: time factor $e^{+j\omega t}$, decaying propagation factor
> $e^{-\gamma R}$, $z$ upward with the soil below $z = 0$, and both end
> currents of a segment positive into it.

Sign conventions are the single largest source of bugs in this class of code.
TUPÃ adopts **one** convention set, and every routine must conform to it
([ADR 0008](../adr/0008-sign-conventions-and-validation.md)).

- **Time factor**: $\exp(+j\omega t)$ (electrical engineering convention). A quantity
  $X$ means the phasor of $x(t) = \text{Re}\{X \exp(j\omega t)\}$.
- **Medium immittance** (per volume element): $W = \sigma + j\omega\varepsilon$.
  All media are linear and isotropic, and non-magnetic unless stated
  ($\mu = \mu_r\mu_0$).
- **Propagation constant**:

  $$\gamma = \sqrt{j\omega\mu (\sigma + j\omega\varepsilon)} = \alpha + j\beta, \quad \text{Re}\, \gamma \geq 0$$

  with the principal square root, so that the **propagation factor**

  $$F(R) = \exp(-\gamma R)$$

  decays with distance ($\alpha > 0$ in any lossy medium) and delays the phase.
- **Geometry**: right-handed Cartesian axes, $z$ up. The air–soil interface is
  the plane $z = 0$; air occupies $z > 0$, soil $z < 0$.
- **Segments and the interface**: each segment belongs wholly to one medium,
  assigned by the sign of its midpoint $z$ (midpoint $z \le 0$ is soil). A
  node may lie on $z = 0$, for example the air/soil junction of
  `common/rod_air.json`. A segment must not lie *in* the plane $z = 0$,
  because its image would coincide with itself (§5). A conductor crossing the
  interface must be split there, so that no single segment spans both media.
- **Segment end currents**: both $I_1$ and $I_2$ are positive **into** the
  segment (from the node toward the segment). Hence

  $$I_\ell = \frac{I_1 - I_2}{2} \quad \text{(mean longitudinal current)}$$
  $$I_t = I_1 + I_2 \quad \text{(total transversal / leakage current)}$$

## 2.1 Where the propagation constant comes from

In a source-free homogeneous medium, the phasor Maxwell equations are
$\nabla\times\mathbf{E} = -j\omega\mu\mathbf{H}$ and
$\nabla\times\mathbf{H} = (\sigma + j\omega\varepsilon)\mathbf{E}$. The second
one already shows why $W = \sigma + j\omega\varepsilon$ is the natural medium
constant: conduction and displacement currents always appear together. Taking
the curl of the first equation and substituting the second gives
$\nabla^2\mathbf{E} = j\omega\mu W\,\mathbf{E} = \gamma^2\mathbf{E}$. A
spherical wave leaving a point is then $e^{-\gamma R}/R$, and choosing
$\text{Re}\,\gamma \ge 0$ selects the wave that dies out with distance.

Two limits are worth remembering:

| Medium | Condition | $\gamma$ | Meaning |
| --- | --- | --- | --- |
| Air, or soil at very high frequency | $\sigma \ll \omega\varepsilon$ | $\approx j\omega\sqrt{\mu\varepsilon}$ | pure delay at speed $1/\sqrt{\mu\varepsilon}$ |
| Soil at low and medium frequency | $\sigma \gg \omega\varepsilon$ | $\approx (1 + j)\sqrt{\omega\mu\sigma/2}$ | diffusion; $1/\alpha$ is the skin depth |

For a soil of 0.01 S/m and $\varepsilon_r = 10$ the two terms of $W$ are equal
at 18 MHz, and at 1 MHz the skin depth is about 5 m. Grounding electrodes of
tens of metres are therefore *not* electrically small for lightning
frequencies, which is why the model keeps $e^{-\gamma R}$.

## 2.2 Mapping to the literature

Portela [1] and the dissertation [3] use the physics convention
$\exp(-i\omega t)$, in which the immittance appears as
$\sigma - i\omega\varepsilon$, the wavenumber is
$k = \sqrt{\mu\varepsilon\omega^2 + i\omega\mu\sigma}$ and the propagation
factor is $\exp(+ikR)$. The two notations are complex conjugates of each
other: $\gamma = j \cdot \text{conj}(k)$, and
$\exp(-\gamma R) = \text{conj}(\exp(+ikR))$. Results (impedance moduli,
time-domain waveforms) are identical; phases are conjugated. Any code mixing
the two conventions in a single expression is wrong. When comparing with
another code, compare impedance moduli and time-domain waveforms, never raw
phases.

## Check yourself

- Verify that $\gamma$ for a good conductor has equal real and imaginary
  parts, and compute the skin depth of copper at 1 MHz (about 65 µm).
- Write the phasor of $\cos(\omega t - \beta R)$ in both conventions and
  confirm that they are conjugates.
- A horizontal electrode is drawn at $z = 0$ in a paper. Why can it not be
  entered like that, and what is the usual remedy? (Its image coincides with
  itself; give it a small depth, at least a few radii.)
