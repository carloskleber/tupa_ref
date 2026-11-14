# Reader's guide

[Theory index](../theory.md) · next: [§1 Model overview](s01-model-overview.md)

This guide is for someone opening the theory for the first time, typically a
master's or doctoral student who will use, extend or cite TUPÃ. It says what
background the chapters assume, which chapters to read for a given goal, and
what the symbols mean.

## What the model is, in one paragraph

A structure made of thin wires (grounding electrodes, towers, line conductors,
a lightning channel) is cut into short straight **segments** joined at
**nodes**. Each segment does two things at once: it carries current along its
axis, and it leaks current sideways into the surrounding soil or air. The
electromagnetic field of every segment acts on every other segment. At one
frequency, those interactions are two dense complex matrices, $Z_\ell$ for the
axial currents and $Z_t$ for the leaked currents. Kirchhoff's current law at
the nodes closes the problem, and one linear solve gives all node voltages and
segment currents. Repeating the solve over many frequencies and applying an
inverse Fourier or Laplace transform gives the response to a lightning current
in time. This is the Hybrid Electromagnetic Model (HEM): field theory for the
couplings, circuit theory for the connections.

## What you should know before starting

| Topic | Level assumed | Where it is used | A standard text |
| --- | --- | --- | --- |
| Maxwell's equations in phasor form, retarded potentials, Lorenz gauge | a graduate electromagnetics course | §2, §3 | Stratton [8]; Harrington [6] |
| Wave propagation in lossy media (propagation constant, skin depth) | undergraduate | §2, §4.3, §7 | Stratton [8] |
| Method of images for a plane interface | undergraduate electrostatics | §5 | Sunde [41] |
| Nodal analysis, incidence matrices | undergraduate circuits | §6 | any circuits text |
| Method of Moments: basis and testing functions | helpful, not required | §1, §10 | Harrington [6]; Gibson [7] |
| Discrete Fourier transform, aliasing, windowing | undergraduate signals | §8 | Gómez & Uribe [17] for the Laplace variant |
| Transmission-line theory (telegrapher equations) | undergraduate | §4.5, §4.6 | Rachidi [85] |
| Numerical quadrature, LU factorisation | undergraduate numerical methods | §4.2, §6 | — |

Numbers in square brackets are entries of [references.md](../references.md).
Entry [3] is the author's dissertation, where the method was first implemented
(in Matlab) and from which the formulation of the chapters is taken.

If the retarded potentials are unfamiliar, read §3 slowly: everything in §4
and §5 is those two potentials integrated over straight segments.

## Reading paths

| Your goal | Read | Then |
| --- | --- | --- |
| Run cases and interpret results | §1, §2, §8, the [worked example](worked-example.md) | `common/README.md` for the input format |
| Understand the method well enough to trust it | §1 to §6 in order, the worked example, §9 | the comparisons in [validation/](../validation/README.md) |
| Study grounding under lightning | add §7 (soil) and §3.1 (touch and step voltages) | §10.1 for the neighbouring methods |
| Study lightning-induced voltages | add §4.5 (channel) and §4.6 | [validation/induced-validation.md](../validation/induced-validation.md) |
| Extend the solver or write a port | everything, then [ARCHITECTURE.md](../ARCHITECTURE.md) | the ADRs cited in each chapter |
| Cite or compare the model | §1, §2, §9, §10 | [BENCHMARKS.md](../BENCHMARKS.md) |

## How the chapters are written

- Each chapter starts with a short **summary** and ends with **check
  yourself** items: limits or special cases that you can verify by hand, and
  that the test suite verifies in code.
- The chapters state the model. They do not record when something was
  implemented or who decided it. That history is in the
  [engineering log](../engineering_log.md), decisions are in the
  [ADRs](../adr/), and the state of each implementation is in
  [ROADMAP.md §1](../ROADMAP.md).
- Section numbers are stable. Code comments and ADRs cite them
  ("theory.md §4.6"), and the [index](../theory.md) maps each number to its
  file.
- Names in `code font` are JSON fields of the input format or identifiers in
  the Fortran reference. You can ignore them on a first reading.
- Measured numbers (errors against papers, run times) are quoted only where
  they tell you how far to trust an approximation. The full tables are in
  [validation/](../validation/README.md).

## Notation

Phasors use the time factor $e^{+j\omega t}$ (§2). Bold symbols are vectors or
matrices. The [glossary](../GLOSSARY.md#symbols) maps each symbol to the
identifier that holds it in the code.

| Symbol | Meaning | Unit |
| --- | --- | --- |
| $\omega$, $f$ | angular frequency, frequency | rad/s, Hz |
| $s = c + j\omega$ | complex frequency of the Laplace transform (§8) | 1/s |
| $\sigma$, $\varepsilon$, $\mu$ | conductivity, permittivity, permeability of a medium | S/m, F/m, H/m |
| $W = \sigma + j\omega\varepsilon$ | immittance of a medium (§2, §7) | S/m |
| $\gamma = \sqrt{j\omega\mu W}$ | propagation constant (§2) | 1/m |
| $\psi$, $\mathbf{A}$ | scalar and vector potential (§3) | V, V·s/m |
| $l_a$, $r_0$ | length and radius of segment $a$ | m |
| $R_{ab}$, $\bar R_{ab}$ | distance between points of two segments; between their midpoints (§4) | m |
| $g(a,b)$ | geometry factor $\iint d\ell_a\,d\ell_b/R_{ab}$ (§4.1) | m |
| $\theta_{ab}$ | angle between the directions of two segments | rad |
| $i_1$, $i_2$ | currents at the two ends of a segment, both positive into it (§2) | A |
| $I_\ell = (i_1 - i_2)/2$ | mean longitudinal current of a segment | A |
| $I_t = i_1 + i_2$ | transversal (leakage) current of a segment | A |
| $u$ | node voltage, referred to remote earth | V |
| $Z_t$ | transversal impedance matrix (§4) | Ω |
| $Z_\ell$ | longitudinal impedance matrix (§4) | Ω |
| $Z_{\text{int}}$ | internal impedance of a conductor (§4.3) | Ω |
| $c_E = 1/(4\pi W)$, $c_M = j\omega\mu/4\pi$ | medium constants of the two kernels (§5) | Ω·m, Ω/m |
| $\Gamma$ | image (reflection) coefficient of the air–soil interface (§5) | — |
| $\mathbf{A}, \mathbf{B}, \mathbf{C}, \mathbf{D}$ | incidence matrices (§6) | — |
| $Z_{\text{eq}}$ | system matrix of size $n_n + 2n_s$ (§6) | mixed |
| $Z_g$ | nodal impedance matrix, $\mathbf{u} = Z_g\,\mathbf{i}_e$ (§6) | Ω |
| $H(\omega)$ | transfer function from an injected current to an output (§8) | Ω or — |
| $N$, $T$, $\Delta t$ | samples, length and step of a time record (§8) | —, s, s |

The vector $\mathbf{A}$ (potential) and the matrix $\mathbf{A}$ (incidence)
never appear in the same chapter.

## Vocabulary that differs between communities

- **Segment** and **electrode** are the same thing here: one straight piece
  of wire with uniform currents. The grounding literature says electrode, the
  antenna literature says segment.
- **Transversal** means leaving the wire sideways (a leakage or shunt
  current). **Longitudinal** means along the wire (a series current).
- **Immittance** is used for $\sigma + j\omega\varepsilon$, a quantity per
  metre that is neither an impedance nor an admittance of a component.
- **Remote earth** is the zero of potential, a point far from every conductor.
- **Harmonic** means sinusoidal steady state at one frequency. **Transient**
  means the time response built from many such solutions.
