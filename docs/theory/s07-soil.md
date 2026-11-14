# 7. Frequency-dependent soil

[Theory index](../theory.md) · previous: [§6 Nodal system](s06-nodal-system.md) · next: [§8 Transient response](s08-transient.md)

> **Summary.** The conductivity of real soil rises with frequency and its
> permittivity falls. Between DC and a few MHz the change is large enough to
> cut impulse impedances substantially in resistive soils. The model enters
> only through the immittance $W(\omega)$ of the soil, so any causal
> dispersion law can be used. Two are available, both power laws: Portela's
> and Alipio–Visacro's.

## 7.1 Why soil is dispersive, and what causality demands

Soil is a mixture of mineral grains, water and air. Charge accumulates at the
grain–water boundaries and takes time to do so, which makes the bulk
polarisation lag behind the field. Seen from outside, $\sigma$ grows and
$\varepsilon$ decreases with frequency, over many decades.

The two are not independent. $W(\omega) = \sigma(\omega) + j\omega\varepsilon(\omega)$
is the response function of a causal, passive medium, so its real and
imaginary parts are a Hilbert-transform pair (the Kramers–Kronig relations;
Bode [9] for the minimum-phase form). A model fitted to measured
$\sigma(\omega)$ therefore fixes $\omega\varepsilon(\omega)$ as well. For a
power law $\omega^\alpha$ the pair is $[1 + j\tan(\pi\alpha/2)]\,\omega^\alpha$,
which is just $(j\omega)^\alpha$ up to a real factor. This is why every model
below has that form, and why it continues to complex frequency as
$s^\alpha$ (§8).

Every dispersive model must reduce to the constant-parameter medium as
$\omega \to 0$ ([ADR 0007](../adr/0007-soil-dispersion-model.md)).

## 7.2 Portela's model

Portela [1, §1] fits measurements of many soil samples with

$$W(\omega) = \sigma_0 + \Delta\sigma \cdot \left[ 1 + j \tan\left(\frac{\pi \alpha}{2}\right) \right] \cdot \left(\frac{\omega}{\omega_0}\right)^\alpha$$

with $\sigma_0$ the low-frequency conductivity, $\alpha \in (0,1)$ the
dispersion exponent and $\Delta\sigma$ the dispersion magnitude at the
reference frequency $\omega_0$.

TUPÃ uses the Lima–Portela parametrisation [31] with
$\omega_0 = 2\pi \cdot 1$ MHz:

$$W(\omega) = \sigma_0 + \Delta_i\left[\cot\left(\frac{\pi\alpha}{2}\right) + j\right]\left(\frac{\omega}{2\pi \cdot 10^6}\right)^{\alpha}$$

It is the same family with $\Delta\sigma = \Delta_i \cot(\pi\alpha/2)$: the
parameter $\Delta_i$ is the *imaginary* part of the dispersive term at
1 MHz. The input fields of the `portela` soil are `sigma0`, `alpha0`
($\alpha$) and `kr` ($\Delta_i$).

> **Reference-frequency caveat.** Parameter values are meaningless without
> $\omega_0$. Portela's earlier form [30] has
> $\omega_0 = 1$ rad/s and a different meaning for its coefficient. Values
> taken from that source, or from case files of the earlier code [3], must be
> converted before use.

Published parameter sets for the Lima–Portela form: Salari [66] §5.3 gives,
for 100 µS/m < σ₀ < 10 mS/m, a median pair $\alpha \approx 0.706$,
$\Delta_i \approx 11.71$ mS/m and two "reasonably safe" pairs (0.806,
9.23 mS/m; 0.856, 7.91 mS/m). Schroeder et al. [68] use the median pair.

## 7.3 The Alipio–Visacro model

The `alipio-visacro` soil is the *mean* curve of the measurement-based causal
model of Alipio & Visacro [14], parametrised by a single free quantity, the
100 Hz conductivity $\sigma_0$. It is also a built-in soil of TAGS;
PRTL-mHEM uses the measurement-based formulas of the same group [13]. The model gives $\rho(f) = \sigma_0^{-1}[1 +
h(f/f_0)^\xi]^{-1}$ and, via the same Hilbert-transform (minimum-phase)
consistency argument of §7.1, a matching
$\varepsilon_r(f)$. Folded into $W(\omega) = \sigma(\omega) + j\omega\varepsilon(\omega)$
directly (avoiding the spurious $f\to0$ singularity that $\varepsilon_r(f)$
alone has, since the reactive term $\omega\varepsilon_0\varepsilon_r(f)$ stays
finite as $f\to0$ even though $\varepsilon_r(f)$ itself diverges there):

$$W(\omega) = \sigma_0 + \Delta\sigma(f)\left[1 + j\tan\left(\frac{\pi\xi}{2}\right)\right]
+ j\omega\varepsilon_0\varepsilon_{r\infty}, \qquad
\Delta\sigma(f) = \sigma_0\, h \left(\frac{f}{f_0}\right)^{\xi}, \quad f = \frac{\omega}{2\pi}$$

with $f_0 = 1\,\text{MHz}$, $\xi = 0.54$, $\varepsilon_{r\infty} = 12$, and
$h(\sigma_0) = 1.26\,(1000\,\sigma_0)^{-0.73}$ ($\sigma_0$ in S/m; the factor
1000 restates it in mS/m, the unit the empirical fit in [14] uses) — valid
for $100\,\text{Hz} \le f \le 4\,\text{MHz}$, reducing to a purely resistive
medium of conductivity $\sigma_0$ as $\omega \to 0$. The *relatively
conservative* and *conservative* bounding curves of [14] are not available.

## 7.4 Other models, and how much the choice matters

A third family is the 13-term Debye expansion of Longmire & Smith [15], as
parametrised by Cavka et al. [16], the usual choice of lightning studies. It
is not available here. Cavka et al. [16] compare the models side by side and
are the reference for cross-checking any implementation. Schroeder et al.
[68] compare Portela, Alipio–Visacro and Longmire–Smith at the level of line
overvoltages: Portela's form disperses most, cutting tower-footing impulse
impedance by up to ~80 % in 4000 Ω·m soil, and the choice of soil model
moves GPR much more than insulator overvoltages.

The effect is not confined to grounding: Alipio, Duarte & De Conti [28] show
it materially changes underground-cable transients for $\rho > 1000$ Ω m
— dispersive soil should be the default, not the exception, in any transient
study. The same holds at the system level. Dispersion cuts the backflashover
rate of a 138 kV line by 29–55 % for realistic resistivity distributions
[105]. A full-wave layered-media MoM finds the GPR of a *struck* electrode
lowered, strongly in resistive soils [104]. The time-domain BEM of [103]
finds the same for multilayer soil with ionisation. One case is an exception:
the GPR that a *nearby* channel induces in an electrode is almost insensitive
to dispersion and follows distance and current instead [104]. The induced
voltages of §4.6.4 behave the same way.

## Check yourself

- Show that $\cot(\pi\alpha/2) + j = e^{j\pi\alpha/2}/\sin(\pi\alpha/2)$, so
  the Lima–Portela term is $\Delta_i\,(j\omega/\omega_0)^\alpha/\sin(\pi\alpha/2)$.
- With the median pair and $\sigma_0 = 1$ mS/m, compute $\sigma$ and
  $\varepsilon_r$ at 100 kHz and at 1 MHz.
- For $\sigma_0 = 1$ mS/m, evaluate $h(\sigma_0)$ of the Alipio–Visacro model
  and the ratio $\sigma(1\,\text{MHz})/\sigma_0$.
