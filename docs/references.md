# References

Numbered as cited in [theory.md](theory.md).

## Core formulation

1. **Portela, C.** — "Frequency and Transient Behavior of Grounding Systems,
   Part I — Physical and Methodological Aspects", *Proceedings IEEE
   International Symposium on Electromagnetic Compatibility*, pp. 379–384,
   Austin, USA, August 1997. Foundation of the formulation: soil dispersion
   models, segment potentials, A/B/C/D nodal equations, FFT transient driver.
2. **Portela, C.** — "Frequency and Transient Behavior of Grounding Systems,
   Part II — Practical Application Examples", same symposium, pp. 385–390.
   Source of the project's primary validation curves.
3. **Arruda, C. K. C.** — *Modelagem de Linhas de Transmissão para Análise de
   Comportamento Quanto a Descargas Atmosféricas* (Modelling of Transmission
   Lines in Analysis of Lightning Performance), M.Sc. dissertation, COPPE/UFRJ,
   2003. DOI: 10.13140/RG.2.2.19894.56644. Geometry-factor separation,
   closed-form geometry factors, internal impedance expressions, linear-system
   derivation.
4. **Salari, J. C.; Azevedo, R. M.; Portela, C.** — "An efficient modeling of
   transmission lines towers and grounding systems for lightning propagation
   studies", *IX SIPDA*, Foz do Iguaçu, Brazil, Nov. 2007. Z_eq assembly as a
   single augmented system.
5. **Visacro, S.; Soares, A., Jr.** — "HEM: A model for simulation of lightning
   related engineering problems", *IEEE Trans. Power Delivery*, vol. 20, no. 2,
   pp. 1206–1208, Apr. 2005. Independent HEM formulation and validation cases.

## Method of Moments background

6. **Harrington, R. F.** — *Field Computation by Moment Methods*, Macmillan,
   1968 (reprint IEEE Press, 1993). The MoM framework: basis functions,
   testing, matrix formulation of integral equations.
7. **Gibson, W. C.** — *The Method of Moments in Electromagnetics*, 2nd ed.,
   CRC Press, 2014. Modern treatment; thin-wire kernels and numerical
   integration strategies.

## Supporting theory

8. **Stratton, J. A.** — *Electromagnetic Theory*, McGraw-Hill, 1941. Retarded
   potentials, spherical wave solutions in dissipative media.
9. **Bode, H.** — *Network Analysis and Feedback Amplifier Design*,
   D. van Nostrand, 1945. Minimum-phase-shift relations underpinning the soil
   dispersion model (real/imaginary part consistency).
10. **Sunde, E. D.** — *Earth Conduction Effects in Transmission Systems*,
    D. van Nostrand, 1949. Classical grounding resistance formulas used as DC
    validation anchors.

## Related implementation notes

- The original TUPÃ implementation (C++/Fortran hybrid, private) and its user
  manual contain the working derivations that this repository's
  [theory.md](theory.md) consolidates; the model itself is fully specified by
  the public references above.
