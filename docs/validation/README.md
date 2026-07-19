# External-reference comparisons

Case-by-case writeups comparing TUPÃ's output against a published paper's
own figures — as opposed to [BENCHMARKS.md](../BENCHMARKS.md), which tracks
the project's formal validation anchors (analytical formulas, tabulated
data, cross-code runs). A comparison lands here when the only available
reference is a plotted curve in a PDF: there is no tabulated data, so the
method (how the curve was read off the plot) is as much a part of the
record as the numbers, and belongs with them rather than folded into
BENCHMARKS.md's anchor table.

## Index

| Case | Reference | Quantity | Result |
| --- | --- | --- | --- |
| [silva2025-fig3.md](silva2025-fig3.md) | Silva et al. 2025 (SBAI, references.md [36]), Fig. 3 | Harmonic input impedance \|Z(ω)\|, 60 m buried electrode, `alipio-visacro` soil, ρ0 = 100/300/1000/2400 Ω·m | DC/high-frequency asymptotes and the resonance dips/peaks (ρ0 = 1000/2400 Ω·m) agree closely; a mid-band knee runs up to ~14% off for ρ0 = 100/300 Ω·m, partly corroborated by an independent discretization-sensitivity study — see writeup for the full table |
| [silva2025-fig4.md](silva2025-fig4.md) | Silva et al. 2025 (SBAI, references.md [36]), Fig. 4 | Time-domain GPR at the injection node, same 60 m electrode/soil, MCS_FST#2 double-peaked first-stroke current (De Conti & Visacro 2007, [38]) | Both current-driven GPR humps agree within ~5% at every ρ0 (worst single point +12.5%, on the front at ρ0 = 2400 Ω·m); the tail (t ≥ 15 µs) has a mild +0.6-2% overestimate for ρ0 = 300/1000/2400 Ω·m — see writeup for the full table and the MCS_FST#1-vs-#2 finding |
| [grcev-fig12.md](grcev-fig12.md) | Grcev et al. 2018 (IEEE TPWRD, references.md [23]), Fig. 12 | Harmonic input impedance \|Z(ω)\| of horizontal buried electrodes vs. the paper's own rigorous full-wave MoM reference model, ℓ = 10/100 m, homogeneous soil, ρ1 = 30/300/3000 Ω·m | DC/HF asymptotes agree within 0-4% on all six curves and the ℓ = 10 m, ρ1 = 3000 Ω·m double resonance is reproduced in shape/depth/location; excluding four slope-sensitive digitization outliers on that resonance and two knees, every other point is within ±11.5% — see writeup for the full table and why this is a closer physics match than the Silva comparisons above |
| [lima-fig6.md](lima-fig6.md) | Lima et al. 2020 (IEEE TEMC, references.md [11]), Fig. 6 (Case #9, MHEM curve only) | Harmonic input impedance \|Z(ω)\| of a 9-electrode distribution-tower counterpoise (4×6 m horizontal + 5×3 m vertical), ρ = 1000 Ω·m | Resonance dip/peak/dip locations agree within ~10-15%, but a systematic −13 to −17% gap runs from DC to ~2 MHz and the resonance amplitudes diverge by up to 2-3× above ~3 MHz; the paper doesn't state this case's electrode radius, burial depth or arm azimuths, so the gap can't be pinned on model vs. inferred-geometry — see writeup for the full table and the radius/depth sensitivity checks |

## Method (applies to every writeup in this folder unless stated otherwise)

The source is a rasterized plot inside a processed-PDF figure (not
redistributed into this public repo — see `CLAUDE.local.md`'s external
reference library note). Data points are read manually off the plot against
its gridlines, cropped and zoomed via ImageMagick for precision; this is
**not** a pixel-traced digitization, so treat individual points as accurate
to roughly the gridline spacing (typically 5-10% of the plotted range),
worse on steep slopes where a small horizontal misread maps to a large
vertical one. Comparison plots overlay the digitized points on TUPÃ's own
computed curve using the versions of the case JSON files under
[../../common/](../../common/); regenerate with
`fpm run --profile release -- ../common/<case>.json` (`fortran/`, see
[fortran/README.md](../../fortran/README.md#running-tupa)).

Figures are plotted with matplotlib from a per-case script alongside this
folder (e.g. [plot_silva2025_fig3.py](plot_silva2025_fig3.py),
[plot_silva2025_fig4.py](plot_silva2025_fig4.py),
[plot_grcev_fig12.py](plot_grcev_fig12.py),
[plot_lima_fig6.py](plot_lima_fig6.py)), which reads the raw
digitized points from a per-case xlsx (e.g.
[silva_fig3.xlsx](silva_fig3.xlsx), [silva_fig4.xlsx](silva_fig4.xlsx),
[grcev_fig12.xlsx](grcev_fig12.xlsx), [Lima_fig6.xlsx](Lima_fig6.xlsx)) and
the TUPÃ curve out of the `fpm run` output above — the writeup's own table
re-samples both onto a coarser common grid for a compact, readable
point-for-point comparison, it is not the script's data source. Rerun the
script after regenerating either input to refresh the SVG under
`../figures/`.
