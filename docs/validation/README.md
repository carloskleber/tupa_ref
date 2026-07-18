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
| [silva2025-fig4.md](silva2025-fig4.md) | Silva et al. 2025 (SBAI, references.md [36]), Fig. 4 | Time-domain GPR at the injection node, same 60 m electrode/soil, MCS_FST#2 double-peaked first-stroke current (De Conti & Visacro 2007, [38]) | Both current-driven GPR humps agree within ~8% at every ρ0; the front (t = 2-6 µs) underestimates ~10-31% and the tail (t ≥ 15 µs) overestimates ~4-9%, both consistently across all four ρ0 — see writeup for the full table and the MCS_FST#1-vs-#2 finding |

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
