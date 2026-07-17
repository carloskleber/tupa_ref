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
| [silva2025-fig3.md](silva2025-fig3.md) | Silva et al. 2025 (SBAI, references.md [36]), Fig. 3 | Harmonic input impedance \|Z(ω)\|, 60 m buried electrode, `alipio-visacro` soil, ρ0 = 100/300/1000/2400 Ω·m | DC and high-frequency asymptotes agree within ~1-3%; the mid-band knee and the resonance dips (ρ0 = 1000/2400 Ω·m) agree in both location and depth — see writeup for the full table |

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
