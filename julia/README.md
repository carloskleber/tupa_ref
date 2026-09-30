# TUPÃ — Julia version

This is a native Julia port of the Fortran HEM solver. It reads the shared
`common/*.json` studies, discretises line and mesh elements, builds the direct
and image geometry matrices, assembles the augmented HEM system, and runs
frequency- or time-domain simulations.

Transient synthesis can apply the same optional frequency-domain Tukey
antialiasing filter as the Fortran code: set `signal.antialiasStart` (fraction
of Nyquist where the raised-cosine roll-off begins, in (0, 1]) in the study
JSON, or pass `antialias_start` to `transient_response`. It is off by default
([ADR 0021](../docs/adr/0021-transient-antialias-filter.md)). The existing
record-tail taper is always applied because it addresses FFT leakage, a
different problem.

**Status: prototype.** On the Grcev Fig. 12 10 m cases it matches the Fortran
solver to ≤ 0.17% in |Z| and, on `portela1997_transient.json`, to under
8·10⁻⁴ of peak in v(t) and i1/i2(t) ([validation](../docs/validation/tupa-vs-mhem.md)).
Known gaps relative to the Fortran code: fixed 64×64 midpoint quadrature for
the geometry factors, no voltage sources (ADR 0016), no `outputs` filtering
(ADR 0013), no results JSON, and a frequency sweep returns its result in
memory instead of writing files. `i1`/`i2` are the segment end currents
I₁/I₂ (positive into the segment), as in the Fortran outputs.

```sh
cd julia
julia --project=. -e 'using Pkg; Pkg.instantiate()'
julia bin/tupa.jl ../common/portela1997_transient.json
julia --project=. -e 'using Pkg; Pkg.test()'
```

The command-line launcher activates the Julia project automatically, so it can
also be called from the repository root:

```sh
julia julia/bin/tupa.jl common/portela1997_transient.json
```

The Portela transient run writes both the numerical results and a two-panel
waveform plot to the current directory:

- `portela1997_transient_transient_results.csv`
- `portela1997_transient_transient_plot.png`

For library use:

```julia
using Tupa
study, input = load_study("../common/portela1997_transient.json")
result = transient_response(study, input[:signal])
```
