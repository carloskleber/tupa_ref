# TAGS

Pure Julia prototype of the Hybrid Electromagnetic Model. Refer to
https://github.com/pedrohnv/transient-analysis-grounding-systems for the
high-performance C implementation.

## Setup

Open this directory in VS Code, select its `Project.toml` as the Julia
environment, and run:

```julia
using Pkg
Pkg.instantiate()
```

From the VS Code terminal, the equivalent command is:

```sh
julia --project=. -e 'using Pkg; Pkg.instantiate()'
```

## Tupa-aligned examples

Two examples use the same geometry, material values, segmentation, source,
frequency grid, and output columns as cases in `tupa_ref/common`:

```sh
julia --project=. examples/tupa_grcev_fig12_l10.jl
julia --project=. examples/tupa_rod.jl
```

`tupa_grcev_fig12_l10.jl` matches the Tupa Grcev 10 m horizontal-electrode
cases for soil resistivities 30, 300, and 3000 ohm-m. It uses 40 segments and
401 frequencies from 100 Hz to 10 MHz.

`tupa_rod.jl` matches `tupa_ref/common/rod.json`: a 3 m vertical rod with six
segments in 100 ohm-m soil and 41 frequencies from 10 Hz to 1 MHz.

Each example writes a CSV with the common comparison schema and a PNG with
impedance magnitude and phase. These cases align inputs; they do not change
the mHEM formulation merely to force agreement. In particular, this code
retains its frequency-dependent transverse image-reflection coefficient and
does not add conductor internal impedance.

The original examples remain available unchanged.
