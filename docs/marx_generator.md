# Marx impulse generator in Tupa

A Marx stage is a capacitor charged to V0 that discharges through the circuit (free response, usually a double exponential). Tupa solves linear networks in the Laplace domain, so the charged capacitor is represented by an equivalent source and the spark gaps by closed switches (a small R and L). Staged firing and arc dynamics are time-varying and out of scope. The requirements analysis (with literature) is in [marx_generator_requirements.md](marx_generator_requirements.md); this page documents what was implemented.

## Charged capacitor: Norton form

For t ≥ 0 a capacitor charged to V0 equals an uncharged C in series with the step V0·u(t). In parallel (Norton) form that is the same series R-L-C `branch` plus a current source at the branch node `a`, returned at `b` (or remote earth):

    i_N(t) = L⁻¹{ V0·Y(s)/s },   Y = sC/(1 + sCR + s²LC)
           = V0·(1/L)·e^{-αt}·sinh(w t)/w     (overdamped; sin for underdamped, t·e^{-αt}/L critical)
           = (V0/R)·e^{-t/(RC)}               (L = 0)

Signal waveform `branchNorton` (`mSignal::tBranchNortonSignal`): `imax` is **V0 in volts**, plus `resistance`, `inductance`, `capacitance` of the branch. The physical series discharge current is `i_N - Y·u` (the current the branch itself carries is `Y·u`).

A plain `step` waveform (`tStepSignal`) is also available. Use `transform: "nlt"` for both: their spectra have a DC term, which is the native case of the Numerical Laplace Transform.

Example: `common/marx_single_stage.json` (100 kV, 1 nF, 2 Ω, 0.5 µH into 50 Ω). `fortran/test/test_marx.f90` checks the load voltage against the closed-form RLC discharge (within 0.5 % of peak after the first samples; the 100 MHz band limit rings in the first ones).

## Lumped element embedded in a wire segment

The `branch` element has no electromagnetic coupling. To put a resistor, gap or capacitor *inside* a conductor, keeping the segment's length, mutual coupling and shunt capacitance, add a `lumped` object to a `line`:

    { "type": "line", ..., "segments": 4,
      "lumped": { "segment": 2, "resistance": 5.0, "inductance": 1e-6, "capacitance": 1e-9 } }

`segment` is 1-based (default: the middle one). The total `R + sL + 1/(sC)` is added to that segment's internal impedance on the Z_long diagonal (`tElectrode::lump*`, `lumpedImpedance`; `Study.f90` `segmentInternalImpedance*`), for both `s = jω` and `s = c + jω`. Only `line` elements carry it. `fortran/test/test_lumped_segment.f90` checks R, C and L+C against the driving-point impedance, harmonic and Laplace.

An embedded **charged** capacitor needs a series EMF in the segment equation (the first `ns` rows of the system have no right-hand side today). That is not implemented; use the Norton form on a `branch` for the charged capacitor, and embed the resistors and gaps.

## Branch current in the transient driver

`transientResponseSignals` takes optional `observeBranchIds` and `branchResponses` (shape `(branches, samples, outputs)`): `Y(s)·(u_a - u_b)` per FFT/NLT bin, then the inverse transform. The JSON `signal` block and the results writers do not expose it yet.
