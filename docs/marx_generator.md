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

## Plane element (wire-grid surface)

`"type": "plane"` approximates a rectangular conducting surface of any orientation by a grid of thin-wire bars, the standard wire-grid model of walls, ceilings and screens (Richmond 1966; Ludwig 1987):

    { "type": "plane", "id": "WallS", "origin": [0, 0, 0.5], "u": [40, 0, 0], "v": [0, 0, 39.5],
      "cellsU": 8, "cellsV": 8, "radius": 0.8, "material": "steel" }

The rectangle spans `origin`, `origin + u`, `origin + v`. Nodes and bars already in the structure are reused, so planes that share an edge join into one connected enclosure (declare a node at a shared point first to name it, for example to ground it). One segment per bar. Planes must stay off `z = 0`: the floor is the soil half-space itself. Guidelines: cell size under λ/10 at the highest frequency, and bar radius from the equal-area rule a ≈ cell/(2π) (Ludwig 1987; verify against your own reference). The cost is the number of bars squared, so a 40 m hall on a 5 m grid is about 650 segments.

## Laboratory case: 2 MV generator, divider, tower mockup, hall

`tools/gen_marx_lab.py` writes `common/marx_lab_2MV.json` (insulator string open) and `common/marx_lab_2MV_flashover.json` (string replaced by a 1 Ω arc). Geometry (x, y, z in m, hall 0..40 in each direction):

| Part | Model |
|---|---|
| Marx generator, (10, 28), 7.3 m | 10 stages of 200 kV, 0.5 µF (50 nF erected, 100 kJ). Each stage is a `branch` (C, R_front 40 Ω + R_gap 0.2 Ω) in series with a 0.7 m body wire; its charge is the Norton source `branchNorton`. Tail resistor 1.4 kΩ from the output to earth. |
| Divider, (14, 24) | 7 m column; embedded 300 Ω damping and 600 pF HV arm in one segment, 300 nF LV arm at the base (ratio 500:1). |
| Tower mockup | 14 m mast on a 3 m footing rod, 6 m crossarm, 4 m insulator string with embedded 100 pF (open) or 1 Ω (flashed), phase clamp `P` 10 m above ground. |
| Phase cable | 50 m, `P` to `F` along the hall diagonal at 10 m, far end open (`--cable-end R` terminates it). |
| Hall | 40 m cube: four wall planes and a ceiling (5 m grid, 0.8 m bars, steel), eight earthing rods at the corners and mid-sides. |

Circuit values follow the standard single-stage design relations of Kuffel et al. (2000), ch. 1: with erected capacitance C_s = 50 nF and load about 1 nF, T1 ≈ 3 R_f C_b and T2 ≈ 0.7 R_t (C_s + C_b). Parasitics beyond those come from the EM model: column inductance and ground capacitance, lead and cable, and the coupling to the enclosure.

Run `tools/impulse_params.py` on the results for peak, T1 and T2 (IEC 60060-1 definitions) of every observed node, with `--scale Dlv=500` to read the divider output as a measurement.

Limits: spark gaps are closed at t = 0 (no staged erection, no breakdown dynamics), and the string flashover is a chosen state, not a computed event. Compare the two runs to see what the flashover does to the cable and tower.

## LabUAT case: 6 MV generator, divider, porticos, tower mockup

`tools/gen_labuat.py` ports the legacy Matlab study `casos/cepel/labuat.*` (oscillations in the laboratory during an impulse test) to `common/labuat_impulse.json` (insulator-string arc closed from t = 0, as in the legacy study) and `common/labuat_impulse_noarc.json`. The passive geometry (two 220 m poles at 30 m, three porticos with yokes, tower mockup, grounding bonds, divider and generator toroids) is carried over verbatim from the legacy `.est`; the toroids are `ring` elements of 10 chords. The legacy two-resistor Norton generator is replaced by the Marx model above:

| Part | Model |
|---|---|
| GI, column (-40.69, -47.97), 24 m | 32 stages of 0.75 m. Stage k: front resistor `Rf` (16 + 12 + gap 0.2 Ω, less the ESR), capacitor branch 1 µF + 1 Ω ESR with the `branchNorton` source (187.5 kV), tail resistors 2 × 130 Ω in parallel across the capacitor, and a 0.25 m-radius body wire; a 33rd 12 Ω front resistor to the output `gif`. Total front 908 Ω, erected capacitance 31.25 nF. |
| Divider, (-30.69, -47.97), 20 m | Five 1.5 nF units, 120/120/120/200/200 Ω bottom to top (CRO 6000-300 datasheet), the 350 Ω external damping resistor added in series at the top, taps tied to the legacy toroids; legacy secondary arm (165.5 kΩ, 0.4965 µF) kept. `--divider legacy` restores 100 Ω / 1.5 nF. |
| Poles, porticos, mockup | As the legacy file (aluminium 0.56 m equivalent bundle radius, steel μr = 100 for the structures). |

Assumptions that are not in the datasheet or the legacy files, each a flag of the script: gap resistance 0.2 Ω, capacitor ESR 1 Ω, Marx column as a 0.25 m wire, tail resistors two in parallel per stage, the intermediate GI toroid tied to the middle of the column (as `labuat_gidiv.est`; the full legacy file leaves it floating), no overshoot compensation, ideal ground images (legacy `solo_ideal`). Stage capacitance and voltage come from the legacy 62.5 nF sections (two in series = 31.25 nF = 1 µF / 32) and 6 MV / 32.

Result on the open case (`impulse_params.py`, 5 MHz, 1024 samples): 4.9 MV at the generator output (82 % of 6 MV), T1 about 9 µs and T2 about 62 µs. The 1.2/50 µs of the datasheet is for its rated load; the 220 m pole is a load of about 3 nF behind 908 Ω of front resistance, which stretches the front (T1 ≈ 3 R_f C_b). With the arc closed the wave is chopped: the pole mid-point peaks near 0.9 MV at 1.2 µs. Not yet exposed: branch currents (the legacy `ilsec`, `ilgi`; the earth wires `atdiv_e1` and `atgi_e1` carry the same currents) and the arc impedance.

References for the plane element: Richmond, J. H. (1966), A wire-grid model for scattering by conducting bodies, *IEEE Trans. Antennas Propag.* 14(6), 782-786; Ludwig, A. C. (1987), Wire grid modeling of surfaces, *IEEE Trans. Antennas Propag.* 35(9), 1045-1048. Cited from memory: check the pages before quoting.
