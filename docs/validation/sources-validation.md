# Voltage sources and two-node sources — validation

Checks behind [ADR 0016](../adr/0016-voltage-sources-by-superposition.md)
(ideal voltage sources by unit-injection superposition) and the two-node
sources of [ADR 0025](../adr/0025-lightning-channel-and-two-node-sources.md)
(`returnNode`: the ±1 current dipole and the delta-gap voltage source;
`quantity: "voltage"` in the transient). Until now these were pinned only by
unit tests and by the `channel_tower*` fixtures, which compare the code with
itself; here each source variant is compared against what **superposition of
plain unit-current runs** predicts, using only the CLI's output files. Like
[phase9-transient-options.md](phase9-transient-options.md) there is no
published curve. Regenerate every table with

```sh
python3 docs/validation/sources_checks.py <path to the tupa executable>
```

(Fortran `fpm`/`build.sh` binary or `rust/target/release/tupa`; both give the
same tables, differing only in the round-off-level entries.)

## Setup

Two vertical 3 m rods (radius 10 mm, 6 segments each) 10 m apart, tops `A_top`
and `B_top` at 0.5 m depth, σ = 0.01 S/m, εr = 10, 10 Hz–1 MHz at 4 points per
decade. Two single-node runs (1 A at `A_top`, then at `B_top`) give the nodal
impedance matrix of the pair: at 10 Hz Z_AA = 30.817 Ω, Z_AB = 1.514 Ω,
hence a dipole impedance Z_d = Z_AA + Z_BB − 2 Z_AB = 58.606 Ω. Z_AB and Z_BA
agree to the last printed digit (reciprocity, 0.0).

## 1. Harmonic

Worst relative error over the 21 frequencies. The 1e-9 entries are the 9
significant digits of the CSV output, not solver error.

| Check | Worst relative error |
| --- | --- |
| Ideal voltage source (100 V at `A_top`): pinned \|u_A − U\| / U | 2e-18 |
| … reported input impedance vs the current-source Z_AA | 0 |
| … u_B vs Z_BA·U / Z_AA | 4.7e-9 |
| Mixed, 100 V at `A_top` plus 2 A∠30° at `B_top`: pinned u_A | 1e-17 |
| … u_B vs the superposition of the two unit solutions | 1.9e-9 |
| Current dipole (A +1 A, B −1 A): u_A − u_B vs Z_AA + Z_BB − 2 Z_AB | 2.7e-9 |
| … u_A vs Z_AA − Z_AB | 2.7e-9 |
| … reported input impedance vs Z_d | 1.9e-9 |
| Delta-gap voltage source (100 V across the pair): pinned u_A − u_B | 2e-18 |
| … u_A vs (Z_AA − Z_AB)·U / Z_d | 4.7e-16 |
| … reported input impedance vs Z_d | 1.9e-9 |

The pinned-voltage rows are satisfied by construction of the constraint
system (ADR 0016) and show that the superposition is solved, not that the
physics is right; the rows against Z_AA, Z_AB and Z_d are the independent
ones. The reported input impedance of a two-node source is
`(u(node) − u(returnNode))/I`, as ADR 0025 defines it, and agrees with Z_d.

## 2. Transient

Source waveforms are Portela-type surges or the 1.2/50 µs double exponential,
`transform: "nlt"` unless stated, 512 samples. Errors are the largest
deviation over the first half of the record divided by the peak (the NLT's
late-record amplification is the caveat of
[phase9-transient-options.md](phase9-transient-options.md) §2).

| Check | Error / peak |
| --- | --- |
| Voltage source, 2 ms front: source-segment current vs U(t)/R_AA | 1.1e-3 |
| … 200 µs front | 4.0e-3 |
| … 25 µs front | 1.2e-2 |
| Delta gap, slow 250/2500 µs surge: segment current vs U(t)/R_d | 3.2e-5 |
| Current dipole, same surge: u_A − u_B vs R_d·i(t) | 3.2e-5 |
| Current dipole: segment current vs i(t) | 1e-16 |
| Pinned node-pair voltage vs U(t), delta gap, 1.2/50 µs, FFT and NLT | 3.9e-9 |

The three rows for the single-node voltage source are not a source error: the
surge's spectrum extends to where Re Z_AA is no longer its DC value (the soil
skin depth is 160 m at 1 kHz; Re Z_AA is 0.3 % down by 1 kHz and 0.5 % by
3 kHz), and the error falls with the front exactly as that bandwidth does. The
dipole rows agree to 3e-5 because the A–B difference is far flatter in
frequency (58.606 Ω at 10 Hz, 58.608 Ω at 1 kHz, 58.606 Ω at 10 kHz) — between 10 Hz and 1 kHz Re Z_AA falls by 0.089 Ω and Re Z_AB by 0.090 Ω, so the
reduction cancels in Z_AA − Z_AB.

## 3. Cross-implementation

The Fortran and Rust executables give the same tables on every row above
(differences only in entries at round-off, 1e-16 and below), and both
reproduce the committed `channel_tower*` golden fixtures at the 1e-6 rule
(`test_common_cases`, `rust/tests/conformance.rs`). The Julia port refuses
`returnNode` and `quantity` (julia/README.md).

## 4. Not covered

- **Voltage sources in the transient beside current sources.** A transient
  with one `quantity: "voltage"` entry and one current entry in the same
  `signal.sources` list is not exercised here.
- **Sources with internal impedance.** An ideal voltage source only; a series
  element is the user's responsibility (ADR 0016).
- **An independent reference for the lightning-channel strike** (a tower hit
  through `channel_tower_gap`) other than the unloaded-channel and speed
  anchors of [channel-validation.md](channel-validation.md).
