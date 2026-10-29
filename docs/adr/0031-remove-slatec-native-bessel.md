# ADR 0031 — Remove SLATEC: native I₀/I₁ ratio for the internal impedance

- **Status**: Accepted; steps 1–9 implemented 2026-10-29 (step 10, the cut-off, and the 40× parity run remain)
- **Date**: 2026-10-29
- **Supersedes**: the SLATEC row of [ADR 0018](0018-author-interview-decisions-2026-07.md)
  ("the cloned `fortran/slatec/` checkout is canonical") and the last sentence
  of [ADR 0001](0001-modern-fortran-reference-implementation.md) ("SLATEC
  remains available for special functions"). Makes the `D1MACH` warm-up of
  [ADR 0024](0024-phase10-numerics.md) unnecessary.
- **Related**: [ADR 0022](0022-rust-implementation.md) (the Rust crate already
  has no SLATEC), [ADR 0030](0030-cross-section-internal-impedance.md)

This document is written to be picked up cold: §6 is the work plan with
checkboxes, §5 the validation recipe, §7 the decisions still open.

## 1. What SLATEC is used for today

Everything below was checked by grep on 2026-10-29 (tupa_ref at `053421d`).

- **One routine, one module.** The Fortran code calls exactly one SLATEC
  routine, `ZBESI` (complex modified Bessel function *I_ν*), twice:
  `internalImpedance` (`Impedance.f90:181`) and `internalImpedanceLaplace`
  (`Impedance.f90:213`), both with `fnu = 0`, `kode = 2` (exponentially
  scaled), `n = 2`, to form the ratio `I₀(ρ)/I₁(ρ)` of the solid-cylinder
  internal impedance (theory.md §4.3). Nothing else in `src/`, `app/`,
  `test/` or `example/` references SLATEC (the FFT is in-repo, ADR 0014).
- **One side effect.** `warmUpMachineConstants` (`Impedance.f90:132`, called
  from `Study.f90:779` and `:895`) exists only because SLATEC's `D1MACH` fills
  its constant table lazily and not race-free (ADR 0024 §"A race found on the
  way"; ARCHITECTURE §7).
- **Domain actually exercised.** `ρ = r₀√(s μ σ)` with `Re s ≥ 0`, hence
  `Re ρ ≥ 0` and **`arg ρ ∈ [0, π/4]`** (exactly π/4 on the `jω` axis, smaller
  with the NLT's `s = c + jω`). The code cuts off at `|ρ| > 500` and takes the
  ratio as 1 there.
- **Cost of keeping it.** The `fortran/slatec` checkout is the author's fork
  (18 MB, 1,445 files, F77 with legacy-only flags). It is an fpm path
  dependency, so every consumer compiles all of it. Consequences seen so far:
  `fpmw` and `build.sh` need `-std=legacy -Wno-argument-mismatch
  -fallow-invalid-boz`; the commercial package (`mom/fortran`) cannot use it
  with `ifx` and carries a 50-file vendored `ZBESI` call closure plus a
  staging script and an MKL `DLAMCH` aliasing trick (`D1MACH` calls `DLAMCH`);
  and since the 2026-10-29 merge (`dependencies.slatec = { path = "slatec" }`,
  `link` without `slatec`) the commercial `ifx` build no longer compiles at
  all (`cblktr.f`, error #6633).
- **Other implementations.** `rust/src/bessel.rs` already replaces `ZBESI`
  (series for `|z| < 20`, Hankel asymptotics beyond; "pinned against SLATEC
  `ZBESI` tables"). Julia calls AMOS through `SpecialFunctions.besseli`, which
  is the same algorithm as `ZBESI`.

## 2. Findings (numerical)

Reference: AMOS via `scipy.special.ive` (the code `ZBESI` is a copy of).
Candidate: the Rust algorithm re-implemented in Python — ascending series for
`|z| < L`, asymptotic series truncated at its smallest term for `|z| ≥ L`,
ratio formed from the two *unscaled-by-exponential* sums (the `e^z/√(2πz)`
factor cancels, so there is no overflow at any `|z|`). 160 log-spaced radii
`10⁻⁶ … 500`, angles 0°–45°:

| arg ρ | max relative error of `I₀/I₁`, switch at 20 | switch at 25 |
|---|---|---|
| 0° – 30° | ≤ 2.7e-15 | ≤ 2.7e-15 |
| 40° | 1.1e-14 | 1.1e-14 |
| **45°** (`jω` axis) | **1.4e-13**, peak at `|ρ| ≈ 21.4` | 3.8e-14 |

- The worst case is at the switch, where the dropped `e^{-2z}` companion of the
  asymptotic series is `≈ e^{-2·14.1}`; moving the switch to 25–30 trades a
  slightly longer series (cancellation `≈ e^{|z|−Re z}`, ≈ 1e-14) for a
  smaller truncation error. Either is ~10⁷ below the 1e-6 release gate.
- **The `|ρ| > 500 → ratio = 1` rule is the largest error in the pipeline, not
  the Bessel evaluation.** The true `I₀/I₁ − 1 ≈ 1/(2ρ)`, i.e. **1.0e-3** at
  the cut-off (a step discontinuity in `z_int` there). With the asymptotic
  expansion `1 + 1/(2ρ) + 3/(8ρ²) + 3/(8ρ³)` the error is 5e-9 at `|ρ| = 100`,
  8e-12 at 500, 3e-14 at 2000, so the cut-off can be dropped entirely.
  `|ρ| > 500` is reached by a 0.3 m-radius tower member in copper from ≈ 6 kHz, or a
  7 mm rod from ≈ 11 MHz. The effect on `Z_int` is small in absolute terms
  (it is the correction to the surface impedance) but it is a systematic
  1e-3 relative error in a term that the 1e-6 gate is meant to cover; it only
  survives because the gate fixtures were generated with the same rule.
- **Efficiency.** The profile of the call is `n_segments × n_frequencies`
  evaluations (`Study.f90:518–520`), each a full `ZBESI` pair, although every
  segment of an element shares radius, σ and μr: the per-metre value is
  identical and only the length factor differs. One evaluation per distinct
  conductor per frequency is `n_segments_per_conductor` times cheaper than any
  Bessel speed-up (a 200-segment mesh bar: 200×). This is the real efficiency
  gain; the native Bessel is mostly a dependency removal.

## 3. Decision

1. **Add `mBessel`** (`fortran/src/Bessel.f90`): a pure, stateless,
   thread-safe `besselI0OverI1(z)` (complex, `Re z ≥ 0`), same algorithm as
   `rust/src/bessel.rs` (series below the switch, asymptotic above), no
   exponential scaling, no error codes beyond a NaN guard. It replaces both
   `ZBESI` calls.
2. **Stage the behaviour change.** First land the replacement with the
   `|ρ| > 500 → 1` cut-off *kept* (results identical to the SLATEC build to
   ≈ 1e-13, no fixture changes). Then, as a separate commit under its own
   changelog line, replace the cut-off by the asymptotic expansion (§7 decision
   1), regenerate the affected fixtures, and bring Rust and Julia along.
3. **Unlink SLATEC from the build**: remove `dependencies.slatec`, the
   `link`/`external-modules` entries, the `build.sh` submodule update and
   legacy flags, `warmUpMachineConstants` and its two calls. The commercial
   package drops `vendor/slatec_zbesi` and the `slatec.lib` alias (its
   `lapack.lib`/`blas.lib` aliases stay: they serve the LU solve).
4. **Cache the per-metre internal impedance per distinct conductor** per
   frequency (§6 step 8) — the efficiency refactor.
5. **Keep the fork repository and the `fortran/slatec` path until the end**, so
   `ZBESI` stays available as the oracle while `mBessel` is validated; delete
   the submodule as the last step (history keeps it).

Out of scope here, but the module is shaped for it: the tubular conductor
(Phase 12 item 2) needs `K₀/K₁` as well; `mBessel` is where they go (series +
asymptotic, same pattern), validated offline against `scipy.special.kve`
because `ZBESK` will no longer be linked. Insulated-conductor and multilayer
formulas are covered by cross-section entries (ADR 0030), not by new Bessel code.

## 4. Consequences

- Positive: one fewer dependency, no legacy-flag build path, the commercial
  `ifx` build works with the plain `Tupa` path dependency again, ~1,400 files
  and 18 MB leave the working tree, the `D1MACH` race and its warm-up
  disappear, Fortran/Rust/Julia agree on the algorithm, and the Bessel call is
  inlinable in the per-segment loop.
- Negative: we own a 60-line numerical kernel (mitigated by the oracle table
  below and by Rust already doing the same); `ZBESI` loses its role as a
  continuously-linked cross-check (replaced by the stored reference table).
- Neutral: results unchanged to 1e-13 (step 2) until the cut-off decision.

## 5. Validation recipe

1. **Reference table** (generate once, offline, check into
   `fortran/test/data/bessel_ratio_reference.csv`): columns `re_z, im_z,
   re_ratio, im_ratio`, from `scipy.special.ive(0,z)/ive(1,z)`, 160 radii
   `10⁻⁶ … 2000` × angles {0, 1, 5, 15, 30, 40, 45}°, plus the 25 points around
   the switch. Script (kept next to the CSV, run with `uv`): the Python in §2
   is the starting point. Tolerance for `test_bessel`: 5e-13 relative.
2. **Transition test** (while the SLATEC path still builds): in
   `test_bessel.f90`, compare against `zbesi` directly on the same grid; this
   test is deleted with the dependency (step 9).
3. **Edge cases**: `|z|` just below/above the switch; `z` real (arg 0);
   `|z| = 10⁻⁸` (ratio → `2/z`); `z = 0` must return NaN/error, not silently
   0 (a zero `ρ` means `ω = 0`, which the drivers never pass — they use
   `freqZeroHz`).
4. **Regression gates unchanged**: `test_impedance`, `test_solve`,
   `test_validation`, `test_common_cases` (1e-6), `test_transient` (NLT
   exercises the `arg ρ < π/4` branch), `test_parallel` — without
   `warmUpMachineConstants`, run 40× to close the old race report.
5. **Efficiency**: benchmark `portelaMesh` and `grid.json` before/after
   (ADR 0024 numbers: 5.21 s / 2.65 s / 1.53 s at 1/2/4 threads for
   `portelaMesh`); record in `docs/BENCHMARKS.md`.

## 6. Work plan (checklist)

All paths relative to the repository root unless noted. Order matters only
where stated; steps 1–4 are enough to unblock the commercial `ifx` build.

- [x] **1. `fortran/src/Bessel.f90`** — `module mBessel`, public
      `besselI0OverI1(z) result(r)`; private `seriesI(z, nu)`,
      `asymptoticSum(z, nu)`; constants `SERIES_LIMIT = 25` (§2, §7.2),
      series stop `|term| < 1e-18|sum|`, max 500 terms, asymptotic stop at the
      smallest term or `1e-17`. Port `rust/src/bessel.rs` line for line (it is
      the tested twin). `pure`, no module state, `dp` kind.
- [x] **2. `fortran/test/test_bessel.f90`** plus the reference CSV (§5.1–5.3).
- [x] **3. `fortran/src/Impedance.f90`** — in `internalImpedance` and
      `internalImpedanceLaplace` replace the `zbesi` block by
      `ratio = besselI0OverI1(rho)`; keep the `|ρ| > 500` rule for now
      (decision 2); delete the `zbesi` interface block and
      `warmUpMachineConstants`; drop `warmUpMachineConstants` from the
      `public` list and from the `use` in `Study.f90:26`, and the two calls at
      `Study.f90:779, :895`.
- [x] **4. Build files** — `fortran/fpm.toml`: delete
      `dependencies.slatec`, `"slatec"` from `link`, `external-modules`; check
      that `[preprocess] cpp.suffixes = ["F90"]` (for `Version.F90`) stays.
      `fortran/build.sh`: delete the `git submodule update --init
      …/slatec` line and the legacy flags (`-std=legacy
      -Wno-argument-mismatch -fallow-invalid-boz`; keep the optimisation
      flags). `fortran/fpmw` (if present): same flags. Verify with a clean
      `fpm build` and `fpm test` (gfortran) **and** with `ifx` (commercial
      `build-ifx.ps1`).
- [x] **5. Commercial package (done 2026-10-29: vendor dir, staging and `slatec.lib` removed) (`mom/`)** — delete
      `fortran/vendor/slatec_zbesi/` and the `Build-SlatecVendor` /
      `slatec.lib` steps of `fortran/build-ifx.ps1` (keep the
      `lapack.lib`/`blas.lib` aliases); `fortran/fpm.toml`: remove
      `"slatec"` from `link` and `external-modules`; `package-dist.ps1`,
      `docs/ARCHITECTURE.md` §5 and the paragraph at lines 79–89, and
      `docs/ROADMAP.md` line 121 (`ZBESK`) updated. (The `mVersion` →
      `mMomVersion` rename made on 2026-10-29 stays.)
- [~] **6. Parity** (ifx: tupa_ref suites + DLL tests pass, only the known stale `channel_loaded` fixture fails; gfortran and 40× `test_parallel` not run yet) — gfortran/ifx full test pass; `test_common_cases` and the
      GUI/DLL tests unchanged; 40× `test_parallel`.
- [x] **7. Docs** (done 2026-10-29; historical ADR/ROADMAP/theory mentions left as history) — `README.md` (tree and "fetch+build SLATEC", lines
      117/123/146/157), `docs/ARCHITECTURE.md` (§2 table row of `mImpedance`,
      §7 `D1MACH` bullet, row of `rust/`), `docs/DISTRIBUTION.md` (rows 12–13,
      the "SLATEC canon" paragraph and row 40), `docs/CONVENTIONS.md` (add a
      Bessel row), `fortran/README.md` (lines 20, 46, 60–78),
      `benchmarks/cross-impl/README.md` (lines 33, 43), `docs/ROADMAP.md`
      (line 95 "SLATEC `ZBESI`", 254, 830 "`ZBESK`"), `CHANGELOG.md`
      (Unreleased: *Changed — SLATEC removed; native `mBessel`*; *Fixed — the
      `D1MACH` race can no longer occur*). Mark ADR 0018's SLATEC row and ADR
      0001's last sentence as superseded by this ADR.
- [ ] **8. Efficiency refactor** (NOT done) — in `tStudy` add `conductorClass(nseg)`
      (index of the distinct `(radius, sigma, mur)` triple per segment, built
      once in `prepareStudy`; channel-loaded and cross-section segments
      (ADR 0030) get their own classes) and, in `fillAtFrequency`, a
      `zIntPerMetre(nClass)` array filled once per frequency (serial or
      `omp parallel do` over classes) from which the loop does
      `zint = zIntPerMetre(class(i)) * geomLength(i)`. `segmentInternalImpedance[Laplace]`
      then return per-metre values. Compare `portelaMesh`/`grid` timings.
- [x] **9. Remove the dependency for good** (done 2026-10-29, no transition test was ever added) — delete the transition comparison
      test (§5.2), `git rm -r fortran/slatec`, delete the `[submodule
      "fortran/slatec"]` entry from `.gitmodules`, and the
      `slatec`-related lines of `.gitignore` if any.
- [ ] **10. Cut-off decision** (§7.1) — if accepted: replace `|ρ| > 500 → 1`
      by the asymptotic branch; regenerate fixtures that cross it (find them
      by running with the old and new rule and diffing `common/*_expected.csv`
      at 1e-9); same change in `rust/src/impedance.rs:244` and
      `julia/src/Impedance.jl:194`; update theory.md §4.3 and the changelog.

## 7. Decisions still open

1. **Drop the `|ρ| > 500 → 1` cut-off?** Recommended: yes, as step 10 after the
   pure replacement is merged. It changes `z_int` by up to 1e-3 relative where
   `|ρ| > 500` and requires regenerating some fixtures plus Rust/Julia changes,
   so it is kept separate from the dependency removal.
2. **Switch radius 20 or 25.** 25 lowers the worst-case error from 1.4e-13 to
   3.8e-14 at no measurable cost; either passes. Rust uses 20; if changed,
   change both.
3. **Keep or delete the `carloskleber/slatec` submodule at the end.** The ADR
   0018 note says the fork may be fine-tuned in place; nothing in the build
   needs it after step 4, but deleting the submodule is irreversible for
   anyone cloning an old tag only through history. Recommended: delete at
   step 9; the fork repository itself is untouched.
4. **K₀/K₁ for the tubular conductor** (Phase 12 item 2): implement when that
   item starts, in `mBessel`; no SLATEC fallback.
