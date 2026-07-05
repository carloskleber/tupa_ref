# TUPÃ — Distribution, packaging and dependencies

How the project is built, what it depends on, and the supply-chain and
licensing facts. Build *instructions* live with the implementation
([../fortran/README.md](../fortran/README.md)); this file records the
policy and the dependency inventory.

## Build chain

| Step | Tool | Notes |
| --- | --- | --- |
| Dependency fetch + build | FPM (`fpm.toml`) | stdlib and feh fetched by fpm; LAPACK/BLAS/SLATEC linked as system/prebuilt libraries |
| SLATEC provisioning | `fortran/build.sh` | clones the author's SLATEC fork into `fortran/slatec/`, `fpm install`s it to `~/.local/lib` with legacy flags |
| Optimised build | `fortran/build.sh` | `--profile release` plus `-O3 -march=native -fopenmp -ffast-math …` |
| Development build/test | `fpm build` / `fpm test` | needs `LIBRARY_PATH=$HOME/.local/lib:$LIBRARY_PATH` for the SLATEC link when not run via build.sh |
| API docs | FORD (`fortran/Tupa.md`) | optional; needs a Python venv (`pip install ford lxml`) |

**Compilers**: latest gfortran is the reference; the code must stay
ifx-compatible (author decision, [ROADMAP §9](ROADMAP.md)). The
`-ffast-math` flag in build.sh predates validation — expect it to be
re-examined when reference cases exist, since it licenses reassociation
that can perturb tight tolerance checks.

**SLATEC canon**: the **cloned checkout in `fortran/slatec/`** (from the
author's fork) is the canonical copy and may be fine-tuned in place; it is
not an untouchable vendored snapshot.

## Dependency inventory

| Dependency | Kind | Source | License | Used for |
| --- | --- | --- | --- | --- |
| stdlib | fpm package | fortran-lang | MIT | version string today; FFT candidate later (ROADMAP Phase 6) |
| fortran-error-handler (feh) | fpm package, **git HEAD (unpinned)** | samharrison7 | BSD-3-Clause | all error raising (`mError`) |
| LAPACK / BLAS | system libraries | distro | BSD-style | `ZGESV` dense complex solve |
| SLATEC | cloned + locally installed | author's fork (github.com/carloskleber/slatec) | public domain | `ZBESI` complex Bessel (internal impedance) |
| FORD, lxml | Python, docs only | PyPI | GPL/BSD | API documentation generation |

Supply-chain note: `feh` is referenced by git URL without a tag or commit
pin — a rebuild can silently pick up upstream changes. Recommendation
(unactioned): pin `rev` in `fpm.toml` when cutting the first tagged
release.

## Packaging and release

- **Distribution form**: source only, built by fpm. No package-registry
  publishing planned; no binary artifacts.
- **Release process** (proposed 2026-07-05, delegated by the author):
  semantic versioning; **0.1.0** when the Phase 2 validation milestone
  passes (Portela-curve case within tolerance, [BENCHMARKS.md](BENCHMARKS.md));
  annotated git tags plus a `CHANGELOG.md` per release; `version` in
  `fpm.toml` kept in sync.
- **No hosted CI**: the merge gate is a local `fpm build && fpm test`
  ([ROADMAP §5](ROADMAP.md)).

## Licensing and code protection

- Project license: **GPLv3** (`LICENSE`; `fpm.toml` declares the SPDX id
  `GPL-3.0-or-later`).
- The repository is **public**: private legacy-code locations and personal
  reference-library paths must never appear in committed files
  ([CONVENTIONS.md](CONVENTIONS.md)); paraphrase copyrighted reference
  material, never paste it.
- No code obfuscation/protection applies or is wanted — the project's value
  is auditability (reference implementation).

## Security surface

Minimal by construction: a CLI reading one local JSON file; no network, no
elevated privileges, no dynamic code. The JSON parser is the only
untrusted-input surface — it enforces hard subset limits (64
items/container, no escapes) and fails through feh
([ADR 0006](adr/0006-json-io.md), [../common/README.md](../common/README.md)).
Malformed numeric fields parse to 0.0 rather than failing — acceptable for
trusted study files, worth tightening if inputs ever come from elsewhere.
