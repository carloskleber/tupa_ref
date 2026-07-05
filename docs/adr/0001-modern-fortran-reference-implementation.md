# ADR 0001 — Modern Fortran as the reference implementation

- **Status**: Accepted
- **Date**: 2026-07-03

## Context

TUPÃ is a rewrite of a mature private C++/Fortran hybrid solver. The rewrite's
purpose is a *reference* implementation: readable, verifiable against theory,
and portable to other languages. The original already delegated the numerical
core (mesh matrices, LAPACK solve, impedance quadrature) to Fortran via
ISO_C_BINDING; the C++ layer contributed object orientation, I/O and
orchestration.

> **Update (2026-07-05).** The lineage is longer than stated: the C++/Fortran
> hybrid is itself a port of the **original Matlab implementation** (the one
> behind the 2003 dissertation), which became available again alongside it
> and is now the model reference of record (references.md, "Related
> implementation notes"). The decision is unchanged — the Matlab reinforces
> it, its physics kernel mapping to modern Fortran at least as naturally as
> the C++ layer did.

## Decision

Implement the first complete TUPÃ in **modern Fortran (2008+)**, built with
**FPM**, using the object-oriented features of the language (derived types,
type-bound procedures, abstract interfaces) to replicate the C++ object model
in a single language. LAPACK/BLAS provide the dense complex solver; SLATEC
remains available for special functions.

## Consequences

- One language for physics *and* orchestration eliminates the C↔Fortran
  interop layer (raw pointers, `bind(c)` index conversions) that the original
  needed. Legacy 0-based index conversions inherited from that layer must be
  removed during the port (see ROADMAP).
- Fortran's ecosystem is weak for I/O (JSON, plotting); those parts need more
  effort than they would in Python (see ADR 0006).
- The dissertation-era numerics (Gauss–Kronrod quadrature, Bessel functions)
  port naturally.
- Python and Rust implementations come later, mapping the same object model
  (ADR 0002).
