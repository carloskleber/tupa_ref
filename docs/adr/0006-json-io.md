# ADR 0006 — JSON study format with a hand-rolled minimal parser

- **Status**: Accepted (parser choice reversible)
- **Date**: 2026-07-03

## Context

The original C++ code used a custom XML dialect (with XSL/CSS for display);
the Matlab before it used keyword-based text case files (`.caso`/`.est`),
which are the natural source when porting reference cases to `common/`.
For the rewrite, a language-neutral, human-writable format is needed so the
same case files drive all implementations. JSON was chosen early. For Fortran,
the mature option is `json-fortran` (FPM-compatible); the repository currently
ships a hand-written recursive-descent parser (`JsonParser.f90`) written to
avoid an external dependency and GFortran issues with recursive derived-type
copies.

## Decision

- **Format**: JSON for study input and structured output; CSV for tabular
  results (gnuplot/pandas friendly). One schema shared by all implementations,
  documented with the reference cases in `common/`.
- **Parser**: keep the **hand-rolled minimal parser** while the schema is
  small (objects, arrays, strings, numbers, booleans; no escapes). This is a
  deliberate, revisitable trade: zero dependencies and full control over
  memory behaviour, at the cost of JSON-subset limitations (64 items/container,
  no string escapes, not reentrant).

## Consequences

- The moment a case file needs escaped strings, >64-element arrays (e.g. long
  explicit frequency lists) or better error reporting, switch to
  `json-fortran` rather than growing the custom parser — the study-building
  layer must therefore talk to a thin reader interface, not to parser
  internals.
- Case files must stay within the supported subset until then; the reference
  cases in `common/` double as parser conformance tests.
- Output JSON writing is trivial (no parser needed) and is not affected.
