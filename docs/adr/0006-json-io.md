# ADR 0006 — JSON study format with a hand-rolled minimal parser

- **Status**: Accepted; parser choice superseded 2026-08-01 (see update below)
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

> **Update (2026-08-01).** Migrated to json-fortran, per the trigger this ADR
> named up front: the hand-rolled parser's limits stopped being purely
> theoretical once a real case (`common/portelaMesh.json`) needed a clear
> error message and there was no room to add validation without them.
> `fortran/src/JsonParser.f90` (`mJsonParser`) is now a thin wrapper around
> `json-fortran`'s `json_core`/`json_value` (added as an FPM git dependency,
> `fpm.toml`) — same public accessor API (`json_child`, `json_item`,
> `json_size`, `json_has`, `json_str`, `json_real`, `json_int`,
> `json_getbool`, plus two new no-key-lookup scalar accessors,
> `json_value_type`/`json_value_real`/`json_value_str`, needed once a couple
> of `Tupa.f90` call sites turned out to read `tJsonValue` fields directly
> instead of going through the accessors), so `fortran/src/Tupa.f90` — the
> only consumer — needed no changes beyond those two call sites. The 64-item
> cap, the no-string-escapes restriction, and the "not reentrant" caveat are
> gone; malformed JSON now raises `mError%raiseError` with json-fortran's own
> line/column-aware message instead of the old parser's generic one. The
> single remaining non-reentrancy note: `mJsonParser` keeps one module-level
> `json_core` instance (mirroring the old parser's single global buffer),
> which is fine for this project's one-file-at-a-time usage but would need
> revisiting for a hypothetical concurrent/multi-file caller.
>
> Alongside the parser swap, `fortran/src/Tupa.f90` gained
> `validateStudyReferences`, called right after `loadStudy` (before any
> geometry-factor or solve work) to resolve every ID a case file
> references — `sources[].node`, `signal.sourceNode`/`observeNodes`/
> `observeElectrodes`, `outputs.nodes`/`electrodes` — against the assembled
> structure, closing a separate but related gap: a bad ID used to surface
> only deep inside `runSweep`/`transientResponse` (after expensive geometry/
> solve work had already run), or, for `outputs.*`, not at all. See ADR
> 0013's exercised note and `common/README.md`'s discretised-ID gotcha for
> the ID-naming details this now enforces up front.
