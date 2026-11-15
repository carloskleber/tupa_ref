# TUPÃ — Theory Reference

This is the index of the electromagnetic model implemented by TUPÃ: the Hybrid
Electromagnetic Model (HEM), an application of the Method of Moments to
lightning and grounding-system transients. The chapters are in
[theory/](theory/).

The theory is **normative** for every implementation (Fortran, Rust, Julia).
Where code and theory disagree, one of them has a bug, and the discrepancy is
resolved before the code is merged
([CONVENTIONS.md](CONVENTIONS.md), [ADR 0008](adr/0008-sign-conventions-and-validation.md)).

New here? Start with the [reader's guide](theory/guide.md): prerequisites,
reading paths and notation. Then follow the
[worked example](theory/worked-example.md), a buried rod solved by hand and
checked against the solver.

## Chapters

Section numbers are stable. Code comments, ADRs and the other documents cite
them as "theory.md §n", and this table says where each one lives.

| § | Chapter | What it answers |
| --- | --- | --- |
| — | [Reader's guide](theory/guide.md) | What do I need to know, what should I read, what do the symbols mean? |
| — | [Worked example](theory/worked-example.md) | How does one small case go through the whole method? |
| 1 | [Model overview](theory/s01-model-overview.md) | What are the unknowns and the premises? |
| 2 | [Conventions](theory/s02-conventions.md) | Which signs, axes and time factor? |
| 3, 3.1 | [Potentials of a segment source](theory/s03-potentials.md) | Where do the kernels come from? How are surface potentials, touch and step voltages obtained? |
| 4, 4.1–4.4 | [Mutual and self impedances](theory/s04-impedances.md) | How are $Z_t$ and $Z_\ell$ computed? Geometry factor, internal impedance, cross-sections, coatings, catenary spans |
| 4.5 | [The lightning-channel element](theory/s04-5-lightning-channel.md) | How is a return stroke with a prescribed speed represented? |
| 4.6, 4.6.1–4.6.5 | [Lightning-induced voltages](theory/s04-6-induced-voltages.md) | How does the model relate to the field-to-line coupling literature? The Nucci base case |
| 5, 5.1 | [The air–soil interface](theory/s05-interface.md) | How is the ground surface handled? Image coefficient, cross-medium coupling |
| 6 | [Nodal system](theory/s06-nodal-system.md) | Which linear system is solved? Lumped branches, sources, the DC point |
| 7 | [Frequency-dependent soil](theory/s07-soil.md) | How do soil parameters vary with frequency? |
| 8 | [Transient response](theory/s08-transient.md) | How is a time response built? FFT and Numerical Laplace Transform |
| 9 | [Validation anchors](theory/s09-validation.md) | What must every implementation reproduce? |
| 10, 10.1 | [Positioning](theory/s10-positioning.md) | How does the model compare with other codes and other methods? |

## What is not in the theory

| Question | Where |
| --- | --- |
| Is feature X implemented, and in which language? | [ROADMAP.md §1](ROADMAP.md) and each implementation's README |
| When was it done, what was measured, what was learned? | [engineering_log.md](engineering_log.md) |
| Why was it decided that way? | [adr/](adr/) |
| How large are the deviations from published results? | [validation/](validation/README.md), [BENCHMARKS.md](BENCHMARKS.md) |
| Which code identifier holds a symbol? | [GLOSSARY.md](GLOSSARY.md#symbols) |
| What are the bibliography entries? | [references.md](references.md) |
| How do I write an input file? | [../common/README.md](../common/README.md) |

## Editing rules

- State the model in the present tense. No dates, no "implemented in phase
  n", no "not yet": status belongs to the roadmap and history to the log.
- A measured number appears only when it tells the reader how far to trust an
  approximation, with a link to the validation page that holds the full data.
- The chapters never name routines, variables, files or cases of the
  earlier private codes, which readers cannot see. They may say that the
  method was first implemented in Matlab, and cite the dissertation [3]. What
  the earlier codes did, and where they were wrong, belongs in
  [ADR 0017](adr/0017-legacy-reinspection-findings.md) and the other
  documents.
- Keep section numbers. A new topic gets a new subsection number at the end
  of its chapter, and this index gets a row.
- Each chapter keeps its summary at the top and its "check yourself" items at
  the end.
- Text taken from the literature is paraphrased and cited by reference
  number.
