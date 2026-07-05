# ADR 0010 — Excitation sources as nodal current injections

- **Status**: Accepted
- **Date**: 2026-07-05

## Context

The augmented system of theory.md §6 takes external currents $\mathbf{i}_e$
injected at nodes as its right-hand side. Lightning studies typically inject
a stroke current at one node, but voltage-driven cases (e.g. energisation,
transfer-impedance setups) need voltage sources. Two mechanisms were
considered (formerly an open decision for Phase 3):

1. **Current-injection equivalents**: convert each voltage source to an
   equivalent current injection (Norton equivalent, or iterate on the
   injected current until the node voltage matches).
2. **Constraint rows**: replace a KCL row by a voltage constraint
   $u(k) = U_s$, making the unknown at that node the source current.

## Decision

Use **current-injection equivalents** (interview decision, 2026-07-05). The
right-hand-side assembly of `injetaSinalF` stays exactly as in theory.md §6;
voltage sources are handled outside the solver kernel by the study layer.

## Consequences

- The solver kernel keeps a single RHS shape; multiple injection patterns per
  frequency reuse one LU factorisation (ADR 0003).
- The primary use case (stroke-current injection) needs no conversion at all.
- Voltage-driven cases pay a conversion step in the study layer; if an exact
  nodal voltage constraint is ever required, that would be a new ADR
  revisiting the constraint-row option — the kernel must not grow ad-hoc
  special cases.
