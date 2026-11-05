"""Profiles along contiguous conductors (no Qt dependency).

Combines a `Study` (topology) with a `Results` (solved values) into, per
contiguous chain of `line`/`catenary` segments, the node voltages and the
per-segment longitudinal and transversal currents against the arc length —
the natural view of a quasi-stationary (DC) solve (`frequencies.dc`), but
valid at any frequency of the sweep. Pure post-processing of solver output:
no physics (theory.md §2: I1, I2 are the segment end currents, positive into
the segment; I_l = (I1 - I2)/2, I_t = I1 + I2).
"""

from __future__ import annotations

import math
from collections import defaultdict
from dataclasses import dataclass

from .model import CatenaryElement, LineElement, Results, Study


@dataclass(frozen=True)
class Segment:
    id: str
    node_a: str
    node_b: str
    length: float


@dataclass(frozen=True)
class Chain:
    """One contiguous run of segments, in walking order."""

    nodes: list[str]
    segments: list[str]
    s_nodes: list[float]
    """Arc length (m) of each node, starting at 0."""

    @property
    def s_mid(self) -> list[float]:
        return [(a + b) / 2 for a, b in zip(self.s_nodes, self.s_nodes[1:])]

    @property
    def lengths(self) -> list[float]:
        return [b - a for a, b in zip(self.s_nodes, self.s_nodes[1:])]


@dataclass(frozen=True)
class Profile:
    chain: Chain
    frequency: float
    voltage: list[complex]
    """At the chain's nodes."""
    longitudinal: list[complex]
    """I_l per segment, positive along the chain's walking direction."""
    transversal: list[complex]
    """I_t per segment (leakage into the medium)."""

    @property
    def total_leakage(self) -> complex:
        return sum(self.transversal, 0j)


def expand_segments(study: Study) -> tuple[list[Segment], set[str]]:
    """Segments of every `line`/`catenary` element (node and segment naming as
    the solver's `line` element: interior nodes `<id>_n<k>`, segments
    `<id>_e<k>`; interior nodes are placed on the chord), plus the element
    types that were skipped."""
    pos = {n.id: n.position for n in study.nodes}
    segs: list[Segment] = []
    skipped: set[str] = set()
    for el in study.elements:
        if not isinstance(el, (LineElement, CatenaryElement)):
            if type(el).__name__ != "BranchElement":
                skipped.add(type(el).__name__)
            continue
        n = max(1, el.segments)
        pa, pb = pos[el.from_node], pos[el.to_node]
        names = [el.from_node] + [f"{el.id}_n{k}" for k in range(1, n)] + [el.to_node]
        for k in range(1, n):
            f = k / n
            pos[names[k]] = tuple(pa[i] + f * (pb[i] - pa[i]) for i in range(3))
        for k in range(1, n + 1):
            segs.append(Segment(f"{el.id}_e{k}", names[k - 1], names[k], math.dist(pos[names[k - 1]], pos[names[k]])))
    return segs, skipped


def find_chains(segs: list[Segment]) -> list[list[tuple[int, str]]]:
    """Maximal paths [(segment index, node entered from)], split at nodes whose
    segment count is not 2; closed loops are returned too."""
    at: dict[str, list[int]] = defaultdict(list)
    for i, sg in enumerate(segs):
        at[sg.node_a].append(i)
        at[sg.node_b].append(i)
    used: set[int] = set()
    out: list[list[tuple[int, str]]] = []

    def walk(start: int, node: str) -> list[tuple[int, str]]:
        path = []
        i = start
        while True:
            used.add(i)
            sg = segs[i]
            nxt = sg.node_b if node == sg.node_a else sg.node_a
            path.append((i, node))
            rest = [j for j in at[nxt] if j != i]
            if len(at[nxt]) != 2 or rest[0] in used:
                return path
            i, node = rest[0], nxt

    for i, sg in enumerate(segs):
        if i in used:
            continue
        for end in (sg.node_a, sg.node_b):
            if len(at[end]) != 2:
                out.append(walk(i, end))
                break
    for i, sg in enumerate(segs):
        if i not in used:
            out.append(walk(i, sg.node_a))
    return out


def build_profiles(study: Study, results: Results, freq_index: int = 0) -> tuple[list[Profile], set[str]]:
    """Profiles of every contiguous chain whose nodes and segments are all in
    `results` (an `outputs` filter can drop some; such chains are omitted), and
    the skipped element types."""
    segs, skipped = expand_segments(study)
    have_n = {n.id for n in results.nodes}
    have_e = {e.id for e in results.electrodes}
    profiles: list[Profile] = []
    for path in find_chains(segs):
        nodes = [path[0][1]]
        s = [0.0]
        for i, entered in path:
            sg = segs[i]
            nodes.append(sg.node_b if entered == sg.node_a else sg.node_a)
            s.append(s[-1] + sg.length)
        ids = [segs[i].id for i, _ in path]
        if not (all(n in have_n for n in nodes) and all(e in have_e for e in ids)):
            continue
        il: list[complex] = []
        it: list[complex] = []
        for (i, entered), eid in zip(path, ids):
            e = results.electrode(eid)
            c1, c2 = e.i1[freq_index], e.i2[freq_index]
            sign = 1.0 if entered == segs[i].node_a else -1.0
            il.append(sign * (c1 - c2) / 2)
            it.append(c1 + c2)
        profiles.append(Profile(
            chain=Chain(nodes=nodes, segments=ids, s_nodes=s),
            frequency=results.frequencies[freq_index],
            voltage=[results.node(n).voltage[freq_index] for n in nodes],
            longitudinal=il,
            transversal=it,
        ))
    return profiles, skipped
