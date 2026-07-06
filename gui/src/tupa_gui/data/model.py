"""Plain dataclasses mirroring the language-agnostic object model (ADR 0002).

No Qt dependency here: this module is unit-testable headless and is the
only place that understands the common/README.md JSON schema (v0).
"""

from __future__ import annotations

from dataclasses import dataclass, field


@dataclass(frozen=True)
class Soil:
    conductivity: float
    permittivity: float
    permeability: float


@dataclass(frozen=True)
class Node:
    id: str
    position: tuple[float, float, float]


@dataclass(frozen=True)
class Material:
    id: str
    epsilonr: float
    mur: float
    sigma: float


@dataclass(frozen=True)
class LineElement:
    """The only element type the schema defines today (common/README.md)."""

    id: str
    from_node: str
    to_node: str
    radius: float
    segments: int
    material: str


@dataclass
class Study:
    title: str
    soil: Soil
    nodes: list[Node] = field(default_factory=list)
    materials: list[Material] = field(default_factory=list)
    elements: list[LineElement] = field(default_factory=list)

    def node(self, node_id: str) -> Node:
        for n in self.nodes:
            if n.id == node_id:
                return n
        raise KeyError(f"unknown node id: {node_id!r}")

    def material(self, material_id: str) -> Material:
        for m in self.materials:
            if m.id == material_id:
                return m
        raise KeyError(f"unknown material id: {material_id!r}")
