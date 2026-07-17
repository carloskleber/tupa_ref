"""Plain dataclasses mirroring the language-agnostic object model (ADR 0002).

No Qt dependency here: this module is unit-testable headless and is the
only place that understands the common/README.md JSON schema (v1, ADR 0013).
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


@dataclass(frozen=True)
class Source:
    """One excitation source per driven node: a current injection (ADR
    0010), or an ideal voltage source (ADR 0016) when `is_voltage` is
    true — `current` then holds the source voltage (V)."""

    node: str
    current: complex
    is_voltage: bool = False


@dataclass(frozen=True)
class FrequencySweep:
    """Log-spaced sweep request (ADR 0013) — the schema's user-facing knob;
    `pointsPerDecade` is converted to a total point count by the solver-side
    reader, not here."""

    min: float
    max: float
    points_per_decade: float


@dataclass(frozen=True)
class Outputs:
    """Opt-in projection over what the results writer stores/emits (ADR
    0013). Omitted or empty lists mean "everything" — mirrored as-is here,
    the GUI does not resolve that default itself."""

    nodes: list[str] = field(default_factory=list)
    electrodes: list[str] = field(default_factory=list)
    quantities: list[str] = field(default_factory=list)


@dataclass(frozen=True)
class Signal:
    """Time-domain excitation spec (ADR 0015) — independent of
    `sources`/`frequencies`; a study may carry either, both, or neither.
    `front`/`jones` only apply to `waveform == "doubleExp"`."""

    waveform: str
    imax: float
    source_node: str
    observe_nodes: list[str]
    nyquist_hz: float
    fft_points: int
    front: str | None = None
    jones: bool = False
    observe_electrodes: list[str] = field(default_factory=list)
    freq_zero_hz: float = 1.0e-6


@dataclass
class Study:
    title: str
    soil: Soil
    nodes: list[Node] = field(default_factory=list)
    materials: list[Material] = field(default_factory=list)
    elements: list[LineElement] = field(default_factory=list)
    sources: list[Source] = field(default_factory=list)
    frequencies: FrequencySweep | None = None
    outputs: Outputs | None = None
    signal: Signal | None = None

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


@dataclass(frozen=True)
class NodeVoltage:
    id: str
    voltage: list[complex]


@dataclass(frozen=True)
class ElectrodeCurrent:
    id: str
    i1: list[complex]
    """Longitudinal current (theory.md §6 naming)."""
    i2: list[complex]
    """Transverse (leakage) current."""


@dataclass
class Results:
    """Mirrors the output JSON schema v0 (ADR 0012). Keyed back to the input
    study's node/element `id`s — only meaningful loaded alongside it."""

    title: str
    frequencies: list[float]
    nodes: list[NodeVoltage] = field(default_factory=list)
    electrodes: list[ElectrodeCurrent] = field(default_factory=list)
    input_impedance: list[complex] | None = None

    def node(self, node_id: str) -> NodeVoltage:
        for n in self.nodes:
            if n.id == node_id:
                return n
        raise KeyError(f"unknown node id: {node_id!r}")

    def electrode(self, electrode_id: str) -> ElectrodeCurrent:
        for e in self.electrodes:
            if e.id == electrode_id:
                return e
        raise KeyError(f"unknown electrode id: {electrode_id!r}")


@dataclass(frozen=True)
class TransientNodeVoltage:
    id: str
    voltage: list[float]
    """Real-valued v(t) (V) — the transient response, not a phasor."""


@dataclass(frozen=True)
class TransientElectrodeCurrent:
    id: str
    i1: list[float]
    """Longitudinal current i1(t) (A)."""
    i2: list[float]
    """Transverse (leakage) current i2(t) (A)."""


@dataclass
class TransientResults:
    """Mirrors the transient results JSON schema (ADR 0015) — real-valued
    time series, structurally parallel to `Results` (ADR 0012) but a
    distinct shape since the axis/quantities are unrelated (`time`, not
    `frequencies`; real values, not `{"re":..,"im":..}` phasors)."""

    title: str
    source_node: str
    time: list[float]
    injected_current: list[float]
    nodes: list[TransientNodeVoltage] = field(default_factory=list)
    electrodes: list[TransientElectrodeCurrent] = field(default_factory=list)

    def node(self, node_id: str) -> TransientNodeVoltage:
        for n in self.nodes:
            if n.id == node_id:
                return n
        raise KeyError(f"unknown node id: {node_id!r}")

    def electrode(self, electrode_id: str) -> TransientElectrodeCurrent:
        for e in self.electrodes:
            if e.id == electrode_id:
                return e
        raise KeyError(f"unknown electrode id: {electrode_id!r}")
