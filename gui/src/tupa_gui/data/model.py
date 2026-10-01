"""Plain dataclasses mirroring the language-agnostic object model (ADR 0002).

No Qt dependency here: this module is unit-testable headless and is the
only place that understands the common/README.md JSON schema (v1, ADR 0013).
"""

from __future__ import annotations

from dataclasses import dataclass, field


@dataclass(frozen=True)
class Soil:
    """Soil dispersion model (`soil.type`, common/README.md): `"linear"`
    takes conductivity/permittivity/permeability; `"portela"` (ADR 0007)
    takes permeability/sigma0/alpha0/kr; `"alipio-visacro"` takes
    permeability/sigma0. Fields a type does not use stay `None`."""

    conductivity: float | None = None
    permittivity: float | None = None
    permeability: float = 1.0
    type: str = "linear"
    sigma0: float | None = None
    alpha0: float | None = None
    kr: float | None = None


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
    """A straight conductor between two pre-declared nodes (common/README.md)."""

    id: str
    from_node: str
    to_node: str
    radius: float
    segments: int
    material: str


@dataclass(frozen=True)
class CatenaryElement(LineElement):
    """A `line` that sags (`"type": "catenary"`, ADR 0023): `sag` is the drop
    at midspan below the chord (m, negative bows upward). Chain node k of
    `segments` sits at `P1 + s(P2 - P1) - 4*sag*s*(1 - s)*z`, s = k/segments
    (theory.md §4.4, `mElementCatenary::nodePositionCatenary`)."""

    sag: float = 0.0

    def chain_points(
        self, start: tuple[float, float, float], end: tuple[float, float, float]
    ) -> list[tuple[float, float, float]]:
        """The `segments + 1` chain-node positions between the end nodes."""
        n = max(self.segments, 1)
        points = []
        for k in range(n + 1):
            s = k / n
            points.append(
                (
                    start[0] + s * (end[0] - start[0]),
                    start[1] + s * (end[1] - start[1]),
                    start[2] + s * (end[2] - start[2]) - 4.0 * self.sag * s * (1.0 - s),
                )
            )
        return points


@dataclass(frozen=True)
class MeshElement:
    """Rectangular grounding grid (`"type": "mesh"`, ADR 0020) — a composite
    element that plants its own `rows_x * rows_y` main nodes and wires them
    with bars along both axes, rather than referencing pre-declared nodes.

    `node_positions`/`bars` mirror `fortran/src/element/Mesh.f90`'s node/bar
    mnemonic and grid formula exactly, so the GUI can render the same
    authored geometry (main nodes + one cylinder per bar) without a
    solver-side structure dump — consistent with G1's scope of showing
    authored geometry, not the solver's per-electrode discretisation
    (`segments` subdivisions are not expanded here; see viewer3d.py header
    and GUI_SDD.md §5.1a)."""

    id: str
    position: tuple[float, float, float]
    length_x: float
    length_y: float
    rows_x: int
    rows_y: int
    radius: float
    segments: int
    material: str

    def node_id(self, row: int, col: int) -> str:
        """Main-node ID at (row, col), 0-based — matches `mElementMesh::meshNodeId`."""
        return f"{self.id}-{row:02d}{col:02d}"

    def node_positions(self) -> dict[str, tuple[float, float, float]]:
        """Main-node id -> position for every (row, col) in the grid."""
        x0, y0, z0 = self.position
        dx = self.length_x / (self.rows_y - 1)
        dy = self.length_y / (self.rows_x - 1)
        return {
            self.node_id(row, col): (x0 + col * dx, y0 + row * dy, z0)
            for row in range(self.rows_x)
            for col in range(self.rows_y)
        }

    def bars(self) -> list[tuple[str, str]]:
        """(from_id, to_id) main-node pairs, one per bar — same topology as
        `mElementMesh::assembleMesh`'s two loops (X-parallel then Y-parallel)."""
        pairs = [
            (self.node_id(row, col), self.node_id(row, col + 1))
            for row in range(self.rows_x)
            for col in range(self.rows_y - 1)
        ]
        pairs += [
            (self.node_id(row, col), self.node_id(row + 1, col))
            for col in range(self.rows_y)
            for row in range(self.rows_x - 1)
        ]
        return pairs


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
class PortelaSurge:
    """Extra fields of `waveform == "portela"` (ADR 0023, theory.md §8):
    front inclination `alpha` (0 = linear ramp) and the end times of the
    front, flat top and tail (s)."""

    alpha: float
    t_front: float
    t_top_end: float
    t_tail_end: float


@dataclass(frozen=True)
class HeidlerTerm:
    """One term of a parametrised Heidler waveform (`signal.terms[]`)."""

    i0: float
    n: float
    tau1: float
    tau2: float


@dataclass(frozen=True)
class Excitation:
    """One waveform and where it is applied: the top-level waveform fields of
    the `signal` block, one `signal.sources[]` entry (superposed injections,
    ADR 0015) or one `signal.signals[]` entry (independent signals sharing a
    transfer function, ADR 0026). `imax` is optional for a Heidler waveform
    given by `terms`; `front`/`jones` only apply to `"doubleExp"`."""

    waveform: str
    node: str
    imax: float | None = None
    name: str | None = None
    front: str | None = None
    jones: bool = False
    portela: PortelaSurge | None = None
    terms: list[HeidlerTerm] = field(default_factory=list)
    frequency_hz: float | None = None
    phase_deg: float = 0.0
    return_node: str | None = None
    quantity: str = "current"
    """`"current"` (A) or `"voltage"` (V, across the node pair — ADR 0025)."""


@dataclass(frozen=True)
class SignalWindow:
    """`signal.window` (ADR 0015 amendment 2026-09-30)."""

    type: str
    placement: str = "spectral"


@dataclass(frozen=True)
class Signal:
    """Time-domain excitation spec (ADR 0015) — independent of
    `sources`/`frequencies`; a study may carry either, both, or neither.

    `form` tells how `excitations` are read: `"single"` (the top-level
    waveform), `"sources"` (simultaneous injections that superpose, one
    response) or `"signals"` (independent signals, one response set each,
    ADR 0026). The first excitation's fields are also reachable as
    `waveform`/`imax`/`front`/`jones`/`source_node`/`portela`."""

    excitations: list[Excitation]
    observe_nodes: list[str]
    nyquist_hz: float
    fft_points: int
    form: str = "single"
    observe_electrodes: list[str] = field(default_factory=list)
    freq_zero_hz: float = 1.0e-6
    antialias_start: float | None = None
    window: SignalWindow | None = None
    transform: str = "fft"
    nlt_damping: float | None = None
    transfer_function: str = "full"

    @property
    def waveform(self) -> str:
        return self.excitations[0].waveform

    @property
    def imax(self) -> float | None:
        return self.excitations[0].imax

    @property
    def front(self) -> str | None:
        return self.excitations[0].front

    @property
    def jones(self) -> bool:
        return self.excitations[0].jones

    @property
    def source_node(self) -> str:
        return self.excitations[0].node

    @property
    def portela(self) -> PortelaSurge | None:
        return self.excitations[0].portela

    @property
    def independent(self) -> bool:
        """True for a list of independent signals (`signal.signals`, ADR 0026)."""
        return self.form == "signals"


@dataclass
class Study:
    title: str
    soil: Soil
    nodes: list[Node] = field(default_factory=list)
    materials: list[Material] = field(default_factory=list)
    elements: list[LineElement | CatenaryElement | MeshElement] = field(default_factory=list)
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
class TransientSignalResult:
    """The response to one excitation: its source node, the injected current
    and the observed nodes/electrodes. A single-signal file carries exactly
    one, unnamed; a file of independent signals (ADR 0026) one per signal."""

    source_node: str
    injected_current: list[float]
    nodes: list[TransientNodeVoltage] = field(default_factory=list)
    electrodes: list[TransientElectrodeCurrent] = field(default_factory=list)
    name: str | None = None

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


@dataclass
class TransientResults:
    """Mirrors the transient results JSON schema (ADR 0015, and ADR 0026 for
    a list of independent signals) — real-valued time series, structurally
    parallel to `Results` (ADR 0012) but a distinct shape since the
    axis/quantities are unrelated (`time`, not `frequencies`; real values,
    not `{"re":..,"im":..}` phasors). `signals` has one entry unless the file
    lists independent signals; the single-signal accessors (`source_node`,
    `nodes`, …) read the first."""

    title: str
    time: list[float]
    signals: list[TransientSignalResult]

    @property
    def independent(self) -> bool:
        """True for a file of independent signals (ADR 0026)."""
        return len(self.signals) > 1 or self.signals[0].name is not None

    @property
    def source_node(self) -> str:
        return self.signals[0].source_node

    @property
    def injected_current(self) -> list[float]:
        return self.signals[0].injected_current

    @property
    def nodes(self) -> list[TransientNodeVoltage]:
        return self.signals[0].nodes

    @property
    def electrodes(self) -> list[TransientElectrodeCurrent]:
        return self.signals[0].electrodes

    def node(self, node_id: str) -> TransientNodeVoltage:
        return self.signals[0].node(node_id)

    def electrode(self, electrode_id: str) -> TransientElectrodeCurrent:
        return self.signals[0].electrode(electrode_id)
