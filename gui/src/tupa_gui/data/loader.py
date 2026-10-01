"""Loaders for input study JSON (common/README.md schema v1, ADR 0013) and
results JSON (ADR 0012 schema v0)."""

from __future__ import annotations

import json
import logging
from dataclasses import replace
from pathlib import Path

from .model import (
    ElectrodeCurrent,
    Excitation,
    FrequencySweep,
    CatenaryElement,
    ChannelElement,
    HeidlerTerm,
    LineElement,
    Material,
    MeshElement,
    Node,
    NodeVoltage,
    Outputs,
    PortelaSurge,
    Results,
    Signal,
    SignalWindow,
    Soil,
    Source,
    Study,
    TransientElectrodeCurrent,
    TransientNodeVoltage,
    TransientResults,
    TransientSignalResult,
)

logger = logging.getLogger(__name__)


class StudyLoadError(ValueError):
    pass


class ResultsLoadError(ValueError):
    pass


def _load_excitation(obj: dict, default_node: str = "", default_terminals: dict | None = None) -> Excitation:
    """One waveform description: the `signal` block itself, a
    `signal.sources[]` entry or a `signal.signals[]` entry. `default_terminals`
    holds the block-level `returnNode`/`quantity` an entry may override."""
    waveform = obj["waveform"]
    terminals = {**(default_terminals or {}), **{k: obj[k] for k in ("returnNode", "quantity") if k in obj}}
    return Excitation(
        waveform=waveform,
        node=obj.get("node", obj.get("sourceNode", default_node)),
        imax=obj.get("imax"),
        name=obj.get("name"),
        front=obj.get("front"),
        jones=obj.get("jones", False),
        portela=PortelaSurge(
            alpha=obj["alpha"],
            t_front=obj["tFront"],
            t_top_end=obj["tTopEnd"],
            t_tail_end=obj["tTailEnd"],
        )
        if waveform == "portela"
        else None,
        terms=[HeidlerTerm(t["i0"], t["n"], t["tau1"], t["tau2"]) for t in obj.get("terms", [])],
        frequency_hz=obj.get("frequencyHz"),
        phase_deg=obj.get("phaseDeg", 0.0),
        return_node=terminals.get("returnNode") or None,
        quantity=terminals.get("quantity", "current"),
    )


def _load_signal(sig: dict) -> Signal:
    """The `signal` block (ADR 0015 and its amendments, ADR 0025, ADR 0026)."""
    block_node = sig.get("sourceNode", "")
    block_terminals = {k: sig[k] for k in ("returnNode", "quantity") if k in sig}
    if "signals" in sig:
        form = "signals"
        excitations = [_load_excitation(e, block_node, block_terminals) for e in sig["signals"]]
        # unnamed entries get the same default name the solvers give them
        excitations = [
            e if e.name is not None else replace(e, name=f"signal{k}") for k, e in enumerate(excitations, 1)
        ]
    elif "sources" in sig:
        form = "sources"
        excitations = [_load_excitation(e) for e in sig["sources"]]
    else:
        form = "single"
        excitations = [_load_excitation(sig, block_node)]
    window = sig.get("window")
    return Signal(
        excitations=excitations,
        form=form,
        observe_nodes=list(sig["observeNodes"]),
        nyquist_hz=sig["nyquistHz"],
        fft_points=sig["fftPoints"],
        observe_electrodes=list(sig.get("observeElectrodes", [])),
        freq_zero_hz=sig.get("freqZeroHz", 1.0e-6),
        antialias_start=sig.get("antialiasStart"),
        window=SignalWindow(type=window["type"], placement=window.get("placement", "spectral")) if window else None,
        transform=sig.get("transform", "fft"),
        nlt_damping=sig.get("nltDamping"),
        transfer_function=sig.get("transferFunction", "full"),
    )


def load_study(path: str | Path) -> Study:
    path = Path(path)
    try:
        raw = json.loads(path.read_text())
    except json.JSONDecodeError as exc:
        raise StudyLoadError(f"{path}: invalid JSON ({exc})") from exc

    try:
        rs = raw["soil"]
        soil = Soil(
            conductivity=rs.get("conductivity"),
            permittivity=rs.get("permittivity"),
            permeability=rs.get("permeability", 1.0),
            type=rs.get("type", "linear"),
            sigma0=rs.get("sigma0"),
            alpha0=rs.get("alpha0"),
            kr=rs.get("kr"),
        )
        nodes = [Node(id=n["id"], position=tuple(n["position"])) for n in raw.get("nodes", [])]
        materials = [Material(**m) for m in raw.get("materials", [])]
    except KeyError as exc:
        raise StudyLoadError(f"{path}: missing required field {exc}") from exc

    elements: list[LineElement | CatenaryElement | MeshElement | ChannelElement] = []
    for e in raw.get("elements", []):
        etype = e.get("type")
        if etype == "line":
            elements.append(
                LineElement(
                    id=e["id"],
                    from_node=e["from"],
                    to_node=e["to"],
                    radius=e["radius"],
                    # optional since ADR 0024 (a `numerics.maxSegmentLength` target
                    # may set the count; the view does not expand subdivisions)
                    segments=e.get("segments", 1),
                    material=e["material"],
                )
            )
        elif etype == "catenary":
            elements.append(
                CatenaryElement(
                    id=e["id"],
                    from_node=e["from"],
                    to_node=e["to"],
                    radius=e["radius"],
                    segments=e.get("segments", 1),
                    material=e["material"],
                    sag=e["sag"],
                )
            )
        elif etype == "mesh":
            elements.append(
                MeshElement(
                    id=e["id"],
                    position=tuple(e["position"]),
                    length_x=e["lengthX"],
                    length_y=e["lengthY"],
                    rows_x=e["rowsX"],
                    rows_y=e["rowsY"],
                    radius=e["radius"],
                    segments=e.get("segments", 1),
                    material=e["material"],
                )
            )
        elif etype == "channel":
            elements.append(
                ChannelElement(
                    id=e["id"],
                    length=e["length"],
                    radius=e["radius"],
                    strike=e.get("strike"),
                    position=tuple(e["position"]) if "position" in e else None,
                    incidence=e.get("incidence", 0.0),
                    azimuth=e.get("azimuth", 0.0),
                    segments=e.get("segments"),
                    first_segment=e.get("firstSegment"),
                    growth=e.get("growth"),
                    max_segment=e.get("maxSegment"),
                    speed=e.get("speed"),
                    inductance=e.get("inductance"),
                    resistance=e.get("resistance"),
                    calibrate=e.get("calibrate", False),
                )
            )
        else:
            logger.warning("%s: skipping element %r of unknown type %r", path, e.get("id"), etype)

    # A source carries either "current" (A, ADR 0010) or "voltage" (V,
    # ADR 0016); neither present defaults to a zero current injection,
    # matching the Fortran reader.
    sources = [
        Source(node=s["node"], current=_complex(s["voltage"]), is_voltage=True)
        if "voltage" in s
        else Source(node=s["node"], current=_complex(s.get("current", {})))
        for s in raw.get("sources", [])
    ]

    frequencies = None
    if "frequencies" in raw:
        f = raw["frequencies"]
        frequencies = FrequencySweep(min=f["min"], max=f["max"], points_per_decade=f["pointsPerDecade"])

    outputs = None
    if "outputs" in raw:
        o = raw["outputs"]
        outputs = Outputs(
            nodes=list(o.get("nodes", [])),
            electrodes=list(o.get("electrodes", [])),
            quantities=list(o.get("quantities", [])),
        )

    signal = _load_signal(raw["signal"]) if "signal" in raw else None

    return Study(
        title=raw.get("title", path.stem),
        soil=soil,
        nodes=nodes,
        materials=materials,
        elements=elements,
        sources=sources,
        frequencies=frequencies,
        outputs=outputs,
        signal=signal,
    )


def _complex(raw: dict) -> complex:
    return complex(raw.get("re", 0.0), raw.get("im", 0.0))


def load_results(path: str | Path) -> Results:
    path = Path(path)
    try:
        raw = json.loads(path.read_text())
    except json.JSONDecodeError as exc:
        raise ResultsLoadError(f"{path}: invalid JSON ({exc})") from exc

    try:
        frequencies = [float(f) for f in raw["frequencies"]]
        nodes = [
            NodeVoltage(id=n["id"], voltage=[_complex(v) for v in n["voltage"]]) for n in raw.get("nodes", [])
        ]
        electrodes = [
            ElectrodeCurrent(
                id=e["id"],
                i1=[_complex(v) for v in e["i1"]],
                i2=[_complex(v) for v in e["i2"]],
            )
            for e in raw.get("electrodes", [])
        ]
    except KeyError as exc:
        raise ResultsLoadError(f"{path}: missing required field {exc}") from exc

    derived = raw.get("derived", {})
    input_impedance = [_complex(v) for v in derived["inputImpedance"]] if "inputImpedance" in derived else None

    return Results(
        title=raw.get("title", path.stem),
        frequencies=frequencies,
        nodes=nodes,
        electrodes=electrodes,
        input_impedance=input_impedance,
    )


def load_transient_results(path: str | Path) -> TransientResults:
    """Load a transient (time-domain) results JSON (ADR 0015) — real-valued
    time series, distinct from `load_results`'s frequency-domain shape."""
    path = Path(path)
    try:
        raw = json.loads(path.read_text())
    except json.JSONDecodeError as exc:
        raise ResultsLoadError(f"{path}: invalid JSON ({exc})") from exc

    try:
        time = [float(v) for v in raw["time"]]
        if "signals" in raw:
            # A list of independent signals (ADR 0026): the response members live inside each entry
            signals = [_load_transient_signal(e) for e in raw["signals"]]
            if not signals:
                raise ResultsLoadError(f"{path}: 'signals' is empty")
        else:
            signals = [_load_transient_signal({**raw, "sourceNode": raw.get("sourceNode", "")}, named=False)]
    except KeyError as exc:
        raise ResultsLoadError(f"{path}: missing required field {exc}") from exc

    return TransientResults(title=raw.get("title", path.stem), time=time, signals=signals)


def _load_transient_signal(raw: dict, named: bool = True) -> TransientSignalResult:
    """The response members of a transient results file (ADR 0015): the file
    itself for a single signal, one `signals[]` entry for ADR 0026."""
    return TransientSignalResult(
        name=raw["name"] if named else None,
        source_node=raw.get("sourceNode", ""),
        injected_current=[float(v) for v in raw["injectedCurrent"]],
        nodes=[TransientNodeVoltage(id=n["id"], voltage=[float(v) for v in n["voltage"]]) for n in raw.get("nodes", [])],
        electrodes=[
            TransientElectrodeCurrent(
                id=e["id"],
                i1=[float(v) for v in e["i1"]],
                i2=[float(v) for v in e["i2"]],
            )
            for e in raw.get("electrodes", [])
        ],
    )


def is_transient_results_file(path: str | Path) -> bool:
    """True if the JSON at `path` is a transient results file (ADR 0015,
    top-level `"time"` key) rather than a frequency-domain one (ADR 0012,
    `"frequencies"`) — lets the GUI dispatch "Open results…" to the right
    loader/panel without the caller parsing JSON itself."""
    path = Path(path)
    try:
        raw = json.loads(path.read_text())
    except json.JSONDecodeError as exc:
        raise ResultsLoadError(f"{path}: invalid JSON ({exc})") from exc
    return "time" in raw
