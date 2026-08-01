"""Loaders for input study JSON (common/README.md schema v1, ADR 0013) and
results JSON (ADR 0012 schema v0)."""

from __future__ import annotations

import json
import logging
from pathlib import Path

from .model import (
    ElectrodeCurrent,
    FrequencySweep,
    LineElement,
    Material,
    MeshElement,
    Node,
    NodeVoltage,
    Outputs,
    Results,
    Signal,
    Soil,
    Source,
    Study,
    TransientElectrodeCurrent,
    TransientNodeVoltage,
    TransientResults,
)

logger = logging.getLogger(__name__)


class StudyLoadError(ValueError):
    pass


class ResultsLoadError(ValueError):
    pass


def load_study(path: str | Path) -> Study:
    path = Path(path)
    try:
        raw = json.loads(path.read_text())
    except json.JSONDecodeError as exc:
        raise StudyLoadError(f"{path}: invalid JSON ({exc})") from exc

    try:
        soil = Soil(**raw["soil"])
        nodes = [Node(id=n["id"], position=tuple(n["position"])) for n in raw.get("nodes", [])]
        materials = [Material(**m) for m in raw.get("materials", [])]
    except KeyError as exc:
        raise StudyLoadError(f"{path}: missing required field {exc}") from exc

    elements: list[LineElement | MeshElement] = []
    for e in raw.get("elements", []):
        etype = e.get("type")
        if etype == "line":
            elements.append(
                LineElement(
                    id=e["id"],
                    from_node=e["from"],
                    to_node=e["to"],
                    radius=e["radius"],
                    segments=e["segments"],
                    material=e["material"],
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
                    segments=e["segments"],
                    material=e["material"],
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

    signal = None
    if "signal" in raw:
        sig = raw["signal"]
        signal = Signal(
            waveform=sig["waveform"],
            imax=sig["imax"],
            source_node=sig["sourceNode"],
            observe_nodes=list(sig["observeNodes"]),
            nyquist_hz=sig["nyquistHz"],
            fft_points=sig["fftPoints"],
            front=sig.get("front"),
            jones=sig.get("jones", False),
            observe_electrodes=list(sig.get("observeElectrodes", [])),
            freq_zero_hz=sig.get("freqZeroHz", 1.0e-6),
        )

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
        injected_current = [float(v) for v in raw["injectedCurrent"]]
        nodes = [TransientNodeVoltage(id=n["id"], voltage=[float(v) for v in n["voltage"]]) for n in raw.get("nodes", [])]
        electrodes = [
            TransientElectrodeCurrent(
                id=e["id"],
                i1=[float(v) for v in e["i1"]],
                i2=[float(v) for v in e["i2"]],
            )
            for e in raw.get("electrodes", [])
        ]
    except KeyError as exc:
        raise ResultsLoadError(f"{path}: missing required field {exc}") from exc

    return TransientResults(
        title=raw.get("title", path.stem),
        source_node=raw.get("sourceNode", ""),
        time=time,
        injected_current=injected_current,
        nodes=nodes,
        electrodes=electrodes,
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
