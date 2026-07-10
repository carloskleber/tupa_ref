"""Loaders for input study JSON (common/README.md schema v0) and results
JSON (ADR 0012 schema v0)."""

from __future__ import annotations

import json
import logging
from pathlib import Path

from .model import ElectrodeCurrent, LineElement, Material, Node, NodeVoltage, Results, Soil, Study

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

    elements: list[LineElement] = []
    for e in raw.get("elements", []):
        if e.get("type") != "line":
            logger.warning("%s: skipping element %r of unknown type %r", path, e.get("id"), e.get("type"))
            continue
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

    return Study(title=raw.get("title", path.stem), soil=soil, nodes=nodes, materials=materials, elements=elements)


def _complex(raw: dict) -> complex:
    return complex(raw["re"], raw["im"])


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
