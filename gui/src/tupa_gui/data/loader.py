"""Loader for input study JSON (common/README.md schema v0)."""

from __future__ import annotations

import json
import logging
from pathlib import Path

from .model import LineElement, Material, Node, Soil, Study

logger = logging.getLogger(__name__)


class StudyLoadError(ValueError):
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
