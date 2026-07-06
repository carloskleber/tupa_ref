"""QTreeView model for an input Study (G0, docs/GUI_SDD.md §7)."""

from __future__ import annotations

from PySide6.QtGui import QStandardItem, QStandardItemModel

from tupa_gui.data import Study


def _row(label: str, value: str = "") -> QStandardItem:
    item = QStandardItem(f"{label}  {value}" if value else label)
    item.setEditable(False)
    return item


def build_study_model(study: Study) -> QStandardItemModel:
    model = QStandardItemModel()
    model.setHorizontalHeaderLabels([study.title])
    root = model.invisibleRootItem()

    soil = _row("Soil")
    soil.appendRow(_row("conductivity", f"{study.soil.conductivity} S/m"))
    soil.appendRow(_row("permittivity (εr)", str(study.soil.permittivity)))
    soil.appendRow(_row("permeability (μr)", str(study.soil.permeability)))
    root.appendRow(soil)

    materials = _row("Materials", f"({len(study.materials)})")
    for m in study.materials:
        item = _row(m.id)
        item.appendRow(_row("εr", str(m.epsilonr)))
        item.appendRow(_row("μr", str(m.mur)))
        item.appendRow(_row("σ", f"{m.sigma} S/m"))
        materials.appendRow(item)
    root.appendRow(materials)

    nodes = _row("Nodes", f"({len(study.nodes)})")
    for n in study.nodes:
        nodes.appendRow(_row(n.id, str(tuple(n.position))))
    root.appendRow(nodes)

    elements = _row("Elements", f"({len(study.elements)})")
    for e in study.elements:
        item = _row(e.id, f"line {e.from_node} -> {e.to_node}")
        item.appendRow(_row("radius", f"{e.radius} m"))
        item.appendRow(_row("segments", str(e.segments)))
        item.appendRow(_row("material", e.material))
        elements.appendRow(item)
    root.appendRow(elements)

    return model
