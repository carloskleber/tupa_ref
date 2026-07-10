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

    sources = _row("Sources", f"({len(study.sources)})")
    for s in study.sources:
        sources.appendRow(_row(s.node, f"{s.current.real:g}{s.current.imag:+g}j A"))
    root.appendRow(sources)

    frequencies = _row("Frequencies")
    if study.frequencies is not None:
        f = study.frequencies
        frequencies.appendRow(_row("min", f"{f.min} Hz"))
        frequencies.appendRow(_row("max", f"{f.max} Hz"))
        frequencies.appendRow(_row("pointsPerDecade", str(f.points_per_decade)))
    else:
        frequencies.setText("Frequencies  (none)")
    root.appendRow(frequencies)

    outputs = _row("Outputs")
    if study.outputs is not None:
        o = study.outputs
        outputs.appendRow(_row("nodes", ", ".join(o.nodes) if o.nodes else "(all)"))
        outputs.appendRow(_row("electrodes", ", ".join(o.electrodes) if o.electrodes else "(all)"))
        outputs.appendRow(_row("quantities", ", ".join(o.quantities) if o.quantities else "(all)"))
    else:
        outputs.setText("Outputs  (none, everything stored)")
    root.appendRow(outputs)

    return model
