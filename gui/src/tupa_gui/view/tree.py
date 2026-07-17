"""QTreeView model for an input Study (G0, docs/GUI_SDD.md §7)."""

from __future__ import annotations

from PySide6.QtCore import Qt
from PySide6.QtGui import QStandardItem, QStandardItemModel

from tupa_gui.data import Study

# Qt.UserRole payload on a node/element's tree item: ("node"|"element", id).
# Lets the controller (main_window) map a tree selection to the matching 3D
# entity, and vice versa, without re-deriving it from the item's label text.
ENTITY_ROLE = Qt.ItemDataRole.UserRole + 1


def _row(label: str, value: str = "") -> QStandardItem:
    item = QStandardItem(f"{label}  {value}" if value else label)
    item.setEditable(False)
    return item


def build_study_model(study: Study) -> tuple[QStandardItemModel, dict[tuple[str, str], QStandardItem]]:
    """Build the tree model plus a (kind, id) -> item map for selection sync."""
    model = QStandardItemModel()
    model.setHorizontalHeaderLabels([study.title])
    root = model.invisibleRootItem()
    entity_items: dict[tuple[str, str], QStandardItem] = {}

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
        item = _row(n.id, str(tuple(n.position)))
        item.setData(("node", n.id), ENTITY_ROLE)
        entity_items[("node", n.id)] = item
        nodes.appendRow(item)
    root.appendRow(nodes)

    elements = _row("Elements", f"({len(study.elements)})")
    for e in study.elements:
        item = _row(e.id, f"line {e.from_node} -> {e.to_node}")
        item.setData(("element", e.id), ENTITY_ROLE)
        entity_items[("element", e.id)] = item
        item.appendRow(_row("radius", f"{e.radius} m"))
        item.appendRow(_row("segments", str(e.segments)))
        item.appendRow(_row("material", e.material))
        elements.appendRow(item)
    root.appendRow(elements)

    sources = _row("Sources", f"({len(study.sources)})")
    for s in study.sources:
        unit = "V" if s.is_voltage else "A"
        sources.appendRow(_row(s.node, f"{s.current.real:g}{s.current.imag:+g}j {unit}"))
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

    signal = _row("Signal")
    if study.signal is not None:
        s = study.signal
        signal.appendRow(_row("waveform", s.waveform))
        signal.appendRow(_row("imax", f"{s.imax} A"))
        if s.front is not None:
            signal.appendRow(_row("front", s.front))
            signal.appendRow(_row("jones", str(s.jones)))
        signal.appendRow(_row("sourceNode", s.source_node))
        signal.appendRow(_row("observeNodes", ", ".join(s.observe_nodes)))
        signal.appendRow(_row("observeElectrodes", ", ".join(s.observe_electrodes) if s.observe_electrodes else "(none)"))
        signal.appendRow(_row("nyquistHz", f"{s.nyquist_hz} Hz"))
        signal.appendRow(_row("fftPoints", str(s.fft_points)))
        signal.appendRow(_row("freqZeroHz", f"{s.freq_zero_hz} Hz"))
    else:
        signal.setText("Signal  (none)")
    root.appendRow(signal)

    return model, entity_items
