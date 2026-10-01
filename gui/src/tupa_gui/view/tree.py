"""QTreeView model for an input Study (G0, docs/GUI_SDD.md §7)."""

from __future__ import annotations

from PySide6.QtCore import Qt
from PySide6.QtGui import QStandardItem, QStandardItemModel

from tupa_gui.data import CatenaryElement, Excitation, LineElement, Study

# Qt.UserRole payload on a node/element's tree item: ("node"|"element", id).
# Lets the controller (main_window) map a tree selection to the matching 3D
# entity, and vice versa, without re-deriving it from the item's label text.
ENTITY_ROLE = Qt.ItemDataRole.UserRole + 1


def _row(label: str, value: str = "") -> QStandardItem:
    item = QStandardItem(f"{label}  {value}" if value else label)
    item.setEditable(False)
    return item


def _append_excitation_rows(parent: QStandardItem, ex: Excitation, with_node: bool = True) -> None:
    """The waveform fields of one excitation (the `signal` block or one entry
    of `signal.sources[]` / `signal.signals[]`)."""
    parent.appendRow(_row("waveform", ex.waveform))
    if ex.imax is not None:
        unit = "V" if ex.quantity == "voltage" else "A"
        parent.appendRow(_row("imax", f"{ex.imax} {unit}"))
    if ex.portela is not None:
        parent.appendRow(_row("α (front inclination)", str(ex.portela.alpha)))
        parent.appendRow(_row("tFront", f"{ex.portela.t_front} s"))
        parent.appendRow(_row("tTopEnd", f"{ex.portela.t_top_end} s"))
        parent.appendRow(_row("tTailEnd", f"{ex.portela.t_tail_end} s"))
    if ex.front is not None:
        parent.appendRow(_row("front", ex.front))
        parent.appendRow(_row("jones", str(ex.jones)))
    for k, t in enumerate(ex.terms, 1):
        parent.appendRow(_row(f"term {k}", f"i0 = {t.i0} A, n = {t.n}, τ1 = {t.tau1} s, τ2 = {t.tau2} s"))
    if ex.frequency_hz is not None:
        parent.appendRow(_row("frequencyHz", f"{ex.frequency_hz} Hz"))
        parent.appendRow(_row("phaseDeg", str(ex.phase_deg)))
    if with_node:
        parent.appendRow(_row("sourceNode", ex.node))
    if ex.return_node:
        parent.appendRow(_row("returnNode", ex.return_node))
    if ex.quantity != "current":
        parent.appendRow(_row("quantity", ex.quantity))


def build_study_model(study: Study) -> tuple[QStandardItemModel, dict[tuple[str, str], QStandardItem]]:
    """Build the tree model plus a (kind, id) -> item map for selection sync."""
    model = QStandardItemModel()
    model.setHorizontalHeaderLabels([study.title])
    root = model.invisibleRootItem()
    entity_items: dict[tuple[str, str], QStandardItem] = {}

    soil = _row("Soil")
    sl = study.soil
    soil.appendRow(_row("type", sl.type))
    if sl.conductivity is not None:
        soil.appendRow(_row("conductivity", f"{sl.conductivity} S/m"))
    if sl.permittivity is not None:
        soil.appendRow(_row("permittivity (εr)", str(sl.permittivity)))
    soil.appendRow(_row("permeability (μr)", str(sl.permeability)))
    if sl.sigma0 is not None:
        soil.appendRow(_row("σ0", f"{sl.sigma0} S/m"))
    if sl.alpha0 is not None:
        soil.appendRow(_row("α0", str(sl.alpha0)))
    if sl.kr is not None:
        soil.appendRow(_row("kr", str(sl.kr)))
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
        if isinstance(e, LineElement):
            kind = "catenary" if isinstance(e, CatenaryElement) else "line"
            item = _row(e.id, f"{kind} {e.from_node} -> {e.to_node}")
            item.setData(("element", e.id), ENTITY_ROLE)
            entity_items[("element", e.id)] = item
            if isinstance(e, CatenaryElement):
                item.appendRow(_row("sag", f"{e.sag} m"))
            item.appendRow(_row("radius", f"{e.radius} m"))
            item.appendRow(_row("segments", str(e.segments)))
            item.appendRow(_row("material", e.material))
        else:  # MeshElement (ADR 0020)
            item = _row(e.id, f"mesh {e.rows_x}x{e.rows_y} rows @ {tuple(e.position)}")
            item.setData(("element", e.id), ENTITY_ROLE)
            entity_items[("element", e.id)] = item
            item.appendRow(_row("position", str(tuple(e.position))))
            item.appendRow(_row("lengthX", f"{e.length_x} m"))
            item.appendRow(_row("lengthY", f"{e.length_y} m"))
            item.appendRow(_row("rowsX", str(e.rows_x)))
            item.appendRow(_row("rowsY", str(e.rows_y)))
            item.appendRow(_row("radius", f"{e.radius} m"))
            item.appendRow(_row("segments (per bar)", str(e.segments)))
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
        if s.form == "single":
            _append_excitation_rows(signal, s.excitations[0])
        else:
            heading = "independent signals" if s.independent else "simultaneous sources"
            signal.appendRow(_row("form", f"{heading} ({len(s.excitations)})"))
            for k, ex in enumerate(s.excitations, 1):
                item = _row(f"{k}. {ex.name}" if ex.name else f"{k}.", f"{ex.waveform} at {ex.node}")
                _append_excitation_rows(item, ex, with_node=False)
                signal.appendRow(item)
        signal.appendRow(_row("observeNodes", ", ".join(s.observe_nodes)))
        signal.appendRow(_row("observeElectrodes", ", ".join(s.observe_electrodes) if s.observe_electrodes else "(none)"))
        signal.appendRow(_row("nyquistHz", f"{s.nyquist_hz} Hz"))
        signal.appendRow(_row("fftPoints", str(s.fft_points)))
        signal.appendRow(_row("freqZeroHz", f"{s.freq_zero_hz} Hz"))
        if s.antialias_start is not None:
            signal.appendRow(_row("antialiasStart", str(s.antialias_start)))
        if s.window is not None:
            signal.appendRow(_row("window", f"{s.window.type} ({s.window.placement})"))
        if s.transform != "fft":
            signal.appendRow(_row("transform", s.transform + (f", c = {s.nlt_damping} 1/s" if s.nlt_damping else "")))
        if s.transfer_function != "full":
            signal.appendRow(_row("transferFunction", s.transfer_function))
    else:
        signal.setText("Signal  (none)")
    root.appendRow(signal)

    return model, entity_items
