import json
from pathlib import Path

import pytest

from tupa_gui.data import LineElement, MeshElement, StudyLoadError, load_study

COMMON = Path(__file__).resolve().parents[2] / "common"


def test_load_example1():
    study = load_study(COMMON / "buried_conductor_short.json")

    assert study.title == "Buried bare conductor, short"
    assert study.soil.conductivity == pytest.approx(0.01)
    assert [n.id for n in study.nodes] == ["Node_1", "Node_2"]
    assert study.node("Node_2").position == (2.0, 0.0, -0.5)
    assert [m.id for m in study.materials] == ["copper"]
    assert len(study.elements) == 1

    line = study.elements[0]
    assert isinstance(line, LineElement)
    assert line.from_node == "Node_1"
    assert line.to_node == "Node_2"
    assert line.segments == 2
    assert line.material == "copper"

    assert study.sources == []
    assert study.frequencies is None
    assert study.outputs is None


def test_load_portela1997_sources_frequencies_outputs():
    study = load_study(COMMON / "portela1997.json")

    assert [s.node for s in study.sources] == ["Node_1"]
    assert study.sources[0].current == complex(1.0, 0.0)

    assert study.frequencies is not None
    assert study.frequencies.min == pytest.approx(10.0)
    assert study.frequencies.max == pytest.approx(1.0e6)
    assert study.frequencies.points_per_decade == pytest.approx(1)

    assert study.outputs is not None
    assert study.outputs.nodes == []
    assert study.outputs.electrodes == []
    assert study.outputs.quantities == ["voltage", "i1", "i2", "inputImpedance"]


def test_load_example2_two_elements():
    study = load_study(COMMON / "buried_conductor_long.json")

    assert [n.id for n in study.nodes] == ["Node_1", "Node_2", "Node_3"]
    assert [e.id for e in study.elements] == ["Line_1", "Line_2"]

    assert study.signal is None


def test_load_portela1997_transient_signal_block():
    study = load_study(COMMON / "portela1997_transient.json")

    assert study.sources == []
    assert study.frequencies is None

    assert study.signal is not None
    signal = study.signal
    assert signal.waveform == "doubleExp"
    assert signal.imax == pytest.approx(30000.0)
    assert signal.front == "f1_2_50"
    assert signal.jones is False
    assert signal.source_node == "Node_1"
    assert signal.observe_nodes == ["Node_1", "Node_2"]
    assert signal.observe_electrodes == ["Line_1_e1"]
    assert signal.nyquist_hz == pytest.approx(1.0e6)
    assert signal.fft_points == 1024
    assert signal.freq_zero_hz == pytest.approx(1.0e-6)  # default, omitted in the JSON


def test_load_portela_mesh_element():
    study = load_study(COMMON / "portelaMesh.json")

    assert study.nodes == []  # the mesh plants its own main nodes, none pre-declared
    assert len(study.elements) == 1

    mesh = study.elements[0]
    assert isinstance(mesh, MeshElement)
    assert mesh.id == "portelaMesh"
    assert mesh.position == (0.0, -32.0, -1.0)
    assert mesh.length_x == pytest.approx(32.0)
    assert mesh.length_y == pytest.approx(32.0)
    assert mesh.rows_x == 5
    assert mesh.rows_y == 5
    assert mesh.radius == pytest.approx(0.005)
    assert mesh.segments == 5
    assert mesh.material == "copper"

    # node/bar expansion mirrors fortran/src/element/Mesh.f90's grid formula
    # (fortran/test/test_mesh_element.f90 covers the Fortran side directly;
    # this pins the GUI's independent Python re-derivation of the same math).
    positions = mesh.node_positions()
    assert len(positions) == 25
    assert positions["portelaMesh-0000"] == pytest.approx((0.0, -32.0, -1.0))
    assert positions["portelaMesh-0404"] == pytest.approx((32.0, 0.0, -1.0))
    assert positions["portelaMesh-0202"] == pytest.approx((16.0, -16.0, -1.0))

    bars = mesh.bars()
    assert len(bars) == 40  # rowsX*(rowsY-1) + rowsY*(rowsX-1) = 5*4 + 5*4
    assert ("portelaMesh-0000", "portelaMesh-0001") in bars
    assert ("portelaMesh-0000", "portelaMesh-0100") in bars


def test_unknown_element_type_is_skipped(tmp_path, caplog):
    data = {
        "title": "t",
        "soil": {"conductivity": 0.01, "permittivity": 1.0, "permeability": 1.0},
        "nodes": [{"id": "Node_1", "position": [0.0, 0.0, 0.0]}],
        "materials": [],
        "elements": [{"type": "circumference", "id": "Ring_1"}],
    }
    path = tmp_path / "study.json"
    path.write_text(json.dumps(data))

    with caplog.at_level("WARNING"):
        study = load_study(path)

    assert study.elements == []
    assert "unknown type" in caplog.text


def test_invalid_json_raises(tmp_path):
    path = tmp_path / "bad.json"
    path.write_text("{not json")

    with pytest.raises(StudyLoadError):
        load_study(path)


def test_missing_soil_raises(tmp_path):
    path = tmp_path / "no_soil.json"
    path.write_text(json.dumps({"title": "t"}))

    with pytest.raises(StudyLoadError):
        load_study(path)


LEGACY_CASES = [
    *(f"torre{i}" for i in range(3)),
    *(f"linha{i}" for i in range(6)),
    "linha5a",
]


@pytest.mark.parametrize("case", LEGACY_CASES)
def test_legacy_base_case_loads(case):
    study = load_study(COMMON / f"{case}.json")
    assert study.nodes and study.elements
    assert study.signal is not None and study.signal.portela is not None
    # every element references declared nodes (catenary included)
    ids = {n.id for n in study.nodes}
    assert all(e.from_node in ids and e.to_node in ids for e in study.elements)


def test_catenary_and_portela_soil():
    from tupa_gui.data import CatenaryElement

    linha4 = load_study(COMMON / "linha4.json")
    cats = [e for e in linha4.elements if isinstance(e, CatenaryElement)]
    assert cats and all(c.sag == 5.0 for c in cats)
    c = cats[0]
    pts = c.chain_points(linha4.node(c.from_node).position, linha4.node(c.to_node).position)
    assert len(pts) == c.segments + 1
    # midpoint drops by exactly `sag` below the chord midpoint (theory.md §4.4)
    a, b = linha4.node(c.from_node).position, linha4.node(c.to_node).position
    assert pts[c.segments // 2][2] == pytest.approx((a[2] + b[2]) / 2 - c.sag)

    torre1 = load_study(COMMON / "torre1.json")
    assert torre1.soil.type == "portela" and torre1.soil.kr == 0.00271357
    assert torre1.soil.conductivity is None


def test_signal_forms():
    """`signal` as one waveform, simultaneous `sources` and independent `signals` (ADR 0026)."""
    single = load_study(COMMON / "portela1997_transient.json").signal
    assert single.form == "single" and not single.independent and len(single.excitations) == 1

    superposed = load_study(COMMON / "portela1997_transient_multi.json").signal
    assert superposed.form == "sources" and not superposed.independent
    assert [(e.node, e.waveform) for e in superposed.excitations] == [
        ("Node_1", "doubleExp"),
        ("Node_2", "doubleExp"),
        ("Node_1", "sine"),
    ]
    assert superposed.excitations[2].frequency_hz == pytest.approx(5000.0)
    assert superposed.excitations[2].phase_deg == pytest.approx(90.0)
    assert superposed.excitations[1].imax == pytest.approx(-30000.0)

    listed = load_study(COMMON / "portela1997_transient_signals.json").signal
    assert listed.form == "signals" and listed.independent
    assert [e.name for e in listed.excitations] == ["front_1p2us", "front_8us", "far_end_hit"]
    # an entry without its own node takes signal.sourceNode
    assert [e.node for e in listed.excitations] == ["Node_1", "Node_1", "Node_2"]
    assert listed.excitations[2].portela is not None
    assert listed.excitations[2].portela.t_front == pytest.approx(2e-6)
    assert listed.excitations[1].front == "f1_2_200"
    assert listed.observe_electrodes == ["Line_1_e4", "Line_1_e7"]


def test_signals_list_uses_default_names_and_block_terminals(tmp_path):
    import json

    case = json.loads((COMMON / "portela1997_transient_signals.json").read_text())
    case["signal"]["returnNode"] = "Node_2"
    case["signal"]["quantity"] = "voltage"
    del case["signal"]["signals"][0]["name"]
    case["signal"]["signals"][1]["quantity"] = "current"
    path = tmp_path / "case.json"
    path.write_text(json.dumps(case))

    listed = load_study(path).signal
    assert listed.excitations[0].name == "signal1"
    # block-level terminals are the defaults, an entry overrides them
    assert listed.excitations[0].return_node == "Node_2" and listed.excitations[0].quantity == "voltage"
    assert listed.excitations[1].quantity == "current"


def test_signal_options_and_terms_heidler():
    nlt = load_study(COMMON / "portela1997_transient_nlt.json").signal
    assert nlt.transform == "nlt"
    hann = load_study(COMMON / "portela1997_transient_hann_time.json").signal
    assert hann.window is not None and (hann.window.type, hann.window.placement) == ("hann", "time")
    interp = load_study(COMMON / "portela1997_transient_interpolated.json").signal
    assert interp.transfer_function == "interpolated"
    # a case with the channel and a two-node source (ADR 0025) loads too
    tower = load_study(COMMON / "channel_tower.json").signal
    assert tower.excitations[0].return_node is not None or tower.excitations[0].node


def test_channel_element_loads_and_plants_its_end_nodes():
    # ADR 0025: the channel was skipped as an unknown element type
    tower = load_study(COMMON / "channel_tower.json")
    ch = next(e for e in tower.elements if e.id == "ch")
    assert (ch.strike, ch.length, ch.radius, ch.speed) == ("Ttop", 1000.0, 0.03, 1.5e8)
    assert (ch.base_id, ch.top_id) == ("ch-base", "ch-top")
    ends = ch.end_positions(tower.node("Ttop").position)
    assert ends["ch-base"] == tower.node("Ttop").position
    assert ends["ch-top"][2] == tower.node("Ttop").position[2] + 1000.0  # vertical: incidence 0

    free = load_study(COMMON / "channel_unloaded.json")
    ch = next(e for e in free.elements if e.id == "ch")
    assert ch.strike is None and ch.position == (0.0, 0.0, 0.0) and ch.segments == 200
