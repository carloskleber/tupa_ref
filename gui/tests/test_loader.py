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
