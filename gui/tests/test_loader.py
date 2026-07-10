import json
from pathlib import Path

import pytest

from tupa_gui.data import StudyLoadError, load_study

COMMON = Path(__file__).resolve().parents[2] / "common"


def test_load_example1():
    study = load_study(COMMON / "example1.json")

    assert study.title == "Example 1 - buried bare conductor"
    assert study.soil.conductivity == pytest.approx(0.01)
    assert [n.id for n in study.nodes] == ["Node_1", "Node_2"]
    assert study.node("Node_2").position == (2.0, 0.0, -0.5)
    assert [m.id for m in study.materials] == ["copper"]
    assert len(study.elements) == 1

    line = study.elements[0]
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
    study = load_study(COMMON / "example2.json")

    assert [n.id for n in study.nodes] == ["Node_1", "Node_2", "Node_3"]
    assert [e.id for e in study.elements] == ["Line_1", "Line_2"]


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
