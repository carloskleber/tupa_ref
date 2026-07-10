import json
from pathlib import Path

import pytest

from tupa_gui.data import ResultsLoadError, load_results

FIXTURES = Path(__file__).resolve().parent / "fixtures"


def test_load_example4_results():
    results = load_results(FIXTURES / "example4_results.json")

    assert results.title == "Example 4 - Portela 1997 buried conductor, full sweep"
    assert len(results.frequencies) == 9
    assert results.frequencies[0] == pytest.approx(100.0)
    assert results.frequencies[-1] == pytest.approx(1.0e6)

    node = results.node("Node_1")
    assert len(node.voltage) == len(results.frequencies)
    assert node.voltage[0] == pytest.approx(14.2856667 - 0.0277243672j)

    electrode = results.electrode("Line_1_e1")
    assert len(electrode.i1) == len(results.frequencies)
    assert len(electrode.i2) == len(results.frequencies)
    assert electrode.i1[0] == pytest.approx(1.0, abs=1e-6)

    assert results.input_impedance is not None
    assert len(results.input_impedance) == len(results.frequencies)
    assert results.input_impedance[0] == pytest.approx(node.voltage[0])


def test_unknown_node_and_electrode_raise_keyerror():
    results = load_results(FIXTURES / "example4_results.json")

    with pytest.raises(KeyError):
        results.node("no such node")
    with pytest.raises(KeyError):
        results.electrode("no such electrode")


def test_missing_derived_input_impedance_is_none(tmp_path):
    data = {
        "title": "t",
        "frequencies": [100.0],
        "nodes": [{"id": "Node_1", "voltage": [{"re": 1.0, "im": 0.0}]}],
        "electrodes": [],
        "derived": {},
    }
    path = tmp_path / "results.json"
    path.write_text(json.dumps(data))

    results = load_results(path)

    assert results.input_impedance is None


def test_invalid_json_raises(tmp_path):
    path = tmp_path / "bad.json"
    path.write_text("{not json")

    with pytest.raises(ResultsLoadError):
        load_results(path)


def test_missing_frequencies_raises(tmp_path):
    path = tmp_path / "no_frequencies.json"
    path.write_text(json.dumps({"title": "t"}))

    with pytest.raises(ResultsLoadError):
        load_results(path)
