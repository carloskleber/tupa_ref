import json
from pathlib import Path

import pytest

from tupa_gui.data import ResultsLoadError, is_transient_results_file, load_transient_results

FIXTURES = Path(__file__).resolve().parent / "fixtures"


def test_load_transient_results():
    results = load_transient_results(FIXTURES / "transient_results.json")

    assert results.title == "Portela 1997 validation conductor - transient GPR under 1.2/50 us surge"
    assert results.source_node == "Node_1"
    assert len(results.time) == 4
    assert results.time[0] == pytest.approx(0.0)
    assert results.time[-1] == pytest.approx(1.5e-6)
    assert len(results.injected_current) == len(results.time)

    node = results.node("Node_2")
    assert len(node.voltage) == len(results.time)
    assert node.voltage[1] == pytest.approx(198765.43)

    electrode = results.electrode("Line_1_e1")
    assert len(electrode.i1) == len(results.time)
    assert len(electrode.i2) == len(results.time)
    assert electrode.i1[0] == pytest.approx(0.0)


def test_unknown_node_and_electrode_raise_keyerror():
    results = load_transient_results(FIXTURES / "transient_results.json")

    with pytest.raises(KeyError):
        results.node("no such node")
    with pytest.raises(KeyError):
        results.electrode("no such electrode")


def test_is_transient_results_file_detects_shape():
    assert is_transient_results_file(FIXTURES / "transient_results.json") is True
    assert is_transient_results_file(FIXTURES / "example4_results.json") is False


def test_invalid_json_raises(tmp_path):
    path = tmp_path / "bad.json"
    path.write_text("{not json")

    with pytest.raises(ResultsLoadError):
        load_transient_results(path)


def test_missing_time_raises(tmp_path):
    path = tmp_path / "no_time.json"
    path.write_text(json.dumps({"title": "t"}))

    with pytest.raises(ResultsLoadError):
        load_transient_results(path)
