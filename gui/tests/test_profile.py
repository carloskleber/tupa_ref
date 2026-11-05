from pathlib import Path

import pytest

from tupa_gui.data import build_profiles, load_results, load_study

FIXTURES = Path(__file__).resolve().parent / "fixtures"


def test_dc_frequency_block_loads():
    study = load_study(FIXTURES / "rod_dc.json")
    assert study.frequencies.dc is True
    assert study.frequencies.min == 10.0


def test_dc_only_block_loads(tmp_path):
    import json

    raw = json.loads((FIXTURES / "rod_dc.json").read_text())
    raw["frequencies"] = {"dc": True, "dcFrequency": 0.01}
    path = tmp_path / "dc_only.json"
    path.write_text(json.dumps(raw))
    f = load_study(path).frequencies
    assert f.dc and f.dc_frequency == 0.01 and f.min is None


def test_rod_profile_at_dc_point():
    study = load_study(FIXTURES / "rod_dc.json")
    results = load_results(FIXTURES / "rod_dc_results.json")
    profiles, skipped = build_profiles(study, results, 0)

    assert not skipped
    assert len(profiles) == 1
    p = profiles[0]
    assert p.frequency == pytest.approx(1e-3)
    assert p.chain.nodes[0] == "Node_1" and p.chain.nodes[-1] == "Node_2"
    assert len(p.chain.nodes) == 7 and len(p.longitudinal) == 6 == len(p.transversal)
    assert p.chain.s_nodes[-1] == pytest.approx(3.0)
    # the injected 1 A all leaks into the soil; I_l decays from ~0.9 A to ~0.1 A
    assert p.total_leakage.real == pytest.approx(1.0, abs=1e-6)
    assert p.longitudinal[0].real > p.longitudinal[-1].real > 0
    assert all(abs(v.imag) < 1e-4 * abs(v.real) for v in p.voltage)


def test_chain_walked_backwards_flips_longitudinal_sign():
    from tupa_gui.data.profile import Segment, find_chains

    # two segments defined head-to-head: the walk enters the second one at its to-node
    segs = [Segment("a", "N1", "N2", 1.0), Segment("b", "N3", "N2", 1.0)]
    (path,) = find_chains(segs)
    assert [(i, n) for i, n in path] == [(0, "N1"), (1, "N2")]
