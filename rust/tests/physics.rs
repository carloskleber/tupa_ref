//! Ports of the Fortran physics/assembly tests (`test_solve`, `test_sweep`,
//! `test_transient`, `test_mesh_element`, `test_validation`) plus a check
//! that every `common/*.json` case loads, validates and assembles.

use num_complex::Complex64;
use std::path::{Path, PathBuf};
use tupa::ctes::PI;
use tupa::element::{Catenary, Element, Line, MeshElement};
use tupa::material::{Linear, Medium};
use tupa::node::Node;
use tupa::signal::{new_double_exp_signal, new_sine_signal, tail_taper};
use tupa::structure::Structure;
use tupa::study::{Source, Study, log_frequency_axis};
use tupa::transient::{
    TransferFunction, Transform, TransientOptions, TransientSource, TransientSpec, Window,
    WindowPlacement, hann_half_window, transient_response,
};
use tupa::{load_study, validate_study_references};

fn common() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("common")
}

/// 10 m, r0 = 7 mm conductor at 0.5 m depth in σ = 0.01 S/m, εr = 10 soil.
fn portela_study(segments: usize) -> Study {
    let mut st = Structure::new(Medium::Linear(Linear::new("soil", 10.0, 1.0, 0.01)));
    st.add_node(Node::new("Node_1", [0.0, 0.0, -0.5]));
    st.add_node(Node::new("Node_2", [10.0, 0.0, -0.5]));
    st.add_material(Linear::new("copper", 1.0, 1.0, 5.96e7));
    st.add_element(Element::Line(Line::new(
        "Line_1", "Node_1", "Node_2", 0.007, segments, "copper",
    )));
    Study::new("portela", st)
}

fn unit_current(node: &str) -> Source {
    Source {
        node: node.into(),
        value: Complex64::new(1.0, 0.0),
        is_voltage: false,
        return_node: None,
    }
}

#[test]
fn passivity_plateau_and_sunde_dc_limit() {
    let mut study = portela_study(10);
    let freqs = [10.0, 1.0e2, 1.0e3, 1.0e4, 1.0e5, 1.0e6];
    let mut zmag = Vec::new();
    for &f in &freqs {
        let out = study.run(2.0 * PI * f, &[unit_current("Node_1")]).unwrap();
        let idx = study.structure.find_node_index("Node_1").unwrap();
        let zin = out.voltage[idx];
        assert!(
            zin.re >= -1e-9 * zin.norm().max(1.0),
            "passivity at {f} Hz: {zin}"
        );
        zmag.push(zin.norm());
    }
    assert!((zmag[0] - zmag[1]).abs() < 0.05 * zmag[1], "DC plateau");
    let (l, r0, depth, sigma) = (10.0_f64, 0.007_f64, 0.5_f64, 0.01_f64);
    let r_dc =
        1.0 / (2.0 * PI * sigma * l) * ((2.0 * l / r0).ln() + (2.0 * l / (2.0 * depth)).ln() - 2.0);
    assert!(
        (zmag[0] - r_dc).abs() < 0.15 * r_dc,
        "Sunde/Dwight: {} vs {r_dc}",
        zmag[0]
    );
}

#[test]
fn sweep_matches_manual_run_loop_and_bounds_hold() {
    let freqs = log_frequency_axis(1.0e2, 1.0e6, 5).unwrap();
    assert_eq!(freqs.len(), 5);
    assert!((freqs[0] - 1.0e2).abs() < 1e-6 && (freqs[4] - 1.0e6).abs() < 1.0);
    let ratio = freqs[1] / freqs[0];
    for k in 1..5 {
        assert!((freqs[k] / freqs[k - 1] - ratio).abs() < 1e-9 * ratio);
    }
    assert!(log_frequency_axis(1.0, 10.0, 1).is_err());

    let mut a = portela_study(4);
    a.run_sweep(&freqs, &[unit_current("Node_1")]).unwrap();
    let zin = a.input_impedance("Node_1").unwrap();
    let vmax = a.max_voltage_magnitude();
    let mut b = portela_study(4);
    for (k, &f) in freqs.iter().enumerate() {
        let out = b.run(2.0 * PI * f, &[unit_current("Node_1")]).unwrap();
        let idx = b.structure.find_node_index("Node_1").unwrap();
        assert!((out.voltage[idx] - zin[k]).norm() < 1e-12 * zin[k].norm());
        assert!(vmax[k] >= zin[k].norm() - 1e-12, "vmax bounds |Zin|");
    }
    assert!(a.input_impedance("Node_2").is_err());
}

#[test]
fn voltage_source_pins_node_voltage_and_matches_current_injection() {
    let freqs = [1.0e2, 1.0e4, 1.0e6];
    let u = Complex64::new(100.0, 0.0);
    let mut cur = portela_study(4);
    cur.run_sweep(&freqs, &[unit_current("Node_1")]).unwrap();
    let zin = cur.input_impedance("Node_1").unwrap();

    let mut volt = portela_study(4);
    volt.run_sweep(
        &freqs,
        &[Source {
            node: "Node_1".into(),
            value: u,
            is_voltage: true,
            return_node: None,
        }],
    )
    .unwrap();
    let idx = volt.structure.find_node_index("Node_1").unwrap();
    let zv_all = volt.input_impedance("Node_1").unwrap();
    for (k, zv) in zv_all.iter().enumerate() {
        assert!((volt.voltage_results.get(idx, k) - u).norm() < 1e-9 * u.norm());
        let zv = *zv;
        // input impedance defined by V/I with the effective source current
        assert!((zv - zin[k]).norm() < 1e-9 * zin[k].norm());
    }
}

#[test]
fn mixed_voltage_and_current_sources_superpose() {
    let mut study = portela_study(2);
    let om = 2.0 * PI * 1.0e3;
    let u = Complex64::new(50.0, 0.0);
    let i2 = Complex64::new(0.5, 0.25);
    let mixed = [
        Source {
            node: "Node_1".into(),
            value: u,
            is_voltage: true,
            return_node: None,
        },
        Source {
            node: "Node_2".into(),
            value: i2,
            is_voltage: false,
            return_node: None,
        },
    ];
    let out = study.run(om, &mixed).unwrap();
    let i1 = out.source_currents[0];
    assert_eq!(out.source_currents[1], i2, "current source keeps its value");
    let n1 = study.structure.find_node_index("Node_1").unwrap();
    assert!((out.voltage[n1] - u).norm() < 1e-9 * u.norm());

    let plain = study
        .run(
            om,
            &[
                Source {
                    node: "Node_1".into(),
                    value: i1,
                    is_voltage: false,
                    return_node: None,
                },
                Source {
                    node: "Node_2".into(),
                    value: i2,
                    is_voltage: false,
                    return_node: None,
                },
            ],
        )
        .unwrap();
    for (a, b) in out.voltage.iter().zip(&plain.voltage) {
        assert!((a - b).norm() < 1e-9 * b.norm().max(1.0));
    }
}

#[test]
fn transient_tracks_low_frequency_impedance_and_antialias_option() {
    let mut study = portela_study(10);
    let spec = |aa: f64| TransientSpec {
        sources: vec![TransientSource {
            node: "Node_1".into(),
            signal: new_double_exp_signal(1.0e3, "f250_2500", false).unwrap(),
            return_node: None,
            is_voltage: false,
        }],
        observe_nodes: vec!["Node_1".into()],
        observe_electrodes: vec!["Line_1_e1".into()],
        nyquist_hz: 1.0e4,
        fft_points: 1024,
        freq_zero_hz: 1.0e-6,
        options: TransientOptions {
            antialias_start: aa,
            ..Default::default()
        },
    };
    let base = transient_response(&mut study, &spec(1.0)).unwrap();
    assert_eq!(base.t.len(), 1024);
    assert_eq!(base.node_responses[0].len(), 1024);
    assert!(base.node_responses[0].iter().all(|v| v.is_finite()));
    assert!(base.t[0].abs() < 1e-12 && base.t.windows(2).all(|w| w[1] > w[0]));
    assert_eq!(base.i1_responses.len(), 1);

    let zin = study.input_impedance("Node_1").unwrap();
    assert!(zin.iter().all(|z| z.re >= -1e-9 * z.norm().max(1.0)));
    let z_low = zin[1].norm();
    let ipeak = base.injected_currents[0]
        .iter()
        .enumerate()
        .fold((0, f64::MIN), |m, (i, &v)| if v > m.1 { (i, v) } else { m })
        .0;
    let ratio = base.node_responses[0][ipeak] / base.injected_currents[0][ipeak];
    assert!((ratio - z_low).abs() < 0.25 * z_low, "{ratio} vs {z_low}");

    let half = transient_response(&mut study, &spec(0.25)).unwrap();
    let dmax = half.node_responses[0]
        .iter()
        .zip(&base.node_responses[0])
        .fold(0.0_f64, |m, (a, b)| m.max((a - b).abs()));
    assert!(dmax > 0.0 && half.node_responses[0].iter().all(|v| v.is_finite()));

    let same = transient_response(&mut study, &spec(1.0)).unwrap();
    let dmax = same.node_responses[0]
        .iter()
        .zip(&base.node_responses[0])
        .fold(0.0_f64, |m, (a, b)| m.max((a - b).abs()));
    assert!(dmax <= 1e-12 * 1e3);

    let mut bad = spec(1.0);
    bad.fft_points = 1000;
    assert!(transient_response(&mut study, &bad).is_err());
}

/// ROADMAP Phase 9 (ADR 0015 amendment 2026-09-30): ports of the Phase 9
/// blocks of `fortran/test/test_transient.f90`.
fn slow_surge_spec(n: usize, observe: &[&str]) -> TransientSpec {
    TransientSpec {
        sources: vec![TransientSource {
            node: "Node_1".into(),
            signal: new_double_exp_signal(1.0e3, "f250_2500", false).unwrap(),
            return_node: None,
            is_voltage: false,
        }],
        observe_nodes: observe.iter().map(|s| s.to_string()).collect(),
        observe_electrodes: vec![],
        nyquist_hz: 1.0e4,
        fft_points: n,
        freq_zero_hz: 1.0e-6,
        options: TransientOptions::default(),
    }
}

fn max_abs_diff(a: &[Vec<f64>], b: &[Vec<f64>]) -> f64 {
    a.iter()
        .zip(b)
        .flat_map(|(x, y)| x.iter().zip(y).map(|(p, q)| (p - q).abs()))
        .fold(0.0, f64::max)
}

fn max_abs(a: &[Vec<f64>]) -> f64 {
    a.iter().flatten().fold(0.0_f64, |m, v| m.max(v.abs()))
}

#[test]
fn phase9_interpolated_transfer_function_tracks_full_solve() {
    let mut study = portela_study(10);
    let full =
        transient_response(&mut study, &slow_surge_spec(1024, &["Node_1", "Node_2"])).unwrap();
    let mut spec = slow_surge_spec(1024, &["Node_1", "Node_2"]);
    spec.options.transfer_function = TransferFunction::Interpolated;
    spec.options.scan_freq_hz = log_frequency_axis(1.0e-6, 1.0e4, 101).unwrap();
    let interp = transient_response(&mut study, &spec).unwrap();
    let peak = max_abs(&full.node_responses);
    assert!(max_abs_diff(&interp.node_responses, &full.node_responses) < 1e-4 * peak);
    // a scan axis that does not reach Nyquist is rejected (no extrapolation)
    spec.options.scan_freq_hz = log_frequency_axis(1.0e-6, 5.0e3, 101).unwrap();
    assert!(transient_response(&mut study, &spec).is_err());
    spec.options.scan_freq_hz = log_frequency_axis(1.0e-6, 1.0e4, 101).unwrap();
    spec.options.transform = Transform::Nlt;
    assert!(transient_response(&mut study, &spec).is_err());
}

#[test]
fn phase9_multiple_injections_superpose() {
    let mut study = portela_study(10);
    let obs = ["Node_1", "Node_2"];
    let full = transient_response(&mut study, &slow_surge_spec(1024, &obs)).unwrap();
    let half = new_double_exp_signal(0.5e3, "f250_2500", false).unwrap();
    let mut spec = slow_surge_spec(1024, &obs);
    spec.sources = vec![
        TransientSource {
            node: "Node_1".into(),
            signal: half.clone(),
            return_node: None,
            is_voltage: false,
        },
        TransientSource {
            node: "Node_1".into(),
            signal: half.clone(),
            return_node: None,
            is_voltage: false,
        },
    ];
    let two = transient_response(&mut study, &spec).unwrap();
    assert_eq!(two.injected_currents.len(), 2);
    assert!(
        max_abs_diff(&two.node_responses, &full.node_responses)
            < 1e-12 * max_abs(&full.node_responses)
    );

    let sine = new_sine_signal(200.0, 2.0e3, 30.0).unwrap();
    spec.sources = vec![
        TransientSource {
            node: "Node_1".into(),
            signal: half.clone(),
            return_node: None,
            is_voltage: false,
        },
        TransientSource {
            node: "Node_2".into(),
            signal: sine.clone(),
            return_node: None,
            is_voltage: false,
        },
    ];
    let both = transient_response(&mut study, &spec).unwrap();
    spec.sources = vec![TransientSource {
        node: "Node_1".into(),
        signal: half,
        return_node: None,
        is_voltage: false,
    }];
    let a = transient_response(&mut study, &spec).unwrap();
    spec.sources = vec![TransientSource {
        node: "Node_2".into(),
        signal: sine,
        return_node: None,
        is_voltage: false,
    }];
    let b = transient_response(&mut study, &spec).unwrap();
    let sum: Vec<Vec<f64>> = a
        .node_responses
        .iter()
        .zip(&b.node_responses)
        .map(|(x, y)| x.iter().zip(y).map(|(p, q)| p + q).collect())
        .collect();
    assert!(max_abs_diff(&both.node_responses, &sum) < 1e-10 * max_abs(&both.node_responses));
}

#[test]
fn phase9_window_placements() {
    let mut study = portela_study(10);
    let mut spec = slow_surge_spec(1024, &["Node_1"]);
    spec.options.window = Window::Hann;
    spec.options.window_placement = WindowPlacement::Time;
    let r = transient_response(&mut study, &spec).unwrap();
    let wave = spec.sources[0].signal.waveform(&r.t);
    let (taper, w) = (tail_taper(1024), hann_half_window(1024).unwrap());
    for k in 0..1024 {
        assert!((r.injected_currents[0][k] - wave[k] * taper[k] * w[k]).abs() < 1e-12 * 1e3);
    }
    spec.options.window_placement = WindowPlacement::Spectral;
    let hann = transient_response(&mut study, &spec).unwrap();
    let mut spec2 = slow_surge_spec(1024, &["Node_1"]);
    spec2.options.antialias_start = 1e-12;
    let tukey = transient_response(&mut study, &spec2).unwrap();
    assert!(
        max_abs_diff(&hann.node_responses, &tukey.node_responses)
            < 1e-8 * max_abs(&hann.node_responses)
    );
}

#[test]
fn phase9_nlt_beats_fft_on_a_short_record() {
    let mut study = portela_study(10);
    let long = transient_response(&mut study, &slow_surge_spec(4096, &["Node_1"])).unwrap();
    let fft = transient_response(&mut study, &slow_surge_spec(256, &["Node_1"])).unwrap();
    let mut spec = slow_surge_spec(256, &["Node_1"]);
    spec.options.transform = Transform::Nlt;
    let nlt = transient_response(&mut study, &spec).unwrap();
    let n = 128;
    let peak = long.node_responses[0][..n]
        .iter()
        .fold(0.0_f64, |m, v| m.max(v.abs()));
    let err = |r: &[f64]| {
        (0..n)
            .map(|k| (r[k] - long.node_responses[0][k]).abs())
            .fold(0.0, f64::max)
            / peak
    };
    let (e_nlt, e_fft) = (err(&nlt.node_responses[0]), err(&fft.node_responses[0]));
    assert!(e_nlt < 1e-4, "NLT error {e_nlt}");
    assert!(e_nlt < e_fft, "NLT {e_nlt} vs FFT {e_fft}");
}

fn mesh_structure(rows_x: usize, rows_y: usize, segments: usize, id: &str) -> Structure {
    let mut st = Structure::new(Medium::Linear(Linear::new("soil", 10.0, 1.0, 0.01)));
    st.add_material(Linear::new("copper", 1.0, 1.0, 5.96e7));
    st.add_element(Element::Mesh(MeshElement {
        id: id.into(),
        position: [0.0, 0.0, -1.0],
        length_x: 4.0,
        length_y: 4.0,
        rows_x,
        rows_y,
        radius: 0.01,
        segments,
        id_material: "copper".into(),
    }));
    st
}

#[test]
fn mesh_element_counts_ids_and_positions() {
    let mut st = mesh_structure(3, 3, 2, "M");
    st.assemble().unwrap();
    assert_eq!(st.nodes.len(), 21, "9 main + 12 internal");
    assert_eq!(st.electrodes.len(), 24, "12 bars * 2 segments");
    let at = |id: &str| st.nodes[st.find_node_index(id).unwrap()].p;
    assert_eq!(at("M-0000"), [0.0, 0.0, -1.0]);
    assert_eq!(at("M-0202"), [4.0, 4.0, -1.0]);
    assert_eq!(at("M-0102"), [4.0, 2.0, -1.0]);
}

#[test]
fn non_square_mesh_has_correct_connectivity() {
    // 3 rows (X-bars) × 4 cols: 3*3 X-bar segments + 4*2 Y-bar segments
    let mut st = mesh_structure(3, 4, 1, "G");
    st.assemble().unwrap();
    assert_eq!(st.nodes.len(), 12);
    assert_eq!(st.electrodes.len(), 17);
    let mut degree = vec![0usize; st.nodes.len()];
    for e in &st.electrodes {
        degree[e.node_indices[0]] += 1;
        degree[e.node_indices[1]] += 1;
    }
    let mut hist = [0usize; 5];
    for d in degree {
        hist[d] += 1;
    }
    // corners 4×2, edge nodes 6×3, interior 2×4 for a 3×4 grid
    assert_eq!((hist[2], hist[3], hist[4]), (4, 6, 2));
}

#[test]
fn mesh_element_rejects_bad_inputs() {
    for (rx, ry, seg, z) in [
        (1, 3, 1, -1.0),
        (101, 3, 1, -1.0),
        (3, 3, 0, -1.0),
        (3, 3, 1, 0.0),
    ] {
        let mut st = mesh_structure(rx, ry, seg, "M");
        if let Element::Mesh(m) = &mut st.elements[0] {
            m.position[2] = z;
        }
        assert!(
            st.assemble().is_err(),
            "({rx},{ry},{seg},{z}) must be rejected"
        );
    }
}

#[test]
fn later_line_can_reference_earlier_mesh_node() {
    let mut st = mesh_structure(2, 2, 1, "M2");
    st.add_node(Node::new("Top", [0.0, 0.0, -0.2]));
    st.add_element(Element::Line(Line::new(
        "Down", "Top", "M2-0000", 0.01, 2, "copper",
    )));
    st.assemble().unwrap();
    assert!(st.find_electrode_index("Down_e2").is_some());
    assert!(st.find_node_index("Down_n1").is_some());
}

#[test]
fn assemble_is_idempotent_and_ids_are_discretised() {
    let mut s = portela_study(10);
    s.structure.assemble().unwrap();
    let (n, e) = (s.structure.nodes.len(), s.structure.electrodes.len());
    s.structure.assemble().unwrap();
    s.structure.assemble().unwrap();
    assert_eq!(
        (s.structure.nodes.len(), s.structure.electrodes.len()),
        (n, e)
    );
    assert_eq!((n, e), (11, 10));
    assert!(s.structure.find_electrode_index("Line_1_e1").is_some());
    assert!(s.structure.find_electrode_index("Line_1").is_none());
}

#[test]
fn assembly_errors_name_the_missing_reference() {
    let mut st = Structure::new(Medium::Linear(Linear::new("soil", 10.0, 1.0, 0.01)));
    st.add_node(Node::new("A", [0.0, 0.0, -1.0]));
    st.add_material(Linear::new("cu", 1.0, 1.0, 5.96e7));
    st.add_element(Element::Line(Line::new("L", "A", "B", 0.01, 2, "cu")));
    let e = st.assemble().unwrap_err();
    assert!(e.message().contains("end node 'B' not found"), "{e}");

    let mut st = Structure::new(Medium::Linear(Linear::new("soil", 10.0, 1.0, 0.01)));
    st.add_node(Node::new("A", [0.0, 0.0, -1.0]));
    st.add_node(Node::new("B", [1.0, 0.0, -1.0]));
    st.add_element(Element::Line(Line::new("L", "A", "B", 0.01, 2, "missing")));
    let e = st.assemble().unwrap_err();
    assert!(e.message().contains("material 'missing' not found"), "{e}");
}

/// Two towers 100 m apart at 30 m, 4-segment catenary with 5 m sag.
fn catenary_structure(sag: f64) -> Structure {
    let mut st = Structure::new(Medium::Linear(Linear::new("soil", 10.0, 1.0, 0.01)));
    st.add_node(Node::new("A", [0.0, 0.0, 30.0]));
    st.add_node(Node::new("B", [100.0, 0.0, 30.0]));
    st.add_material(Linear::new("steel", 1.0, 100.0, 5.88e6));
    st.add_element(Element::Catenary(Catenary {
        line: Line::new("C", "A", "B", 0.005, 4, "steel"),
        sag,
    }));
    st
}

#[test]
fn catenary_nodes_follow_the_parabolic_profile() {
    // theory.md §4.4: z = z_chord - 4·sag·s(1-s); midspan drops by the sag
    let mut st = catenary_structure(5.0);
    st.assemble().unwrap();
    let expect = [(25.0, 30.0 - 3.75), (50.0, 25.0), (75.0, 30.0 - 3.75)];
    for (k, (x, z)) in expect.iter().enumerate() {
        let p = st.nodes[st.find_node_index(&format!("C_n{}", k + 1)).unwrap()].p;
        assert!(
            (p[0] - x).abs() < 1e-12 && p[1] == 0.0 && (p[2] - z).abs() < 1e-12,
            "{p:?}"
        );
    }
    assert_eq!(st.electrodes.len(), 4);
    assert!(st.find_electrode_index("C_e4").is_some());

    let e = catenary_structure(40.0).assemble().unwrap_err();
    assert!(
        e.message().contains("crosses the air-soil interface"),
        "{e}"
    );
}

#[test]
fn every_common_case_loads_validates_and_assembles() {
    let mut n_cases = 0;
    for entry in std::fs::read_dir(common()).unwrap() {
        let path = entry.unwrap().path();
        if path.extension().and_then(|e| e.to_str()) != Some("json") {
            continue;
        }
        let mut case = load_study(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()));
        validate_study_references(&mut case).unwrap_or_else(|e| panic!("{}: {e}", path.display()));
        assert!(
            !case.study.structure.electrodes.is_empty(),
            "{}",
            path.display()
        );
        n_cases += 1;
    }
    assert!(n_cases >= 25, "found only {n_cases} cases");
}

#[test]
fn portela_mesh_topology_matches_fortran_pins() {
    // common/README: 185 nodes, 200 electrodes (verified in test_mesh_element.f90)
    let mut case = load_study(common().join("portelaMesh.json")).unwrap();
    validate_study_references(&mut case).unwrap();
    assert_eq!(case.study.structure.nodes.len(), 185);
    assert_eq!(case.study.structure.electrodes.len(), 200);
}

#[test]
fn rod_air_is_nan_free_and_near_analytical_resistance() {
    // ADR 0019: air hardcoded to vacuum; README quotes Zin(low f) ≈ 20.9 Ω
    let mut case = load_study(common().join("rod_air.json")).unwrap();
    validate_study_references(&mut case).unwrap();
    let sources = case.sources.clone().unwrap();
    let freqs = vec![10.0, 100.0];
    case.study.run_sweep(&freqs, &sources).unwrap();
    let zin = case.study.input_impedance(&sources[0].node).unwrap();
    assert!(zin.iter().all(|z| z.re.is_finite() && z.im.is_finite()));
    assert!((zin[0].re - 20.9).abs() < 0.5, "Zin(10 Hz) = {}", zin[0]);
}

// ---------------------------------------------------------------------
// ROADMAP Phase 10b — lightning channel (ADR 0025): ports of
// `fortran/test/test_channel.f90`.
// ---------------------------------------------------------------------

use tupa::channel_calibration::calibrate_channel;
use tupa::element::Channel;
use tupa::element::channel::Piecewise;
use tupa::mesh::ImageModel;

fn uniform_breaks(length: f64, n: usize) -> Vec<f64> {
    (0..=n).map(|k| length * k as f64 / n as f64).collect()
}

/// 30 m tower with a 3 m footing, 300 m loaded channel above its top.
fn tower_with_channel() -> Study {
    let mut st = Structure::new(Medium::Linear(Linear::new("soil", 10.0, 1.0, 0.001)));
    st.add_node(Node::new("Tfoot", [0.0, 0.0, 0.0]));
    st.add_node(Node::new("Ttop", [0.0, 0.0, 30.0]));
    st.add_node(Node::new("Rend", [0.0, 0.0, -3.0]));
    st.add_material(Linear::new("steel", 1.0, 1.0, 5.0e6));
    st.add_element(Element::Line(Line::new(
        "Tower", "Tfoot", "Ttop", 0.3, 6, "steel",
    )));
    st.add_element(Element::Line(Line::new(
        "Rod", "Tfoot", "Rend", 0.0125, 3, "steel",
    )));
    let mut ch = Channel::new("ch", "Ttop", 300.0, 0.03, uniform_breaks(300.0, 15));
    ch.speed = Piecewise::uniform(1.5e8);
    ch.resistance = Piecewise::uniform(0.5);
    st.add_element(Element::Channel(ch));
    Study::new("tower with channel", st)
}

#[test]
fn channel_assembly_tilted_and_profiled() {
    let mut st = Structure::new(Medium::Linear(Linear::new("soil", 10.0, 1.0, 0.01)));
    st.add_node(Node::new("S", [1.0, 2.0, 10.0]));
    st.add_node(Node::new("T", [1.0, 2.0, 0.0]));
    st.add_material(Linear::new("m", 1.0, 1.0, 5.0e6));
    st.add_element(Element::Line(Line::new("L", "S", "T", 0.1, 2, "m")));
    let mut ch = Channel::new("ch", "S", 100.0, 0.03, uniform_breaks(100.0, 4));
    ch.incidence_deg = 30.0;
    ch.azimuth_deg = 90.0;
    ch.speed = Piecewise::uniform(1.5e8);
    ch.resistance = Piecewise {
        up_to: vec![40.0, 1000.0],
        value: vec![2.0, 0.5],
    };
    st.add_element(Element::Channel(ch));
    st.assemble().unwrap();

    let base = st.find_node_index("ch-base").unwrap();
    let top = st.find_node_index("ch-top").unwrap();
    let s = st.find_node_index("S").unwrap();
    assert_ne!(base, s, "the base is a separate node");
    assert_eq!(st.nodes[base].p, st.nodes[s].p);
    let d = [
        st.nodes[top].p[0] - st.nodes[s].p[0],
        st.nodes[top].p[1] - st.nodes[s].p[1],
        st.nodes[top].p[2] - st.nodes[s].p[2],
    ];
    assert!(d[0].abs() < 1e-9 && (d[1] - 50.0).abs() < 1e-9);
    assert!((d[2] - 50.0 * 3.0_f64.sqrt()).abs() < 1e-9);
    assert_eq!(st.electrodes.len(), 6);
    let e1 = &st.electrodes[st.find_electrode_index("ch_e1").unwrap()];
    assert_eq!(e1.loading.unwrap().resistance, 2.0);
    let e4 = &st.electrodes[st.find_electrode_index("ch_e4").unwrap()];
    assert_eq!(e4.loading.unwrap().resistance, 0.5);
    assert!(
        st.electrodes[st.find_electrode_index("L_e1").unwrap()]
            .loading
            .is_none()
    );
}

#[test]
fn two_node_sources_current_dipole_and_voltage_gap() {
    let freq = [1.0e5, 1.0e6];
    let one = Complex64::new(1.0, 0.0);
    let mut dip = tower_with_channel();
    let mut dip_src = Source::new("Ttop", one, false);
    dip_src.return_node = Some("ch-base".into());
    dip.run_sweep(&freq, &[dip_src]).unwrap();

    let mut two = tower_with_channel();
    two.run_sweep(
        &freq,
        &[
            Source::new("Ttop", one, false),
            Source::new("ch-base", -one, false),
        ],
    )
    .unwrap();
    let i_top = dip.structure.find_node_index("Ttop").unwrap();
    let i_base = dip.structure.find_node_index("ch-base").unwrap();
    for k in 0..2 {
        for i in [i_top, i_base] {
            let (a, b) = (dip.voltage_results.get(i, k), two.voltage_results.get(i, k));
            assert!((a - b).norm() < 1e-9 * b.norm().max(1.0));
        }
    }

    let zin = dip.input_impedance("Ttop").unwrap();
    let gap = dip.voltage_results.get(i_top, 0) - dip.voltage_results.get(i_base, 0);
    assert!((zin[0] - gap).norm() < 1e-9);

    let mut v = tower_with_channel();
    let mut v_src = Source::new("Ttop", zin[0], true);
    v_src.return_node = Some("ch-base".into());
    v.run_sweep(&freq, &[v_src]).unwrap();
    let g = v.voltage_results.get(i_top, 0) - v.voltage_results.get(i_base, 0);
    assert!((g - zin[0]).norm() < 1e-8 * zin[0].norm(), "gap voltage");
    assert!(
        (v.sweep_source_currents_freq[0][0] - one).norm() < 1e-8,
        "the gap draws the dipole current"
    );
    assert!(
        (v.voltage_results.get(i_top, 0) - dip.voltage_results.get(i_top, 0)).norm()
            < 1e-8 * zin[0].norm()
    );

    // Dipoles sharing the return node add up there
    let om = 2.0 * PI * freq[0];
    let mut a_src = vec![
        Source::new("Ttop", one, false),
        Source::new("Tfoot", one, false),
    ];
    for s in &mut a_src {
        s.return_node = Some("ch-base".into());
    }
    let a = dip.run(om, &a_src).unwrap();
    let b = two
        .run(
            om,
            &[
                Source::new("Ttop", one, false),
                Source::new("Tfoot", one, false),
                Source::new("ch-base", -2.0 * one, false),
            ],
        )
        .unwrap();
    assert!(
        (a.voltage[i_base] - b.voltage[i_base]).norm() < 1e-9 * b.voltage[i_base].norm().max(1.0)
    );
}

#[test]
fn unloaded_channel_follows_chens_current() {
    // Chen's analytic step response of a monopole over ground (x2 the dipole
    // value, Baba & Rakov), convolved with the 1 us ramp
    use tupa::signal::new_portela_signal;
    const C0: f64 = 299_792_458.0;
    let (eta, a0) = (376.730313668_f64, 0.23);
    let step_integral = |z: f64, t: f64| -> f64 {
        let t0 = z / C0;
        if t <= t0 {
            return 0.0;
        }
        let m = 4000;
        let dt = (t - t0) / m as f64;
        (1..=m)
            .map(|k| {
                let tau = t0 + (k as f64 - 0.5) * dt;
                let arg = ((C0 * tau).powi(2) - z * z).sqrt() / a0;
                if arg <= 1.0 {
                    0.0
                } else {
                    2.0 * (2.0 / eta) * (PI / (2.0 * arg.ln())).atan() * dt
                }
            })
            .sum()
    };
    let mut st = Structure::new(Medium::Linear(Linear::new("soil", 10.0, 1.0, 0.01)));
    st.add_element(Element::Channel(Channel::new(
        "ch",
        "",
        1000.0,
        a0,
        uniform_breaks(1000.0, 100),
    )));
    let mut study = Study::new("chen", st);
    study.image_model = Some(ImageModel::Ideal);
    let spec = TransientSpec {
        sources: vec![TransientSource {
            node: "ch-base".into(),
            signal: new_portela_signal(5.0e6, 0.0, 1.0e-6, 1.0e3, 2.0e3).unwrap(),
            return_node: None,
            is_voltage: true,
        }],
        observe_nodes: vec!["ch-base".into()],
        observe_electrodes: vec!["ch_e31".into()],
        nyquist_hz: 5.0e6,
        fft_points: 512,
        freq_zero_hz: 1.0e-6,
        options: TransientOptions {
            transform: Transform::Nlt,
            ..TransientOptions::default()
        },
    };
    let r = transient_response(&mut study, &spec).unwrap();
    let z = 305.0;
    let (mut err, mut peak, mut n) = (0.0_f64, 0.0_f64, 0);
    for (k, &t) in r.t.iter().enumerate() {
        if !(3.0e-6..=4.5e-6).contains(&t) {
            continue;
        }
        let reference = 5.0e6 / 1.0e-6 * (step_integral(z, t) - step_integral(z, t - 1.0e-6));
        err = err.max((r.i1_responses[0][k] - reference).abs());
        peak = peak.max(reference);
        n += 1;
    }
    assert!(n >= 12);
    assert!(err < 0.015 * peak, "worst deviation {err} of peak {peak}");
}

#[test]
fn channel_calibration_reaches_target_speed() {
    let mut ch = Channel::new("ch", "", 1500.0, 0.03, uniform_breaks(1500.0, 75));
    ch.speed = Piecewise::uniform(1.5e8);
    ch.resistance = Piecewise::uniform(0.5);
    ch.want_calibration = true;
    calibrate_channel(&mut ch).unwrap();
    assert!(ch.calibrated);
    assert!((ch.calibrated_speed - 1.5e8).abs() < 9.0e5);
    assert!(
        ch.load_scale > 0.8 && ch.load_scale < 1.3,
        "{}",
        ch.load_scale
    );
}
