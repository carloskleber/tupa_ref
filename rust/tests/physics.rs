//! Ports of the Fortran physics/assembly tests (`test_solve`, `test_sweep`,
//! `test_transient`, `test_mesh_element`, `test_validation`) plus a check
//! that every `common/*.json` case loads, validates and assembles.

use num_complex::Complex64;
use std::path::{Path, PathBuf};
use tupa::ctes::PI;
use tupa::element::{Catenary, Element, Line, MeshElement};
use tupa::material::{Linear, Medium};
use tupa::node::Node;
use tupa::signal::new_double_exp_signal;
use tupa::structure::Structure;
use tupa::study::{Source, Study, log_frequency_axis};
use tupa::transient::{TransientSpec, transient_response};
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
        },
        Source {
            node: "Node_2".into(),
            value: i2,
            is_voltage: false,
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
                },
                Source {
                    node: "Node_2".into(),
                    value: i2,
                    is_voltage: false,
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
        signal: new_double_exp_signal(1.0e3, "f250_2500", false).unwrap(),
        source_node: "Node_1".into(),
        observe_nodes: vec!["Node_1".into()],
        observe_electrodes: vec!["Line_1_e1".into()],
        nyquist_hz: 1.0e4,
        fft_points: 1024,
        freq_zero_hz: 1.0e-6,
        antialias_start: aa,
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
    let ipeak = base
        .injected_current
        .iter()
        .enumerate()
        .fold((0, f64::MIN), |m, (i, &v)| if v > m.1 { (i, v) } else { m })
        .0;
    let ratio = base.node_responses[0][ipeak] / base.injected_current[ipeak];
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
