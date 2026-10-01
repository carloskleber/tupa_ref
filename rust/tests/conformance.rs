//! Conformance harness (ROADMAP Phase 8 item 2): walks `common/*_expected.csv`,
//! runs the matching case and diffs at 1e-6 relative (same rule as
//! `fortran/test/test_common_cases.f90`), plus an independent passivity check.

use std::fs;
use std::path::{Path, PathBuf};
use tupa::potentials::compute_observations;
use tupa::results_writer::{observation_csv, results_csv, transient_csv, transient_signals_csv};
use tupa::transient::{transient_response, transient_signals};
use tupa::{load_study, validate_study_references};

fn common() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("common")
}

/// Expected-CSV → (case json, source node). Mirrors `compareCase` calls.
fn source_node_for(case: &str) -> &'static str {
    match case {
        "grid" => "Node_A",
        _ => "Node_1",
    }
}

/// Keyed comparison: rows are matched by `(frequency_hz, quantity, id)` — the
/// text fields must agree exactly — and `re`/`im` must agree within `reltol`
/// of the row scale `max(1e-6, |re|, |im|)` (the floor keeps round-off-zero
/// rows from amplifying machine noise; same rule as
/// `fortran/test/test_common_cases.f90`).
///
/// Matching by key rather than by position makes the harness independent of
/// electrode ordering. This matters for `grid_expected.csv`, which predates
/// the FIFO element-order fix of ADR 0020 and lists the elements in reverse
/// declaration order (see `rust/README.md`).
fn diff_csv(fresh: &str, expected: &str, reltol: f64) -> Result<(), String> {
    let mut fl = fresh.lines();
    let mut el = expected.lines();
    let (fh, eh) = (fl.next(), el.next());
    if fh != eh {
        return Err(format!("header mismatch: {fh:?} vs {eh:?}"));
    }
    let parse = |line: &str| -> Result<(String, f64, f64), String> {
        let f: Vec<&str> = line.split(',').collect();
        if f.len() != 5 {
            return Err(format!("malformed row: {line}"));
        }
        let p = |s: &str| s.trim().parse::<f64>().map_err(|x| format!("{line}: {x}"));
        Ok((format!("{},{},{}", f[0], f[1], f[2]), p(f[3])?, p(f[4])?))
    };
    let mut fresh_rows = std::collections::HashMap::new();
    for l in fl {
        let (k, re, im) = parse(l)?;
        if fresh_rows.insert(k.clone(), (re, im)).is_some() {
            return Err(format!("duplicate fresh row {k}"));
        }
    }
    let mut n_expected = 0;
    for l in el {
        n_expected += 1;
        let (k, ere, eim) = parse(l)?;
        let Some(&(fre, fim)) = fresh_rows.get(&k) else {
            return Err(format!("expected row missing from fresh run: {k}"));
        };
        let scale = 1.0e-6_f64.max(ere.abs()).max(eim.abs());
        if (fre - ere).abs() >= reltol * scale || (fim - eim).abs() >= reltol * scale {
            return Err(format!(
                "{k}: fresh ({fre}, {fim}) vs expected ({ere}, {eim})"
            ));
        }
    }
    if n_expected != fresh_rows.len() {
        return Err(format!(
            "row count differs: {} fresh vs {n_expected} expected",
            fresh_rows.len()
        ));
    }
    Ok(())
}

fn check_case(name: &str) {
    let study = check_harmonic(name);
    let zin = study.input_impedance(source_node_for(name)).expect("zin");
    for (k, z) in zin.iter().enumerate() {
        assert!(
            z.re >= -1.0e-9 * z.norm().max(1.0),
            "{name}: Re(Zin) < 0 at sweep point {}: {z}",
            k + 1
        );
    }
}

/// Harmonic sweep of `<name>.json` against `<name>_expected.csv`, rows
/// filtered by the case's `outputs` block like the written results.
fn check_harmonic(name: &str) -> tupa::Study {
    let json = common().join(format!("{name}.json"));
    let expected_path = common().join(format!("{name}_expected.csv"));
    let mut case = load_study(&json).expect("load");
    validate_study_references(&mut case).expect("validate");
    let sources = case.sources.clone().expect("sources");
    let freq = case.freq_hz.clone().expect("frequencies");
    case.study.run_sweep(&freq, &sources).expect("sweep");
    let o = &case.outputs;
    let study = case.study;
    let fresh = results_csv(
        &study,
        o.nodes.as_deref(),
        o.electrodes.as_deref(),
        o.quantities.as_deref(),
    )
    .expect("csv");
    let expected = fs::read_to_string(&expected_path).expect("expected csv");
    if let Err(msg) = diff_csv(&fresh, &expected, 1.0e-6) {
        panic!("{name}: fresh run differs from fixture: {msg}");
    }
    study
}

/// Transient fixtures (`time_s,quantity,id,value`, ROADMAP Phase 9): rows
/// matched by position with identical text fields; values within
/// `reltol·max(|expected|, 1e-3·peak)`, peak = largest |expected| of that
/// (quantity, id) series (same rule as `fortran/test/test_common_cases.f90`).
type TransientRows = (String, Vec<(String, String, f64)>);

fn diff_transient_csv(fresh: &str, expected: &str, reltol: f64) -> Result<(), String> {
    let split = |text: &str| -> Result<TransientRows, String> {
        let mut lines = text.lines();
        let head = lines.next().unwrap_or("").to_string();
        let rows = lines
            .map(|l| {
                let p = l.rfind(',').ok_or(format!("malformed row: {l}"))?;
                let key = l[..p].to_string();
                let series = key[key.find(',').unwrap_or(0) + 1..].to_string();
                let v = l[p + 1..]
                    .trim()
                    .parse::<f64>()
                    .map_err(|e| format!("{l}: {e}"))?;
                Ok((key, series, v))
            })
            .collect::<Result<Vec<_>, String>>()?;
        Ok((head, rows))
    };
    let (fh, fr) = split(fresh)?;
    let (eh, er) = split(expected)?;
    if fh != eh {
        return Err(format!("header mismatch: {fh:?} vs {eh:?}"));
    }
    if fr.len() != er.len() {
        return Err(format!(
            "row count differs: {} fresh vs {} expected",
            fr.len(),
            er.len()
        ));
    }
    let mut peak = std::collections::HashMap::new();
    for (_, s, v) in &er {
        let p = peak.entry(s.clone()).or_insert(0.0_f64);
        *p = p.max(v.abs());
    }
    for ((fk, _, fv), (ek, es, ev)) in fr.iter().zip(&er) {
        if fk != ek {
            return Err(format!("row key differs: {fk} vs {ek}"));
        }
        let scale = ev.abs().max(1.0e-3 * peak[es]);
        if (fv - ev).abs() > reltol * scale {
            return Err(format!("{ek}: fresh {fv} vs expected {ev}"));
        }
    }
    Ok(())
}

fn check_transient_case(name: &str) {
    check_transient_case_as(name, name);
}

/// `check_transient_case` against `<fixture>_expected.csv` (a case whose
/// harmonic sweep owns `<name>_expected.csv`).
fn check_transient_case_as(name: &str, fixture: &str) {
    let json = common().join(format!("{name}.json"));
    let expected =
        fs::read_to_string(common().join(format!("{fixture}_expected.csv"))).expect("expected csv");
    let mut case = load_study(&json).expect("load");
    validate_study_references(&mut case).expect("validate");
    let spec = case.transient.clone().expect("signal block");
    let nodes: Vec<String> = spec.sources.iter().map(|s| s.node.clone()).collect();
    let fresh = if spec.signal_names.len() > 1 {
        // A list of independent signals (ADR 0026): `signal` column
        let r = transient_signals(&mut case.study, &spec).expect("transient");
        transient_signals_csv(
            &spec.signal_names,
            &nodes,
            &spec.observe_nodes,
            &spec.observe_electrodes,
            &r,
        )
    } else {
        let r = transient_response(&mut case.study, &spec).expect("transient");
        transient_csv(&nodes, &spec.observe_nodes, &spec.observe_electrodes, &r)
    };
    if let Err(msg) = diff_transient_csv(&fresh, &expected, 1.0e-6) {
        panic!("{name}: fresh transient run differs from fixture: {msg}");
    }
}

#[test]
fn portela1997_transient_interpolated_matches_fixture() {
    check_transient_case("portela1997_transient_interpolated");
}

#[test]
fn portela1997_transient_hann_matches_fixture() {
    check_transient_case("portela1997_transient_hann");
}

#[test]
fn portela1997_transient_hann_time_matches_fixture() {
    check_transient_case("portela1997_transient_hann_time");
}

#[test]
fn portela1997_transient_multi_matches_fixture() {
    check_transient_case("portela1997_transient_multi");
}

#[test]
fn portela1997_transient_signals_matches_fixture() {
    check_transient_case("portela1997_transient_signals");
}

#[test]
fn portela1997_transient_nlt_matches_fixture() {
    check_transient_case("portela1997_transient_nlt");
}

#[test]
fn portela1997_matches_fixture() {
    check_case("portela1997");
}

#[test]
fn rod_matches_fixture() {
    check_case("rod");
}

#[test]
fn grid_matches_fixture() {
    check_case("grid");
}

/// Ideal images (`numerics.imageModel: "ideal"`, ROADMAP Phase 10 item 2):
/// the low-frequency-limit pin; the cases above run the Γ(ω) default.
#[test]
fn portela1997_ideal_matches_fixture() {
    check_case("portela1997_ideal");
}

/// ROADMAP Phase 10 item 6: the 32x32 m grid (185 nodes, 200 electrodes),
/// harmonic sweep with its `outputs` filter and scan-fed transient.
#[test]
fn portela_mesh_matches_fixture() {
    let json = common().join("portelaMesh.json");
    let mut case = load_study(&json).expect("load");
    validate_study_references(&mut case).expect("validate");
    let sources = case.sources.clone().expect("sources");
    let freq = case.freq_hz.clone().expect("frequencies");
    case.study.run_sweep(&freq, &sources).expect("sweep");
    let o = &case.outputs;
    let fresh = results_csv(
        &case.study,
        o.nodes.as_deref(),
        o.electrodes.as_deref(),
        o.quantities.as_deref(),
    )
    .expect("csv");
    let expected =
        fs::read_to_string(common().join("portelaMesh_expected.csv")).expect("expected csv");
    if let Err(msg) = diff_csv(&fresh, &expected, 1.0e-6) {
        panic!("portelaMesh: fresh run differs from fixture: {msg}");
    }
}

#[test]
fn portela_mesh_transient_matches_fixture() {
    check_transient_case_as("portelaMesh", "portelaMesh_transient");
}

/// ROADMAP Phase 10b: the unloaded Chen/Baba configuration, and a graded,
/// speed-calibrated channel (its fixture pins the calibration too).
#[test]
fn channel_unloaded_matches_fixture() {
    check_transient_case("channel_unloaded");
}

#[test]
fn channel_loaded_matches_fixture() {
    check_transient_case("channel_loaded");
}

/// Tower strike with a two-node current source and a delta-gap voltage
/// source (ADR 0025), harmonic and transient.
#[test]
fn channel_tower_matches_fixtures() {
    check_harmonic("channel_tower");
    check_transient_case_as("channel_tower", "channel_tower_transient");
}

#[test]
fn channel_tower_gap_matches_fixtures() {
    check_harmonic("channel_tower_gap");
    check_transient_case_as("channel_tower_gap", "channel_tower_gap_transient");
}

/// ROADMAP Phase 11 (ADR 0027): harmonic sweep and observation results of a
/// 16x16 m grid with a tower riser. The observation CSV has the shape
/// `frequency_hz,quantity,id,x,y,z,re,im`; rows are matched by position with
/// textually equal key fields.
#[test]
fn grid_safety_matches_fixtures() {
    let json = common().join("grid_safety.json");
    let mut case = load_study(&json).expect("load");
    validate_study_references(&mut case).expect("validate");
    let sources = case.sources.clone().expect("sources");
    let freq = case.freq_hz.clone().expect("frequencies");
    case.study.run_sweep(&freq, &sources).expect("sweep");
    let o = &case.outputs;
    let fresh = results_csv(
        &case.study,
        o.nodes.as_deref(),
        o.electrodes.as_deref(),
        o.quantities.as_deref(),
    )
    .expect("csv");
    let expected =
        fs::read_to_string(common().join("grid_safety_expected.csv")).expect("expected csv");
    if let Err(msg) = diff_csv(&fresh, &expected, 1.0e-6) {
        panic!("grid_safety: harmonic results differ from fixture: {msg}");
    }

    compute_observations(&mut case.study).expect("observations");
    let fresh = observation_csv(&case.study).expect("observation csv");
    let expected = fs::read_to_string(common().join("grid_safety_potentials_expected.csv"))
        .expect("expected observation csv");
    if let Err(msg) = diff_observation_csv(&fresh, &expected, 1.0e-6) {
        panic!("grid_safety: observation results differ from fixture: {msg}");
    }
}

fn diff_observation_csv(fresh: &str, expected: &str, reltol: f64) -> Result<(), String> {
    let (mut fl, mut el) = (fresh.lines(), expected.lines());
    let (fh, eh) = (fl.next(), el.next());
    if fh != eh {
        return Err(format!("header mismatch: {fh:?} vs {eh:?}"));
    }
    let split = |line: &str| -> Result<(String, f64, f64), String> {
        let f: Vec<&str> = line.split(',').collect();
        if f.len() != 8 {
            return Err(format!("malformed row: {line}"));
        }
        let p = |s: &str| s.trim().parse::<f64>().map_err(|x| format!("{line}: {x}"));
        Ok((f[..6].join(","), p(f[6])?, p(f[7])?))
    };
    let mut n = 0;
    loop {
        match (fl.next(), el.next()) {
            (None, None) => return Ok(()),
            (Some(f), Some(e)) => {
                n += 1;
                let ((fk, fre, fim), (ek, ere, eim)) = (split(f)?, split(e)?);
                if fk != ek {
                    return Err(format!("row {n}: key {fk} vs {ek}"));
                }
                let scale = 1.0e-6_f64.max(ere.abs()).max(eim.abs());
                if (fre - ere).abs() >= reltol * scale || (fim - eim).abs() >= reltol * scale {
                    return Err(format!(
                        "{ek}: fresh ({fre}, {fim}) vs expected ({ere}, {eim})"
                    ));
                }
            }
            _ => return Err("row count differs".to_string()),
        }
    }
}

#[test]
fn every_expected_fixture_has_a_test() {
    let mut names: Vec<String> = fs::read_dir(common())
        .unwrap()
        .filter_map(|e| e.ok())
        .filter_map(|e| {
            e.file_name()
                .to_str()
                .and_then(|n| n.strip_suffix("_expected.csv").map(String::from))
        })
        .collect();
    names.sort();
    assert_eq!(
        names,
        vec![
            "channel_loaded",
            "channel_tower",
            "channel_tower_gap",
            "channel_tower_gap_transient",
            "channel_tower_transient",
            "channel_unloaded",
            "grid",
            "grid_safety",
            "grid_safety_potentials",
            "portela1997",
            "portela1997_ideal",
            "portela1997_transient_hann",
            "portela1997_transient_hann_time",
            "portela1997_transient_interpolated",
            "portela1997_transient_multi",
            "portela1997_transient_nlt",
            "portela1997_transient_signals",
            "portelaMesh",
            "portelaMesh_transient",
            "rod"
        ],
        "new golden fixture in common/: add a check_case() test for it"
    );
}
