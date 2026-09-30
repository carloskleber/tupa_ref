//! Conformance harness (ROADMAP Phase 8 item 2): walks `common/*_expected.csv`,
//! runs the matching case and diffs at 1e-6 relative (same rule as
//! `fortran/test/test_common_cases.f90`), plus an independent passivity check.

use std::fs;
use std::path::{Path, PathBuf};
use tupa::results_writer::{results_csv, transient_csv};
use tupa::transient::transient_response;
use tupa::{load_study, run_study_from_file, validate_study_references};

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
    let json = common().join(format!("{name}.json"));
    let expected_path = common().join(format!("{name}_expected.csv"));
    let study = run_study_from_file(json.to_str().unwrap()).expect("run");
    let fresh = results_csv(&study, None, None, None).expect("csv");
    let expected = fs::read_to_string(&expected_path).expect("expected csv");
    if let Err(msg) = diff_csv(&fresh, &expected, 1.0e-6) {
        panic!("{name}: fresh run differs from fixture: {msg}");
    }

    let zin = study.input_impedance(source_node_for(name)).expect("zin");
    for (k, z) in zin.iter().enumerate() {
        assert!(
            z.re >= -1.0e-9 * z.norm().max(1.0),
            "{name}: Re(Zin) < 0 at sweep point {}: {z}",
            k + 1
        );
    }
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
    let json = common().join(format!("{name}.json"));
    let expected =
        fs::read_to_string(common().join(format!("{name}_expected.csv"))).expect("expected csv");
    let mut case = load_study(&json).expect("load");
    validate_study_references(&mut case).expect("validate");
    let spec = case.transient.clone().expect("signal block");
    let r = transient_response(&mut case.study, &spec).expect("transient");
    let nodes: Vec<String> = spec.sources.iter().map(|s| s.node.clone()).collect();
    let fresh = transient_csv(&nodes, &spec.observe_nodes, &spec.observe_electrodes, &r);
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
            "grid",
            "portela1997",
            "portela1997_transient_hann",
            "portela1997_transient_hann_time",
            "portela1997_transient_interpolated",
            "portela1997_transient_multi",
            "portela1997_transient_nlt",
            "rod"
        ],
        "new golden fixture in common/: add a check_case() test for it"
    );
}
