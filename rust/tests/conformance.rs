//! Conformance harness (ROADMAP Phase 8 item 2): walks `common/*_expected.csv`,
//! runs the matching case and diffs at 1e-6 relative (same rule as
//! `fortran/test/test_common_cases.f90`), plus an independent passivity check.

use std::fs;
use std::path::{Path, PathBuf};
use tupa::results_writer::results_csv;
use tupa::run_study_from_file;

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
        vec!["grid", "portela1997", "rod"],
        "new golden fixture in common/: add a check_case() test for it"
    );
}
