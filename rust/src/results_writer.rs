//! CSV and JSON result writers (`mResultsWriter`, ADR 0012/0013/0015).
//! Numbers use the Fortran `ES16.8` layout (`3.08173006E+01`) so files are
//! byte-comparable with the reference implementation.

use crate::error::{Result, TupaError};
use crate::study::Study;
use crate::transient::TransientResult;
use num_complex::Complex64;
use std::fmt::Write as _;

/// `ES16.8`-style formatting: `d.dddddddd` `E±ee` (three-digit exponents drop
/// the `E`, as Fortran does).
pub fn fmt_real(x: f64) -> String {
    if x.is_nan() {
        return "NaN".to_string();
    }
    if x.is_infinite() {
        return if x > 0.0 {
            "Infinity".to_string()
        } else {
            "-Infinity".to_string()
        };
    }
    let s = format!("{x:.8e}");
    let (mant, exp) = s.split_once('e').expect("exponent");
    let exp: i32 = exp.parse().expect("integer exponent");
    let sign = if exp < 0 { '-' } else { '+' };
    let a = exp.abs();
    if a >= 100 {
        format!("{mant}{sign}{a}")
    } else {
        format!("{mant}E{sign}{a:02}")
    }
}

fn fmt_complex_json(v: Complex64) -> String {
    format!("{{\"re\": {}, \"im\": {}}}", fmt_real(v.re), fmt_real(v.im))
}

fn json_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => {
                let _ = write!(out, "\\u{:04x}", c as u32);
            }
            c => out.push(c),
        }
    }
    out
}

/// `wanted(name, list)`: no list means everything.
fn wanted(name: &str, list: Option<&[String]>) -> bool {
    list.is_none_or(|l| l.iter().any(|x| x == name))
}

fn require_sweep(study: &Study, who: &str) -> Result<()> {
    if study.voltage_results.frequency_count() == 0 {
        return Err(TupaError::new(format!(
            "{who}: study has no sweep results (call runSweep first)"
        )));
    }
    Ok(())
}

/// Sweep results as tidy CSV (`frequency_hz,quantity,id,re,im`).
pub fn results_csv(
    study: &Study,
    node_ids: Option<&[String]>,
    electrode_ids: Option<&[String]>,
    quantities: Option<&[String]>,
) -> Result<String> {
    require_sweep(study, "writeResultsCsv")?;
    let nf = study.voltage_results.frequency_count();
    let nno = study.voltage_results.entity_count();
    let nseg = study.long_current_results.entity_count();
    let mut out = String::from("frequency_hz,quantity,id,re,im\n");
    let row = |out: &mut String, f: f64, q: &str, id: &str, v: Complex64| {
        let _ = writeln!(
            out,
            "{},{},{},{},{}",
            fmt_real(f),
            q,
            id,
            fmt_real(v.re),
            fmt_real(v.im)
        );
    };
    for k in 0..nf {
        let f = study.sweep_freq_hz[k];
        for i in 0..nno {
            let id = study.voltage_results.entity_id(i);
            if !(wanted(id, node_ids) && wanted("voltage", quantities)) {
                continue;
            }
            row(&mut out, f, "voltage", id, study.voltage_results.get(i, k));
        }
        for i in 0..nseg {
            let id = study.long_current_results.entity_id(i);
            if !wanted(id, electrode_ids) {
                continue;
            }
            if wanted("i1", quantities) {
                row(&mut out, f, "i1", id, study.long_current_results.get(i, k));
            }
            if wanted("i2", quantities) {
                row(&mut out, f, "i2", id, study.trans_current_results.get(i, k));
            }
        }
    }
    Ok(out)
}

fn join_complex(results: &crate::result::ResultSet, i: usize, nf: usize) -> String {
    (0..nf)
        .map(|k| fmt_complex_json(results.get(i, k)))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Sweep results as JSON (ADR 0012 shape, `outputs` filtering per ADR 0013).
pub fn results_json(
    study: &Study,
    node_ids: Option<&[String]>,
    electrode_ids: Option<&[String]>,
    quantities: Option<&[String]>,
) -> Result<String> {
    require_sweep(study, "writeResultsJson")?;
    let nf = study.voltage_results.frequency_count();
    let nno = study.voltage_results.entity_count();
    let nseg = study.long_current_results.entity_count();
    let mut out = String::new();
    out.push_str("{\n");
    let _ = writeln!(out, "  \"title\": \"{}\",", json_escape(&study.title));
    let freqs: Vec<String> = study.sweep_freq_hz.iter().map(|&f| fmt_real(f)).collect();
    let _ = writeln!(out, "  \"frequencies\": [{}],", freqs.join(", "));

    out.push_str("  \"nodes\": [\n");
    let mut first = true;
    for i in 0..nno {
        let id = study.voltage_results.entity_id(i);
        if !(wanted(id, node_ids) && wanted("voltage", quantities)) {
            continue;
        }
        if !first {
            out.push_str(",\n");
        }
        first = false;
        let _ = write!(
            out,
            "    {{ \"id\": \"{}\", \"voltage\": [{}] }}",
            json_escape(id),
            join_complex(&study.voltage_results, i, nf)
        );
    }
    if !first {
        out.push('\n');
    }
    out.push_str("  ],\n");

    out.push_str("  \"electrodes\": [\n");
    let mut first = true;
    for i in 0..nseg {
        let id = study.long_current_results.entity_id(i);
        let want_i1 = wanted(id, electrode_ids) && wanted("i1", quantities);
        let want_i2 = wanted(id, electrode_ids) && wanted("i2", quantities);
        if !(want_i1 || want_i2) {
            continue;
        }
        if !first {
            out.push_str(",\n");
        }
        first = false;
        let _ = write!(out, "    {{ \"id\": \"{}\"", json_escape(id));
        if want_i1 {
            let _ = write!(
                out,
                ", \"i1\": [{}]",
                join_complex(&study.long_current_results, i, nf)
            );
        }
        if want_i2 {
            let _ = write!(
                out,
                ", \"i2\": [{}]",
                join_complex(&study.trans_current_results, i, nf)
            );
        }
        out.push_str(" }");
    }
    if !first {
        out.push('\n');
    }
    out.push_str("  ],\n");

    out.push_str("  \"derived\": {\n");
    if !study.sweep_source_ids.is_empty() && wanted("inputImpedance", quantities) {
        let zin = study.input_impedance(&study.sweep_source_ids[0])?;
        let items: Vec<String> = zin.iter().map(|&z| fmt_complex_json(z)).collect();
        let _ = writeln!(out, "    \"inputImpedance\": [{}]", items.join(", "));
    }
    out.push_str("  }\n}\n");
    Ok(out)
}

/// Transient results as CSV (`time_s,quantity,id,value`). With several
/// sources, one `injectedCurrent` row per distinct source node (first-appearance
/// order) holding the net current injected there (ADR 0015 amendment
/// 2026-09-30).
pub fn transient_csv(
    source_nodes: &[String],
    observe_nodes: &[String],
    observe_electrodes: &[String],
    r: &TransientResult,
) -> String {
    let first_of: Vec<usize> = (0..source_nodes.len())
        .map(|i| {
            (0..i)
                .find(|&j| source_nodes[j] == source_nodes[i])
                .unwrap_or(i)
        })
        .collect();
    let mut out = String::from("time_s,quantity,id,value\n");
    for k in 0..r.t.len() {
        let t = fmt_real(r.t[k]);
        for i in 0..source_nodes.len() {
            if first_of[i] != i {
                continue;
            }
            let mut net = r.injected_currents[i][k];
            for (j, &f) in first_of.iter().enumerate().skip(i + 1) {
                if f == i {
                    net += r.injected_currents[j][k];
                }
            }
            let _ = writeln!(
                out,
                "{t},injectedCurrent,{},{}",
                source_nodes[i],
                fmt_real(net)
            );
        }
        for (i, id) in observe_nodes.iter().enumerate() {
            let _ = writeln!(out, "{t},voltage,{id},{}", fmt_real(r.node_responses[i][k]));
        }
        for (i, id) in observe_electrodes.iter().enumerate() {
            let _ = writeln!(out, "{t},i1,{id},{}", fmt_real(r.i1_responses[i][k]));
            let _ = writeln!(out, "{t},i2,{id},{}", fmt_real(r.i2_responses[i][k]));
        }
    }
    out
}

fn join_real(v: &[f64]) -> String {
    v.iter()
        .map(|&x| fmt_real(x))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Transient results as JSON (ADR 0015). `sourceNode`/`injectedCurrent` are
/// the first source; `sources` lists every source when there is more than one
/// (ADR 0015 amendment 2026-09-30).
pub fn transient_json(
    title: &str,
    source_nodes: &[String],
    observe_nodes: &[String],
    observe_electrodes: &[String],
    r: &TransientResult,
) -> String {
    let mut out = String::new();
    out.push_str("{\n");
    let _ = writeln!(out, "  \"title\": \"{}\",", json_escape(title));
    let _ = writeln!(
        out,
        "  \"sourceNode\": \"{}\",",
        json_escape(&source_nodes[0])
    );
    let _ = writeln!(out, "  \"time\": [{}],", join_real(&r.t));
    let _ = writeln!(
        out,
        "  \"injectedCurrent\": [{}],",
        join_real(&r.injected_currents[0])
    );
    if source_nodes.len() > 1 {
        out.push_str("  \"sources\": [\n");
        for (i, id) in source_nodes.iter().enumerate() {
            let _ = write!(
                out,
                "    {{ \"node\": \"{}\", \"current\": [{}] }}",
                json_escape(id),
                join_real(&r.injected_currents[i])
            );
            if i + 1 < source_nodes.len() {
                out.push(',');
            }
            out.push('\n');
        }
        out.push_str("  ],\n");
    }
    out.push_str("  \"nodes\": [\n");
    for (i, id) in observe_nodes.iter().enumerate() {
        let _ = write!(
            out,
            "    {{ \"id\": \"{}\", \"voltage\": [{}] }}",
            json_escape(id),
            join_real(&r.node_responses[i])
        );
        if i + 1 < observe_nodes.len() {
            out.push(',');
        }
        out.push('\n');
    }
    if observe_nodes.is_empty() {
        out.push('\n');
    }
    if observe_electrodes.is_empty() {
        out.push_str("  ],\n  \"electrodes\": []\n");
    } else {
        out.push_str("  ],\n  \"electrodes\": [\n");
        for (i, id) in observe_electrodes.iter().enumerate() {
            let _ = write!(
                out,
                "    {{ \"id\": \"{}\", \"i1\": [{}], \"i2\": [{}] }}",
                json_escape(id),
                join_real(&r.i1_responses[i]),
                join_real(&r.i2_responses[i])
            );
            if i + 1 < observe_electrodes.len() {
                out.push(',');
            }
            out.push('\n');
        }
        out.push_str("  ]\n");
    }
    out.push_str("}\n");
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn es16_8_formatting() {
        assert_eq!(fmt_real(30.8173006), "3.08173006E+01");
        assert_eq!(fmt_real(-0.00982484349), "-9.82484349E-03");
        assert_eq!(fmt_real(0.0), "0.00000000E+00");
        assert_eq!(fmt_real(10.0), "1.00000000E+01");
        assert_eq!(fmt_real(1.0e100), "1.00000000+100");
        assert_eq!(fmt_real(1.0e-100), "1.00000000-100");
        assert_eq!(fmt_real(f64::NAN), "NaN");
    }

    #[test]
    fn wanted_semantics() {
        let l = vec!["a".to_string()];
        assert!(wanted("a", Some(&l)));
        assert!(!wanted("b", Some(&l)));
        assert!(wanted("b", None));
    }
}
