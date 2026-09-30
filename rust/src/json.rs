//! JSON schema v1 reader (`mJsonParser` + the `tupa` loader).
//!
//! Typed `serde` structs with `#[serde(default)]` everywhere reproduce the
//! Fortran reader's leniency: a missing number is 0, a missing string is
//! empty, unknown keys are ignored, unknown element types are skipped with a
//! warning. (A *present* value of the wrong JSON type is an error here,
//! where the Fortran reader silently yields 0.)

use crate::element::{Element, Line, MeshElement};
use crate::error::{Result, TupaError};
use crate::material::{AlipioVisacroSoil, Linear, Medium, PortelaSoil};
use crate::node::Node;
use crate::signal::{Signal, new_double_exp_signal, new_heidler_signal, new_heidler_signal_terms};
use crate::structure::Structure;
use crate::study::{Source, Study, log_frequency_axis};
use crate::transient::TransientSpec;
use serde::Deserialize;

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct SoilSpec {
    #[serde(rename = "type")]
    kind: Option<String>,
    conductivity: f64,
    permittivity: f64,
    permeability: f64,
    sigma0: f64,
    alpha0: f64,
    kr: f64,
}

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct NodeSpec {
    id: String,
    position: Vec<f64>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct MaterialSpec {
    id: String,
    epsilonr: f64,
    mur: f64,
    sigma: f64,
}

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct ElementSpec {
    #[serde(rename = "type")]
    kind: String,
    id: String,
    from: String,
    to: String,
    radius: f64,
    segments: f64,
    material: String,
    position: Vec<f64>,
    #[serde(rename = "lengthX")]
    length_x: f64,
    #[serde(rename = "lengthY")]
    length_y: f64,
    #[serde(rename = "rowsX")]
    rows_x: f64,
    #[serde(rename = "rowsY")]
    rows_y: f64,
}

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct ReIm {
    re: f64,
    im: f64,
}

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct SourceSpec {
    node: String,
    current: Option<ReIm>,
    voltage: Option<ReIm>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct FrequencySpec {
    min: f64,
    max: f64,
    #[serde(rename = "pointsPerDecade")]
    points_per_decade: f64,
}

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct OutputsSpec {
    nodes: Option<Vec<String>>,
    electrodes: Option<Vec<String>>,
    quantities: Option<Vec<String>>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct HeidlerTermSpec {
    i0: f64,
    n: f64,
    tau1: f64,
    tau2: f64,
}

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct SignalSpec {
    waveform: String,
    imax: Option<f64>,
    front: String,
    jones: bool,
    terms: Option<Vec<HeidlerTermSpec>>,
    #[serde(rename = "sourceNode")]
    source_node: String,
    #[serde(rename = "observeNodes")]
    observe_nodes: Vec<String>,
    #[serde(rename = "observeElectrodes")]
    observe_electrodes: Option<Vec<String>>,
    #[serde(rename = "nyquistHz")]
    nyquist_hz: f64,
    #[serde(rename = "fftPoints")]
    fft_points: f64,
    #[serde(rename = "freqZeroHz")]
    freq_zero_hz: Option<f64>,
    #[serde(rename = "antialiasStart")]
    antialias_start: Option<f64>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct CaseSpec {
    title: String,
    soil: SoilSpec,
    nodes: Vec<NodeSpec>,
    materials: Vec<MaterialSpec>,
    elements: Vec<ElementSpec>,
    sources: Option<Vec<SourceSpec>>,
    frequencies: Option<FrequencySpec>,
    outputs: Option<OutputsSpec>,
    signal: Option<SignalSpec>,
}

/// Output selection (`outputs` block). `None` = "everything".
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Outputs {
    /// Node ids to keep
    pub nodes: Option<Vec<String>>,
    /// Electrode ids to keep
    pub electrodes: Option<Vec<String>>,
    /// Quantities (`voltage`, `i1`, `i2`, `inputImpedance`)
    pub quantities: Option<Vec<String>>,
}

/// Transient request as loaded (`signal` block).
pub type TransientRequest = TransientSpec;

/// A fully loaded case file.
#[derive(Debug)]
pub struct LoadedCase {
    /// The study (structure not yet assembled)
    pub study: Study,
    /// `sources` block, if present
    pub sources: Option<Vec<Source>>,
    /// Frequency axis from the `frequencies` block, if present
    pub freq_hz: Option<Vec<f64>>,
    /// `outputs` block (all `None` when absent)
    pub outputs: Outputs,
    /// `signal` block, if present
    pub transient: Option<TransientRequest>,
}

fn pos3(v: &[f64]) -> [f64; 3] {
    [
        v.first().copied().unwrap_or(0.0),
        v.get(1).copied().unwrap_or(0.0),
        v.get(2).copied().unwrap_or(0.0),
    ]
}

/// Truncate a JSON number to an integer count like Fortran's `int(real)`;
/// negatives clamp to 0 (rejected later by the element checks).
fn count(x: f64) -> usize {
    if x <= 0.0 { 0 } else { x as usize }
}

/// Parse a case from a JSON string.
pub fn load_study_str(text: &str) -> Result<LoadedCase> {
    let spec: CaseSpec = serde_json::from_str(text)?;
    build(spec)
}

/// Parse a case from a file.
pub fn load_study(path: impl AsRef<std::path::Path>) -> Result<LoadedCase> {
    let path = path.as_ref();
    let text = std::fs::read_to_string(path)
        .map_err(|e| TupaError::new(format!("cannot read '{}': {e}", path.display())))?;
    load_study_str(&text)
}

fn build(spec: CaseSpec) -> Result<LoadedCase> {
    let soil = match spec.soil.kind.as_deref().unwrap_or("linear") {
        "linear" => Medium::Linear(Linear::new(
            "soil",
            spec.soil.permittivity,
            spec.soil.permeability,
            spec.soil.conductivity,
        )),
        "portela" => Medium::Portela(PortelaSoil {
            id: "soil".into(),
            mur: spec.soil.permeability,
            sigma0: spec.soil.sigma0,
            alpha0: spec.soil.alpha0,
            kr: spec.soil.kr,
        }),
        "alipio-visacro" => Medium::AlipioVisacro(AlipioVisacroSoil {
            id: "soil".into(),
            mur: spec.soil.permeability,
            sigma0: spec.soil.sigma0,
        }),
        other => {
            return Err(TupaError::new(format!(
                "mTupa: unknown soil.type '{other}' (expected linear, portela or alipio-visacro)"
            )));
        }
    };

    let mut structure = Structure::new(soil);
    for n in &spec.nodes {
        structure.add_node(Node::new(n.id.clone(), pos3(&n.position)));
    }
    for m in &spec.materials {
        structure.add_material(Linear::new(m.id.clone(), m.epsilonr, m.mur, m.sigma));
    }
    for e in &spec.elements {
        match e.kind.as_str() {
            "line" => structure.add_element(Element::Line(Line::new(
                e.id.clone(),
                e.from.clone(),
                e.to.clone(),
                e.radius,
                count(e.segments),
                e.material.clone(),
            ))),
            "mesh" => structure.add_element(Element::Mesh(MeshElement {
                id: e.id.clone(),
                position: pos3(&e.position),
                length_x: e.length_x,
                length_y: e.length_y,
                rows_x: count(e.rows_x),
                rows_y: count(e.rows_y),
                radius: e.radius,
                segments: count(e.segments),
                id_material: e.material.clone(),
            })),
            other => eprintln!(" mTupa: unknown element type '{other}' — skipped"),
        }
    }

    let study = Study::new(spec.title.clone(), structure);

    let sources = spec.sources.as_ref().map(|list| {
        list.iter()
            .map(|s| {
                // "voltage" wins if both are present; neither → zero current
                if let Some(v) = &s.voltage {
                    Source {
                        node: s.node.clone(),
                        value: num_complex::Complex64::new(v.re, v.im),
                        is_voltage: true,
                    }
                } else if let Some(c) = &s.current {
                    Source {
                        node: s.node.clone(),
                        value: num_complex::Complex64::new(c.re, c.im),
                        is_voltage: false,
                    }
                } else {
                    Source {
                        node: s.node.clone(),
                        value: num_complex::Complex64::new(0.0, 0.0),
                        is_voltage: false,
                    }
                }
            })
            .collect::<Vec<_>>()
    });

    let freq_hz = match &spec.frequencies {
        Some(f) => {
            // ADR 0013: nPoints = round(pointsPerDecade * log10(max/min)) + 1
            let raw = (f.points_per_decade * (f.max / f.min).log10()).round() + 1.0;
            let n = if raw.is_finite() && raw > 2.0 {
                raw as usize
            } else {
                2
            };
            Some(log_frequency_axis(f.min, f.max, n)?)
        }
        None => None,
    };

    let outputs = spec
        .outputs
        .map(|o| Outputs {
            nodes: o.nodes,
            electrodes: o.electrodes,
            quantities: o.quantities,
        })
        .unwrap_or_default();

    let transient = match spec.signal {
        Some(s) => Some(build_signal(s)?),
        None => None,
    };

    Ok(LoadedCase {
        study,
        sources,
        freq_hz,
        outputs,
        transient,
    })
}

fn build_signal(s: SignalSpec) -> Result<TransientSpec> {
    let imax = s.imax.unwrap_or(0.0);
    let signal: Signal = match s.waveform.as_str() {
        "heidler" => match &s.terms {
            Some(terms) => {
                let i0: Vec<f64> = terms.iter().map(|t| t.i0).collect();
                let n: Vec<f64> = terms.iter().map(|t| t.n).collect();
                let tau1: Vec<f64> = terms.iter().map(|t| t.tau1).collect();
                let tau2: Vec<f64> = terms.iter().map(|t| t.tau2).collect();
                new_heidler_signal_terms(&i0, &n, &tau1, &tau2, s.imax)?
            }
            None => new_heidler_signal(imax),
        },
        "doubleExp" => new_double_exp_signal(imax, &s.front, s.jones)?,
        other => {
            return Err(TupaError::new(format!(
                "mTupa: unknown signal.waveform '{other}' (expected heidler or doubleExp)"
            )));
        }
    };
    let antialias_start = match s.antialias_start {
        Some(a) => {
            if a <= 0.0 || a > 1.0 {
                return Err(TupaError::new(
                    "mTupa: signal.antialiasStart must be in (0, 1]",
                ));
            }
            a
        }
        None => 1.0,
    };
    Ok(TransientSpec {
        signal,
        source_node: s.source_node,
        observe_nodes: s.observe_nodes,
        observe_electrodes: s.observe_electrodes.unwrap_or_default(),
        nyquist_hz: s.nyquist_hz,
        fft_points: count(s.fft_points),
        freq_zero_hz: s.freq_zero_hz.unwrap_or(1.0e-6),
        antialias_start,
    })
}

fn require_node(study: &Study, id: &str, field: &str) -> Result<()> {
    if study.structure.find_node_index(id).is_none() {
        return Err(TupaError::new(format!(
            "mTupa: {field} references unknown node '{id}'"
        )));
    }
    Ok(())
}

fn require_electrode(study: &Study, id: &str, field: &str) -> Result<()> {
    if study.structure.find_electrode_index(id).is_none() {
        return Err(TupaError::new(format!(
            "mTupa: {field} references unknown electrode '{id}' (discretised segment IDs look \
             like '<element id>_e<n>', not the input element/boundary-node ID — see \
             common/README.md)"
        )));
    }
    Ok(())
}

/// Upfront ID cross-reference validation (`validateStudyReferences`):
/// assembles the structure, then checks every referenced node/electrode.
pub fn validate_study_references(case: &mut LoadedCase) -> Result<()> {
    case.study.structure.assemble()?;
    let study = &case.study;
    if let Some(sources) = &case.sources {
        for s in sources {
            require_node(study, &s.node, "sources[].node")?;
        }
    }
    if let Some(t) = &case.transient {
        require_node(study, &t.source_node, "signal.sourceNode")?;
        for id in &t.observe_nodes {
            require_node(study, id, "signal.observeNodes")?;
        }
        for id in &t.observe_electrodes {
            require_electrode(study, id, "signal.observeElectrodes")?;
        }
    }
    if let Some(ids) = &case.outputs.nodes {
        for id in ids {
            require_node(study, id, "outputs.nodes")?;
        }
    }
    if let Some(ids) = &case.outputs.electrodes {
        for id in ids {
            require_electrode(study, id, "outputs.electrodes")?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const CASE: &str = r#"{
      "title": "t",
      "soil": { "conductivity": 0.01, "permittivity": 10.0, "permeability": 1.0 },
      "nodes": [ { "id": "A", "position": [0,0,-0.5] }, { "id": "B", "position": [10,0,-0.5] } ],
      "materials": [ { "id": "cu", "epsilonr": 1.0, "mur": 1.0, "sigma": 5.96e7 } ],
      "elements": [ { "type": "line", "id": "L", "from": "A", "to": "B",
                      "radius": 0.007, "segments": 4, "material": "cu" } ],
      "sources": [ { "node": "A", "current": { "re": 1.0, "im": 0.0 } } ],
      "frequencies": { "min": 10.0, "max": 1.0e6, "pointsPerDecade": 1 },
      "outputs": { "quantities": ["voltage"] }
    }"#;

    #[test]
    fn loads_sources_frequencies_outputs() {
        let c = load_study_str(CASE).unwrap();
        let s = c.sources.unwrap();
        assert_eq!(s.len(), 1);
        assert_eq!(s[0].node, "A");
        assert!(!s[0].is_voltage);
        let f = c.freq_hz.unwrap();
        assert_eq!(f.len(), 6);
        assert!((f[0] - 10.0).abs() < 1e-9 && (f[5] - 1.0e6).abs() < 1e-3);
        assert_eq!(c.outputs.quantities.unwrap(), vec!["voltage".to_string()]);
    }

    #[test]
    fn frequency_axis_rule_matches_adr_0013() {
        let text = CASE.replace("\"pointsPerDecade\": 1", "\"pointsPerDecade\": 8");
        let c = load_study_str(&text).unwrap();
        assert_eq!(c.freq_hz.unwrap().len(), 41); // 8*5 + 1
    }

    #[test]
    fn unknown_soil_type_is_rejected() {
        let text = CASE.replace("\"conductivity\"", "\"type\": \"bogus\", \"conductivity\"");
        assert!(load_study_str(&text).is_err());
    }

    #[test]
    fn voltage_source_wins_over_current() {
        let text = CASE.replace(
            "\"current\": { \"re\": 1.0, \"im\": 0.0 }",
            "\"current\": { \"re\": 1.0, \"im\": 0.0 }, \"voltage\": { \"re\": 5.0, \"im\": 0.0 }",
        );
        let c = load_study_str(&text).unwrap();
        let s = &c.sources.unwrap()[0];
        assert!(s.is_voltage);
        assert_eq!(s.value.re, 5.0);
    }

    #[test]
    fn validation_reports_unknown_references() {
        let mut c = load_study_str(&CASE.replace("\"node\": \"A\"", "\"node\": \"Z\"")).unwrap();
        let e = validate_study_references(&mut c).unwrap_err();
        assert!(
            e.message()
                .contains("sources[].node references unknown node 'Z'")
        );

        let text = CASE.replace(
            "\"outputs\": { \"quantities\": [\"voltage\"] }",
            "\"outputs\": { \"electrodes\": [\"L\"] }",
        );
        let mut c = load_study_str(&text).unwrap();
        let e = validate_study_references(&mut c).unwrap_err();
        assert!(e.message().contains("unknown electrode 'L'"));
        assert!(e.message().contains("_e<n>"));
    }

    #[test]
    fn valid_case_passes_validation_and_assembles() {
        let mut c = load_study_str(CASE).unwrap();
        validate_study_references(&mut c).unwrap();
        assert_eq!(c.study.structure.nodes.len(), 5);
        assert_eq!(c.study.structure.electrodes.len(), 4);
        assert!(c.study.structure.find_electrode_index("L_e4").is_some());
    }

    #[test]
    fn unknown_element_type_is_skipped() {
        let text = CASE.replace("\"type\": \"line\"", "\"type\": \"catenary\"");
        let c = load_study_str(&text).unwrap();
        assert!(c.study.structure.elements.is_empty());
    }

    #[test]
    fn signal_block_defaults() {
        let text = CASE.replace(
            "\"outputs\"",
            "\"signal\": { \"waveform\": \"doubleExp\", \"imax\": 30000, \"front\": \"f1_2_50\", \
              \"sourceNode\": \"A\", \"observeNodes\": [\"A\"], \"nyquistHz\": 1e6, \"fftPoints\": 1024 }, \"outputs\"",
        );
        let c = load_study_str(&text).unwrap();
        let t = c.transient.unwrap();
        assert_eq!(t.fft_points, 1024);
        assert_eq!(t.freq_zero_hz, 1.0e-6);
        assert_eq!(t.antialias_start, 1.0);
        assert!(t.observe_electrodes.is_empty());
    }

    #[test]
    fn antialias_range_is_checked() {
        let text = CASE.replace(
            "\"outputs\"",
            "\"signal\": { \"waveform\": \"doubleExp\", \"imax\": 1, \"front\": \"f1_2_50\", \
              \"sourceNode\": \"A\", \"observeNodes\": [\"A\"], \"nyquistHz\": 1e6, \"fftPoints\": 64, \
              \"antialiasStart\": 1.5 }, \"outputs\"",
        );
        assert!(load_study_str(&text).is_err());
    }
}
