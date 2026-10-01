//! JSON schema v1 reader (`mJsonParser` + the `tupa` loader).
//!
//! Typed `serde` structs with `#[serde(default)]` everywhere reproduce the
//! Fortran reader's leniency: a missing number is 0, a missing string is
//! empty, unknown keys are ignored, unknown element types are skipped with a
//! warning. (A *present* value of the wrong JSON type is an error here,
//! where the Fortran reader silently yields 0.)

use crate::channel_calibration::calibrate_channel;
use crate::element::channel::{Piecewise, graded_breaks};
use crate::element::{Catenary, Channel, Element, Line, MeshElement};
use crate::error::{Result, TupaError};
use crate::geometry::GeometryKernel;
use crate::material::{AlipioVisacroSoil, Linear, Medium, PortelaSoil};
use crate::mesh::ImageModel;
use crate::node::Node;
use crate::signal::{
    Signal, new_double_exp_signal, new_heidler_signal, new_heidler_signal_terms,
    new_portela_signal, new_sine_signal,
};
use crate::structure::Structure;
use crate::study::{Source, Study, log_frequency_axis};
use crate::transient::{
    TransferFunction, Transform, TransientOptions, TransientSource, TransientSpec, Window,
    WindowPlacement,
};
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
    segments: Option<f64>,
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
    sag: f64,
    // `channel` (ADR 0025)
    strike: String,
    length: f64,
    incidence: f64,
    azimuth: f64,
    speed: Option<ProfileSpec>,
    inductance: Option<f64>,
    resistance: Option<ProfileSpec>,
    calibrate: bool,
    #[serde(rename = "maxSegment")]
    max_segment: Option<f64>,
    #[serde(rename = "firstSegment")]
    first_segment: Option<f64>,
    growth: Option<f64>,
}

/// A channel profile: one number, or `[{upTo, value}]` pieces.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum ProfileSpec {
    Uniform(f64),
    Pieces(Vec<PieceSpec>),
}

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct PieceSpec {
    #[serde(rename = "upTo")]
    up_to: f64,
    value: f64,
}

impl ProfileSpec {
    fn build(&self, key: &str) -> Result<Piecewise> {
        match self {
            ProfileSpec::Uniform(v) => Ok(Piecewise::uniform(*v)),
            ProfileSpec::Pieces(p) => {
                if p.is_empty() {
                    return Err(TupaError::new(format!(
                        "mTupa: channel '{key}' profile must hold at least one piece"
                    )));
                }
                if p.windows(2).any(|w| w[1].up_to <= w[0].up_to) {
                    return Err(TupaError::new(format!(
                        "mTupa: channel '{key}' profile breaks must be ascending"
                    )));
                }
                Ok(Piecewise {
                    up_to: p.iter().map(|x| x.up_to).collect(),
                    value: p.iter().map(|x| x.value).collect(),
                })
            }
        }
    }
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
    #[serde(rename = "returnNode")]
    return_node: String,
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

/// Waveform fields shared by the `signal` block and each `signal.sources[]`
/// entry (ROADMAP Phase 9 item 4).
#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct WaveformSpec {
    waveform: String,
    imax: Option<f64>,
    front: String,
    jones: bool,
    terms: Option<Vec<HeidlerTermSpec>>,
    alpha: f64,
    #[serde(rename = "tFront")]
    t_front: f64,
    #[serde(rename = "tTopEnd")]
    t_top_end: f64,
    #[serde(rename = "tTailEnd")]
    t_tail_end: f64,
    #[serde(rename = "frequencyHz")]
    frequency_hz: f64,
    #[serde(rename = "phaseDeg")]
    phase_deg: f64,
}

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct SignalSourceSpec {
    node: String,
    #[serde(rename = "returnNode")]
    return_node: String,
    quantity: Option<String>,
    #[serde(flatten)]
    wave: WaveformSpec,
}

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct WindowSpec {
    #[serde(rename = "type")]
    kind: String,
    placement: Option<String>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct SignalSpec {
    #[serde(flatten)]
    wave: WaveformSpec,
    sources: Option<Vec<SignalSourceSpec>>,
    window: Option<WindowSpec>,
    transform: Option<String>,
    #[serde(rename = "nltDamping")]
    nlt_damping: Option<f64>,
    #[serde(rename = "transferFunction")]
    transfer_function: Option<String>,
    #[serde(rename = "sourceNode")]
    source_node: String,
    #[serde(rename = "returnNode")]
    return_node: String,
    quantity: Option<String>,
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

/// `numerics` block (ADR 0024, ROADMAP Phase 10).
#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct NumericsSpec {
    kernel: Option<String>,
    #[serde(rename = "imageModel")]
    image_model: Option<String>,
    #[serde(rename = "maxSegmentLength")]
    max_segment_length: Option<f64>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct CaseSpec {
    title: String,
    soil: SoilSpec,
    numerics: Option<NumericsSpec>,
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

    let mut kernel = None;
    let mut image_model = None;
    let mut max_segment_length = 0.0;
    if let Some(num) = &spec.numerics {
        kernel = match num.kernel.as_deref() {
            None => None,
            Some("single") => Some(GeometryKernel::Single),
            Some("double") => Some(GeometryKernel::Double),
            Some(other) => {
                return Err(TupaError::new(format!(
                    "mTupa: unknown numerics.kernel '{other}' (expected single or double)"
                )));
            }
        };
        image_model = match num.image_model.as_deref() {
            None => None,
            Some("frequency-dependent") => Some(ImageModel::FrequencyDependent),
            Some("ideal") => Some(ImageModel::Ideal),
            Some(other) => {
                return Err(TupaError::new(format!(
                    "mTupa: unknown numerics.imageModel '{other}' (expected frequency-dependent or ideal)"
                )));
            }
        };
        if let Some(l) = num.max_segment_length {
            if l <= 0.0 {
                return Err(TupaError::new(
                    "mTupa: numerics.maxSegmentLength must be positive",
                ));
            }
            max_segment_length = l;
        }
    }

    let mut structure = Structure::new(soil);
    for n in &spec.nodes {
        structure.add_node(Node::new(n.id.clone(), pos3(&n.position)));
    }
    let chord = |from: &str, to: &str| -> f64 {
        match (
            structure.find_node_index(from),
            structure.find_node_index(to),
        ) {
            (Some(a), Some(b)) => {
                let (pa, pb) = (structure.nodes[a].p, structure.nodes[b].p);
                ((pa[0] - pb[0]).powi(2) + (pa[1] - pb[1]).powi(2) + (pa[2] - pb[2]).powi(2)).sqrt()
            }
            _ => 0.0,
        }
    };
    // Segment counts: `segments` (default 1), raised to ceil(length / target)
    // when the study carries a target — the target only ever refines
    // (ROADMAP Phase 10 item 3)
    let segments_for = |e: &ElementSpec, length: f64| -> usize {
        let mut n = e.segments.map(count).unwrap_or(1);
        if max_segment_length > 0.0 && length > 0.0 {
            n = n.max((length / max_segment_length - 1.0e-9).ceil() as usize);
        }
        n
    };
    let element_segments: Vec<usize> = spec
        .elements
        .iter()
        .map(|e| match e.kind.as_str() {
            "line" => segments_for(e, chord(&e.from, &e.to)),
            "catenary" => {
                let c = chord(&e.from, &e.to);
                // parabolic arc length: c + 8 s² / (3 c)
                let arc = if c > 0.0 {
                    c + 8.0 * e.sag * e.sag / (3.0 * c)
                } else {
                    c
                };
                segments_for(e, arc)
            }
            "mesh" => {
                // one count serves every bar: the longest bar sets the target
                let bar_x = e.length_x / (count(e.rows_x).max(1) as f64);
                let bar_y = e.length_y / (count(e.rows_y).max(1) as f64);
                segments_for(e, bar_x.max(bar_y))
            }
            _ => 0,
        })
        .collect();
    for m in &spec.materials {
        structure.add_material(Linear::new(m.id.clone(), m.epsilonr, m.mur, m.sigma));
    }
    for (e, &nseg) in spec.elements.iter().zip(&element_segments) {
        match e.kind.as_str() {
            "line" => structure.add_element(Element::Line(Line::new(
                e.id.clone(),
                e.from.clone(),
                e.to.clone(),
                e.radius,
                nseg,
                e.material.clone(),
            ))),
            "catenary" => structure.add_element(Element::Catenary(Catenary {
                line: Line::new(
                    e.id.clone(),
                    e.from.clone(),
                    e.to.clone(),
                    e.radius,
                    nseg,
                    e.material.clone(),
                ),
                sag: e.sag,
            })),
            "mesh" => structure.add_element(Element::Mesh(MeshElement {
                id: e.id.clone(),
                position: pos3(&e.position),
                length_x: e.length_x,
                length_y: e.length_y,
                rows_x: count(e.rows_x),
                rows_y: count(e.rows_y),
                radius: e.radius,
                segments: nseg,
                id_material: e.material.clone(),
            })),
            "channel" => {
                structure.add_element(Element::Channel(build_channel(e, max_segment_length)?))
            }
            other => eprintln!(" mTupa: unknown element type '{other}' — skipped"),
        }
    }

    // Channels that asked for it are calibrated now, once their elements and
    // the study's segment-length target are known (ADR 0025)
    for el in &mut structure.elements {
        if let Element::Channel(c) = el {
            if c.want_calibration {
                calibrate_channel(c)?;
            }
        }
    }

    let mut study = Study::new(spec.title.clone(), structure);
    study.kernel = kernel;
    study.image_model = image_model;
    study.max_segment_length = max_segment_length;

    let sources = spec.sources.as_ref().map(|list| {
        list.iter()
            .map(|s| {
                // "voltage" wins if both are present; neither → zero current
                let (value, is_voltage) = if let Some(v) = &s.voltage {
                    (num_complex::Complex64::new(v.re, v.im), true)
                } else if let Some(c) = &s.current {
                    (num_complex::Complex64::new(c.re, c.im), false)
                } else {
                    (num_complex::Complex64::new(0.0, 0.0), false)
                };
                Source {
                    node: s.node.clone(),
                    value,
                    is_voltage,
                    return_node: Some(s.return_node.clone()).filter(|r| !r.is_empty()),
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
        Some(s) => Some(build_signal(s, freq_hz.as_deref())?),
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

fn build_waveform(w: &WaveformSpec) -> Result<Signal> {
    let imax = w.imax.unwrap_or(0.0);
    Ok(match w.waveform.as_str() {
        "heidler" => match &w.terms {
            Some(terms) => {
                let i0: Vec<f64> = terms.iter().map(|t| t.i0).collect();
                let n: Vec<f64> = terms.iter().map(|t| t.n).collect();
                let tau1: Vec<f64> = terms.iter().map(|t| t.tau1).collect();
                let tau2: Vec<f64> = terms.iter().map(|t| t.tau2).collect();
                new_heidler_signal_terms(&i0, &n, &tau1, &tau2, w.imax)?
            }
            None => new_heidler_signal(imax),
        },
        "doubleExp" => new_double_exp_signal(imax, &w.front, w.jones)?,
        "portela" => new_portela_signal(imax, w.alpha, w.t_front, w.t_top_end, w.t_tail_end)?,
        "sine" => new_sine_signal(imax, w.frequency_hz, w.phase_deg)?,
        other => {
            return Err(TupaError::new(format!(
                "mTupa: unknown signal.waveform '{other}' (expected heidler, doubleExp, portela or sine)"
            )));
        }
    })
}

/// `quantity` of a transient source (ADR 0025): `current` (default) or
/// `voltage`; returns whether the waveform is a voltage.
fn parse_quantity(q: Option<&str>) -> Result<bool> {
    match q {
        None | Some("current") => Ok(false),
        Some("voltage") => Ok(true),
        Some(other) => Err(TupaError::new(format!(
            "mTupa: unknown signal quantity '{other}' (expected current or voltage)"
        ))),
    }
}

/// Build a `channel` element from its JSON spec (ADR 0025).
fn build_channel(e: &ElementSpec, max_segment_length: f64) -> Result<Channel> {
    let has_position = !e.position.is_empty();
    if e.strike.is_empty() != has_position {
        return Err(TupaError::new(format!(
            "mTupa: channel '{}' needs exactly one of strike (node) and position",
            e.id
        )));
    }
    if e.length <= 0.0 || e.radius <= 0.0 {
        return Err(TupaError::new(format!(
            "mTupa: channel '{}' needs positive length and radius",
            e.id
        )));
    }
    if e.speed.is_some() && e.inductance.is_some() {
        return Err(TupaError::new(format!(
            "mTupa: channel '{}': give either speed or inductance, not both",
            e.id
        )));
    }
    let breaks = if let Some(segments) = e.segments {
        // Uniform chain; the study target can only refine it
        let mut n = count(segments).max(1);
        if max_segment_length > 0.0 {
            n = n.max((e.length / max_segment_length - 1.0e-9).ceil() as usize);
        }
        (0..=n).map(|k| e.length * k as f64 / n as f64).collect()
    } else {
        let mut max_seg = e.max_segment.unwrap_or(f64::MAX);
        if max_segment_length > 0.0 {
            max_seg = max_seg.min(max_segment_length);
        }
        if max_seg >= f64::MAX || max_seg <= 0.0 {
            return Err(TupaError::new(format!(
                "mTupa: channel '{}' needs segments, maxSegment or numerics.maxSegmentLength",
                e.id
            )));
        }
        let first = e.first_segment.unwrap_or(max_seg);
        let growth = e.growth.unwrap_or(1.0);
        if first <= 0.0 || growth < 1.0 {
            return Err(TupaError::new(format!(
                "mTupa: channel '{}' needs firstSegment > 0 and growth >= 1",
                e.id
            )));
        }
        graded_breaks(e.length, first, growth, max_seg)
    };
    let mut ch = Channel::new(e.id.clone(), e.strike.clone(), e.length, e.radius, breaks);
    ch.foot = pos3(&e.position);
    ch.incidence_deg = e.incidence;
    ch.azimuth_deg = e.azimuth;
    if let Some(p) = &e.speed {
        ch.speed = p.build("speed")?;
    }
    if let Some(p) = &e.resistance {
        ch.resistance = p.build("resistance")?;
    }
    ch.inductance = e.inductance;
    if e.calibrate {
        if ch.speed.is_unset() {
            return Err(TupaError::new(format!(
                "mTupa: channel '{}': calibrate needs a target speed",
                e.id
            )));
        }
        ch.want_calibration = true;
    }
    Ok(ch)
}

fn build_signal(s: SignalSpec, freq_hz: Option<&[f64]>) -> Result<TransientSpec> {
    let sources = match &s.sources {
        Some(list) => {
            if !s.source_node.is_empty() || !s.wave.waveform.is_empty() {
                return Err(TupaError::new(
                    "mTupa: signal.sources cannot be combined with signal.sourceNode/waveform",
                ));
            }
            if list.is_empty() {
                return Err(TupaError::new(
                    "mTupa: signal.sources must hold at least one source",
                ));
            }
            list.iter()
                .map(|src| {
                    Ok(TransientSource {
                        node: src.node.clone(),
                        signal: build_waveform(&src.wave)?,
                        return_node: Some(src.return_node.clone()).filter(|r| !r.is_empty()),
                        is_voltage: parse_quantity(src.quantity.as_deref())?,
                    })
                })
                .collect::<Result<Vec<_>>>()?
        }
        None => vec![TransientSource {
            node: s.source_node.clone(),
            signal: build_waveform(&s.wave)?,
            return_node: Some(s.return_node.clone()).filter(|r| !r.is_empty()),
            is_voltage: parse_quantity(s.quantity.as_deref())?,
        }],
    };

    let mut options = TransientOptions::default();
    if let Some(a) = s.antialias_start {
        if a <= 0.0 || a > 1.0 {
            return Err(TupaError::new(
                "mTupa: signal.antialiasStart must be in (0, 1]",
            ));
        }
        options.antialias_start = a;
    }
    if let Some(w) = &s.window {
        options.window = match w.kind.as_str() {
            "none" => Window::None,
            "hann" => Window::Hann,
            other => {
                return Err(TupaError::new(format!(
                    "mTupa: unknown signal.window.type '{other}' (expected none or hann)"
                )));
            }
        };
        options.window_placement = match w.placement.as_deref().unwrap_or("spectral") {
            "spectral" => WindowPlacement::Spectral,
            "time" => WindowPlacement::Time,
            other => {
                return Err(TupaError::new(format!(
                    "mTupa: unknown signal.window.placement '{other}' (expected spectral or time)"
                )));
            }
        };
    }
    if let Some(t) = &s.transform {
        options.transform = match t.as_str() {
            "fft" => Transform::Fft,
            "nlt" => Transform::Nlt,
            other => {
                return Err(TupaError::new(format!(
                    "mTupa: unknown signal.transform '{other}' (expected fft or nlt)"
                )));
            }
        };
    }
    if let Some(c) = s.nlt_damping {
        if c <= 0.0 {
            return Err(TupaError::new("mTupa: signal.nltDamping must be > 0"));
        }
        if options.transform != Transform::Nlt {
            return Err(TupaError::new(
                "mTupa: signal.nltDamping requires signal.transform = \"nlt\"",
            ));
        }
        options.nlt_damping = c;
    }
    let freq_zero_hz = s.freq_zero_hz.unwrap_or(1.0e-6);
    if let Some(tf) = &s.transfer_function {
        match tf.as_str() {
            "full" => {}
            "interpolated" => {
                if options.transform == Transform::Nlt {
                    return Err(TupaError::new(
                        "mTupa: signal.transform \"nlt\" cannot be combined with \
                         signal.transferFunction \"interpolated\"",
                    ));
                }
                let Some(axis) = freq_hz else {
                    return Err(TupaError::new(
                        "mTupa: signal.transferFunction \"interpolated\" needs a \"frequencies\" \
                         block (the scan grid)",
                    ));
                };
                let (first, last) = (axis[0], axis[axis.len() - 1]);
                if first > freq_zero_hz * (1.0 + 1e-9) || last < s.nyquist_hz * (1.0 - 1e-9) {
                    return Err(TupaError::new(format!(
                        "mTupa: signal.transferFunction \"interpolated\": the frequencies axis must \
                         span [freqZeroHz, nyquistHz] = {freq_zero_hz:.3E} .. {:.3E} Hz (no \
                         extrapolation)",
                        s.nyquist_hz
                    )));
                }
                options.transfer_function = TransferFunction::Interpolated;
                options.scan_freq_hz = axis.to_vec();
            }
            other => {
                return Err(TupaError::new(format!(
                    "mTupa: unknown signal.transferFunction '{other}' (expected full or interpolated)"
                )));
            }
        }
    }

    Ok(TransientSpec {
        sources,
        observe_nodes: s.observe_nodes,
        observe_electrodes: s.observe_electrodes.unwrap_or_default(),
        nyquist_hz: s.nyquist_hz,
        fft_points: count(s.fft_points),
        freq_zero_hz,
        options,
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
            if let Some(r) = &s.return_node {
                require_node(study, r, "sources[].returnNode")?;
            }
        }
    }
    if let Some(t) = &case.transient {
        let field = if t.sources.len() > 1 {
            "signal.sources[].node"
        } else {
            "signal.sourceNode"
        };
        for src in &t.sources {
            require_node(study, &src.node, field)?;
            if let Some(r) = &src.return_node {
                require_node(study, r, "signal.sources[].returnNode")?;
            }
        }
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
        let text = CASE.replace("\"type\": \"line\"", "\"type\": \"circumference\"");
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
        assert_eq!(t.options.antialias_start, 1.0);
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
