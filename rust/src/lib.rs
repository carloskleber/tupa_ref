//! TUPÃ — Rust implementation of the Hybrid Electromagnetic Model (HEM).
//!
//! An independent implementation of the public contract (JSON schema v1 +
//! `common/` cases, ADR 0002/0018) that mirrors the Fortran reference module
//! by module (ROADMAP Phase 8), so a reviewer can audit kernel against
//! kernel. `f64` and `Complex64` throughout.

#![forbid(unsafe_code)]

pub mod bessel;
pub mod ctes;
pub mod electrode;
pub mod element;
pub mod error;
pub mod fft;
pub mod geometry;
pub mod geometry_cache;
pub mod impedance;
pub mod json;
pub mod linalg;
pub mod material;
pub mod mesh;
pub mod node;
pub mod result;
pub mod results_writer;
pub mod signal;
pub mod special;
pub mod structure;
pub mod study;
pub mod transient;
pub mod verbosity;

pub use error::{Result, TupaError};
pub use json::{LoadedCase, load_study, load_study_str, validate_study_references};
pub use study::{Source, Study};

use std::path::Path;
use verbosity::{VERB_NORMAL, VERB_VERBOSE, verbose};

/// Basename of `path` without directory and extension.
pub fn basename_no_ext(path: &str) -> String {
    Path::new(path)
        .file_stem()
        .map(|s| s.to_string_lossy().into_owned())
        .unwrap_or_default()
}

/// Options of the command-line driver.
#[derive(Debug, Clone, Default)]
pub struct RunOptions {
    /// Geometry/quadrature options
    pub geometry: geometry::GeometryOptions,
    /// Image model of studies that do not state one (CLI `--image-model`)
    pub image_model: mesh::ImageModel,
    /// Print the assembled nodes/electrodes and stop before any physics
    pub dump_structure: bool,
    /// Directory the result files are written to (default: current dir)
    pub output_dir: Option<std::path::PathBuf>,
}

/// Load a case, run the sweep and/or transient it asks for and write
/// `<base>_results.{csv,json}` / `<base>_transient_results.{csv,json}`
/// (same names as the Fortran executable).
pub fn run_from_file(filename: &str, opts: &RunOptions) -> Result<()> {
    let start = std::time::Instant::now();
    if verbosity::verbosity_level() == VERB_VERBOSE {
        println!("\n Loading study {filename}");
    }
    let mut case = load_study(filename)?;
    case.study.options = opts.geometry;
    case.study.default_image_model = opts.image_model;
    validate_study_references(&mut case)?;

    if opts.dump_structure {
        print!("{}", case.study.dump_structure());
        return Ok(());
    }

    let base = basename_no_ext(filename);
    let out_path = |suffix: &str| -> std::path::PathBuf {
        let name = format!("{base}{suffix}");
        match &opts.output_dir {
            Some(d) => d.join(name),
            None => name.into(),
        }
    };

    let run_sweep = case.sources.is_some() && case.freq_hz.is_some();
    let run_transient = case.transient.is_some();

    if run_sweep {
        let sources = case.sources.clone().expect("sources");
        let freq = case.freq_hz.clone().expect("frequencies");
        case.study.run_sweep(&freq, &sources)?;
        case.study.report();

        let o = &case.outputs;
        let csv = results_writer::results_csv(
            &case.study,
            o.nodes.as_deref(),
            o.electrodes.as_deref(),
            o.quantities.as_deref(),
        )?;
        let json = results_writer::results_json(
            &case.study,
            o.nodes.as_deref(),
            o.electrodes.as_deref(),
            o.quantities.as_deref(),
        )?;
        let (csv_file, json_file) = (out_path("_results.csv"), out_path("_results.json"));
        std::fs::write(&csv_file, csv)?;
        std::fs::write(&json_file, json)?;
        verbose(
            VERB_NORMAL,
            &format!("Wrote {} and {}", csv_file.display(), json_file.display()),
        );
    }

    if run_transient {
        let spec = case.transient.clone().expect("transient");
        let result = transient::transient_response(&mut case.study, &spec)?;
        if !run_sweep {
            case.study.report();
        }
        let source_nodes: Vec<String> = spec.sources.iter().map(|s| s.node.clone()).collect();
        let csv = results_writer::transient_csv(
            &source_nodes,
            &spec.observe_nodes,
            &spec.observe_electrodes,
            &result,
        );
        let json = results_writer::transient_json(
            &case.study.title,
            &source_nodes,
            &spec.observe_nodes,
            &spec.observe_electrodes,
            &result,
        );
        let (csv_file, json_file) = (
            out_path("_transient_results.csv"),
            out_path("_transient_results.json"),
        );
        std::fs::write(&csv_file, csv)?;
        std::fs::write(&json_file, json)?;
        verbose(
            VERB_NORMAL,
            &format!("Wrote {} and {}", csv_file.display(), json_file.display()),
        );
    }

    if !(run_sweep || run_transient) {
        case.study.report();
        verbose(
            VERB_NORMAL,
            "(structure-only case: no sources/frequencies/signal block -- nothing to solve)",
        );
    }

    verbose(
        VERB_NORMAL,
        &format!(
            "Simulation duration: {}",
            format_duration(start.elapsed().as_secs_f64())
        ),
    );
    Ok(())
}

/// Load a case and run only its harmonic sweep (`runStudyFromFile`).
pub fn run_study_from_file(filename: &str) -> Result<Study> {
    let mut case = load_study(filename)?;
    validate_study_references(&mut case)?;
    let (Some(sources), Some(freq)) = (case.sources.clone(), case.freq_hz.clone()) else {
        return Err(TupaError::new(format!(
            "runStudyFromFile: '{filename}' has no 'sources'/'frequencies' block to sweep (ADR 0013)"
        )));
    };
    case.study.run_sweep(&freq, &sources)?;
    Ok(case.study)
}

fn format_duration(seconds: f64) -> String {
    let hours = (seconds / 3600.0) as u64;
    let remainder = seconds - hours as f64 * 3600.0;
    let minutes = (remainder / 60.0) as u64;
    let secs = remainder - minutes as f64 * 60.0;
    let mut s = String::new();
    if hours > 0 {
        s.push_str(&format!("{hours} h "));
    }
    if hours > 0 || minutes > 0 {
        s.push_str(&format!("{minutes} min "));
    }
    s.push_str(&format!("{secs:.4} s"));
    s
}
