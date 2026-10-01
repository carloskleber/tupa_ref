//! `tupa` command-line driver — same arguments and output files as the
//! Fortran executable (`fortran/app/main.f90`), plus `--dump-structure`.

use std::process::ExitCode;
use tupa::geometry::{GeometryKernel, GeometryOptions};
use tupa::mesh::ImageModel;
use tupa::verbosity::{VERB_QUIET, VERB_VERBOSE, set_verbosity};
use tupa::{RunOptions, run_from_file};

const USAGE: &str = "Usage: tupa [-v|--verbose] [-q|--quiet] [--epsrel <value>] \
[--kernel single|double] [--image-model frequency-dependent|ideal] [--no-cache] \
[--dump-structure] [--output-dir <dir>] <study.json>";

fn main() -> ExitCode {
    let mut filename: Option<String> = None;
    let mut geometry = GeometryOptions::default();
    let mut image_model = ImageModel::default();
    let mut dump = false;
    let mut output_dir = None;

    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-v" | "--verbose" => set_verbosity(VERB_VERBOSE),
            "-q" | "--quiet" => set_verbosity(VERB_QUIET),
            "--no-cache" => geometry.use_cache = false,
            "--dump-structure" => dump = true,
            "--epsrel" => {
                let Some(v) = args.next() else {
                    eprintln!("error: --epsrel requires a value, e.g. --epsrel 1.0e-6");
                    return ExitCode::FAILURE;
                };
                match v.parse::<f64>() {
                    Ok(x) if x > 0.0 && x.is_finite() => geometry.eps_rel = x,
                    _ => {
                        eprintln!("error: --epsrel: invalid value '{v}' (must be a positive real)");
                        return ExitCode::FAILURE;
                    }
                }
            }
            "--kernel" => match args.next().as_deref() {
                Some("single") => geometry.kernel = GeometryKernel::Single,
                Some("double") => geometry.kernel = GeometryKernel::Double,
                other => {
                    eprintln!(
                        "error: --kernel: invalid value '{}' (expected single or double)",
                        other.unwrap_or("")
                    );
                    return ExitCode::FAILURE;
                }
            },
            "--image-model" => match args.next().as_deref() {
                Some("frequency-dependent") => image_model = ImageModel::FrequencyDependent,
                Some("ideal") => image_model = ImageModel::Ideal,
                other => {
                    eprintln!(
                        "error: --image-model: invalid value '{}' (expected frequency-dependent or ideal)",
                        other.unwrap_or("")
                    );
                    return ExitCode::FAILURE;
                }
            },
            "--output-dir" => {
                let Some(v) = args.next() else {
                    eprintln!("error: --output-dir requires a directory");
                    return ExitCode::FAILURE;
                };
                output_dir = Some(std::path::PathBuf::from(v));
            }
            _ => filename = Some(arg),
        }
    }

    let Some(filename) = filename else {
        println!(" {USAGE}");
        eprintln!("error: missing study file argument");
        return ExitCode::FAILURE;
    };

    let opts = RunOptions {
        geometry,
        image_model,
        dump_structure: dump,
        output_dir,
    };
    match run_from_file(&filename, &opts) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("error: {e}");
            ExitCode::FAILURE
        }
    }
}
