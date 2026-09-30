//! Transfer-function transient driver (`mTransient`): one unit-current sweep
//! over the one-sided FFT frequency axis gives the transfer functions of
//! every observed node/electrode; the tail-tapered excitation spectrum
//! (optionally Tukey anti-alias filtered, ADR 0021) is multiplied in and
//! transformed back (ADR 0014/0015/0019).

use crate::ctes::PI;
use crate::error::{Result, TupaError};
use crate::fft::{fft_forward, fft_inverse, is_power_of_two};
use crate::result::ResultSet;
use crate::signal::{Signal, tail_taper};
use crate::study::{Source, Study};
use num_complex::Complex64;

/// Time axis `t_k = k/(2 f_nyq)`, `k = 0..n−1`.
pub fn sample_time_axis(nyquist_hz: f64, n_samples: usize) -> Vec<f64> {
    let dt = 1.0 / (2.0 * nyquist_hz);
    (0..n_samples).map(|k| k as f64 * dt).collect()
}

/// One-sided frequency axis (`n/2 + 1` bins, 0…Nyquist); the DC bin is
/// replaced by `freq_zero_hz` (ADR 0019).
pub fn one_sided_frequency_axis(nyquist_hz: f64, n_samples: usize, freq_zero_hz: f64) -> Vec<f64> {
    let n_bins = n_samples / 2 + 1;
    let mut f: Vec<f64> = (0..n_bins)
        .map(|k| k as f64 * nyquist_hz / (n_bins - 1) as f64)
        .collect();
    f[0] = freq_zero_hz;
    f
}

/// Tukey raised-cosine roll-off: 1 up to `taper_start` (fraction of
/// Nyquist), cosine to 0 at Nyquist (ADR 0021).
pub fn tukey_antialias_filter(n_bins: usize, taper_start: f64) -> Result<Vec<f64>> {
    if n_bins < 2 {
        return Err(TupaError::new(
            "tukeyAntialiasFilter: nBins must be at least 2",
        ));
    }
    if taper_start <= 0.0 || taper_start > 1.0 {
        return Err(TupaError::new(
            "tukeyAntialiasFilter: taperStart must be in (0, 1]",
        ));
    }
    Ok((0..n_bins)
        .map(|k| {
            let x = k as f64 / (n_bins - 1) as f64;
            if x <= taper_start {
                1.0
            } else {
                0.5 * (1.0 + (PI * (x - taper_start) / (1.0 - taper_start)).cos())
            }
        })
        .collect())
}

/// Transient request (mirrors the JSON `signal` block, ADR 0015).
#[derive(Debug, Clone)]
pub struct TransientSpec {
    /// Excitation waveform
    pub signal: Signal,
    /// Node where the current is injected
    pub source_node: String,
    /// Nodes whose voltage is returned
    pub observe_nodes: Vec<String>,
    /// Electrodes whose `i1(t)`, `i2(t)` are returned (empty = none)
    pub observe_electrodes: Vec<String>,
    /// Nyquist frequency (Hz)
    pub nyquist_hz: f64,
    /// Number of time/FFT samples (power of two)
    pub fft_points: usize,
    /// Replacement frequency for the DC bin (Hz)
    pub freq_zero_hz: f64,
    /// Anti-alias roll-off start as fraction of Nyquist, in (0, 1]
    pub antialias_start: f64,
}

/// Time-domain results.
#[derive(Debug, Clone, PartialEq)]
pub struct TransientResult {
    /// Time axis (s)
    pub t: Vec<f64>,
    /// Injected current `i(t)` after the tail taper (A)
    pub injected_current: Vec<f64>,
    /// Observed node voltages `[node][sample]` (V)
    pub node_responses: Vec<Vec<f64>>,
    /// `i1(t)` of observed electrodes `[electrode][sample]` (A)
    pub i1_responses: Vec<Vec<f64>>,
    /// `i2(t)` of observed electrodes `[electrode][sample]` (A)
    pub i2_responses: Vec<Vec<f64>>,
}

fn spectrum_to_time_series(
    results: &ResultSet,
    entity: usize,
    excitation: &[Complex64],
    antialias: &[f64],
    n_bins: usize,
    n_samples: usize,
) -> Result<Vec<f64>> {
    let h = |k: usize| results.get(entity, k);
    let mut full = vec![Complex64::new(0.0, 0.0); n_samples];
    for k in 0..n_bins - 1 {
        full[k] = h(k) * excitation[k] * antialias[k];
    }
    full[0] = Complex64::new(full[0].re, 0.0);
    // k (1-based) = nBins..nSamples  →  sourceBin = 2 nBins − k
    for k1 in n_bins..=n_samples {
        let source_bin = 2 * n_bins - k1; // 1-based
        let sb = source_bin - 1;
        full[k1 - 1] = (h(sb) * excitation[sb]).conj() * antialias[sb];
    }
    fft_inverse(&mut full)?;
    Ok(full.iter().map(|z| z.re).collect())
}

/// Run the transient pipeline on a prepared or unprepared study (performs the
/// unit-current sweep on the one-sided frequency axis).
pub fn transient_response(study: &mut Study, spec: &TransientSpec) -> Result<TransientResult> {
    let n = spec.fft_points;
    if !is_power_of_two(n) {
        return Err(TupaError::new(
            "transientResponse: nSamples must be a power of two",
        ));
    }

    let t = sample_time_axis(spec.nyquist_hz, n);
    let taper = tail_taper(n);
    let wave = spec.signal.waveform(&t);
    let injected: Vec<f64> = wave.iter().zip(&taper).map(|(w, k)| w * k).collect();

    let mut excitation: Vec<Complex64> = injected.iter().map(|&v| Complex64::new(v, 0.0)).collect();
    fft_forward(&mut excitation)?;

    let n_bins = n / 2 + 1;
    let antialias = tukey_antialias_filter(n_bins, spec.antialias_start)?;
    let freq_hz = one_sided_frequency_axis(spec.nyquist_hz, n, spec.freq_zero_hz);

    study.run_sweep(
        &freq_hz,
        &[Source {
            node: spec.source_node.clone(),
            value: Complex64::new(1.0, 0.0),
            is_voltage: false,
        }],
    )?;

    let mut node_responses = Vec::with_capacity(spec.observe_nodes.len());
    for id in &spec.observe_nodes {
        let idx = study
            .structure
            .find_node_index(id)
            .ok_or_else(|| TupaError::new(format!("transientResponse: node '{id}' not found")))?;
        node_responses.push(spectrum_to_time_series(
            &study.voltage_results,
            idx,
            &excitation,
            &antialias,
            n_bins,
            n,
        )?);
    }

    let mut i1 = Vec::with_capacity(spec.observe_electrodes.len());
    let mut i2 = Vec::with_capacity(spec.observe_electrodes.len());
    for id in &spec.observe_electrodes {
        let idx = study.structure.find_electrode_index(id).ok_or_else(|| {
            TupaError::new(format!("transientResponse: electrode '{id}' not found"))
        })?;
        i1.push(spectrum_to_time_series(
            &study.long_current_results,
            idx,
            &excitation,
            &antialias,
            n_bins,
            n,
        )?);
        i2.push(spectrum_to_time_series(
            &study.trans_current_results,
            idx,
            &excitation,
            &antialias,
            n_bins,
            n,
        )?);
    }

    Ok(TransientResult {
        t,
        injected_current: injected,
        node_responses,
        i1_responses: i1,
        i2_responses: i2,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn axes() {
        let t = sample_time_axis(1.0e6, 8);
        assert!((t[1] - 0.5e-6).abs() < 1e-18);
        let f = one_sided_frequency_axis(1.0e6, 8, 1e-6);
        assert_eq!(f.len(), 5);
        assert_eq!(f[0], 1e-6);
        assert!((f[4] - 1.0e6).abs() < 1e-6);
    }

    #[test]
    fn tukey_filter_shape() {
        let f = tukey_antialias_filter(101, 0.5).unwrap();
        assert_eq!(f[0], 1.0);
        assert_eq!(f[50], 1.0);
        assert!(f[100].abs() < 1e-12);
        assert!(tukey_antialias_filter(1, 0.5).is_err());
        assert!(tukey_antialias_filter(10, 0.0).is_err());
        let off = tukey_antialias_filter(11, 1.0).unwrap();
        assert!(off.iter().all(|&v| v == 1.0));
    }
}
