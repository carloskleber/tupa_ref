//! Transfer-function transient driver (`mTransient`): one unit-current sweep
//! over the one-sided FFT frequency axis gives the transfer functions of
//! every observed node/electrode; the tail-tapered excitation spectrum
//! (optionally Tukey anti-alias filtered, ADR 0021) is multiplied in and
//! transformed back (ADR 0014/0015/0019).
//!
//! ROADMAP Phase 9 (ADR 0015 amendment 2026-09-30), all opt-in through
//! [`TransientOptions`]: transfer function solved on a scan axis and
//! pchip-interpolated onto the bins (item 1), half-Hann window in the
//! spectral or time placement (item 2), several simultaneous injections
//! superposed as `Σ_k H_k·X_k` (item 4), and the Numerical Laplace
//! Transform (item 5).

use crate::ctes::PI;
use crate::error::{Result, TupaError};
use crate::fft::{fft_forward, fft_inverse, is_power_of_two};
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

/// Half-Hann window `0.5·[1 + cos(π(k−1)/(n−1))]`: 1 at the first sample, 0
/// at the last (ROADMAP Phase 9 item 2).
pub fn hann_half_window(n: usize) -> Result<Vec<f64>> {
    if n < 2 {
        return Err(TupaError::new("hannHalfWindow: n must be at least 2"));
    }
    Ok((0..n)
        .map(|k| 0.5 * (1.0 + (PI * k as f64 / (n - 1) as f64).cos()))
        .collect())
}

/// Default NLT damping `c = ln(N²)/T`, `T = N/(2 f_nyq)` (ROADMAP Phase 9 item 5).
pub fn nlt_default_damping(nyquist_hz: f64, n_samples: usize) -> f64 {
    (n_samples as f64).powi(2).ln() * 2.0 * nyquist_hz / n_samples as f64
}

fn sign1(v: f64) -> f64 {
    if v > 0.0 {
        1.0
    } else if v < 0.0 {
        -1.0
    } else {
        0.0
    }
}

fn pchip_end(h1: f64, h2: f64, del1: f64, del2: f64) -> f64 {
    let d = ((2.0 * h1 + h2) * del1 - h1 * del2) / (h1 + h2);
    if sign1(d) != sign1(del1) {
        0.0
    } else if sign1(del1) != sign1(del2) && d.abs() > (3.0 * del1).abs() {
        3.0 * del1
    } else {
        d
    }
}

fn pchip_slopes(x: &[f64], y: &[f64]) -> Vec<f64> {
    let n = x.len();
    let h: Vec<f64> = (0..n - 1).map(|k| x[k + 1] - x[k]).collect();
    let del: Vec<f64> = (0..n - 1).map(|k| (y[k + 1] - y[k]) / h[k]).collect();
    if n == 2 {
        return vec![del[0]; 2];
    }
    let mut d = vec![0.0; n];
    for k in 0..n - 2 {
        if sign1(del[k]) * sign1(del[k + 1]) > 0.0 {
            let hs = h[k] + h[k + 1];
            let w1 = (h[k] + hs) / (3.0 * hs);
            let w2 = (hs + h[k + 1]) / (3.0 * hs);
            let dmax = del[k].abs().max(del[k + 1].abs());
            let dmin = del[k].abs().min(del[k + 1].abs());
            d[k + 1] = dmin / (w1 * (del[k] / dmax) + w2 * (del[k + 1] / dmax));
        }
    }
    d[0] = pchip_end(h[0], h[1], del[0], del[1]);
    d[n - 1] = pchip_end(h[n - 2], h[n - 3], del[n - 2], del[n - 3]);
    d
}

fn hermite_eval(y0: f64, y1: f64, d0: f64, d1: f64, h: f64, sx: f64) -> f64 {
    let delta = (y1 - y0) / h;
    let c = (3.0 * delta - 2.0 * d0 - d1) / h;
    let b = (d0 - 2.0 * delta + d1) / h.powi(2);
    ((b * sx + c) * sx + d0) * sx + y0
}

/// Componentwise (Re, Im) pchip interpolation — port of Matlab `pchip`
/// (`pchipslopes`/`pchipend`), the legacy `imitancia.m` interpolation
/// (ROADMAP Phase 9 item 1). `x` strictly ascending; queries are clamped to
/// `[x₀, x_{n−1}]` (no extrapolation).
pub fn pchip_interpolate(x: &[f64], y: &[Complex64], xq: &[f64]) -> Result<Vec<Complex64>> {
    let n = x.len();
    if n < 2 || y.len() != n {
        return Err(TupaError::new(
            "pchipInterpolate: need at least two samples and size(y) == size(x)",
        ));
    }
    if x.windows(2).any(|w| w[1] <= w[0]) {
        return Err(TupaError::new(
            "pchipInterpolate: abscissae must be strictly ascending",
        ));
    }
    let yr: Vec<f64> = y.iter().map(|z| z.re).collect();
    let yi: Vec<f64> = y.iter().map(|z| z.im).collect();
    let dr = pchip_slopes(x, &yr);
    let di = pchip_slopes(x, &yi);
    Ok(xq
        .iter()
        .map(|&q| {
            let xc = q.max(x[0]).min(x[n - 1]);
            // bisection: x[j] <= xc < x[j+1], last knot → last interval
            let (mut lo, mut hi) = (0usize, n - 1);
            while hi - lo > 1 {
                let mid = (lo + hi) / 2;
                if xc >= x[mid] {
                    lo = mid;
                } else {
                    hi = mid;
                }
            }
            let j = lo;
            let h = x[j + 1] - x[j];
            let sx = xc - x[j];
            Complex64::new(
                hermite_eval(yr[j], yr[j + 1], dr[j], dr[j + 1], h, sx),
                hermite_eval(yi[j], yi[j + 1], di[j], di[j + 1], h, sx),
            )
        })
        .collect())
}

/// Window function (ROADMAP Phase 9 item 2).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Window {
    /// No window
    #[default]
    None,
    /// Falling half of a Hann window
    Hann,
}

/// Where the window acts.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum WindowPlacement {
    /// On the one-sided response spectrum `H·X`
    #[default]
    Spectral,
    /// On the sampled excitation record
    Time,
}

/// Inverse transform.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Transform {
    /// Plain FFT, `s = jω`
    #[default]
    Fft,
    /// Numerical Laplace Transform, `s = c + jω`
    Nlt,
}

/// How the transfer function reaches the FFT bins.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum TransferFunction {
    /// Solve every bin
    #[default]
    Full,
    /// Solve the scan axis and pchip-interpolate
    Interpolated,
}

/// Opt-in transient options (ADR 0015 amendment 2026-09-30); the default is
/// the Phase 6 pipeline.
#[derive(Debug, Clone, PartialEq)]
pub struct TransientOptions {
    /// Band-edge Tukey filter start (ADR 0021); 1 = off
    pub antialias_start: f64,
    /// Window function
    pub window: Window,
    /// Window placement
    pub window_placement: WindowPlacement,
    /// Inverse transform
    pub transform: Transform,
    /// NLT damping c (1/s); 0 = `nlt_default_damping`
    pub nlt_damping: f64,
    /// Full per-bin solve or scan-fed interpolation
    pub transfer_function: TransferFunction,
    /// Ascending scan axis (Hz) for `Interpolated`
    pub scan_freq_hz: Vec<f64>,
}

impl Default for TransientOptions {
    fn default() -> Self {
        TransientOptions {
            antialias_start: 1.0,
            window: Window::None,
            window_placement: WindowPlacement::Spectral,
            transform: Transform::Fft,
            nlt_damping: 0.0,
            transfer_function: TransferFunction::Full,
            scan_freq_hz: Vec::new(),
        }
    }
}

/// One injection of a transient run.
#[derive(Debug, Clone)]
pub struct TransientSource {
    /// Injection node
    pub node: String,
    /// Excitation waveform
    pub signal: Signal,
    /// Return node of a two-node source (ADR 0025); `None` = against remote
    /// earth
    pub return_node: Option<String>,
    /// The waveform is a source voltage (V) across the node pair instead of
    /// an injected current (A) (ADR 0025)
    pub is_voltage: bool,
}

impl TransientSource {
    /// A current injection at `node`.
    pub fn current(node: impl Into<String>, signal: Signal) -> Self {
        Self {
            node: node.into(),
            signal,
            return_node: None,
            is_voltage: false,
        }
    }
}

/// Transient request (mirrors the JSON `signal` block, ADR 0015).
#[derive(Debug, Clone)]
pub struct TransientSpec {
    /// Current injections (one for the single-source form)
    pub sources: Vec<TransientSource>,
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
    /// Phase 9 options (and the ADR 0021 filter)
    pub options: TransientOptions,
}

/// Time-domain results.
#[derive(Debug, Clone, PartialEq)]
pub struct TransientResult {
    /// Time axis (s)
    pub t: Vec<f64>,
    /// Injected current per source after the tail taper and any time window (A)
    pub injected_currents: Vec<Vec<f64>>,
    /// Observed node voltages `[node][sample]` (V)
    pub node_responses: Vec<Vec<f64>>,
    /// `i1(t)` of observed electrodes `[electrode][sample]` (A)
    pub i1_responses: Vec<Vec<f64>>,
    /// `i2(t)` of observed electrodes `[electrode][sample]` (A)
    pub i2_responses: Vec<Vec<f64>>,
}

fn spectrum_to_time_series(
    product: &[Complex64],
    filter: &[f64],
    n_bins: usize,
    n_samples: usize,
) -> Result<Vec<f64>> {
    let mut full = vec![Complex64::new(0.0, 0.0); n_samples];
    for k in 0..n_bins - 1 {
        full[k] = product[k] * filter[k];
    }
    full[0] = Complex64::new(full[0].re, 0.0);
    // k (1-based) = nBins..nSamples  →  sourceBin = 2 nBins − k
    for k1 in n_bins..=n_samples {
        let sb = 2 * n_bins - k1 - 1;
        full[k1 - 1] = product[sb].conj() * filter[sb];
    }
    fft_inverse(&mut full)?;
    Ok(full.iter().map(|z| z.re).collect())
}

fn scan_spans(scan: &[f64], freq_zero_hz: f64, nyquist_hz: f64) -> bool {
    scan.len() >= 2
        && scan.windows(2).all(|w| w[1] > w[0])
        && scan[0] <= freq_zero_hz * (1.0 + 1e-9)
        && scan[scan.len() - 1] >= nyquist_hz * (1.0 - 1e-9)
}

/// Run the transient pipeline (`transientResponseSources`): one unit-current
/// sweep per source, `P = Σ_k H_k·X_k` per observe point, then filter,
/// conjugate-symmetric rebuild and inverse FFT (undamped by `e^{ct}` under
/// NLT).
pub fn transient_response(study: &mut Study, spec: &TransientSpec) -> Result<TransientResult> {
    let n = spec.fft_points;
    let o = &spec.options;
    if spec.sources.is_empty() {
        return Err(TupaError::new(
            "transientResponse: need at least one source and one node per source",
        ));
    }
    if !is_power_of_two(n) {
        return Err(TupaError::new(
            "transientResponse: nSamples must be a power of two",
        ));
    }
    let nlt = o.transform == Transform::Nlt;
    let interpolated = o.transfer_function == TransferFunction::Interpolated;
    if nlt && interpolated {
        return Err(TupaError::new(
            "transientResponse: transform 'nlt' cannot be combined with transferFunction 'interpolated'",
        ));
    }
    if interpolated && !scan_spans(&o.scan_freq_hz, spec.freq_zero_hz, spec.nyquist_hz) {
        return Err(TupaError::new(
            "transientResponse: the interpolation scan axis must be ascending and span \
             [freqZeroHz, nyquistHz] (no extrapolation)",
        ));
    }
    if o.nlt_damping < 0.0 {
        return Err(TupaError::new(
            "transientResponse: nltDamping must be >= 0 (0 = default ln(N^2)/T)",
        ));
    }

    let n_bins = n / 2 + 1;
    let t = sample_time_axis(spec.nyquist_hz, n);
    let taper = tail_taper(n);
    let time_window = if o.window == Window::Hann && o.window_placement == WindowPlacement::Time {
        Some(hann_half_window(n)?)
    } else {
        None
    };
    let c = if nlt {
        if o.nlt_damping == 0.0 {
            nlt_default_damping(spec.nyquist_hz, n)
        } else {
            o.nlt_damping
        }
    } else {
        0.0
    };
    let damp: Vec<f64> = t.iter().map(|&tk| (-c * tk).exp()).collect();

    let mut injected_currents = Vec::with_capacity(spec.sources.len());
    let mut excitations = Vec::with_capacity(spec.sources.len());
    for src in &spec.sources {
        let wave = src.signal.waveform(&t);
        let mut inj: Vec<f64> = wave.iter().zip(&taper).map(|(w, k)| w * k).collect();
        if let Some(w) = &time_window {
            for (v, wk) in inj.iter_mut().zip(w) {
                *v *= wk;
            }
        }
        let mut x: Vec<Complex64> = if nlt {
            inj.iter()
                .zip(&damp)
                .map(|(&v, &d)| Complex64::new(v * d, 0.0))
                .collect()
        } else {
            inj.iter().map(|&v| Complex64::new(v, 0.0)).collect()
        };
        fft_forward(&mut x)?;
        injected_currents.push(inj);
        excitations.push(x);
    }

    let mut filter = tukey_antialias_filter(n_bins, o.antialias_start)?;
    if o.window == Window::Hann && o.window_placement == WindowPlacement::Spectral {
        for (f, w) in filter.iter_mut().zip(hann_half_window(n_bins)?) {
            *f *= w;
        }
    }

    let bin_freq = one_sided_frequency_axis(
        spec.nyquist_hz,
        n,
        if nlt { 0.0 } else { spec.freq_zero_hz },
    );
    let solve_freq = if interpolated {
        o.scan_freq_hz.clone()
    } else {
        bin_freq.clone()
    };

    // Discretised IDs resolve only after assembly (idempotent)
    study.prepare()?;
    let node_idx: Vec<usize> =
        spec.observe_nodes
            .iter()
            .map(|id| {
                study.structure.find_node_index(id).ok_or_else(|| {
                    TupaError::new(format!("transientResponse: node '{id}' not found"))
                })
            })
            .collect::<Result<_>>()?;
    let elec_idx: Vec<usize> = spec
        .observe_electrodes
        .iter()
        .map(|id| {
            study.structure.find_electrode_index(id).ok_or_else(|| {
                TupaError::new(format!("transientResponse: electrode '{id}' not found"))
            })
        })
        .collect::<Result<_>>()?;

    let zero = Complex64::new(0.0, 0.0);
    let mut p_node = vec![vec![zero; n_bins]; node_idx.len()];
    let mut p_i1 = vec![vec![zero; n_bins]; elec_idx.len()];
    let mut p_i2 = vec![vec![zero; n_bins]; elec_idx.len()];

    let to_bins = |row: Vec<Complex64>| -> Result<Vec<Complex64>> {
        if interpolated {
            pchip_interpolate(&solve_freq, &row, &bin_freq)
        } else {
            Ok(row)
        }
    };
    let accumulate = |p: &mut [Complex64], h: &[Complex64], x: &[Complex64], first: bool| {
        for k in 0..n_bins {
            if first {
                p[k] = h[k] * x[k];
            } else {
                p[k] += h[k] * x[k];
            }
        }
    };

    for (is, src) in spec.sources.iter().enumerate() {
        study.run_sweep_damped(
            &solve_freq,
            &[Source {
                node: src.node.clone(),
                value: Complex64::new(1.0, 0.0),
                is_voltage: src.is_voltage,
                return_node: src.return_node.clone(),
            }],
            c,
        )?;
        let nf = solve_freq.len();
        for (io, &idx) in node_idx.iter().enumerate() {
            let h = to_bins((0..nf).map(|k| study.voltage_results.get(idx, k)).collect())?;
            accumulate(&mut p_node[io], &h, &excitations[is], is == 0);
        }
        for (io, &idx) in elec_idx.iter().enumerate() {
            let h = to_bins(
                (0..nf)
                    .map(|k| study.long_current_results.get(idx, k))
                    .collect(),
            )?;
            accumulate(&mut p_i1[io], &h, &excitations[is], is == 0);
            let h = to_bins(
                (0..nf)
                    .map(|k| study.trans_current_results.get(idx, k))
                    .collect(),
            )?;
            accumulate(&mut p_i2[io], &h, &excitations[is], is == 0);
        }
    }

    let undamp: Vec<f64> = t.iter().map(|&tk| (c * tk).exp()).collect();
    let synth = |p: &[Complex64]| -> Result<Vec<f64>> {
        let mut y = spectrum_to_time_series(p, &filter, n_bins, n)?;
        if nlt {
            for (v, u) in y.iter_mut().zip(&undamp) {
                *v *= u;
            }
        }
        Ok(y)
    };

    Ok(TransientResult {
        t: t.clone(),
        injected_currents,
        node_responses: p_node.iter().map(|p| synth(p)).collect::<Result<_>>()?,
        i1_responses: p_i1.iter().map(|p| synth(p)).collect::<Result<_>>()?,
        i2_responses: p_i2.iter().map(|p| synth(p)).collect::<Result<_>>()?,
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
    fn hann_window_shape() {
        let w = hann_half_window(101).unwrap();
        assert_eq!(w[0], 1.0);
        assert!(w[100].abs() < 1e-15 && (w[50] - 0.5).abs() < 1e-15);
        let t = tukey_antialias_filter(101, 1e-12).unwrap();
        assert!(w.iter().zip(&t).all(|(a, b)| (a - b).abs() < 1e-9));
        assert!(hann_half_window(1).is_err());
    }

    #[test]
    fn pchip_matches_reference_values() {
        // SciPy PchipInterpolator (same algorithm as Matlab pchip)
        let c = |v: &[f64]| v.iter().map(|&x| Complex64::new(x, -x)).collect::<Vec<_>>();
        let y = pchip_interpolate(
            &[1.0, 2.0, 3.0, 4.0],
            &c(&[1.0, 4.0, 2.0, 3.0]),
            &[1.5, 2.5, 3.5],
        )
        .unwrap();
        for (a, e) in y.iter().zip([3.1875, 3.0, 2.1875]) {
            assert!((a.re - e).abs() < 1e-14 && (a.im + e).abs() < 1e-14);
        }
        let y = pchip_interpolate(
            &[0.0, 1.0, 3.0, 4.0, 7.0],
            &c(&[0.0, 2.0, 3.0, 7.0, 8.0]),
            &[0.5, 2.0, 3.5, 5.5, 6.9],
        )
        .unwrap();
        let e = [
            1.2053571428571428,
            2.471042471042471,
            5.032069382815652,
            7.76865671641791,
            7.999049198452184,
        ];
        for (a, e) in y.iter().zip(e) {
            assert!((a.re - e).abs() < 1e-12);
        }
        // linear data exact, clamping at the ends
        let y =
            pchip_interpolate(&[0.0, 1.0, 2.5], &c(&[1.0, 4.0, 8.5]), &[0.3, 2.5, 9.0]).unwrap();
        assert!(
            (y[0].re - 1.9).abs() < 1e-14
                && (y[1].re - 8.5).abs() < 1e-14
                && (y[2].re - 8.5).abs() < 1e-14
        );
        assert!(pchip_interpolate(&[0.0, 0.0], &c(&[1.0, 2.0]), &[0.0]).is_err());
    }

    #[test]
    fn nlt_default_damping_is_ln_n2_over_t() {
        let c = nlt_default_damping(1.0e4, 256);
        assert!((c - (256.0f64 * 256.0).ln() / (256.0 / 2.0e4)).abs() < 1e-9 * c);
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
