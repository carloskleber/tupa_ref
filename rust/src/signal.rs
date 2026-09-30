//! Time-domain excitation waveforms (`mSignal`, ADR 0015): Heidler (legacy
//! fixed 6-term set [38] and the standard parametrised form [37, 39]) and
//! double exponential (± Jones front), plus the pre-transform tail taper.

use crate::error::{Result, TupaError};
use crate::special::erfc;

/// Heidler sum: `i(t) = Σ_k (I0_k/η_k) ratio/(1+ratio) e^{-t/τ2_k}`,
/// `ratio = (t/τ1_k)^{n_k}`.
#[derive(Debug, Clone, PartialEq)]
pub struct HeidlerSignal {
    /// Peak amplitude used when `rescale` (A)
    pub imax: f64,
    /// Per-term amplitudes
    pub i0: Vec<f64>,
    /// Per-term exponents
    pub n: Vec<f64>,
    /// Per-term front time constants (s)
    pub tau1: Vec<f64>,
    /// Per-term decay time constants (s)
    pub tau2: Vec<f64>,
    /// Peak-normalise to `imax` (legacy set: always; parametrised: only if
    /// `imax` was given)
    pub rescale: bool,
}

/// Double exponential `imax/(k(α−β)) (e^{-βt} − e^{-αt})`, optionally with the
/// Jones `e^{-(αt)²}` front.
#[derive(Debug, Clone, PartialEq)]
pub struct DoubleExpSignal {
    /// Peak amplitude (A)
    pub imax: f64,
    /// Front rate α (1/s)
    pub alpha: f64,
    /// Tail rate β (1/s)
    pub beta: f64,
    /// Nominal front time (s)
    pub t_front: f64,
    /// Jones Gaussian front
    pub jones: bool,
}

/// A source waveform.
#[derive(Debug, Clone, PartialEq)]
pub enum Signal {
    /// Heidler function sum
    Heidler(HeidlerSignal),
    /// Double exponential
    DoubleExp(DoubleExpSignal),
}

/// Legacy 6-term Heidler set of De Conti & Visacro [38] (MCS_FST#1), rescaled
/// to `imax`.
pub fn new_heidler_signal(imax: f64) -> Signal {
    Signal::Heidler(HeidlerSignal {
        imax,
        i0: vec![6.0, 5.0, 5.0, 8.0, 22.0, 20.0],
        n: vec![2.0, 3.0, 5.0, 9.0, 21.0, 2.0],
        tau1: [3.0, 3.5, 4.8, 6.0, 7.0, 70.0]
            .iter()
            .map(|x| x * 1.0e-6)
            .collect(),
        tau2: [76.0, 10.0, 30.0, 26.0, 23.2, 200.0]
            .iter()
            .map(|x| x * 1.0e-6)
            .collect(),
        rescale: true,
    })
}

/// Standard parametrised Heidler sum; `imax = None` keeps physical
/// amplitudes, `Some` rescales the peak.
pub fn new_heidler_signal_terms(
    i0: &[f64],
    n: &[f64],
    tau1: &[f64],
    tau2: &[f64],
    imax: Option<f64>,
) -> Result<Signal> {
    if n.len() != i0.len() || tau1.len() != i0.len() || tau2.len() != i0.len() {
        return Err(TupaError::new(
            "newHeidlerSignalTerms: i0/n/tau1/tau2 must all have one entry per term",
        ));
    }
    if i0.is_empty() {
        return Err(TupaError::new(
            "newHeidlerSignalTerms: at least one term is required",
        ));
    }
    if tau1.iter().any(|&t| t <= 0.0)
        || tau2.iter().any(|&t| t <= 0.0)
        || n.iter().any(|&v| v < 1.0)
    {
        return Err(TupaError::new(
            "newHeidlerSignalTerms: tau1/tau2 must be > 0 and n >= 1",
        ));
    }
    Ok(Signal::Heidler(HeidlerSignal {
        imax: imax.unwrap_or(0.0),
        i0: i0.to_vec(),
        n: n.to_vec(),
        tau1: tau1.to_vec(),
        tau2: tau2.to_vec(),
        rescale: imax.is_some(),
    }))
}

/// Double-exponential surge by standard name (`f1_2_5`, `f1_2_50`,
/// `f1_2_200`, `f250_2500`).
pub fn new_double_exp_signal(imax: f64, name: &str, jones: bool) -> Result<Signal> {
    let (t_front, alpha, beta) = match name {
        "f1_2_5" => (1.2e-6, 1.25e6, 2.8736e5),
        "f1_2_50" => (1.2e-6, 2.4691e6, 1.4663e4),
        "f1_2_200" => (1.2e-6, 2.6247e6, 3521.1),
        "f250_2500" => (250.0e-6, 9615.4, 347.58),
        _ => {
            return Err(TupaError::new(format!(
                "newDoubleExpSignal: unknown waveform '{name}' (expected f1_2_5, f1_2_50, f1_2_200 or f250_2500)"
            )));
        }
    };
    Ok(Signal::DoubleExp(DoubleExpSignal {
        imax,
        alpha,
        beta,
        t_front,
        jones,
    }))
}

impl Signal {
    /// Waveform samples at times `t` (s); zero for `t ≤ 0`.
    pub fn waveform(&self, t: &[f64]) -> Vec<f64> {
        match self {
            Signal::Heidler(h) => h.waveform(t),
            Signal::DoubleExp(d) => d.waveform(t),
        }
    }
}

impl HeidlerSignal {
    fn waveform(&self, t: &[f64]) -> Vec<f64> {
        let eta: Vec<f64> = (0..self.i0.len())
            .map(|k| {
                (-(self.tau1[k] / self.tau2[k])
                    * (self.n[k] * self.tau2[k] / self.tau1[k]).powf(1.0 / self.n[k]))
                .exp()
            })
            .collect();
        let mut i = vec![0.0_f64; t.len()];
        for (idx, &tv) in t.iter().enumerate() {
            let tp = tv.max(0.0);
            for (k, &eta_k) in eta.iter().enumerate() {
                let ratio = (tp / self.tau1[k]).powf(self.n[k]);
                let term = if tv > 0.0 {
                    (self.i0[k] / eta_k) * ratio / (1.0 + ratio) * (-tp / self.tau2[k]).exp()
                } else {
                    0.0
                };
                i[idx] += term;
            }
        }
        if self.rescale {
            let peak = i.iter().fold(0.0_f64, |m, v| m.max(v.abs()));
            if peak > 0.0 {
                for v in &mut i {
                    *v = *v / peak * self.imax;
                }
            }
        }
        i
    }
}

impl DoubleExpSignal {
    fn waveform(&self, t: &[f64]) -> Vec<f64> {
        let k = if self.jones {
            ((-self.beta * self.t_front).exp() - (-(self.alpha * self.t_front).powi(2)).exp())
                / (self.alpha - self.beta)
        } else {
            ((-self.beta * self.t_front).exp() - (-self.alpha * self.t_front).exp())
                / (self.alpha - self.beta)
        };
        t.iter()
            .map(|&tv| {
                if tv > 0.0 {
                    let tp = tv.max(0.0);
                    let tail = (-self.beta * tp).exp();
                    let front = if self.jones {
                        (-(self.alpha * tp).powi(2)).exp()
                    } else {
                        (-self.alpha * tp).exp()
                    };
                    self.imax / (k * (self.alpha - self.beta)) * (tail - front)
                } else {
                    0.0
                }
            })
            .collect()
    }
}

/// Smooth pre-transform tail taper `0.5 erfc((k − 0.8 n)/(n/20))`, `k = 1..n`.
pub fn tail_taper(n: usize) -> Vec<f64> {
    let pos = 0.8 * n as f64;
    let deltan = n as f64 / 20.0;
    (1..=n)
        .map(|k| 0.5 * erfc((k as f64 - pos) / deltan))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn double_exp_peak_is_imax_for_standard_front() {
        let s = new_double_exp_signal(30_000.0, "f1_2_50", false).unwrap();
        let t: Vec<f64> = (0..20_000).map(|k| k as f64 * 1e-8).collect();
        let w = s.waveform(&t);
        let peak = w.iter().cloned().fold(0.0, f64::max);
        // k normalises the value at t_front-to-crest convention; peak must be
        // of the order of imax
        assert!(
            peak > 0.9 * 30_000.0 && peak < 1.3 * 30_000.0,
            "peak = {peak}"
        );
        assert_eq!(w[0], 0.0);
    }

    #[test]
    fn waveform_is_zero_before_t_zero() {
        let s = new_heidler_signal(30_000.0);
        let w = s.waveform(&[-1e-6, 0.0]);
        assert_eq!(w, vec![0.0, 0.0]);
    }

    #[test]
    fn legacy_heidler_rescales_to_imax() {
        let s = new_heidler_signal(30_000.0);
        let t: Vec<f64> = (0..4000).map(|k| k as f64 * 5e-8).collect();
        let w = s.waveform(&t);
        let peak = w.iter().fold(0.0_f64, |m, v| m.max(v.abs()));
        assert!((peak - 30_000.0).abs() < 1e-6);
    }

    #[test]
    fn parametrised_heidler_keeps_physical_amplitude() {
        let s = new_heidler_signal_terms(&[10_000.0], &[2.0], &[1.9e-6], &[485e-6], None).unwrap();
        let t: Vec<f64> = (0..20_000).map(|k| k as f64 * 5e-8).collect();
        let w = s.waveform(&t);
        let peak = w.iter().cloned().fold(0.0, f64::max);
        // one Heidler term peaks near I0 (η < 1 normalises the crest to ≈ I0)
        assert!(
            peak > 0.8 * 10_000.0 && peak < 1.1 * 10_000.0,
            "peak = {peak}"
        );
    }

    #[test]
    fn rejects_bad_terms() {
        assert!(new_heidler_signal_terms(&[1.0], &[0.5], &[1e-6], &[1e-5], None).is_err());
        assert!(new_heidler_signal_terms(&[], &[], &[], &[], None).is_err());
        assert!(new_double_exp_signal(1.0, "nope", false).is_err());
    }

    #[test]
    fn taper_is_one_early_zero_late() {
        let w = tail_taper(1024);
        assert!((w[0] - 1.0).abs() < 1e-12);
        assert!(w[1023] < 1e-3);
        assert!((w[(0.8 * 1024.0) as usize - 1] - 0.5).abs() < 0.05);
        for k in 1..w.len() {
            assert!(w[k] <= w[k - 1] + 1e-15);
        }
    }
}
