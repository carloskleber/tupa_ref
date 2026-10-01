//! Calibration of a lightning channel's series loading against a prescribed
//! return-stroke speed (`mChannelCalibration`, ROADMAP Phase 10b item 1,
//! theory.md §4.5, ADR 0025).
//!
//! The closed-form `L'(z)` is only a starting value: against full-wave
//! results it is off by up to 0.12c in speed. The calibration runs the
//! channel alone above ideal ground, measures its speed with Baba & Rakov's
//! metric, and solves for the scale factor κ on the closed form that makes the
//! measured speed equal the target. Segmentation, radius and `R'` are the
//! channel's own, so the calibrated κ also absorbs the discretisation's
//! numerical dispersion; a calibration is valid for that set only. The
//! calibration channel is lossless (`R' = 0`): the front-tracking metric is
//! defined for the front of a lossless loaded wire, and a resistive wire's
//! convex, slowly rising front makes it ill-conditioned (Baba & Rakov note
//! the same for their speeds); `R'` then only damps the calibrated wave.
//!
//! Measurement: the channel is excited at its base by a current ramp (1 µs
//! rise, then constant) and solved by the Numerical Laplace Transform (a
//! steady final value is its native case). At two heights, `0.2 L` and
//! `0.8 L` of the calibration length `L = min(channel length, 2.5 km)`, the
//! front of the segment current is characterised by the point where the
//! tangent through its 10 % and 90 % levels crosses the time axis; the speed
//! is the height difference over the difference of those times. The target of
//! a channel with a speed profile is the harmonic-mean speed between the two
//! heights.

use crate::ctes::C;
use crate::electrode::Loading;
use crate::element::{Channel, Element};
use crate::error::{Result, TupaError};
use crate::material::{Linear, Medium};
use crate::mesh::ImageModel;
use crate::signal::new_portela_signal;
use crate::structure::Structure;
use crate::study::Study;
use crate::transient::{
    Transform, TransientOptions, TransientSource, TransientSpec, transient_response,
};
use crate::verbosity::{VERB_NORMAL, VERB_VERBOSE, verbose, verbosity_level};

/// Rise time of the calibration current ramp (s).
const RISE_TIME: f64 = 1.0e-6;
/// Longest channel part simulated for the calibration (m).
const MAX_CAL_LENGTH: f64 = 2500.0;
const N_SAMPLES: usize = 512;
const MAX_ITER: usize = 20;
/// Relative speed tolerance at which the iteration stops.
const TOLERANCE: f64 = 2.0e-3;
/// Relative speed error up to which the best evaluation is accepted when the
/// iteration does not reach `TOLERANCE`: the metric has a jitter of about
/// 0.5 % (one time sample, and the ripple of the NLT synthesis).
const ACCEPT: f64 = 6.0e-3;
/// Credible range of the calibrated scale: outside it the speed metric has
/// locked onto something other than the wave (segments too long for the
/// radius and rise time), and the loading would be meaningless.
const KAPPA_RANGE: std::ops::RangeInclusive<f64> = 0.25..=4.0;

/// Time where the 10–90 % tangent of the first front of the series `i(t)`
/// crosses the time axis (Baba & Rakov's wave-front tracking, theory.md
/// §4.5). The front's peak is searched for in a window that ends
/// `2·t_rise + 0.2·t_direct` after the start, but never later than midway
/// between the expected direct arrival `t_direct` and the expected arrival of
/// the wave reflected from the top `t_reflected`: a longer window would let
/// the slow creep of the plateau and the late-record noise of the NLT
/// (amplified as `e^{ct}`) move the peak, and with it the 90 % level. The
/// 90 % point is the first rise through its level, the 10 % point the last
/// rise through its level before that (robust to a band-limit precursor
/// ripple). `t_rise` is the rise time of the excitation ramp. Returns 0 if
/// the front is not found.
pub fn front_onset_time(t: &[f64], i: &[f64], t_direct: f64, t_reflected: f64, t_rise: f64) -> f64 {
    let t_cut = (0.5 * (t_direct + t_reflected.max(t_direct)))
        .min(t_direct + 2.0 * t_rise + 0.2 * t_direct);
    let mut peak = f64::NEG_INFINITY;
    let mut k_peak = 0;
    for k in 0..t.len() {
        if t[k] > t_cut {
            break;
        }
        if i[k] > peak {
            peak = i[k];
            k_peak = k + 1; // 1-based like the Fortran reference
        }
    }
    if k_peak < 2 || peak <= 0.0 {
        return 0.0;
    }
    // 90 %: the first rise through the level; 10 %: the last rise through its
    // level *before* that, so a precursor ripple ahead of the front (band
    // limit) cannot be taken for the start of the front
    let rise_at = |m: usize, level: f64| i[m - 2] < level && i[m - 1] >= level;
    let interp = |m: usize, level: f64| {
        t[m - 2] + (level - i[m - 2]) / (i[m - 1] - i[m - 2]) * (t[m - 1] - t[m - 2])
    };
    let Some(m90) = (2..=k_peak).find(|&m| rise_at(m, 0.9 * peak)) else {
        return 0.0;
    };
    let t90 = interp(m90, 0.9 * peak);
    let Some(m10) = (2..=m90).rev().find(|&m| rise_at(m, 0.1 * peak)) else {
        return 0.0;
    };
    let t10 = interp(m10, 0.1 * peak);
    if t90 <= t10 {
        return 0.0;
    }
    t10 - (t90 - t10) / 8.0
}

/// 0-based index of the segment whose midpoint is nearest to `s`.
fn segment_at(brk: &[f64], s: f64) -> usize {
    let mut best = f64::MAX;
    let mut j = 0;
    for k in 0..brk.len() - 1 {
        let d = (0.5 * (brk[k] + brk[k + 1]) - s).abs();
        if d < best {
            best = d;
            j = k;
        }
    }
    j
}

/// Speed the calibration aims at between heights `s1` and `s2`: the uniform
/// target, or for a profile the harmonic mean `(s2 − s1) / ∫ ds / v(s)`.
fn target_speed(ch: &Channel, s1: f64, s2: f64) -> f64 {
    const NSUB: usize = 2000;
    let ds = (s2 - s1) / NSUB as f64;
    let mut t_travel = 0.0;
    for k in 1..=NSUB {
        let sm = s1 + (k as f64 - 0.5) * ds;
        t_travel += ds / ch.speed.eval(sm, C);
    }
    (s2 - s1) / t_travel
}

/// Find the loading scale `ch.load_scale` that gives the channel its target
/// speed; sets `calibrated` and `calibrated_speed`.
pub fn calibrate_channel(ch: &mut Channel) -> Result<()> {
    if ch.speed.is_unset() {
        return Err(TupaError::new(format!(
            "tChannel '{}': calibrate needs a target speed",
            ch.id
        )));
    }
    // Calibration channel: the first part of the channel, vertical, on its own foot
    let n = ch.breaks.len() - 1;
    let mut n_seg = n;
    for j in 1..=n {
        if ch.breaks[j] >= MAX_CAL_LENGTH * (1.0 - 1.0e-9) {
            n_seg = j;
            break;
        }
    }
    let brk: Vec<f64> = ch.breaks[..=n_seg].to_vec();
    let l_cal = brk[n_seg];
    let j1 = segment_at(&brk, 0.2 * l_cal);
    let j2 = segment_at(&brk, 0.8 * l_cal);
    if j1 >= j2 {
        return Err(TupaError::new(format!(
            "tChannel '{}': too few segments to calibrate the speed",
            ch.id
        )));
    }
    // Heights actually observed: the segment midpoints
    let s1 = 0.5 * (brk[j1] + brk[j1 + 1]);
    let s2 = 0.5 * (brk[j2] + brk[j2 + 1]);
    let v_target = target_speed(ch, s1, s2);

    let mut cal = Channel::new("cal", "", l_cal, ch.radius, brk.clone());
    cal.speed = ch.speed.clone();

    let mut structure = Structure::new(Medium::Linear(Linear::new("soil", 10.0, 1.0, 0.01)));
    structure.add_element(Element::Channel(cal.clone()));
    let mut mini = Study::new("channel calibration", structure);
    mini.image_model = Some(ImageModel::Ideal);
    mini.structure.assemble()?;

    let v_min = ch.speed.value.iter().copied().fold(f64::MAX, f64::min);
    let t_rec = 1.5 * l_cal / v_min;
    let nyq = N_SAMPLES as f64 / (2.0 * t_rec);

    // Speed of the calibration channel at loading scale `scale`; the geometry
    // factors are reused, only the loading of the segments changes.
    let mut measure = |scale: f64| -> Result<f64> {
        cal.load_scale = scale;
        for k in 0..n_seg {
            let (r, l) = cal.segment_loading(k, 0.0)?;
            mini.structure.electrodes[k].loading = Some(Loading {
                resistance: r,
                inductance: l,
            });
        }
        let spec = TransientSpec {
            sources: vec![TransientSource::current(
                "cal-base",
                new_portela_signal(1.0, 0.0, RISE_TIME, 1.0e3, 2.0e3)?,
            )],
            observe_nodes: vec!["cal-base".into()],
            observe_electrodes: vec![format!("cal_e{}", j1 + 1), format!("cal_e{}", j2 + 1)],
            nyquist_hz: nyq,
            fft_points: N_SAMPLES,
            freq_zero_hz: 1.0e-6,
            options: TransientOptions {
                transform: Transform::Nlt,
                ..TransientOptions::default()
            },
        };
        let res = transient_response(&mut mini, &spec)?;
        let t1 = front_onset_time(
            &res.t,
            &res.i1_responses[0],
            s1 / v_target,
            (2.0 * l_cal - s1) / v_target,
            RISE_TIME,
        );
        let t2 = front_onset_time(
            &res.t,
            &res.i1_responses[1],
            s2 / v_target,
            (2.0 * l_cal - s2) / v_target,
            RISE_TIME,
        );
        Ok(if t2 <= t1 { 0.0 } else { (s2 - s1) / (t2 - t1) })
    };

    // Secant iteration on h(κ) = (c/v(κ))² − (c/v_t)², nearly linear in κ
    let h_tgt = (C / v_target).powi(2);
    let mut kappa = 1.0;
    let mut k_prev = 0.0;
    let mut h_prev = 0.0;
    let mut v_meas;
    let (mut best_err, mut best_k, mut best_v) = (f64::MAX, kappa, 0.0);
    for iter in 1..=MAX_ITER {
        v_meas = measure(kappa)?;
        if v_meas <= 0.0 {
            return Err(TupaError::new(format!(
                "tChannel '{}': the calibration could not measure a speed",
                ch.id
            )));
        }
        if verbosity_level() == VERB_VERBOSE {
            println!("   kappa = {kappa:10.5}  v = {v_meas:12.5E}  target = {v_target:12.5E}");
        }
        let err = (v_meas - v_target).abs() / v_target;
        if err < best_err {
            best_err = err;
            best_k = kappa;
            best_v = v_meas;
        }
        if err <= TOLERANCE {
            break;
        }
        let h_cur = (C / v_meas).powi(2) - h_tgt;
        if iter == 1 {
            // First step: assume (c/v)² − 1 proportional to κ
            k_prev = kappa;
            h_prev = h_cur;
            kappa *= (h_tgt - 1.0) / ((C / v_meas).powi(2) - 1.0).max(1.0e-6);
        } else {
            let slope = (h_cur - h_prev) / (kappa - k_prev);
            k_prev = kappa;
            h_prev = h_cur;
            if slope == 0.0 {
                break;
            }
            kappa -= h_cur / slope;
        }
        if kappa <= 0.0 {
            kappa = 0.5 * k_prev;
        }
    }

    if best_err > ACCEPT {
        return Err(TupaError::new(format!(
            "tChannel '{}': the loading calibration did not reach the target speed (measured speed \
             does not respond to the loading — target above the unloaded channel's speed?)",
            ch.id
        )));
    }
    if !KAPPA_RANGE.contains(&best_k) {
        return Err(TupaError::new(format!(
            "tChannel '{}': the calibrated loading scale is implausible (outside 0.25-4): the speed \
             metric is not reliable for this radius and segmentation — use shorter segments",
            ch.id
        )));
    }
    ch.load_scale = best_k;
    ch.calibrated = true;
    ch.want_calibration = false;
    ch.calibrated_speed = best_v;
    verbose(VERB_NORMAL, &format!(" Channel '{}' calibrated", ch.id));
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn front_tracking_on_a_ramp() {
        let t: Vec<f64> = (0..400).map(|k| k as f64 * 5.0e-8).collect();
        let mut i: Vec<f64> = t
            .iter()
            .map(|&x| ((x - 2.0e-6) / 1.0e-6).clamp(0.0, 1.0))
            .collect();
        assert!((front_onset_time(&t, &i, 2.0e-6, 9.0e-6, 1.0e-6) - 2.0e-6).abs() < 1e-12);
        for (k, v) in i.iter_mut().enumerate() {
            if t[k] > 10.0e-6 {
                *v = 5.0;
            }
        }
        assert!((front_onset_time(&t, &i, 2.0e-6, 9.0e-6, 1.0e-6) - 2.0e-6).abs() < 1e-12);
    }
}
