//! Lightning return-stroke channel in air (`mElementChannel`, ROADMAP Phase
//! 10b item 1, theory.md §4.5, ADR 0025): a chain of straight segments rising
//! from a strike point, loaded with a distributed series impedance
//! `z_ch = R' + jωL'` that slows the current wave to a prescribed
//! return-stroke speed.
//!
//! The channel is an ordinary wire in the model; the loading sits in the
//! internal-impedance slot of the self impedance ([`Loading`] on the
//! [`Electrode`]). The wire itself is a perfect conductor — the channel's own
//! resistance is `R'`.
//!
//! Nodes: the base node `<id>-base` coincides with the strike node but is a
//! separate node (the source of a strike is a two-node source between them),
//! intermediate nodes are `<id>_n<k>`, the top node is `<id>-top`; segments
//! are `<id>_e<k>`, numbered upward from the base. A free-standing channel
//! (no strike node, e.g. above ideal ground) has only its base node, at its
//! foot position. The strike node must be attached to something: an isolated
//! node makes the nodal system singular.

use crate::ctes::{C, MU0, PI};
use crate::electrode::{Electrode, Loading};
use crate::error::{Result, TupaError};
use crate::material::Linear;
use crate::node::Node;
use crate::structure::Structure;

/// Piecewise-constant profile along the channel: `value[i]` applies up to and
/// including `up_to[i]` (the previous break excluded); the last value
/// continues beyond the last break. Empty = unset.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Piecewise {
    /// Upper break of each piece (m along the axis)
    pub up_to: Vec<f64>,
    /// Value of each piece
    pub value: Vec<f64>,
}

impl Piecewise {
    /// One value for the whole channel.
    pub fn uniform(v: f64) -> Self {
        Self {
            up_to: vec![f64::MAX],
            value: vec![v],
        }
    }

    /// `true` when no value was given.
    pub fn is_unset(&self) -> bool {
        self.value.is_empty()
    }

    /// Value at distance `s` along the axis; `default` where unset.
    pub fn eval(&self, s: f64, default: f64) -> f64 {
        let Some(&last) = self.value.last() else {
            return default;
        };
        self.up_to
            .iter()
            .position(|&u| s <= u)
            .map_or(last, |i| self.value[i])
    }
}

/// Break distances of a channel graded from its foot: segment `k` has length
/// `min(first · growth^(k-1), max_segment)` until the length is covered, then
/// every segment is scaled by the same factor so the chain ends exactly at
/// `length`. The scaling keeps every ratio between adjacent segments, so it
/// never exceeds `growth` (theory.md §4.5: bounded adjacent-length ratio);
/// `max_segment` and `first` can only shrink by it. `growth = 1` gives a
/// uniform chain of `⌈length / max_segment⌉` segments.
pub fn graded_breaks(length: f64, first: f64, growth: f64, max_segment: f64) -> Vec<f64> {
    let mut seg = Vec::new();
    let mut total = 0.0;
    let mut l = first.min(max_segment);
    while total < length * (1.0 - 1.0e-12) && seg.len() <= 1_000_000 {
        seg.push(l);
        total += l;
        l = (l * growth).min(max_segment);
    }
    let scale = length / total;
    let mut breaks = Vec::with_capacity(seg.len() + 1);
    breaks.push(0.0);
    for s in &seg {
        breaks.push(breaks[breaks.len() - 1] + s * scale);
    }
    let n = breaks.len();
    breaks[n - 1] = length;
    breaks
}

/// Closed-form series inductance per unit length (H/m) that slows a wire of
/// the given radius at height `z` to `speed`: `L' = L0 (c²/v² − 1)`,
/// `L0 = μ0/(2π) ln(2z/r0)` (theory.md §4.5). A starting value for a
/// calibration, not the final loading.
pub fn channel_loading_estimate(z: f64, radius: f64, speed: f64) -> f64 {
    let l0 = MU0 / (2.0 * PI) * (2.0 * z / radius).ln();
    l0 * (C.powi(2) / speed.powi(2) - 1.0)
}

/// Unit vector of the channel axis (pointing up for zero incidence).
pub fn channel_unit_vector(incidence_deg: f64, azimuth_deg: f64) -> [f64; 3] {
    let th = incidence_deg * PI / 180.0;
    let ph = azimuth_deg * PI / 180.0;
    [th.sin() * ph.cos(), th.sin() * ph.sin(), th.cos()]
}

/// A lightning channel above a strike node.
#[derive(Debug, Clone, PartialEq)]
pub struct Channel {
    /// Element id (prefix of the generated nodes and segments)
    pub id: String,
    /// Strike node id; empty = free-standing at `foot`
    pub id_strike: String,
    /// Foot of a free-standing channel (m)
    pub foot: [f64; 3],
    /// Channel length along its axis (m)
    pub length: f64,
    /// Angle of the axis from the vertical (degrees)
    pub incidence_deg: f64,
    /// Azimuth of the axis in the xy plane, from +x (degrees)
    pub azimuth_deg: f64,
    /// Wire radius (m)
    pub radius: f64,
    /// Distances of the chain nodes along the axis, `0 … length` (m)
    pub breaks: Vec<f64>,
    /// Target return-stroke speed `v(s)` (m/s); unset = unloaded unless
    /// `inductance` is given
    pub speed: Piecewise,
    /// Explicit uniform series inductance `L'` (H/m)
    pub inductance: Option<f64>,
    /// Series resistance `R'(s)` (Ω/m); unset = 0
    pub resistance: Piecewise,
    /// Factor on the closed-form `L'(z)` of a speed-loaded channel; 1 until
    /// calibrated
    pub load_scale: f64,
    /// Set by the loader (`"calibrate": true`); cleared once calibrated
    pub want_calibration: bool,
    /// `load_scale` came from a calibration
    pub calibrated: bool,
    /// Speed measured on the channel alone with the final loading (m/s)
    pub calibrated_speed: f64,
}

impl Channel {
    /// New channel with the given break distances; loading is set on the
    /// returned value.
    pub fn new(
        id: impl Into<String>,
        id_strike: impl Into<String>,
        length: f64,
        radius: f64,
        breaks: Vec<f64>,
    ) -> Self {
        Self {
            id: id.into(),
            id_strike: id_strike.into(),
            foot: [0.0; 3],
            length,
            incidence_deg: 0.0,
            azimuth_deg: 0.0,
            radius,
            breaks,
            speed: Piecewise::default(),
            inductance: None,
            resistance: Piecewise::default(),
            load_scale: 1.0,
            want_calibration: false,
            calibrated: false,
            calibrated_speed: 0.0,
        }
    }

    /// Number of segments.
    pub fn n_segments(&self) -> usize {
        self.breaks.len() - 1
    }

    /// Series loading `(R', L')` of segment `k` (0-based from the base), with
    /// the strike node at height `z_strike`. Errors when the segment is too
    /// low for the closed-form estimate (`2z ≤ r0`) or the target speed
    /// exceeds `c`.
    pub fn segment_loading(&self, k: usize, z_strike: f64) -> Result<(f64, f64)> {
        let s_mid = 0.5 * (self.breaks[k] + self.breaks[k + 1]);
        let r_prime = self.resistance.eval(s_mid, 0.0);
        let c = C;
        let l_prime = if let Some(l) = self.inductance {
            l
        } else if !self.speed.is_unset() {
            let v = self.speed.eval(s_mid, c);
            if v <= 0.0 || v > c * (1.0 + 1.0e-12) {
                return Err(TupaError::new(format!(
                    "tChannel '{}': the target speed must be in (0, c]",
                    self.id
                )));
            }
            let z = z_strike + s_mid * (self.incidence_deg * PI / 180.0).cos();
            if 2.0 * z <= self.radius {
                return Err(TupaError::new(format!(
                    "tChannel '{}': a segment is too close to the ground for the speed-loading \
                     formula (2 z <= radius)",
                    self.id
                )));
            }
            self.load_scale * channel_loading_estimate(z, self.radius, v.min(c))
        } else {
            0.0
        };
        Ok((r_prime, l_prime))
    }

    /// Plant the base, intermediate and top nodes and the loaded segments.
    pub fn assemble(&self, structure: &mut Structure) -> Result<()> {
        let p_strike = if self.id_strike.is_empty() {
            self.foot
        } else {
            let idx = structure.find_node_index(&self.id_strike).ok_or_else(|| {
                TupaError::new(format!(
                    "tChannel '{}': strike node '{}' not found",
                    self.id, self.id_strike
                ))
            })?;
            structure.nodes[idx].p
        };
        let u = channel_unit_vector(self.incidence_deg, self.azimuth_deg);
        let n = self.n_segments();
        if u[2] <= 1.0e-12 {
            return Err(TupaError::new(format!(
                "tChannel '{}': the channel must rise (incidence < 90 degrees)",
                self.id
            )));
        }
        if p_strike[2] < 0.0 {
            return Err(TupaError::new(format!(
                "tChannel '{}': the strike node must be in air (z >= 0)",
                self.id
            )));
        }

        let mut node_idx = Vec::with_capacity(n + 1);
        for k in 0..=n {
            let id = if k == 0 {
                format!("{}-base", self.id)
            } else if k == n {
                format!("{}-top", self.id)
            } else {
                format!("{}_n{}", self.id, k)
            };
            let b = self.breaks[k];
            node_idx.push(structure.add_node(Node::new(
                id,
                [
                    p_strike[0] + b * u[0],
                    p_strike[1] + b * u[1],
                    p_strike[2] + b * u[2],
                ],
            )));
        }
        for k in 0..n {
            let (r, l) = self.segment_loading(k, p_strike[2])?;
            structure.add_electrode(Electrode {
                id: format!("{}_e{}", self.id, k + 1),
                node_indices: [node_idx[k], node_idx[k + 1]],
                radius: self.radius,
                material: Linear::new("channel", 1.0, 1.0, 0.0),
                loading: Some(Loading {
                    resistance: r,
                    inductance: l,
                }),
            });
        }
        Ok(())
    }

    /// Human-readable description.
    pub fn report(&self) -> String {
        let mut s = format!(
            "Element ID: {}, Lightning channel from node {}, length {:.3} m, segments {}, \
             radius {:.4} m\n",
            self.id,
            self.id_strike,
            self.length,
            self.n_segments(),
            self.radius
        );
        if let Some(l) = self.inductance {
            s.push_str(&format!("  Series loading L' = {l:.4E} H/m\n"));
        } else if !self.speed.is_unset() {
            s.push_str(&format!(
                "  Speed-loaded (target {:.4E} m/s)",
                self.speed.value[0]
            ));
            if self.calibrated {
                s.push_str(&format!(", calibrated scale {:.4}", self.load_scale));
            }
            s.push('\n');
        }
        s
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn grading_respects_ratio_and_endpoints() {
        let b = graded_breaks(3000.0, 5.0, 1.15, 20.0);
        let n = b.len() - 1;
        assert_eq!(b[0], 0.0);
        assert!((b[n] - 3000.0).abs() < 1e-9);
        let seg: Vec<f64> = b.windows(2).map(|w| w[1] - w[0]).collect();
        assert!(seg.windows(2).all(|w| w[1] / w[0] <= 1.15 + 1e-12));
        assert!(seg.iter().all(|&l| l <= 20.0 + 1e-12));
        assert!(seg[0] <= 5.0 + 1e-12);
        let u = graded_breaks(95.0, 10.0, 1.0, 10.0);
        assert_eq!(u.len() - 1, 10);
        assert!((u[1] - 9.5).abs() < 1e-12);
    }

    #[test]
    fn loading_estimate_matches_theory_numbers() {
        let c = C;
        assert!((channel_loading_estimate(500.0, 0.03, c / 2.0) - 6.2e-6).abs() < 0.1e-6);
        assert!((channel_loading_estimate(500.0, 0.03, c / 3.0) - 16.7e-6).abs() < 0.3e-6);
        assert!(channel_loading_estimate(500.0, 0.03, c).abs() < 1e-12);
    }

    #[test]
    fn piecewise_evaluation() {
        let p = Piecewise {
            up_to: vec![500.0, 1500.0],
            value: vec![3.0, 1.0],
        };
        assert_eq!(p.eval(500.0, 0.0), 3.0);
        assert_eq!(p.eval(501.0, 0.0), 1.0);
        assert_eq!(p.eval(9.0e3, 0.0), 1.0);
        assert_eq!(Piecewise::default().eval(1.0, 7.0), 7.0);
    }
}
