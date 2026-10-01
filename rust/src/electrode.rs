//! Cylindrical conductor segment between two nodes (`mElectrode`).

use crate::material::Linear;

/// Series loading per unit length of a lightning-channel segment, standing
/// in for its internal impedance (theory.md §4.5): `z_ch = R' + jωL'`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Loading {
    /// Resistance per unit length `R'` (Ω/m)
    pub resistance: f64,
    /// Inductance per unit length `L'` (H/m)
    pub inductance: f64,
}

/// One discretised conductor segment.
#[derive(Debug, Clone, PartialEq)]
pub struct Electrode {
    /// Identifier, `<element id>_e<k>` for line-generated segments
    pub id: String,
    /// 0-based indices of the end nodes in `Structure::nodes`
    pub node_indices: [usize; 2],
    /// Cylinder radius (m)
    pub radius: f64,
    /// Conductor material (copy of the owning element's material); a
    /// placeholder for a loaded segment, whose `loading` replaces the
    /// skin-effect impedance
    pub material: Linear,
    /// Lightning-channel loading (ROADMAP Phase 10b); `None` for conductors
    pub loading: Option<Loading>,
}
