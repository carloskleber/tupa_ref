//! Cylindrical conductor segment between two nodes (`mElectrode`).

use crate::material::Linear;

/// One discretised conductor segment.
#[derive(Debug, Clone, PartialEq)]
pub struct Electrode {
    /// Identifier, `<element id>_e<k>` for line-generated segments
    pub id: String,
    /// 0-based indices of the end nodes in `Structure::nodes`
    pub node_indices: [usize; 2],
    /// Cylinder radius (m)
    pub radius: f64,
    /// Conductor material (copy of the owning element's material)
    pub material: Linear,
}
