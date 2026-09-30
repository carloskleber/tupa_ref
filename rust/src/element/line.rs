//! Straight line element (`mElementLine`): discretised into `segments`
//! equal electrodes joined by internal nodes.

use crate::electrode::Electrode;
use crate::error::{Result, TupaError};
use crate::node::Node;
use crate::structure::Structure;

/// A straight conductor between two named nodes.
#[derive(Debug, Clone, PartialEq)]
pub struct Line {
    /// Element id (prefix of generated `_n<k>` nodes and `_e<k>` electrodes)
    pub id: String,
    /// Start node id
    pub id_node_start: String,
    /// End node id
    pub id_node_end: String,
    /// Conductor radius (m)
    pub radius: f64,
    /// Number of electrodes (segments)
    pub n_electrodes: usize,
    /// Conductor material id
    pub id_material: String,
}

impl Line {
    /// Construct a line element.
    pub fn new(
        id: impl Into<String>,
        id_node_start: impl Into<String>,
        id_node_end: impl Into<String>,
        radius: f64,
        n_electrodes: usize,
        id_material: impl Into<String>,
    ) -> Self {
        Self {
            id: id.into(),
            id_node_start: id_node_start.into(),
            id_node_end: id_node_end.into(),
            radius,
            n_electrodes,
            id_material: id_material.into(),
        }
    }

    /// Add `n-1` internal nodes and `n` electrodes to `structure`; returns
    /// the number of internal nodes created.
    pub fn assemble(&self, structure: &mut Structure) -> Result<usize> {
        self.assemble_sagging(structure, 0.0)
    }

    /// `assemble` with the chain nodes on the parabolic profile of
    /// [`profile_point`] (`sag = 0` is the straight line); shared with
    /// [`super::Catenary`].
    pub(crate) fn assemble_sagging(&self, structure: &mut Structure, sag: f64) -> Result<usize> {
        let idx_start = structure
            .find_node_index(&self.id_node_start)
            .ok_or_else(|| {
                TupaError::new(format!(
                    "tLine '{}': start node '{}' not found",
                    self.id, self.id_node_start
                ))
            })?;
        let idx_end = structure
            .find_node_index(&self.id_node_end)
            .ok_or_else(|| {
                TupaError::new(format!(
                    "tLine '{}': end node '{}' not found",
                    self.id, self.id_node_end
                ))
            })?;
        let material = structure
            .find_material(&self.id_material)
            .cloned()
            .ok_or_else(|| {
                TupaError::new(format!(
                    "tLine '{}': material '{}' not found",
                    self.id, self.id_material
                ))
            })?;
        if self.n_electrodes < 1 {
            return Err(TupaError::new(format!(
                "tLine '{}': segments must be >= 1",
                self.id
            )));
        }

        let n = self.n_electrodes;
        let p_start = structure.nodes[idx_start].p;
        let p_end = structure.nodes[idx_end].p;

        let mut node_idx = Vec::with_capacity(n + 1);
        node_idx.push(idx_start);
        for k in 1..n {
            let p = profile_point(p_start, p_end, k, n, sag);
            node_idx.push(structure.add_node(Node::new(format!("{}_n{}", self.id, k), p)));
        }
        node_idx.push(idx_end);

        for k in 1..=n {
            structure.add_electrode(Electrode {
                id: format!("{}_e{}", self.id, k),
                node_indices: [node_idx[k - 1], node_idx[k]],
                radius: self.radius,
                material: material.clone(),
            });
        }
        Ok(n - 1)
    }

    /// Human-readable description.
    pub fn report(&self) -> String {
        format!(
            "Element ID: {}, Material: {}, Nodes: {}, Radius: {:.3} m\n",
            self.id,
            self.id_material,
            self.n_electrodes + 1,
            self.radius
        )
    }
}

/// Chain node `k` of `n` between `p_start` and `p_end`: `k` equal chord steps,
/// lowered by `4·sag·s·(1 − s)` at `s = k/n` (theory.md §4.4; `sag = 0` is the
/// straight line).
pub(crate) fn profile_point(
    p_start: [f64; 3],
    p_end: [f64; 3],
    k: usize,
    n: usize,
    sag: f64,
) -> [f64; 3] {
    let kf = k as f64;
    let nf = n as f64;
    let s = kf / nf;
    [
        p_start[0] + kf * ((p_end[0] - p_start[0]) / nf),
        p_start[1] + kf * ((p_end[1] - p_start[1]) / nf),
        p_start[2] + kf * ((p_end[2] - p_start[2]) / nf) - 4.0 * sag * s * (1.0 - s),
    ]
}
