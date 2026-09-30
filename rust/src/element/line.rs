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
        let nf = n as f64;
        let inc = [
            (p_end[0] - p_start[0]) / nf,
            (p_end[1] - p_start[1]) / nf,
            (p_end[2] - p_start[2]) / nf,
        ];

        let mut node_idx = Vec::with_capacity(n + 1);
        node_idx.push(idx_start);
        for k in 1..n {
            let kf = k as f64;
            let p = [
                p_start[0] + kf * inc[0],
                p_start[1] + kf * inc[1],
                p_start[2] + kf * inc[2],
            ];
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
