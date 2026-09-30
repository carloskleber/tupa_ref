//! Catenary element (`mElementCatenary`, ADR 0023): a sagging span between
//! two nodes, discretised into straight segments like [`Line`] but with the
//! chain nodes on the legacy parabolic profile (theory.md §4.4).

use super::line::{Line, profile_point};
use crate::error::{Result, TupaError};
use crate::structure::Structure;

/// A parabolic span: the [`Line`] fields plus the midspan sag.
#[derive(Debug, Clone, PartialEq)]
pub struct Catenary {
    /// Endpoints, radius, segment count and material
    pub line: Line,
    /// Downward displacement at midspan (m); negative bows upward
    pub sag: f64,
}

impl Catenary {
    /// Reject a span that leaves its end nodes' half-space, then add its
    /// nodes and electrodes to `structure`.
    pub fn assemble(&self, structure: &mut Structure) -> Result<()> {
        let l = &self.line;
        if let (Some(a), Some(b)) = (
            structure.find_node_index(&l.id_node_start),
            structure.find_node_index(&l.id_node_end),
        ) {
            let (pa, pb) = (structure.nodes[a].p, structure.nodes[b].p);
            let crosses = (1..l.n_electrodes).any(|k| {
                let z = profile_point(pa, pb, k, l.n_electrodes, self.sag)[2];
                (pa[2].min(pb[2]) >= 0.0 && z < 0.0) || (pa[2].max(pb[2]) <= 0.0 && z > 0.0)
            });
            if crosses {
                return Err(TupaError::new(format!(
                    "tCatenary '{}': the sag profile crosses the air-soil interface (z = 0)",
                    l.id
                )));
            }
        }
        l.assemble_sagging(structure, self.sag).map(|_| ())
    }

    /// Human-readable description.
    pub fn report(&self) -> String {
        format!("Catenary, sag {:.3} m\n{}", self.sag, self.line.report())
    }
}
