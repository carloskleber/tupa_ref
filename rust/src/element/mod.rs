//! Geometric elements that discretise into nodes and electrodes
//! (`mElement`): straight `line` and composite rectangular `mesh` (ADR 0020).

pub mod line;
pub mod mesh;

use crate::error::Result;
use crate::structure::Structure;
pub use line::Line;
pub use mesh::MeshElement;

/// A geometric element of the structure.
#[derive(Debug, Clone, PartialEq)]
pub enum Element {
    /// Straight conductor between two nodes
    Line(Line),
    /// Rectangular axis-aligned grounding grid
    Mesh(MeshElement),
}

impl Element {
    /// Element identifier.
    pub fn id(&self) -> &str {
        match self {
            Element::Line(l) => &l.id,
            Element::Mesh(m) => &m.id,
        }
    }

    /// Resolve references and append this element's nodes and electrodes to
    /// `structure`.
    pub fn assemble(&self, structure: &mut Structure) -> Result<()> {
        match self {
            Element::Line(l) => l.assemble(structure).map(|_| ()),
            Element::Mesh(m) => m.assemble(structure),
        }
    }

    /// One- or two-line human-readable description (appended to the study
    /// report).
    pub fn report(&self) -> String {
        match self {
            Element::Line(l) => l.report(),
            Element::Mesh(m) => m.report(),
        }
    }
}
