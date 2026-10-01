//! Geometric elements that discretise into nodes and electrodes
//! (`mElement`): straight `line`, composite rectangular `mesh` (ADR 0020) and
//! sagging `catenary` (ADR 0023) and the lightning `channel` (ADR 0025).

pub mod catenary;
pub mod channel;
pub mod line;
pub mod mesh;

use crate::error::Result;
use crate::structure::Structure;
pub use catenary::Catenary;
pub use channel::Channel;
pub use line::Line;
pub use mesh::MeshElement;

/// A geometric element of the structure.
#[derive(Debug, Clone, PartialEq)]
pub enum Element {
    /// Straight conductor between two nodes
    Line(Line),
    /// Rectangular axis-aligned grounding grid
    Mesh(MeshElement),
    /// Parabolic sagging span between two nodes
    Catenary(Catenary),
    /// Loaded lightning channel in air
    Channel(Channel),
}

impl Element {
    /// Element identifier.
    pub fn id(&self) -> &str {
        match self {
            Element::Line(l) => &l.id,
            Element::Mesh(m) => &m.id,
            Element::Catenary(c) => &c.line.id,
            Element::Channel(c) => &c.id,
        }
    }

    /// Resolve references and append this element's nodes and electrodes to
    /// `structure`.
    pub fn assemble(&self, structure: &mut Structure) -> Result<()> {
        match self {
            Element::Line(l) => l.assemble(structure).map(|_| ()),
            Element::Mesh(m) => m.assemble(structure),
            Element::Catenary(c) => c.assemble(structure),
            Element::Channel(c) => c.assemble(structure),
        }
    }

    /// One- or two-line human-readable description (appended to the study
    /// report).
    pub fn report(&self) -> String {
        match self {
            Element::Line(l) => l.report(),
            Element::Mesh(m) => m.report(),
            Element::Catenary(c) => c.report(),
            Element::Channel(c) => c.report(),
        }
    }
}
