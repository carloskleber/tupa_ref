//! Container for nodes, elements, materials, electrodes and the soil medium
//! (`mStructure`). Elements are kept in JSON declaration order; later
//! elements may reference nodes created by earlier ones (e.g. a `line`
//! attached to a `mesh`'s main nodes), so `assemble` visits them in order.

use crate::electrode::Electrode;
use crate::element::Element;
use crate::error::Result;
use crate::material::{Linear, Medium};
use crate::node::Node;

/// Geometric structure of a study.
#[derive(Debug, Clone)]
pub struct Structure {
    /// Soil half-space (z < 0)
    pub soil: Medium,
    /// Air half-space (z > 0): hardcoded vacuum, ADR 0019
    pub air: Linear,
    /// Boundary and internal nodes
    pub nodes: Vec<Node>,
    /// Discretised segments (populated by `assemble`)
    pub electrodes: Vec<Electrode>,
    /// Elements awaiting assembly, declaration order
    pub elements: Vec<Element>,
    /// Conductor materials in declaration order
    pub materials: Vec<Linear>,
    assembled: bool,
}

impl Structure {
    /// Empty structure over the given soil.
    pub fn new(soil: Medium) -> Self {
        Self {
            soil,
            air: Linear::new("air", 1.0, 1.0, 0.0),
            nodes: Vec::new(),
            electrodes: Vec::new(),
            elements: Vec::new(),
            materials: Vec::new(),
            assembled: false,
        }
    }

    /// Append a node; returns its 0-based index.
    pub fn add_node(&mut self, node: Node) -> usize {
        self.nodes.push(node);
        self.nodes.len() - 1
    }

    /// Append an electrode.
    pub fn add_electrode(&mut self, e: Electrode) {
        self.electrodes.push(e);
    }

    /// Append an element (assembled later, in this order).
    pub fn add_element(&mut self, e: Element) {
        self.elements.push(e);
    }

    /// Append a conductor material.
    pub fn add_material(&mut self, m: Linear) {
        self.materials.push(m);
    }

    /// 0-based index of the node with the given id.
    pub fn find_node_index(&self, id: &str) -> Option<usize> {
        self.nodes.iter().position(|n| n.id == id)
    }

    /// 0-based index of the electrode with the given id.
    pub fn find_electrode_index(&self, id: &str) -> Option<usize> {
        self.electrodes.iter().position(|e| e.id == id)
    }

    /// Material by id; the most recently added of a duplicated id wins
    /// (the Fortran list prepends).
    pub fn find_material(&self, id: &str) -> Option<&Linear> {
        self.materials.iter().rev().find(|m| m.id == id)
    }

    /// Whether `assemble` has run.
    pub fn is_assembled(&self) -> bool {
        self.assembled
    }

    /// Discretise every element into nodes and electrodes (idempotent).
    pub fn assemble(&mut self) -> Result<()> {
        if self.assembled {
            return Ok(());
        }
        let elements = std::mem::take(&mut self.elements);
        let mut result = Ok(());
        for e in &elements {
            if let Err(err) = e.assemble(self) {
                result = Err(err);
                break;
            }
        }
        self.elements = elements;
        result?;
        self.assembled = true;
        Ok(())
    }
}
