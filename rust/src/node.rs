//! Geometric point of the conductor network (`mNode`).

/// Discretisation node: identifier + position (m). Voltages live in the
/// result store, not here.
#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    /// Unique string identifier within a structure
    pub id: String,
    /// Position `[x, y, z]` in metres (z up, air–soil interface at z = 0)
    pub p: [f64; 3],
}

impl Node {
    /// Construct a node.
    pub fn new(id: impl Into<String>, p: [f64; 3]) -> Self {
        Self { id: id.into(), p }
    }
}
