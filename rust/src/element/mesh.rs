//! Composite rectangular grounding-grid element (`mElementMesh`, ADR 0020).
//!
//! `rows_x` bars parallel to X (each `length_x` long, spaced along Y) and
//! `rows_y` bars parallel to Y, wired between a regular grid of main nodes
//! named `<mesh id>-<row:02><col:02>`. Each bar is a `Line` with `segments`
//! electrodes.

use super::line::Line;
use crate::error::{Result, TupaError};
use crate::node::Node;
use crate::structure::Structure;

/// Maximum rows per direction (two-digit node-ID mnemonic).
pub const MAX_MESH_ROWS: usize = 100;

/// A rectangular axis-aligned grounding grid.
#[derive(Debug, Clone, PartialEq)]
pub struct MeshElement {
    /// Element id
    pub id: String,
    /// Corner position `[x, y, z]` (z ≠ 0)
    pub position: [f64; 3],
    /// Extent along X (m)
    pub length_x: f64,
    /// Extent along Y (m)
    pub length_y: f64,
    /// Bars parallel to X
    pub rows_x: usize,
    /// Bars parallel to Y
    pub rows_y: usize,
    /// Conductor radius (m)
    pub radius: f64,
    /// Segments per bar
    pub segments: usize,
    /// Conductor material id
    pub id_material: String,
}

fn row_col_tag(row: usize, col: usize) -> String {
    format!("{row:02}{col:02}")
}

impl MeshElement {
    fn node_id(&self, row: usize, col: usize) -> String {
        format!("{}-{}", self.id, row_col_tag(row, col))
    }

    fn err(&self, msg: &str) -> TupaError {
        TupaError::new(format!("tMeshElement '{}': {}", self.id, msg))
    }

    /// Plant the main nodes, then wire every adjacent pair with a bar
    /// (X-parallel bars first, then Y-parallel).
    pub fn assemble(&self, structure: &mut Structure) -> Result<()> {
        if self.rows_x < 2 || self.rows_y < 2 {
            return Err(self.err("rowsX and rowsY must each be >= 2"));
        }
        if self.rows_x > MAX_MESH_ROWS || self.rows_y > MAX_MESH_ROWS {
            return Err(self.err("rowsX/rowsY must each be <= 100 (2-digit node ID mnemonic)"));
        }
        if self.segments < 1 {
            return Err(self.err("segments must be >= 1"));
        }
        if self.length_x <= 0.0 || self.length_y <= 0.0 {
            return Err(self.err("lengthX and lengthY must be > 0"));
        }
        if self.position[2] == 0.0 {
            return Err(self.err(
                "position z = 0 (exactly on the air-soil interface) is not supported — a segment \
                 straddling the interface is not well-defined by the image-method formulation \
                 (theory.md §2, §5)",
            ));
        }
        if structure.find_material(&self.id_material).is_none() {
            return Err(self.err(&format!("material '{}' not found", self.id_material)));
        }

        for row in 0..self.rows_x {
            for col in 0..self.rows_y {
                let p = [
                    self.position[0] + col as f64 * self.length_x / (self.rows_y - 1) as f64,
                    self.position[1] + row as f64 * self.length_y / (self.rows_x - 1) as f64,
                    self.position[2] + 0.0,
                ];
                structure.add_node(Node::new(self.node_id(row, col), p));
            }
        }

        for row in 0..self.rows_x {
            for col in 0..self.rows_y - 1 {
                self.assemble_bar(structure, row, col, row, col + 1)?;
            }
        }
        for col in 0..self.rows_y {
            for row in 0..self.rows_x - 1 {
                self.assemble_bar(structure, row, col, row + 1, col)?;
            }
        }
        Ok(())
    }

    fn assemble_bar(
        &self,
        structure: &mut Structure,
        row1: usize,
        col1: usize,
        row2: usize,
        col2: usize,
    ) -> Result<()> {
        let bar_id = format!(
            "{}-{}-{}",
            self.id,
            row_col_tag(row1, col1),
            row_col_tag(row2, col2)
        );
        let bar = Line::new(
            bar_id,
            self.node_id(row1, col1),
            self.node_id(row2, col2),
            self.radius,
            self.segments,
            self.id_material.clone(),
        );
        bar.assemble(structure).map(|_| ())
    }

    /// Number of bars.
    pub fn n_bars(&self) -> usize {
        self.rows_x * self.rows_y.saturating_sub(1) + self.rows_y * self.rows_x.saturating_sub(1)
    }

    /// Human-readable description.
    pub fn report(&self) -> String {
        let n_nodes = self.rows_x * self.rows_y + self.n_bars() * self.segments.saturating_sub(1);
        format!(
            "Element ID: {}, Material: {}, {}x{} rows, {:.3}m x {:.3}m, radius {:.4}m\n  Nodes: {} ({}x{} main), Electrodes: {}\n",
            self.id,
            self.id_material,
            self.rows_x,
            self.rows_y,
            self.length_x,
            self.length_y,
            self.radius,
            n_nodes,
            self.rows_x,
            self.rows_y,
            self.n_bars() * self.segments
        )
    }
}
