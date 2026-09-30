//! Frequency-domain result storage (`mResult`): one complex value per
//! (entity, frequency) pair, where an entity is a node (voltages) or an
//! electrode (longitudinal or transversal end currents).

use num_complex::Complex64;

/// Results of one quantity over a frequency axis.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct ResultSet {
    entity_ids: Vec<String>,
    omega: Vec<f64>,
    data: Vec<Complex64>,
}

impl ResultSet {
    /// Allocate zero-filled storage for the given entities and ω axis (rad/s).
    pub fn new(entity_ids: Vec<String>, omega: Vec<f64>) -> Self {
        let n = entity_ids.len() * omega.len();
        Self {
            entity_ids,
            omega,
            data: vec![Complex64::new(0.0, 0.0); n],
        }
    }

    /// Number of entities.
    pub fn entity_count(&self) -> usize {
        self.entity_ids.len()
    }

    /// Number of frequency points.
    pub fn frequency_count(&self) -> usize {
        self.omega.len()
    }

    /// Identifier of entity `i` (0-based).
    pub fn entity_id(&self, i: usize) -> &str {
        &self.entity_ids[i]
    }

    /// Angular-frequency axis (rad/s).
    pub fn omega(&self) -> &[f64] {
        &self.omega
    }

    /// Value of entity `i` at frequency index `k` (both 0-based).
    pub fn get(&self, i: usize, k: usize) -> Complex64 {
        self.data[i * self.omega.len() + k]
    }

    /// Store the value of entity `i` at frequency index `k`.
    pub fn set(&mut self, i: usize, k: usize, v: Complex64) {
        let nf = self.omega.len();
        self.data[i * nf + k] = v;
    }
}
