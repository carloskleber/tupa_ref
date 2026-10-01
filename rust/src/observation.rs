//! Observation-point specification and result containers for the
//! grounding-safety outputs (`mObservation`, ROADMAP Phase 11, theory.md
//! §3.1, ADR 0027).

use crate::result::ResultSet;

/// One explicit observation point.
#[derive(Debug, Clone, PartialEq)]
pub struct ObservationPoint {
    /// User-assigned id
    pub id: String,
    /// Position (m); `z <= 0` is soil, `z > 0` air (theory.md §2)
    pub p: [f64; 3],
}

/// Rectangular grid of observation points in one horizontal plane.
#[derive(Debug, Clone, PartialEq)]
pub struct GridSpec {
    /// Grid id; its sites are named `<id>_<ix>_<iy>` (1-based)
    pub id: String,
    /// `(x, y)` of the first grid point (m)
    pub origin: [f64; 2],
    /// Plane height (m); 0 = the soil surface
    pub z: f64,
    /// Extent along x (m)
    pub length_x: f64,
    /// Extent along y (m)
    pub length_y: f64,
    /// Points along x (a count of 1 puts the row at the origin)
    pub nx: usize,
    /// Points along y
    pub ny: usize,
    /// Also compute the step-voltage map
    pub step: bool,
    /// Step length (m) of the map
    pub step_length: f64,
    /// Azimuths tried per grid point for the step map
    pub step_directions: usize,
}

/// Touch-voltage site: the legacy geometric definition (theory.md §3.1).
#[derive(Debug, Clone, PartialEq)]
pub struct TouchSpec {
    /// User-assigned id (defaults to the node id)
    pub id: String,
    /// Designated node whose potential `u` is the reference
    pub node: String,
    /// Circle radius (m), centred on the node's `(x, y)`
    pub radius: f64,
    /// Points on the circle
    pub n_points: usize,
    /// Height of the circle's plane (m); 0 = the soil surface
    pub z: f64,
}

/// Step pair: `Δψ = ψ(to) − ψ(from)`.
#[derive(Debug, Clone, PartialEq)]
pub struct StepSpec {
    /// User-assigned id
    pub id: String,
    /// First point (m)
    pub from: [f64; 3],
    /// Second point (m)
    pub to: [f64; 3],
}

/// The parsed `"observation"` block.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Observation {
    /// `observation.points`
    pub points: Vec<ObservationPoint>,
    /// `observation.grid`
    pub grid: Option<GridSpec>,
    /// `observation.touch`
    pub touch: Vec<TouchSpec>,
    /// `observation.steps`
    pub steps: Vec<StepSpec>,
}

impl Observation {
    /// No observation was requested.
    pub fn is_empty(&self) -> bool {
        self.points.is_empty()
            && self.grid.is_none()
            && self.touch.is_empty()
            && self.steps.is_empty()
    }

    /// Number of potential sites: the explicit points, then the grid.
    pub fn site_count(&self) -> usize {
        self.points.len() + self.grid.as_ref().map_or(0, |g| g.nx * g.ny)
    }

    /// Id and position of potential site `i` (0-based).
    pub fn site(&self, i: usize) -> (String, [f64; 3]) {
        if let Some(pt) = self.points.get(i) {
            return (pt.id.clone(), pt.p);
        }
        let g = self.grid.as_ref().expect("site index within the grid");
        let j = i - self.points.len();
        let (ix, iy) = (j / g.ny, j % g.ny);
        let mut p = [g.origin[0], g.origin[1], g.z];
        if g.nx > 1 {
            p[0] += ix as f64 * g.length_x / (g.nx - 1) as f64;
        }
        if g.ny > 1 {
            p[1] += iy as f64 * g.length_y / (g.ny - 1) as f64;
        }
        (format!("{}_{}_{}", g.id, ix + 1, iy + 1), p)
    }
}

/// Outputs of `potentials::compute_observations`, one set per frequency of
/// the sweep. `touch` and `step_map` hold real magnitudes in the real part.
#[derive(Debug, Clone, Default)]
pub struct ObservationResults {
    /// ψ at every site (points, then the grid in x-major order)
    pub potentials: ResultSet,
    /// Potential `u` of each touch site's node (the ground potential rise)
    pub gpr: ResultSet,
    /// Touch voltage `max_k |ψ_k − u|` per touch site
    pub touch: ResultSet,
    /// `Δψ` of each step pair
    pub steps: ResultSet,
    /// Step voltage at every grid point (empty unless the grid asked for it)
    pub step_map: ResultSet,
    /// Position of every `potentials` site
    pub site_pos: Vec<[f64; 3]>,
    /// Frequency axis (Hz)
    pub freq_hz: Vec<f64>,
}
