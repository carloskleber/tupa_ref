//! Geometry-factor layer (`mGeometry`): mean/image distances, real geometry
//! factors and direction cosines for pairs of straight cylindrical segments
//! (theory.md §4–5, ADR 0004). Frequency-independent; computed once per
//! geometry. No dependency on the object model — plain endpoints and radii.

use crate::geometry_cache::{GeometryCache, geom_cache_key};
use crate::impedance::{DEFAULT_QUAD_EPS_REL, geometry_factor_2d};

/// A 3-vector.
pub type Vec3 = [f64; 3];

/// Threshold on `|va × vb|` below which two unit vectors count as parallel
/// (Matlab `barraquad`'s `cruz < 1e-20`).
pub const PARALLEL_TOL: f64 = 1.0e-20;
/// Touching/collinearity tolerance of the closed-form parallel formula
/// (Matlab `NUMP`).
pub const NUMP: f64 = 1.0e-6;

fn sub(a: &Vec3, b: &Vec3) -> Vec3 {
    [a[0] - b[0], a[1] - b[1], a[2] - b[2]]
}

fn dot(a: &Vec3, b: &Vec3) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

fn norm(a: &Vec3) -> f64 {
    dot(a, a).sqrt()
}

fn cross(u: &Vec3, v: &Vec3) -> Vec3 {
    [
        u[1] * v[2] - u[2] * v[1],
        u[2] * v[0] - u[0] * v[2],
        u[0] * v[1] - u[1] * v[0],
    ]
}

/// Decompose a segment into its unit direction and length.
pub fn segment_vector(p1: &Vec3, p2: &Vec3) -> (Vec3, f64) {
    let d = sub(p2, p1);
    let length = norm(&d);
    ([d[0] / length, d[1] / length, d[2] / length], length)
}

/// Coincident (self) geometry factor, axis-to-surface (theory.md §4.2):
/// `g_self = 2 [ l ln((l+h)/r0) − h + r0 ]`, `h = √(l² + r0²)`.
pub fn self_geometry_factor(l: f64, r0: f64) -> f64 {
    let h = (l * l + r0 * r0).sqrt();
    2.0 * (l * ((l + h) / r0).ln() - h + r0)
}

/// Mirror a position or direction through the z = 0 air–soil interface.
pub fn image_vector(d: &Vec3) -> Vec3 {
    [d[0], d[1], -d[2]]
}

/// Distance between two points (segment midpoints), theory.md §4.1.
pub fn mean_distance(p1: &Vec3, p2: &Vec3) -> f64 {
    norm(&sub(p2, p1))
}

/// `cos θ` between two direction vectors; 0 if either has zero length.
pub fn direction_cosine(d1: &Vec3, d2: &Vec3) -> f64 {
    let n1 = norm(d1);
    let n2 = norm(d2);
    if n1 <= 0.0 || n2 <= 0.0 {
        return 0.0;
    }
    dot(d1, d2) / (n1 * n2)
}

/// Closed-form mutual geometry factor of two PARALLEL segments (theory.md
/// §4.2; Matlab `barraquad.m` `posparal`). `None` signals a degenerate
/// (NaN/Inf) result: the caller falls back to quadrature.
#[allow(clippy::too_many_arguments)]
fn parallel_geometry_factor(
    a1: &Vec3,
    a2: &Vec3,
    la: f64,
    va: &Vec3,
    b1: &Vec3,
    b2: &Vec3,
    lb: f64,
    vb: &Vec3,
) -> Option<f64> {
    // g = ∫∫ dla dlb / R does not depend on the segments' orientation
    // (theory.md §4.2; the sign lives in cos θ), so an opposite-direction b is
    // traversed backwards and only the same-direction branches are needed. The
    // legacy `posparal` opposite-direction branches were wrong whenever
    // la != lb (ADR 0017 finding 8).
    let (c1, c2, vc) = if dot(va, vb) > 0.0 {
        (*b1, *b2, *vb)
    } else {
        (*b2, *b1, [-vb[0], -vb[1], -vb[2]])
    };
    let da1b1 = norm(&sub(a1, &c1));
    let da1b2 = norm(&sub(a1, &c2));
    let da2b1 = norm(&sub(a2, &c1));
    let da2b2 = norm(&sub(a2, &c2));
    let mut x2 = la;

    let (xi1, xi2, d11, d12, d21, d22);
    if da1b2 > da2b1 {
        xi1 = dot(&sub(&c1, a1), va);
        xi2 = xi1 + lb;
        (d11, d12, d21, d22) = (da1b1, da1b2, da2b1, da2b2);
    } else {
        x2 = lb;
        xi1 = dot(&sub(a1, &c1), &vc);
        xi2 = xi1 + la;
        (d11, d12, d21, d22) = (da1b1, da2b1, da1b2, da2b2);
    }

    let l11 = xi1;
    let l21 = xi1 - x2;
    let l22 = xi2 - x2;
    let y = (d11 * d11 - l11 * l11).max(0.0).sqrt() / la;

    let g = if y < NUMP {
        if xi1.abs() < NUMP
            || (xi1 - x2).abs() < NUMP
            || xi2.abs() < NUMP
            || (xi2 - x2).abs() < NUMP
        {
            // touching, non-overlapping collinear segments (see Fortran
            // comment): exact for both orientations
            (la + lb) * (la + lb).ln() - la * la.ln() - lb * lb.ln()
        } else {
            x2 * ((x2 - xi2) / (x2 - xi1)).ln()
                + xi1 * (-(x2 - xi1) / xi1).ln()
                + xi2 * (-xi2 / (x2 - xi2)).ln()
        }
    } else {
        d11 - d12 - d21
            + d22
            + x2 * ((d22 + l22) / (d21 + l21)).ln()
            + xi1 * ((d11 - xi1) / (d21 - l21)).ln()
            + xi2 * ((d22 - l22) / (d12 - xi2)).ln()
    };

    if g.is_finite() { Some(g) } else { None }
}

/// Numerical options of the geometry build.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GeometryOptions {
    /// Relative-error factor of the 2-D quadrature (CLI `--epsrel`)
    pub eps_rel: f64,
    /// Memoise quadrature results (CLI `--no-cache` disables)
    pub use_cache: bool,
    /// Always use quadrature, even for parallel pairs (testing oracle)
    pub force_numeric: bool,
}

impl Default for GeometryOptions {
    fn default() -> Self {
        Self {
            eps_rel: DEFAULT_QUAD_EPS_REL,
            use_cache: true,
            force_numeric: false,
        }
    }
}

/// General mutual geometry factor `g(a,b) = ∫ dl_a dl_b / R_ab` (theory.md
/// §4.2) for non-coincident, non-identical segments: closed form for parallel
/// pairs, memoised adaptive 2-D quadrature otherwise.
pub fn mutual_geometry_factor(
    a1: &Vec3,
    a2: &Vec3,
    b1: &Vec3,
    b2: &Vec3,
    opts: &GeometryOptions,
    cache: &mut GeometryCache,
) -> f64 {
    let (va, la) = segment_vector(a1, a2);
    let (vb, lb) = segment_vector(b1, b2);

    let mut try_closed = !opts.force_numeric;
    if try_closed {
        try_closed = norm(&cross(&va, &vb)) < PARALLEL_TOL;
    }
    if try_closed {
        if let Some(g) = parallel_geometry_factor(a1, a2, la, &va, b1, b2, lb, &vb) {
            return g;
        }
    }

    let use_cache = cache.is_enabled() && opts.use_cache;
    let mut key = None;
    if use_cache {
        let k = geom_cache_key(a1, a2, la, b1, b2, lb);
        if let Some(g) = cache.get(&k) {
            return g;
        }
        key = Some(k);
    }
    let g = geometry_factor_2d(a1, &va, la, b1, &vb, lb, opts.eps_rel);
    if let Some(k) = key {
        cache.put(k, g);
    }
    g
}

/// Full `n × n` geometry matrices of a set of segments (row-major `Vec<f64>`,
/// symmetric).
#[derive(Debug, Clone, PartialEq)]
pub struct GeometryMatrices {
    /// Number of segments
    pub n: usize,
    /// Direct geometry factor
    pub g: Vec<f64>,
    /// Image geometry factor
    pub gi: Vec<f64>,
    /// Mean distance (diagonal = radius)
    pub rbar: Vec<f64>,
    /// Image mean distance
    pub rbari: Vec<f64>,
    /// Direction cosine (diagonal = 1)
    pub cos_theta: Vec<f64>,
    /// Image direction cosine
    pub cos_theta_i: Vec<f64>,
    /// Cache statistics of this build
    pub cache_stats: crate::geometry_cache::CacheStats,
}

impl GeometryMatrices {
    /// Flat index of `(i, j)`.
    #[inline]
    pub fn idx(&self, i: usize, j: usize) -> usize {
        i * self.n + j
    }
}

/// Build the geometry matrices (theory.md §4–5). Self entries: closed-form
/// `g_self`, `Rbar = r0`, `cosθ = 1`. Image entries (also the diagonal, a
/// segment against its own image) always go through `mutual_geometry_factor`.
/// Mutual pairs in different media (`pos[i] != pos[j]`, 1 = air, 2 = soil)
/// are skipped and zeroed, exactly as `calcZMutual` would discard them
/// (ADR 0005). Pass `pos = None` to compute every pair.
pub fn build_geometry_matrices(
    p1: &[Vec3],
    p2: &[Vec3],
    radius: &[f64],
    pos: Option<&[u8]>,
    opts: &GeometryOptions,
) -> GeometryMatrices {
    let n = p1.len();
    let mut cache = GeometryCache::new(opts.use_cache);

    let mut dir = Vec::with_capacity(n);
    let mut len = Vec::with_capacity(n);
    let mut mid = Vec::with_capacity(n);
    for i in 0..n {
        let (d, l) = segment_vector(&p1[i], &p2[i]);
        dir.push(d);
        len.push(l);
        mid.push([
            0.5 * (p1[i][0] + p2[i][0]),
            0.5 * (p1[i][1] + p2[i][1]),
            0.5 * (p1[i][2] + p2[i][2]),
        ]);
    }

    let mut m = GeometryMatrices {
        n,
        g: vec![0.0; n * n],
        gi: vec![0.0; n * n],
        rbar: vec![0.0; n * n],
        rbari: vec![0.0; n * n],
        cos_theta: vec![0.0; n * n],
        cos_theta_i: vec![0.0; n * n],
        cache_stats: Default::default(),
    };

    for i in 0..n {
        for j in i..n {
            let mixed = i != j && pos.is_some_and(|p| p[i] != p[j]);
            let (ij, ji) = (i * n + j, j * n + i);

            if i == j {
                m.g[ij] = self_geometry_factor(len[i], radius[i]);
                m.rbar[ij] = radius[i];
                m.cos_theta[ij] = 1.0;
            } else if !mixed {
                let gij = mutual_geometry_factor(&p1[i], &p2[i], &p1[j], &p2[j], opts, &mut cache);
                m.g[ij] = gij;
                m.g[ji] = gij;
                let rb = mean_distance(&mid[i], &mid[j]);
                m.rbar[ij] = rb;
                m.rbar[ji] = rb;
                let ct = direction_cosine(&dir[i], &dir[j]);
                m.cos_theta[ij] = ct;
                m.cos_theta[ji] = ct;
            }

            if mixed {
                continue;
            }

            // image term: segment i against the mirror image of segment j
            let p1i = image_vector(&p1[j]);
            let p2i = image_vector(&p2[j]);
            let midi = image_vector(&mid[j]);
            let diri = image_vector(&dir[j]);
            let gij = mutual_geometry_factor(&p1[i], &p2[i], &p1i, &p2i, opts, &mut cache);
            m.gi[ij] = gij;
            m.gi[ji] = gij;
            let rbi = mean_distance(&mid[i], &midi);
            m.rbari[ij] = rbi;
            m.rbari[ji] = rbi;
            let cti = direction_cosine(&dir[i], &diri);
            m.cos_theta_i[ij] = cti;
            m.cos_theta_i[ji] = cti;
        }
    }
    m.cache_stats = cache.stats();
    m
}

#[cfg(test)]
mod tests {
    use super::*;

    fn opts() -> GeometryOptions {
        GeometryOptions::default()
    }

    #[test]
    fn self_factor_matches_defining_integral() {
        // g_self = 2∫0^l (l-u)/√(u²+r0²) du, checked by quadrature
        let (l, r0) = (0.5, 0.007);
        let mut f = |u: f64| 2.0 * (l - u) / (u * u + r0 * r0).sqrt();
        let (num, _) = crate::impedance::dqag_k15(&mut f, 0.0, l, 0.0, 1e-12);
        let g = self_geometry_factor(l, r0);
        assert!((g - num).abs() / g < 1e-9, "{g} vs {num}");
    }

    #[test]
    fn parallel_closed_form_matches_quadrature() {
        let a1 = [0.0, 0.0, -1.0];
        let a2 = [2.0, 0.0, -1.0];
        let b1 = [0.5, 1.5, -1.0];
        let b2 = [3.5, 1.5, -1.0];
        let mut cache = GeometryCache::new(false);
        let closed = mutual_geometry_factor(&a1, &a2, &b1, &b2, &opts(), &mut cache);
        let numeric = mutual_geometry_factor(
            &a1,
            &a2,
            &b1,
            &b2,
            &GeometryOptions {
                force_numeric: true,
                eps_rel: 1e-9,
                ..opts()
            },
            &mut cache,
        );
        assert!(
            (closed - numeric).abs() / closed < 1e-6,
            "{closed} vs {numeric}"
        );
    }

    #[test]
    fn opposite_direction_unequal_lengths_match_quadrature() {
        // A 2 m air segment against the image of a 4 m one (legacy torre2): the
        // legacy opposite-direction branches gave -173 instead of +0.092
        // (ADR 0017 finding 8).
        let a1 = [0.0, 0.0, 50.0];
        let a2 = [0.0, 0.0, 48.0];
        let b1 = [0.0, 5.0, -40.0];
        let b2 = [0.0, 5.0, -36.0];
        let mut cache = GeometryCache::new(false);
        let closed = mutual_geometry_factor(&a1, &a2, &b1, &b2, &opts(), &mut cache);
        let reversed = mutual_geometry_factor(&a1, &a2, &b2, &b1, &opts(), &mut cache);
        let numeric = mutual_geometry_factor(
            &a1,
            &a2,
            &b1,
            &b2,
            &GeometryOptions {
                force_numeric: true,
                eps_rel: 1e-9,
                ..opts()
            },
            &mut cache,
        );
        assert!(
            (closed - numeric).abs() / numeric < 1e-6,
            "{closed} vs {numeric}"
        );
        assert!((closed - reversed).abs() / closed < 1e-12);
    }

    #[test]
    fn collinear_touching_segments_use_log_formula() {
        let a1 = [0.0, 0.0, -1.0];
        let a2 = [1.0, 0.0, -1.0];
        let b1 = [1.0, 0.0, -1.0];
        let b2 = [3.0, 0.0, -1.0];
        let mut cache = GeometryCache::new(false);
        let g = mutual_geometry_factor(&a1, &a2, &b1, &b2, &opts(), &mut cache);
        let (la, lb) = (1.0_f64, 2.0_f64);
        let exact = (la + lb) * (la + lb).ln() - la * la.ln() - lb * lb.ln();
        assert!((g - exact).abs() < 1e-12);
    }

    #[test]
    fn image_geometry_of_horizontal_segment() {
        // image of a horizontal segment at depth h is at distance 2h
        let p1 = [[0.0, 0.0, -0.5]];
        let p2 = [[1.0, 0.0, -0.5]];
        let m = build_geometry_matrices(&p1, &p2, &[0.01], None, &opts());
        assert!((m.rbari[0] - 1.0).abs() < 1e-12);
        assert!((m.cos_theta_i[0] - 1.0).abs() < 1e-12);
        assert!((m.cos_theta[0] - 1.0).abs() < 1e-12);
    }

    #[test]
    fn vertical_segment_image_cosine_is_minus_one() {
        let p1 = [[0.0, 0.0, -0.5]];
        let p2 = [[0.0, 0.0, -1.5]];
        let m = build_geometry_matrices(&p1, &p2, &[0.01], None, &opts());
        assert!((m.cos_theta_i[0] + 1.0).abs() < 1e-12);
    }

    #[test]
    fn mixed_media_pairs_are_zeroed() {
        let p1 = [[0.0, 0.0, 1.0], [0.0, 0.0, -1.0]];
        let p2 = [[0.0, 0.0, 2.0], [0.0, 0.0, -2.0]];
        let m = build_geometry_matrices(&p1, &p2, &[0.01, 0.01], Some(&[1, 2]), &opts());
        assert_eq!(m.g[1], 0.0);
        assert_eq!(m.gi[1], 0.0);
        assert_eq!(m.rbar[1], 0.0);
        assert!(m.g[0] > 0.0 && m.g[3] > 0.0);
    }

    #[test]
    fn symmetric_matrices() {
        let p1 = [[0.0, 0.0, -0.5], [1.0, 0.0, -0.5], [1.0, 1.0, -0.5]];
        let p2 = [[1.0, 0.0, -0.5], [1.0, 1.0, -0.5], [0.0, 1.0, -0.5]];
        let m = build_geometry_matrices(&p1, &p2, &[0.01; 3], None, &opts());
        for i in 0..3 {
            for j in 0..3 {
                assert_eq!(m.g[i * 3 + j], m.g[j * 3 + i]);
                assert_eq!(m.gi[i * 3 + j], m.gi[j * 3 + i]);
            }
        }
    }
}
