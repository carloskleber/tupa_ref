//! Memoisation of quadrature-computed mutual geometry factors
//! (`mGeometryCache`).
//!
//! A segment pair is determined up to congruence by the two lengths and the
//! four cross endpoint distances; each is rounded to `SIG_DIGITS` significant
//! digits and the key canonicalised over the 8 labelling symmetries of
//! `g(a,b)`. First stored value wins.

use std::collections::HashMap;

const SIG_DIGITS: i32 = 10;

/// Canonical congruence key (bit patterns of the quantised numbers).
pub type Key = [u64; 6];

/// Hit/miss statistics.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct CacheStats {
    /// Lookups that found an entry
    pub hits: u64,
    /// Lookups that did not
    pub misses: u64,
    /// Entries stored
    pub entries: usize,
}

/// The memo table. A disabled cache always misses and stores nothing.
#[derive(Debug, Default)]
pub struct GeometryCache {
    enabled: bool,
    table: HashMap<Key, f64>,
    hits: u64,
    misses: u64,
}

fn norm2(a: &[f64; 3], b: &[f64; 3]) -> f64 {
    let d = [b[0] - a[0], b[1] - a[1], b[2] - a[2]];
    (d[0] * d[0] + d[1] * d[1] + d[2] * d[2]).sqrt()
}

/// Round `v` to `SIG_DIGITS` significant digits; values < 1e-30 collapse to 0.
fn quantize(v: f64) -> f64 {
    if v < 1.0e-30 {
        0.0
    } else {
        let s = 10.0_f64.powi(SIG_DIGITS - 1 - v.log10().floor() as i32);
        (v * s).round() / s
    }
}

fn lex_less(x: &[f64; 6], y: &[f64; 6]) -> bool {
    for i in 0..6 {
        if x[i] < y[i] {
            return true;
        } else if x[i] > y[i] {
            return false;
        }
    }
    false
}

/// Canonical key for the pair `(a1-a2, b1-b2)` with lengths `la`, `lb`.
pub fn geom_cache_key(
    a1: &[f64; 3],
    a2: &[f64; 3],
    la: f64,
    b1: &[f64; 3],
    b2: &[f64; 3],
    lb: f64,
) -> Key {
    let d = [
        [quantize(norm2(a1, b1)), quantize(norm2(a1, b2))],
        [quantize(norm2(a2, b1)), quantize(norm2(a2, b2))],
    ];
    let laq = quantize(la);
    let lbq = quantize(lb);

    let mut first = true;
    let mut key = [0.0_f64; 6];
    for sw in 0..2 {
        let (m, l1, l2) = if sw == 0 {
            (d, laq, lbq)
        } else {
            ([[d[0][0], d[1][0]], [d[0][1], d[1][1]]], lbq, laq)
        };
        for ra in 0..2 {
            let (r1, r2) = (ra, 1 - ra);
            for rb in 0..2 {
                let (c1, c2) = (rb, 1 - rb);
                let cand = [l1, l2, m[r1][c1], m[r1][c2], m[r2][c1], m[r2][c2]];
                if first || lex_less(&cand, &key) {
                    key = cand;
                }
                first = false;
            }
        }
    }
    key.map(f64::to_bits)
}

impl GeometryCache {
    /// New cache; `enabled = false` makes it a no-op (CLI `--no-cache`).
    pub fn new(enabled: bool) -> Self {
        Self {
            enabled,
            ..Self::default()
        }
    }

    /// Whether lookups/inserts are active.
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    /// Look up a key, updating statistics.
    pub fn get(&mut self, key: &Key) -> Option<f64> {
        if !self.enabled {
            return None;
        }
        match self.table.get(key) {
            Some(&g) => {
                self.hits += 1;
                Some(g)
            }
            None => {
                self.misses += 1;
                None
            }
        }
    }

    /// Insert; an existing key keeps its first value.
    pub fn put(&mut self, key: Key, g: f64) {
        if self.enabled {
            self.table.entry(key).or_insert(g);
        }
    }

    /// Empty the table and reset statistics.
    pub fn clear(&mut self) {
        self.table.clear();
        self.hits = 0;
        self.misses = 0;
    }

    /// Statistics since the last `clear`.
    pub fn stats(&self) -> CacheStats {
        CacheStats {
            hits: self.hits,
            misses: self.misses,
            entries: self.table.len(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn key_is_invariant_to_labelling() {
        let a1 = [0.0, 0.0, -1.0];
        let a2 = [1.0, 0.0, -1.0];
        let b1 = [0.0, 2.0, -1.0];
        let b2 = [0.0, 3.0, -1.0];
        let k = geom_cache_key(&a1, &a2, 1.0, &b1, &b2, 1.0);
        assert_eq!(k, geom_cache_key(&a2, &a1, 1.0, &b1, &b2, 1.0));
        assert_eq!(k, geom_cache_key(&b1, &b2, 1.0, &a1, &a2, 1.0));
        assert_eq!(k, geom_cache_key(&a1, &a2, 1.0, &b2, &b1, 1.0));
    }

    #[test]
    fn key_distinguishes_lengths_with_same_cross_distances() {
        let a1 = [0.0, 0.0, 0.0];
        let b1 = [0.0, 1.0, 0.0];
        let k1 = geom_cache_key(&a1, &[1.0, 0.0, 0.0], 1.0, &b1, &[1.0, 1.0, 0.0], 1.0);
        let k2 = geom_cache_key(&a1, &[2.0, 0.0, 0.0], 2.0, &b1, &[2.0, 1.0, 0.0], 2.0);
        assert_ne!(k1, k2);
    }

    #[test]
    fn disabled_cache_never_hits() {
        let mut c = GeometryCache::new(false);
        let k = [0u64; 6];
        c.put(k, 1.0);
        assert_eq!(c.get(&k), None);
        assert_eq!(c.stats().entries, 0);
    }

    #[test]
    fn first_value_wins() {
        let mut c = GeometryCache::new(true);
        let k = [1u64; 6];
        c.put(k, 1.0);
        c.put(k, 2.0);
        assert_eq!(c.get(&k), Some(1.0));
        assert_eq!(
            c.stats(),
            CacheStats {
                hits: 1,
                misses: 0,
                entries: 1
            }
        );
    }
}
