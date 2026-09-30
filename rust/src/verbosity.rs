//! CLI verbosity levels (`mVerbosity`).

use std::sync::atomic::{AtomicU8, Ordering};

/// Suppress all informational output.
pub const VERB_QUIET: u8 = 0;
/// Default level.
pub const VERB_NORMAL: u8 = 1;
/// Progress output per assembly step and frequency.
pub const VERB_VERBOSE: u8 = 2;

static LEVEL: AtomicU8 = AtomicU8::new(VERB_NORMAL);

/// Set the global verbosity level.
pub fn set_verbosity(level: u8) {
    LEVEL.store(level, Ordering::Relaxed);
}

/// Current global verbosity level.
pub fn verbosity_level() -> u8 {
    LEVEL.load(Ordering::Relaxed)
}

/// Print `msg` if the current level is at least `level`.
pub fn verbose(level: u8, msg: &str) {
    if verbosity_level() >= level {
        println!(" {msg}");
    }
}
