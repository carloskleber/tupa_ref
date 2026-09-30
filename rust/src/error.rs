//! Error type (`mError`). No panics on user input: every boundary failure is
//! a `TupaError`; messages follow the Fortran `raiseError` texts where practical.

use std::fmt;

/// A critical error with a human-readable message.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TupaError {
    message: String,
}

impl TupaError {
    /// Create an error from any message.
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }

    /// The message text.
    pub fn message(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for TupaError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for TupaError {}

impl From<std::io::Error> for TupaError {
    fn from(e: std::io::Error) -> Self {
        Self::new(format!("I/O error: {e}"))
    }
}

impl From<serde_json::Error> for TupaError {
    fn from(e: serde_json::Error) -> Self {
        Self::new(format!("JSON error: {e}"))
    }
}

/// Crate-wide result alias.
pub type Result<T> = std::result::Result<T, TupaError>;
