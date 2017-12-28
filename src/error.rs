use std::error::Error;
use std::fmt;
use util::Loc;

#[derive(Debug, Clone, Copy)]
pub struct CompilerError {
    pub error: &'static str,
    pub location: Loc,
}

impl CompilerError {
    pub fn ice() -> CompilerError {
        CompilerError::new("ICE, this is a bug")
    }

    pub fn new(error: &'static str) -> CompilerError {
        CompilerError {
            error,
            location: Loc::new(0, 0),
        }
    }

    pub fn with_loc(error: &'static str, location: Loc) -> CompilerError {
        CompilerError { error, location }
    }
}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.location.col == 0 || self.location.ln == 0 {
            // Don't print impossible locations. Remember, Humans count positions from 1.
            write!(f, r#"Compiler Error: "{}""#, self.error)
        } else {
            write!(f, "Compiler Error: \"{}\" @({})", self.error, self.location)
        }
    }
}

impl Error for CompilerError {
    fn description(&self) -> &str {
        "A compiler error has occurred"
    }
}

pub type CompilerResult<T> = Result<T, CompilerError>;
