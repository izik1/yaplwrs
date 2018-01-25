use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Loc {
    pub ln: usize,
    pub col: usize,
}

impl Loc {
    pub fn new(ln: usize, col: usize) -> Self {
        Loc { ln, col }
    }

    pub fn from_tuple(pos: (usize, usize)) -> Loc {
        Loc {
            ln: pos.0,
            col: pos.1,
        }
    }

    pub fn from_string(string: &str, index: usize) -> Loc {
        Self::from_tuple(str_index_ln_col(string, index).expect("Failed to create Loc."))
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.ln, self.col)
    }
}

pub fn str_index_ln_col(string: &str, index: usize) -> Result<(usize, usize), &'static str> {
    if string.chars().count() <= index {
        Err("Index out of range of string")
    } else if string.chars().nth(index) == Some('\n') {
        Err("Trying to get line / column of unprintable character")
    } else {
        let mut ln = 1;
        let mut column = 1;
        for ch in string.chars().take(index) {
            if ch == '\n' {
                ln += 1;
                column = 0;
            }

            column += 1;
        }

        Ok((ln, column))
    }
}

pub fn str_fn_ln_col(
    string: &str,
    f: &Fn(char) -> bool,
) -> ::error::CompilerResult<Option<(usize, usize)>> {
    use error::CompilerError;
    let mut ln = 1;
    let mut col = 1;
    for c in string.chars() {
        if f(c) {
            if c == '\n' {
                return Err(CompilerError::new(
                    "Trying to get line / column of unprintable character",
                ));
            }

            return Ok(Some((ln, col)));
        }

        if c == '\n' {
            ln += 1;
            col = 0;
        }

        col += 1;
    }

    Ok(None)
}
