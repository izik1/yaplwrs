use std::iter::Peekable;
use std::str::Chars;
use std::ops::DerefMut;
use token::*;
use util::Loc;
use error::*;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_lex() {
        assert_eq!(Lexer::new("").lex_all().unwrap(), vec![])
    }

    #[test]
    fn identifiers() {
        assert_eq!(
            Lexer::new("a A2").lex_all().unwrap(),
            vec![
                Token::new(Loc::new(1, 1), TokenType::Identifier("a".to_string())),
                Token::new(Loc::new(1, 3), TokenType::Identifier("A2".to_string())),
            ]
        )
    }

    #[test]
    fn grammar_at_end_of_input() {
        assert_eq!(
            Lexer::new("(").lex_all().unwrap(),
            vec![
                Token::new(Loc::new(1, 1), TokenType::Grammar(Grammar::OpenParen)),
            ]
        )
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    chars: Box<Peekable<Chars<'a>>>,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input,
            chars: Box::new(input.chars().peekable()),
            pos: 0,
        }
    }

    pub fn lex_all(mut self) -> CompilerResult<Vec<Token>> {
        let mut tokens = Vec::new();
        while let Some(t) = self.lex()? {
            tokens.push(t);
        }

        Ok(tokens)
    }

    pub fn lex(&mut self) -> CompilerResult<Option<Token>> {
        let chars = self.chars.deref_mut();
        let src = self.input;

        let mut pos = self.pos;

        // Skip whitespaces
        loop {
            // Note: the following lines are in their own scope to
            // limit how long 'chars' is borrowed, and in order to allow
            // it to be borrowed again in the loop by 'chars.next()'.
            {
                let ch = chars.peek();

                if ch.is_none() {
                    self.pos = pos;
                    return Ok(None);
                }

                if !ch.unwrap().is_whitespace() {
                    break;
                }
            }

            chars.next();
            pos += 1;
        }

        let start = pos;
        let next = chars.next();

        if next.is_none() {
            return Ok(None);
        }

        let next = next.unwrap();

        pos += 1;
        let res = match next {
            '0'...'9' => {
                loop {
                    match chars.peek() {
                        Some(ch) if ch.is_numeric() => {}
                        _ => break,
                    }

                    chars.next();
                    pos += 1;
                }

                if let Ok(num) = src[start..pos].parse::<u64>() {
                    Ok(Some(Token::new(
                        Loc::from_string(self.input, start),
                        TokenType::Integer(num),
                    )))
                } else {
                    Err(CompilerError::with_loc(
                        "ICE, this is a bug -- Failed to parse number.",
                        Loc::from_string(self.input, start),
                    ))
                }
            }

            'A'...'Z' | 'a'...'z' | '_' => {
                loop {
                    match chars.peek() {
                        Some(ch) if ch == &'_' || ch.is_alphanumeric() => {}
                        _ => break,
                    };

                    chars.next();
                    pos += 1;
                }

                Ok(Some(Token::new(
                    Loc::from_string(self.input, start),
                    match &src[start..pos] {
                        "const" => TokenType::Const,
                        "fn" => TokenType::Function,
                        "if" => TokenType::If,
                        id => TokenType::Identifier(id.to_string()),
                    },
                )))
            }

            _ => {
                let ops = vec![
                    ("(", Grammar::OpenParen),
                    (")", Grammar::CloseParen),
                    ("{", Grammar::OpenBrace),
                    ("}", Grammar::CloseBrace),
                    ("->", Grammar::Arrow),
                ];

                loop {
                    match chars.peek() {
                        None => break,
                        Some(_)
                            if ops.iter()
                                .filter(|op| op.0.contains(&src[start..pos + 1]))
                                .count() == 0 =>
                        {
                            break
                        }
                        _ => {
                            pos += 1;
                            chars.next().unwrap();
                        }
                    }
                }

                let search = || ops.iter().filter(|op| op.0.contains(&src[start..pos]));

                if search().count() == 1 {
                    Ok(Some(Token::new(
                        Loc::from_string(self.input, start),
                        TokenType::Grammar(search().next().unwrap().1),
                    )))
                } else {
                    Err(CompilerError::with_loc(
                        "ICE, this is a bug",
                        Loc::from_string(self.input, start),
                    ))
                }
            }
        }?;

        self.pos = pos;
        Ok(res)
    }
}
