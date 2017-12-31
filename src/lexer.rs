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

    #[test]
    fn op_longest_match() {
        assert_eq!(
            Lexer::new("->").lex_all().unwrap(),
            vec![
                Token::new(Loc::new(1, 1), TokenType::Grammar(Grammar::Arrow)),
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

    fn nom_whitespace(&mut self) {
        let chars = self.chars.deref_mut();
        loop {
            match chars.peek() {
                None => return,
                Some(ch) if !ch.is_whitespace() => break,
                _ => {}
            }

            chars.next();
            self.pos += 1;
        }
    }

    pub fn lex(&mut self) -> CompilerResult<Option<Token>> {
        self.nom_whitespace();

        let chars = self.chars.deref_mut();

        let next = match chars.next() {
            Some(ch) => ch,
            None => return Ok(None),
        };

        let src = self.input;

        let start = self.pos;
        let mut pos = self.pos + 1;

        let res: Option<Token> = match next {
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
                        Loc::from_string(src, start),
                        TokenType::Integer(num),
                    )))
                } else {
                    Err(CompilerError::with_loc(
                        "ICE, this is a bug -- Failed to parse number.",
                        Loc::from_string(src, start),
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
                    Loc::from_string(src, start),
                    match &src[start..pos] {
                        "const" => TokenType::Const,
                        "fn" => TokenType::Function,
                        "if" => TokenType::If,
                        id => TokenType::Identifier(id.to_string()),
                    },
                )))
            }

            _ => {
                let ops = hashmap![
                    "->" => Grammar::Arrow,
                    "(" => Grammar::OpenParen,
                    ")" => Grammar::CloseParen,
                    "{" => Grammar::OpenBrace,
                    "}" => Grammar::CloseBrace,
                    ";" => Grammar::SemiColon,
                    "-" => Grammar::Minus,
                ];

                while let Some(_) = chars.peek() {
                    if ops.keys().any(|key| key.contains(&src[start..pos + 1])) {
                        pos += 1;
                        chars.next().unwrap();
                    } else {
                        break;
                    }
                }

                let loc = Loc::from_string(self.input, start);

                if let Some(op) = ops.get(&src[start..pos]) {
                    Ok(Some(Token::new(loc, TokenType::Grammar(*op))))
                } else {
                    Err(CompilerError::with_loc("ICE, this is a bug", loc))
                }
            }
        }?;

        self.pos = pos;
        Ok(res)
    }
}
