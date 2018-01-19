use std::iter::Peekable;
use std::str::Chars;
use token::*;
use util::Loc;
use error::*;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_lex() {
        assert_eq!(Lexer::new("").unwrap().lex_all().unwrap(), vec![])
    }

    #[test]
    fn identifiers() {
        assert_eq!(
            Lexer::new("a A2").unwrap().lex_all().unwrap(),
            vec![
                Token::new(Loc::new(1, 1), TokenType::Identifier("a".to_string())),
                Token::new(Loc::new(1, 3), TokenType::Identifier("A2".to_string())),
            ]
        )
    }

    #[test]
    fn grammar_at_end_of_input() {
        assert_eq!(
            Lexer::new("(").unwrap().lex_all().unwrap(),
            vec![
                Token::new(Loc::new(1, 1), TokenType::Grammar(Grammar::OpenParen)),
            ]
        )
    }

    #[test]
    fn op_longest_match() {
        assert_eq!(
            Lexer::new("->").unwrap().lex_all().unwrap(),
            vec![
                Token::new(Loc::new(1, 1), TokenType::Grammar(Grammar::Arrow)),
            ]
        )
    }

    #[test]
    fn suffixed_numbers() {
        assert_eq!(
            Lexer::new("123u8").unwrap().lex_all().unwrap(),
            vec![
                Token::new(
                    Loc::new(1, 1),
                    TokenType::Integer("123".to_string(), "u8".to_string()),
                ),
            ]
        )
    }

    #[test]
    fn underscores_in_numbers() {
        assert_eq!(
            Lexer::new("123_45_6__u32").unwrap().lex_all().unwrap(),
            vec![
                Token::new(
                    Loc::new(1, 1),
                    TokenType::Integer("123456".to_string(), "u32".to_string()),
                ),
            ]
        )
    }

    #[test] // Better name needed.
    fn no_unicode_crashes() {
        let _ = do catch { Lexer::new("fn 日本語() {}")?.lex_all() };
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    chars: Box<Peekable<Chars<'a>>>,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> CompilerResult<Lexer<'a>> {
        if input.is_ascii() {
            Ok(Lexer {
                input,
                chars: Box::new(input.chars().peekable()),
                pos: 0,
            })
        } else {
            Err(CompilerError::new(""))
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
        while let Some(ch) = self.chars.peek() {
            if ch.is_whitespace() {
                self.chars.next();
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    fn lex_number(&mut self) -> Token {
        let start = self.pos;

        let mut num = String::new();

        while let Some(ch) = self.chars.peek() {
            if ch.is_numeric() || ch == &'_' {
                if ch.is_numeric() {
                    num.push(*ch);
                }

                self.chars.next();
                self.pos += 1;
            } else {
                break;
            }
        }

        let num_pos = self.pos;

        while let Some(ch) = self.chars.peek() {
            if ch == &'_' || ch.is_alphanumeric() {
                self.chars.next();
                self.pos += 1;
            } else {
                break;
            }
        }

        let suffix = if self.pos > num_pos {
            self.input[num_pos..self.pos].to_string()
        } else {
            "i32".to_string()
        };

        Token::new(
            Loc::from_string(self.input, start),
            TokenType::Integer(num, suffix),
        )
    }

    fn lex_identifier(&mut self) -> Token {
        let start = self.pos;

        while let Some(ch) = self.chars.peek() {
            if ch == &'_' || ch.is_alphanumeric() {
                self.chars.next();
                self.pos += 1;
            } else {
                break;
            }
        }

        Token::new(
            Loc::from_string(self.input, start),
            match &self.input[start..self.pos] {
                "const" => TokenType::Keyword(Keyword::Const),
                "fn" => TokenType::Keyword(Keyword::Function),
                "if" => TokenType::Keyword(Keyword::If),
                "else" => TokenType::Keyword(Keyword::Else),
                id => TokenType::Identifier(id.to_string()),
            },
        )
    }

    fn lex_operator(&mut self) -> CompilerResult<Token> {
        let start = self.pos;
        let mut pos = self.pos;

        let ops = hashmap![
                    "->" => Grammar::Arrow,
                    "(" => Grammar::OpenParen,
                    ")" => Grammar::CloseParen,
                    "{" => Grammar::OpenBrace,
                    "}" => Grammar::CloseBrace,
                    ";" => Grammar::SemiColon,
                    ":" => Grammar::Colon,
                    "," => Grammar::Comma,
                    "+" => Grammar::Plus,
                    "-" => Grammar::Minus,
                    "*" => Grammar::Star,
                    "/" => Grammar::Slash,
                ];

        while let Some(_) = self.chars.peek() {
            if ops.keys()
                .any(|key| key.contains(&self.input[start..pos + 1]))
            {
                pos += 1;
                self.chars.next();
            } else {
                break;
            }
        }

        let loc = Loc::from_string(self.input, start);

        if let Some(op) = ops.get(&self.input[start..pos]) {
            self.pos = pos;
            Ok(Token::new(loc, TokenType::Grammar(*op)))
        } else {
            Err(CompilerError::with_loc("ICE, this is a bug", loc))
        }
    }

    pub fn lex(&mut self) -> CompilerResult<Option<Token>> {
        self.nom_whitespace();
        Ok(if let Some(lookahead) = self.chars.peek() {
            Some(match *lookahead {
                '0'...'9' => self.lex_number(),
                'A'...'Z' | 'a'...'z' | '_' => self.lex_identifier(),
                _ => self.lex_operator()?,
            })
        } else {
            None
        })
    }
}
