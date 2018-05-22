use std::{iter::Peekable, str::Chars};
use token::*;
use util::Span;

#[derive(Debug, Clone)]
pub enum Error {
    NonAsciiString(usize),
    UnmappedCharacter(usize),
}

type Result<T> = ::std::result::Result<T, Error>;

#[cfg(test)]
mod tests {
    use super::*;

    proptest! {
        #[test]
        fn assert_no_panics(ref s in ".*") {
            let _: Result<_> = do catch { Lexer::new(s)?.lex_all() };
        }

        #[test]
        fn identifier(ref s in "[A-Za-z_][A-Za-z_0-9]*") {
            assert_eq!(
                Lexer::new(s).unwrap().lex_all().unwrap(),
                vec![Token::new(Span::new(0, s.len()), TokenType::Identifier(s.to_string()))]
             );

        }

        #[test]
        fn suffixed_number(ref num in "[0-9][0-9_]*", ref suffix in "[A-Za-z][A-Za-z_]*") {
            assert_eq!(
                Lexer::new(&format!("{}{}", num, suffix)).unwrap().lex_all().unwrap(),
                vec![Token::new(Span::new(0, num.len() + suffix.len()), TokenType::Integer(str::replace(num, "_", ""), Some(suffix.to_string())))]
             );
        }
    }

    #[test]
    fn empty() {
        assert_eq!(Lexer::new("").unwrap().lex_all().unwrap(), vec![])
    }

    #[test]
    fn identifiers() {
        assert_eq!(
            Lexer::new("a A2").unwrap().lex_all().unwrap(),
            vec![
                Token::new(Span::new(0, 1), TokenType::Identifier("a".to_string())),
                Token::new(Span::new(2, 2), TokenType::Identifier("A2".to_string())),
            ]
        )
    }

    #[test]
    fn colon_is_colon() {
        assert_eq!(
            Lexer::new(&format!("{}", TokenType::Grammar(Grammar::Colon)))
                .unwrap()
                .lex_all()
                .unwrap(),
            vec![Token::new(
                Span::new(0, 1),
                TokenType::Grammar(Grammar::Colon),
            )]
        )
    }

    #[test]
    fn grammar_at_end_of_input() {
        assert_eq!(
            Lexer::new("(").unwrap().lex_all().unwrap(),
            vec![Token::new(
                Span::new(0, 1),
                TokenType::Grammar(Grammar::OpenParen),
            )]
        )
    }

    #[test]
    fn op_longest_match() {
        assert_eq!(
            Lexer::new("->").unwrap().lex_all().unwrap(),
            vec![Token::new(
                Span::new(0, 2),
                TokenType::Grammar(Grammar::Arrow),
            )]
        )
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    chars: Box<Peekable<Chars<'a>>>,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Result<Lexer<'a>> {
        if let Some(pos) = input.chars().position(|c| !c.is_ascii()) {
            Err(Error::NonAsciiString(pos))
        } else {
            Ok(Lexer {
                input,
                chars: Box::new(input.chars().peekable()),
                pos: 0,
            })
        }
    }

    pub fn lex_all(mut self) -> Result<Vec<Token>> {
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
            Some(self.input[num_pos..self.pos].to_string())
        } else {
            None
        };

        Token::new(
            Span::new(start, self.pos - start),
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
            Span::new(start, self.pos - start),
            match &self.input[start..self.pos] {
                "const" => TokenType::Keyword(Keyword::Const),
                "fn" => TokenType::Keyword(Keyword::Function),
                "if" => TokenType::Keyword(Keyword::If),
                "else" => TokenType::Keyword(Keyword::Else),
                id => TokenType::Identifier(id.to_string()),
            },
        )
    }

    fn lex_operator(&mut self) -> Result<Token> {
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
            if ops
                .keys()
                .any(|key| key.contains(&self.input[start..pos + 1]))
            {
                pos += 1;
                self.chars.next();
            } else {
                break;
            }
        }

        if let Some(op) = ops.get(&self.input[start..pos]) {
            self.pos = pos;
            Ok(Token::new(
                Span::new(start, pos - start),
                TokenType::Grammar(*op),
            ))
        } else {
            Err(Error::UnmappedCharacter(start))
        }
    }

    pub fn lex(&mut self) -> Result<Option<Token>> {
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
