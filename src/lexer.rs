use crate::token::{self, Token};
use crate::util::Span;
use std::{collections::HashMap, iter::Peekable, str::Chars};

#[derive(Debug, Clone)]
pub enum Error {
    NonAsciiString(usize),
    UnmappedCharacter(usize),
}

type Result<T> = ::std::result::Result<T, Error>;

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;

    proptest! {
        #[test]
        fn assert_no_panics(ref s in ".*") {
            let _ = Lexer::new(s).map(<Lexer<'_> as Itertools>::collect_vec);
        }

        #[test]
        fn ident(ref s in "[A-Za-z_][A-Za-z_0-9]*") {
            assert_eq!(
                Lexer::new(s).unwrap().collect_vec(),
                vec![Token::new(Span::new(0, s.len()), token::Kind::Ident(s.to_string()))]
             );

        }

        #[test]
        fn suffixed_number(ref num in "[0-9][0-9_]*", ref suffix in "[A-Za-z][A-Za-z_]*") {
            assert_eq!(
                Lexer::new(&format!("{}{}", num, suffix)).unwrap().collect_vec(),
                vec![Token::new(Span::new(0, num.len() + suffix.len()), token::Kind::Integer(str::replace(num, "_", ""), Some(suffix.to_string())))]
             );
        }
    }

    #[test]
    fn empty() {
        assert_eq!(Lexer::new("").unwrap().collect_vec(), vec![])
    }

    #[test]
    fn identifiers() {
        assert_eq!(
            Lexer::new("a A2").unwrap().collect_vec(),
            vec![
                Token::new(Span::new(0, 1), token::Kind::Ident("a".to_string())),
                Token::new(Span::new(2, 2), token::Kind::Ident("A2".to_string())),
            ]
        )
    }

    #[test]
    fn colon_is_colon() {
        assert_eq!(
            Lexer::new(":").unwrap().collect_vec(),
            vec![Token::new(
                Span::new(0, 1),
                token::Kind::Grammar(token::Grammar::Colon),
            )]
        )
    }

    #[test]
    fn invalid_operator() {
        assert_eq!(
            Lexer::new("%").unwrap().collect_vec(),
            vec![Token::new(Span::new(0, 1), token::Kind::Err("%".to_string())),]
        )
    }

    #[test]
    fn grammar_at_end_of_input() {
        assert_eq!(
            Lexer::new("(").unwrap().collect_vec(),
            vec![Token::new(
                Span::new(0, 1),
                token::Kind::Grammar(token::Grammar::OpenParen),
            )]
        )
    }

    #[test]
    fn op_longest_match() {
        assert_eq!(
            Lexer::new("->").unwrap().collect_vec(),
            vec![Token::new(
                Span::new(0, 2),
                token::Kind::Grammar(token::Grammar::Arrow),
            )]
        )
    }
}

#[cfg(test)]
pub(crate) fn is_keyword(s: &str) -> bool {
    keyword_map().contains_key(s)
}

fn keyword_map() -> HashMap<&'static str, token::Kind> {
    hashmap! [
        "const" => token::Kind::Keyword(token::Keyword::Const),
        "fn" => token::Kind::Keyword(token::Keyword::Function),
        "if" => token::Kind::Keyword(token::Keyword::If),
        "else" => token::Kind::Keyword(token::Keyword::Else),
    ]
}

fn operator_map() -> HashMap<&'static str, token::Grammar> {
    hashmap![
        "->" => token::Grammar::Arrow,
        "(" => token::Grammar::OpenParen,
        ")" => token::Grammar::CloseParen,
        "{" => token::Grammar::OpenBrace,
        "}" => token::Grammar::CloseBrace,
        ";" => token::Grammar::SemiColon,
        ":" => token::Grammar::Colon,
        "," => token::Grammar::Comma,
        "+" => token::Grammar::Plus,
        "-" => token::Grammar::Minus,
        "*" => token::Grammar::Star,
        "/" => token::Grammar::Slash,
    ]
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

    fn skip_while<F: Fn(&char) -> bool>(&mut self, f: F) {
        while self.chars.peek().map_or(false, &f) {
            self.chars.next();
            self.pos += 1;
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

        self.skip_while(|ch| ch == &'_' || ch.is_alphanumeric());

        let suffix = if self.pos > num_pos {
            Some(self.input[num_pos..self.pos].to_string())
        } else {
            None
        };

        Token::new(
            Span::new(start, self.pos - start),
            token::Kind::Integer(num, suffix),
        )
    }

    fn lex_ident(&mut self) -> Token {
        let start = self.pos;

        self.skip_while(|ch| ch == &'_' || ch.is_alphanumeric());

        let token_string = &self.input[start..self.pos];

        Token::new(
            Span::new(start, self.pos - start),
            keyword_map()
                .get(token_string)
                .cloned()
                .unwrap_or_else(|| token::Kind::Ident(token_string.to_string())),
        );

        Token::new(
            Span::new(start, self.pos - start),
            match &self.input[start..self.pos] {
                "const" => token::Kind::Keyword(token::Keyword::Const),
                "fn" => token::Kind::Keyword(token::Keyword::Function),
                "if" => token::Kind::Keyword(token::Keyword::If),
                "else" => token::Kind::Keyword(token::Keyword::Else),
                id => token::Kind::Ident(id.to_string()),
            },
        )
    }

    fn lex_operator(&mut self) -> Token {
        let start = self.pos;
        let mut pos = self.pos;

        let ops = operator_map();

        while let Some(_) = self.chars.peek() {
            if ops
                .keys()
                .any(|key| key.starts_with(&self.input[start..=pos]))
            {
                pos += 1;
                self.chars.next();
            } else {
                break;
            }
        }

        self.pos = pos;
        let span = Span::new(start, pos - start);

        let tt = if let Some(op) = ops.get(&self.input[start..pos]) {
            token::Kind::Grammar(*op)
        } else {
            token::Kind::Err(self.input[start..pos].to_string())
        };

        Token::new(span, tt)
    }

    pub fn lex_invalid_token(&mut self) -> Token {
        let start = self.pos;
        let ops = operator_map();

        while let Some(lookahead) = self.chars.peek() {
            if lookahead.is_alphanumeric() || ops.keys().any(|k| k.starts_with(*lookahead)) {
                break;
            }

            self.pos += 1;
            self.chars.next();
        }

        let span = Span::new(start, self.pos);
        Token::new(
            span,
            token::Kind::Err(self.input[start..self.pos].to_string()),
        )
    }

    fn lex(&mut self) -> Option<Token> {
        self.skip_while(|c| c.is_whitespace());
        if let Some(lookahead) = self.chars.peek() {
            Some(match lookahead {
                '0'..='9' => self.lex_number(),
                'A'..='Z' | 'a'..='z' | '_' => self.lex_ident(),
                c => {
                    if operator_map().keys().any(|k| k.starts_with(*c)) {
                        self.lex_operator()
                    } else {
                        self.lex_invalid_token()
                    }
                }
            })
        } else {
            None
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        self.lex()
    }
}
