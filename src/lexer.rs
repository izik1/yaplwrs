use std::iter::Peekable;
use std::str::Chars;
use token::*;
use util::Loc;
use error::*;

#[cfg(test)]
mod tests {
    use super::*;

    proptest! {
        #[test]
        fn assert_no_panics(ref s in ".*") {
            let _ = do catch { Lexer::new(s)?.lex_all() };
        }

        #[test]
        fn identifier(ref s in "[A-Za-z_][A-Za-z_0-9]*") {
            assert_eq!(
                Lexer::new(s).unwrap().lex_all().unwrap(),
                vec![Token::new(Loc::new(1, 1), TokenType::Identifier(s.to_string()))]
             );

        }

        #[test]
        fn suffixed_number(ref num in "[0-9][0-9_]*", ref suffix in "[A-Za-z][A-Za-z_]*") {
            assert_eq!(
                Lexer::new(&format!("{}{}", num, suffix)).unwrap().lex_all().unwrap(),
                vec![Token::new(Loc::new(1, 1), TokenType::Integer(str::replace(num, "_", ""), suffix.to_string()))]
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
