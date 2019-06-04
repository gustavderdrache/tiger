use std::{
    iter::Peekable,
    str::{CharIndices, FromStr},
};

use crate::{
    pos::{Position, Span},
    token::{Token, TokenData},
};

#[derive(Debug, Clone)]
pub enum LexError {
    IntegerRangeError(Position, <isize as FromStr>::Err),
    IllegalCharacter(Position, char),
    UnterminatedString(Position),
    UnterminatedComment(Position),
    IllegalCharacterCode(Position, <u8 as FromStr>::Err),
    IllegalBacklash(Position, char),
    IllegalControlChar(Position, char),
}

fn is_safe_control_char(ch: char) -> bool {
    // cf. https://perldoc.perl.org/perlop.html#Quote-and-Quote-like-Operators
    ch == '@' || ch == '[' || ch == ']' || ch == '?' || ch == '_' || ch.is_ascii_alphabetic()
}

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    iter: Peekable<CharIndices<'a>>,
    position: usize,
    line: usize,
    column: usize,
    current: Option<(usize, char)>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut iter = input.char_indices().peekable();
        let current = iter.next();

        Self {
            input,
            iter,
            position: 0,
            line: 1,
            column: 1,
            current,
        }
    }

    fn current_char(&self) -> Option<char> {
        self.current.map(|(_, ch)| ch)
    }

    fn lookahead_char(&mut self) -> Option<char> {
        self.iter.peek().map(|(_, ch)| *ch)
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        let result = self.iter.next();
        if let Some((off, ch)) = result {
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }

            self.position = off;
        }

        self.current = result;
        result
    }

    fn skip_whitespace(&mut self) -> Result<(), LexError> {
        while let Some(ch) = self.current_char() {
            if ch == '/' {
                match self.lookahead_char() {
                    Some('/') => {
                        self.skip_until_newline();
                        continue;
                    }

                    Some('*') => {
                        self.skip_nested_comments()?;
                        continue;
                    }

                    _ => {}
                }
            }

            if !ch.is_whitespace() {
                break;
            }

            self.advance();
        }

        Ok(())
    }

    fn skip_until_newline(&mut self) {
        loop {
            let ch = match self.current_char() {
                None => return,
                Some(ch) => ch,
            };

            if ch == '\n' {
                return;
            }

            self.advance();
        }
    }

    fn skip_nested_comments(&mut self) -> Result<(), LexError> {
        let mut level: usize = 0;
        let start = self.current_position();

        loop {
            match self.current_char() {
                None => return Err(LexError::UnterminatedComment(start)),

                Some('/') => {
                    self.advance();
                    if let Some('*') = self.current_char() {
                        self.advance();
                        level += 1;
                    }
                }

                Some('*') => {
                    self.advance();
                    if let Some('/') = self.current_char() {
                        self.advance();
                        level -= 1;

                        if level == 0 {
                            break;
                        }
                    }
                }

                _ => {
                    self.advance();
                }
            }
        }

        Ok(())
    }

    fn advance_while<F>(&mut self, predicate: F) -> &str
    where
        F: Fn(char) -> bool,
    {
        let start = self.current_position().offset;

        while let Some(ch) = self.current_char() {
            if !predicate(ch) {
                break;
            }

            self.advance();
        }

        if self.current_char().is_some() {
            let end = self.current_position().offset;
            &self.input[start..end]
        } else {
            &self.input[start..]
        }
    }

    fn scan_string(&mut self) -> Result<String, LexError> {
        self.advance();

        let mut buffer = String::new();

        loop {
            let ch = match self.current_char() {
                None => return Err(LexError::UnterminatedString(self.current_position())),
                Some(ch) => ch,
            };

            if ch == '"' {
                self.advance();
                break;
            }

            if ch == '\\' {
                self.advance();
                match self.current_char() {
                    None => return Err(LexError::UnterminatedString(self.current_position())),
                    Some('n') => {
                        self.advance();
                        buffer.push('\n');
                        continue;
                    }
                    Some('t') => {
                        self.advance();
                        buffer.push('\t');
                        continue;
                    }
                    Some('"') => {
                        self.advance();
                        buffer.push('"');
                        continue;
                    }
                    Some('^') => {
                        self.advance();

                        let ch = match self.current_char() {
                            None => {
                                return Err(LexError::UnterminatedString(self.current_position()))
                            }
                            Some(ch) => ch,
                        };

                        if !is_safe_control_char(ch) {
                            return Err(LexError::IllegalControlChar(self.current_position(), ch));
                        }

                        let code = ch as u8;
                        buffer.push((code ^ 64).into());
                        self.advance();
                        continue;
                    }
                    Some(ch) if ch.is_ascii_digit() => {
                        let start = self.current_position();

                        let digits = self.advance_while(|ch| ch.is_ascii_digit());
                        let result = digits.parse::<u8>();
                        let code = match result {
                            Ok(parsed) => parsed,
                            Err(err) => return Err(LexError::IllegalCharacterCode(start, err)),
                        };

                        buffer.push(code.into());
                        continue;
                    }
                    Some(ch) => return Err(LexError::IllegalBacklash(self.current_position(), ch)),
                }
            }

            self.advance();
            buffer.push(ch);
        }

        Ok(buffer)
    }

    fn current_position(&self) -> Position {
        Position {
            offset: self.position,
            line: self.line,
            column: self.column,
        }
    }

    fn create_token(&self, data: TokenData, start: Position) -> Token {
        Token {
            data,
            span: Span {
                start,
                end: self.current_position(),
            },
        }
    }

    fn scan(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace()?;

        let start = self.current_position();
        let current = match self.current_char() {
            Some(ch) => ch,
            None => {
                return Ok(Token {
                    data: TokenData::Eof,
                    span: Span { start, end: start },
                })
            }
        };

        if let Some(data) = TokenData::from_single_char(current) {
            self.advance();

            return Ok(self.create_token(data, start));
        }

        match current {
            ':' => {
                self.advance();

                match self.current_char() {
                    Some('=') => {
                        self.advance();
                        return Ok(self.create_token(TokenData::ColonEquals, start));
                    }

                    _ => return Ok(self.create_token(TokenData::Colon, start)),
                }
            }

            '<' => {
                self.advance();
                match self.current_char() {
                    Some('=') => {
                        self.advance();
                        return Ok(self.create_token(TokenData::LessThanEquals, start));
                    }

                    Some('>') => {
                        self.advance();
                        return Ok(self.create_token(TokenData::NotEquals, start));
                    }

                    _ => return Ok(self.create_token(TokenData::LessThan, start)),
                }
            }

            '>' => {
                self.advance();
                match self.current_char() {
                    Some('=') => {
                        self.advance();
                        return Ok(self.create_token(TokenData::GreaterThanEquals, start));
                    }

                    _ => return Ok(self.create_token(TokenData::GreaterThan, start)),
                }
            }

            _ => (),
        }

        if current == '"' {
            return match self.scan_string() {
                Ok(buffer) => Ok(self.create_token(TokenData::String(buffer), start)),
                Err(err) => Err(err),
            };
        }

        if current.is_ascii_alphabetic() {
            let ident_or_keyword = self
                .advance_while(|ch| ch.is_ascii_alphanumeric())
                .to_owned();

            return Ok(self.create_token(TokenData::from_string(ident_or_keyword), start));
        }

        if current.is_ascii_digit() {
            let number = self.advance_while(|ch| ch.is_ascii_digit());

            return match number.parse() {
                Ok(parsed) => Ok(self.create_token(TokenData::Integer(parsed), start)),
                Err(err) => Err(LexError::IntegerRangeError(start, err)),
            };
        }

        self.advance();
        Err(LexError::IllegalCharacter(start, current))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.scan())
    }
}
