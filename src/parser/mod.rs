use std::{fmt::Debug, iter::Peekable};

use enum_map::EnumMap;

use crate::{
    expression::*,
    lexer::{LexError, Lexer},
    token::{Token, TokenKind},
};

mod infix;
mod prefix;

use prefix::prefix_token_map;
pub use prefix::PrefixParser;

#[derive(Debug, Clone)]
pub enum ParseError {
    LexicalError(LexError),
    UnexpectedToken(Token),
}

impl From<LexError> for ParseError {
    fn from(e: LexError) -> Self {
        ParseError::LexicalError(e)
    }
}

pub trait TokenPattern: Debug {
    fn matches(self, token: TokenKind) -> bool;
}

impl TokenPattern for TokenKind {
    fn matches(self, token: TokenKind) -> bool {
        self == token
    }
}

impl<'a> TokenPattern for &'a [TokenKind] {
    fn matches(self, token: TokenKind) -> bool {
        self.iter().any(|&k| k == token)
    }
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    prefix_parsers: EnumMap<TokenKind, Option<PrefixParser>>,
    current: Result<Token, ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer::new(input).peekable();
        let current = lexer.next().unwrap().map_err(Into::into);

        Self {
            lexer,
            prefix_parsers: prefix_token_map(),
            current,
        }
    }

    pub fn advance(&mut self) -> Result<Token, ParseError> {
        self.current = self.lexer.next().unwrap().map_err(Into::into);
        self.current.clone()
    }

    pub fn peek_token(&mut self) -> Result<Token, ParseError> {
        self.lexer.peek().cloned().unwrap().map_err(Into::into)
    }

    pub fn matches<P>(&mut self, pattern: P) -> bool
    where
        P: TokenPattern,
    {
        match &self.current {
            Ok(tok) => pattern.matches(tok.kind()),
            Err(_) => false,
        }
    }

    pub fn expect<P>(&mut self, pattern: P) -> Result<Token, ParseError>
    where
        P: TokenPattern,
    {
        let tok = self.current.clone()?;

        if pattern.matches(tok.kind()) {
            Ok(tok.clone())
        } else {
            Err(ParseError::UnexpectedToken(tok.clone()))
        }
    }

    pub fn parse(&mut self) -> Result<Expression, ParseError> {
        let tok = self.current.clone()?;

        let parser = match self.prefix_parsers[tok.kind()] {
            None => return Err(ParseError::UnexpectedToken(tok)),
            Some(p) => p,
        };

        let _ = self.advance();
        parser(self, &tok)
    }
}
