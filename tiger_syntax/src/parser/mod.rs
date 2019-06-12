use std::fmt;

use enum_map::EnumMap;

use crate::{
    expression::*,
    lexer::{LexError, Lexer},
    pos::Span,
    token::{Token, TokenKind},
};

mod infix;
mod prefix;

use infix::{infix_token_map, InfixParser};
use prefix::{prefix_token_map, PrefixParser};

#[derive(Debug, Clone)]
pub enum ParseError {
    LexicalError(LexError),
    UnexpectedToken(Token),
    DuplicateArgument(String, Span),
    DuplicateField(String, Span),
}

impl From<LexError> for ParseError {
    fn from(e: LexError) -> Self {
        ParseError::LexicalError(e)
    }
}

pub(crate) trait TokenPattern: fmt::Debug {
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

#[derive(Debug, PartialOrd, Ord, Eq, PartialEq, Clone, Copy)]
pub(crate) enum Precedence {
    None,

    // Operators
    Or,
    And,
    Equality,
    Additive,
    Multiplicative,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum Associativity {
    None,
    Left,
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    infix_parsers: EnumMap<TokenKind, Option<InfixParser>>,
    prefix_parsers: EnumMap<TokenKind, Option<PrefixParser>>,
    current: Result<Token, ParseError>,
}

impl<'a> fmt::Debug for Parser<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Parser")
            .field("lexer", &self.lexer)
            .field("current", &self.current)
            .finish()
    }
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer::new(input);
        let current = lexer.next().unwrap().map_err(Into::into);

        Self {
            lexer,
            infix_parsers: infix_token_map(),
            prefix_parsers: prefix_token_map(),
            current,
        }
    }

    pub(crate) fn advance(&mut self) -> Result<Token, ParseError> {
        self.current = self.lexer.next().unwrap().map_err(Into::into);
        self.current.clone()
    }

    pub fn current(&self) -> Result<Token, ParseError> {
        self.current.clone()
    }

    fn current_precedence(&self) -> Precedence {
        let kind = match &self.current {
            Err(_) => return Precedence::None,
            Ok(tok) => tok.kind(),
        };

        match &self.infix_parsers[kind] {
            Some(infix) => infix.precedence,
            None => Precedence::None,
        }
    }

    pub(crate) fn matches<P>(&mut self, pattern: P) -> bool
    where
        P: TokenPattern,
    {
        match &self.current {
            Ok(tok) => pattern.matches(tok.kind()),
            Err(_) => false,
        }
    }

    pub(crate) fn expect<P>(&mut self, pattern: P) -> Result<Token, ParseError>
    where
        P: TokenPattern,
    {
        let tok = self.current()?;

        if pattern.matches(tok.kind()) {
            let _ = self.advance()?;
            Ok(tok.clone())
        } else {
            Err(ParseError::UnexpectedToken(tok.clone()))
        }
    }

    pub(crate) fn parse_expression(
        &mut self,
        precedence: Precedence,
    ) -> Result<Expression, ParseError> {
        let tok = self.current()?;

        let parser = match self.prefix_parsers[tok.kind()] {
            None => return Err(ParseError::UnexpectedToken(tok)),
            Some(p) => p,
        };

        let _ = self.advance();
        let mut lhs = parser(self, &tok)?;

        loop {
            if precedence >= self.current_precedence() {
                break;
            }

            let tok = self.current()?;
            let parser = match self.infix_parsers[tok.kind()] {
                None => return Err(ParseError::UnexpectedToken(tok)),
                Some(p) => p,
            };

            let _ = self.advance();
            lhs = parser.parse(self, lhs, &tok)?;
        }

        Ok(lhs)
    }

    pub fn parse_program(&mut self) -> Result<Expression, ParseError> {
        let expr = self.parse_expression(Precedence::None)?;
        let _ = self.expect(TokenKind::Eof)?;
        Ok(expr)
    }
}
