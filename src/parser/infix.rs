use enum_map::EnumMap;

use crate::{
    expression::*,
    lexer::{Lexer, LexError},
    token::{Token, TokenKind},
};

#[derive(Debug, PartialOrd, Ord, Eq, PartialEq, Clone, Copy)]
enum Precedence {
    None,

    // Operators
    Or,
    And,
    Equality,
    Additive,
    Multiplicative,

    // The "of" pseudo-operator
    // Used to make sure that "foo[] of bar" is easy to spot, parse-wise
    Of,

    // record access, subscripts, and calls
    Access,
}
