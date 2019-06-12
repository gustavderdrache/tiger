use enum_kinds::EnumKind;
use enum_map::Enum;

use crate::pos::Spanned;

#[derive(Debug, Clone, EnumKind)]
#[enum_kind(TokenKind, derive(Enum))]
pub enum TokenData {
    // Values
    String(String),
    Integer(isize),
    Identifier(String),

    // Paired Tokens
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    OpenBrack,
    CloseBrack,

    // Symbols
    Comma,
    Colon,
    Semicolon,
    Dot,
    Plus,
    Minus,
    Star,
    Slash,
    Equals,
    NotEquals,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
    Ampersand,
    VerticalBar,
    ColonEquals,

    // Keywords
    KwWhile,
    KwFor,
    KwTo,
    KwBreak,
    KwLet,
    KwIn,
    KwEnd,
    KwFunction,
    KwVar,
    KwType,
    KwArray,
    KwIf,
    KwThen,
    KwElse,
    KwDo,
    KwOf,
    KwNil,

    // etc.
    Eof,
}

impl TokenData {
    pub fn from_single_char(ch: char) -> Option<Self> {
        match ch {
            '[' => Some(TokenData::OpenBrack),
            ']' => Some(TokenData::CloseBrack),
            '(' => Some(TokenData::OpenParen),
            ')' => Some(TokenData::CloseParen),
            '{' => Some(TokenData::OpenBrace),
            '}' => Some(TokenData::CloseBrace),

            '+' => Some(TokenData::Plus),
            '-' => Some(TokenData::Minus),
            '*' => Some(TokenData::Star),
            '=' => Some(TokenData::Equals),
            '&' => Some(TokenData::Ampersand),
            '|' => Some(TokenData::VerticalBar),
            '/' => Some(TokenData::Slash),

            ',' => Some(TokenData::Comma),
            ';' => Some(TokenData::Semicolon),
            '.' => Some(TokenData::Dot),

            _ => None,
        }
    }

    pub fn from_string(maybe_identifer: String) -> Self {
        match <AsRef<str>>::as_ref(&maybe_identifer) {
            "while" => TokenData::KwWhile,
            "for" => TokenData::KwFor,
            "to" => TokenData::KwTo,
            "break" => TokenData::KwBreak,
            "let" => TokenData::KwLet,
            "in" => TokenData::KwIn,
            "end" => TokenData::KwEnd,
            "function" => TokenData::KwFunction,
            "var" => TokenData::KwVar,
            "type" => TokenData::KwType,
            "array" => TokenData::KwArray,
            "if" => TokenData::KwIf,
            "then" => TokenData::KwThen,
            "else" => TokenData::KwElse,
            "do" => TokenData::KwDo,
            "of" => TokenData::KwOf,
            "nil" => TokenData::KwNil,

            _ => TokenData::Identifier(maybe_identifer),
        }
    }
}

pub(crate) type Token = Spanned<TokenData>;
