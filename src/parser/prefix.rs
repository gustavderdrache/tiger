use enum_map::EnumMap;

use crate::{
    expression::*,
    token::{Token, TokenData, TokenKind},
};

use super::{ParseError, Parser};

pub type PrefixParser = &'static dyn Fn(&mut Parser, &Token) -> Result<Expression, ParseError>;

fn prefix_parser_negation(parser: &mut Parser, tok: &Token) -> Result<Expression, ParseError> {
    let expr = parser.parse()?;
    let span = tok.span.extend(expr.span);

    let data = ExpressionData::UnaryOperation(
        UnaryOperator {
            data: UnaryOperatorData::Negate,
            span: tok.span,
        },
        expr.into(),
    );

    Ok(Expression { data, span })
}

fn prefix_parser_nil(_: &mut Parser, tok: &Token) -> Result<Expression, ParseError> {
    let data = ExpressionData::Nil;

    Ok(Expression {
        data,
        span: tok.span,
    })
}

fn prefix_parser_break(_: &mut Parser, tok: &Token) -> Result<Expression, ParseError> {
    let data = ExpressionData::Break;

    Ok(Expression {
        data,
        span: tok.span,
    })
}

fn prefix_parser_integer(_: &mut Parser, tok: &Token) -> Result<Expression, ParseError> {
    let value = match &tok.data {
        TokenData::Integer(value) => value,
        _ => panic!(
            "Unexpected token data {:?} in prefix_parser_integer()",
            tok.data
        ),
    };

    let data = ExpressionData::Integer(*value);
    Ok(Expression {
        data,
        span: tok.span,
    })
}

fn prefix_parser_string(_: &mut Parser, tok: &Token) -> Result<Expression, ParseError> {
    let value = match &tok.data {
        TokenData::String(value) => value.clone(),
        _ => panic!(
            "Unexpected token data {:?} in prefix_parser_string()",
            tok.data
        ),
    };

    let data = ExpressionData::String(value);
    Ok(Expression {
        data,
        span: tok.span,
    })
}

fn prefix_parser_parens(parser: &mut Parser, tok: &Token) -> Result<Expression, ParseError> {
    if parser.matches(TokenKind::CloseParen) {
        let close = parser.expect(TokenKind::CloseParen)?;

        let span = tok.span.extend(close.span);
        return Ok(Expression {
            data: ExpressionData::Void,
            span,
        });
    }

    let mut expressions = Vec::new();

    loop {
        let expression = parser.parse()?;
        expressions.push(expression);

        if !parser.matches(TokenKind::Semicolon) {
            break;
        }

        parser.expect(TokenKind::Semicolon)?;
        parser.advance()?;
    }

    let end = parser.expect(TokenKind::CloseParen)?;

    if expressions.len() != 1 {
        return Ok(Expression {
            data: ExpressionData::Sequence(expressions),
            span: tok.span.extend(end.span),
        })
    }

    Ok(expressions.pop().unwrap())
}

pub fn prefix_token_map() -> EnumMap<TokenKind, Option<PrefixParser>> {
    let mut map = EnumMap::default();

    map[TokenKind::Minus] = Some(&prefix_parser_negation as PrefixParser);
    map[TokenKind::KwNil] = Some(&prefix_parser_nil);
    map[TokenKind::KwBreak] = Some(&prefix_parser_break);
    map[TokenKind::String] = Some(&prefix_parser_string);
    map[TokenKind::Integer] = Some(&prefix_parser_integer);
    map[TokenKind::OpenParen] = Some(&prefix_parser_parens);

    map
}
