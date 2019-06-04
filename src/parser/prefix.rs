use enum_map::EnumMap;

use crate::{
    expression::*,
    pos::Span,
    token::{Token, TokenData, TokenKind},
};

use super::{ParseError, Parser};

pub type PrefixParser = &'static dyn Fn(&mut Parser, &Token) -> Result<Expression, ParseError>;

fn parse_negation(parser: &mut Parser, tok: &Token) -> Result<Expression, ParseError> {
    let expr = parser.parse_expression()?;
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

fn parse_nil(_: &mut Parser, tok: &Token) -> Result<Expression, ParseError> {
    let data = ExpressionData::Nil;

    Ok(Expression {
        data,
        span: tok.span,
    })
}

fn parse_break(_: &mut Parser, tok: &Token) -> Result<Expression, ParseError> {
    let data = ExpressionData::Break;

    Ok(Expression {
        data,
        span: tok.span,
    })
}

fn parse_integer(_: &mut Parser, tok: &Token) -> Result<Expression, ParseError> {
    let value = match &tok.data {
        TokenData::Integer(value) => value,
        _ => panic!("Unexpected token data {:?} in parse_integer()", tok.data),
    };

    let data = ExpressionData::Integer(*value);
    Ok(Expression {
        data,
        span: tok.span,
    })
}

fn parse_string(_: &mut Parser, tok: &Token) -> Result<Expression, ParseError> {
    let value = match &tok.data {
        TokenData::String(value) => value.clone(),
        _ => panic!("Unexpected token data {:?} in parse_string()", tok.data),
    };

    let data = ExpressionData::String(value);
    Ok(Expression {
        data,
        span: tok.span,
    })
}

fn parse_parens(parser: &mut Parser, tok: &Token) -> Result<Expression, ParseError> {
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
        let expression = parser.parse_expression()?;
        expressions.push(expression);

        if !parser.matches(TokenKind::Semicolon) {
            break;
        }

        parser.expect(TokenKind::Semicolon)?;
    }

    let end = parser.expect(TokenKind::CloseParen)?;

    if expressions.len() != 1 {
        return Ok(Expression {
            data: ExpressionData::Sequence(expressions),
            span: tok.span.extend(end.span),
        });
    }

    Ok(expressions.pop().unwrap())
}

fn parse_while(parser: &mut Parser, start_token: &Token) -> Result<Expression, ParseError> {
    let predicate = parser.parse_expression()?;
    parser.expect(TokenKind::KwDo)?;
    let body = parser.parse_expression()?;

    let end_span = body.span;

    let data = ExpressionData::While {
        predicate: predicate.into(),
        body: body.into(),
    };

    Ok(Expression {
        data,
        span: start_token.span.extend(end_span),
    })
}

fn parse_for(parser: &mut Parser, start_token: &Token) -> Result<Expression, ParseError> {
    let ident_token = parser.expect(TokenKind::Identifier)?;
    let ident = match &ident_token.data {
        TokenData::Identifier(ident) => ident.to_owned(),
        _ => panic!("Failed to read identifier from token {:?}", ident_token),
    };

    parser.expect(TokenKind::ColonEquals)?;

    let start_expr = parser.parse_expression()?;

    parser.expect(TokenKind::KwTo)?;

    let end_expr = parser.parse_expression()?;

    parser.expect(TokenKind::KwDo)?;

    let body = parser.parse_expression()?;

    let end_span = body.span;

    let data = ExpressionData::For {
        binding: ident,
        lower: start_expr.into(),
        upper: end_expr.into(),
        body: body.into(),
    };

    Ok(Expression {
        data,
        span: start_token.span.extend(end_span),
    })
}

fn parse_function_list(
    parser: &mut Parser,
    ident: String,
    start_span: Span,
) -> Result<Expression, ParseError> {
    parser.expect(TokenKind::OpenParen)?;

    let mut args = Vec::new();

    loop {
        if parser.matches(TokenKind::CloseParen) {
            break;
        }

        let arg = parser.parse_expression()?;
        args.push(arg);

        if parser.matches(TokenKind::Comma) {
            parser.expect(TokenKind::Comma)?;
            continue;
        }

        break;
    }

    let end_token = parser.expect(TokenKind::CloseParen)?;

    let data = ExpressionData::FunctionCall(ident, args);
    Ok(Expression {
        data,
        span: start_span.extend(end_token.span),
    })
}

fn parse_record_subscript(parser: &mut Parser) -> Result<Subscript, ParseError> {
    let start_span = parser.expect(TokenKind::Dot)?.span;

    let subscript_token = parser.expect(TokenKind::Identifier)?;

    let span = start_span.extend(subscript_token.span);

    let value = match subscript_token.data {
        TokenData::Identifier(ident) => ident,
        _ => panic!("Unexpected token data {:?} in parse_record_subscript()", subscript_token),
    };

    let data = SubscriptData::Record(value);
    Ok(Subscript { data, span })
}

fn parse_array_subscript(parser: &mut Parser) -> Result<Subscript, ParseError> {
    let start_span = parser.expect(TokenKind::OpenBrack)?.span;
    let subscript = parser.parse_expression()?;
    let end_span = parser.expect(TokenKind::CloseBrack)?.span;

    let data = SubscriptData::Array(subscript);
    Ok(Subscript {
        data,
        span: start_span.extend(end_span),
    })
}

fn parse_subscripts(
    parser: &mut Parser,
    ident: String,
    mut subscripts: Vec<Subscript>,
    start_span: Span,
) -> Result<Expression, ParseError> {
    let mut end_span = start_span;

    loop {
        if parser.matches(TokenKind::Dot) {
            let subscript = parse_record_subscript(parser)?;
            end_span = subscript.span;
            subscripts.push(subscript);
            continue;
        }

        if parser.matches(TokenKind::OpenBrack) {
            let subscript = parse_array_subscript(parser)?;
            end_span = subscript.span;
            subscripts.push(subscript);
            continue;
        }

        break;
    }

    let span = start_span.extend(end_span);

    let data = LvalueData {
        name: ident,
        subscripts,
    };
    let lvalue = Lvalue { data, span };

    let data = ExpressionData::Lvalue(lvalue);
    Ok(Expression { data, span })
}

fn parse_array_or_subscript(
    parser: &mut Parser,
    type_or_var_name: String,
    start_span: Span,
) -> Result<Expression, ParseError> {
    let subscript_start = parser.expect(TokenKind::OpenBrack)?.span;
    let subscript_or_size = parser.parse_expression()?;
    let subscript_end = parser.expect(TokenKind::CloseBrack)?.span;

    if parser.matches(TokenKind::KwOf) {
        parser.expect(TokenKind::KwOf)?;

        let init = parser.parse_expression()?;
        let end_span = init.span;

        let data = ExpressionData::Array {
            name: type_or_var_name,
            size: subscript_or_size.into(),
            init: init.into(),
        };
        return Ok(Expression {
            data,
            span: start_span.extend(end_span),
        });
    }

    let subscript_data = SubscriptData::Array(subscript_or_size);
    let subscript = Subscript {
        data: subscript_data,
        span: subscript_start.extend(subscript_end),
    };

    parse_subscripts(parser, type_or_var_name, vec![subscript], start_span)
}

fn parse_identifier(parser: &mut Parser, token: &Token) -> Result<Expression, ParseError> {
    let value = match &token.data {
        TokenData::Identifier(ident) => ident.to_owned(),
        _ => panic!("Unexpected token data {:?} in parse_identifier()", token),
    };

    if parser.matches(TokenKind::OpenParen) {
        return parse_function_list(parser, value, token.span);
    }

    if parser.matches(TokenKind::Dot) {
        return parse_subscripts(parser, value, Vec::new(), token.span);
    }

    if parser.matches(TokenKind::OpenBrack) {
        return parse_array_or_subscript(parser, value, token.span);
    }

    let lvalue_data = LvalueData {
        name: value,
        subscripts: Vec::new(),
    };
    let lvalue = Lvalue {
        data: lvalue_data,
        span: token.span,
    };

    let data = ExpressionData::Lvalue(lvalue);
    Ok(Expression {
        data,
        span: token.span,
    })
}

pub fn prefix_token_map() -> EnumMap<TokenKind, Option<PrefixParser>> {
    let mut map = EnumMap::default();

    map[TokenKind::Minus] = Some(&parse_negation as PrefixParser);
    map[TokenKind::KwNil] = Some(&parse_nil);
    map[TokenKind::KwBreak] = Some(&parse_break);
    map[TokenKind::String] = Some(&parse_string);
    map[TokenKind::Integer] = Some(&parse_integer);
    map[TokenKind::OpenParen] = Some(&parse_parens);
    map[TokenKind::KwWhile] = Some(&parse_while);
    map[TokenKind::KwFor] = Some(&parse_for);
    map[TokenKind::Identifier] = Some(&parse_identifier);

    map
}
