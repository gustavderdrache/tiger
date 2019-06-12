use enum_map::EnumMap;
use indexmap::IndexMap;

use crate::{
    expression::*,
    pos::{Span, Spanned},
    token::{Token, TokenData, TokenKind},
};

use super::{ParseError, Parser, Precedence};

type ParserFunction<T> = fn(&mut Parser, &Token) -> Result<T, ParseError>;

pub(crate) type PrefixParser = ParserFunction<Expression>;

const PARSE_NEGATION: PrefixParser = |parser, tok| {
    let expr = parser.parse_expression(Precedence::None)?;
    let span = tok.span.extend(expr.span);

    let data = ExpressionData::UnaryOperation(
        UnaryOperator {
            data: UnaryOperatorData::Negate,
            span: tok.span,
        },
        expr.into(),
    );

    Ok(Expression { data, span })
};

const PARSE_NIL: PrefixParser = |_, tok| {
    Ok(Expression {
        data: ExpressionData::Nil,
        span: tok.span,
    })
};

const PARSE_BREAK: PrefixParser = |_, tok| {
    Ok(Expression {
        data: ExpressionData::Break,
        span: tok.span,
    })
};

const PARSE_INTEGER: PrefixParser = |_, tok| {
    let value = match &tok.data {
        TokenData::Integer(value) => *value,
        _ => panic!("Unexpected token data {:?} in parse_integer()", tok.data),
    };

    let data = ExpressionData::Integer(value);
    Ok(Expression {
        data,
        span: tok.span,
    })
};

const PARSE_STRING: PrefixParser = |_, tok| {
    let value = match &tok.data {
        TokenData::String(value) => value.clone(),
        _ => panic!("Unexpected token data {:?} in parse_string()", tok.data),
    };

    let data = ExpressionData::String(value);
    Ok(Expression {
        data,
        span: tok.span,
    })
};

const PARSE_PARENS: PrefixParser = |parser, tok| {
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
        let expression = parser.parse_expression(Precedence::None)?;
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
};

const PARSE_WHILE: PrefixParser = |parser, start_token| {
    let predicate = parser.parse_expression(Precedence::None)?;
    parser.expect(TokenKind::KwDo)?;
    let body = parser.parse_expression(Precedence::None)?;

    let end_span = body.span;

    let data = ExpressionData::While {
        predicate: predicate.into(),
        body: body.into(),
    };

    Ok(Expression {
        data,
        span: start_token.span.extend(end_span),
    })
};

const PARSE_FOR: PrefixParser = |parser, start_token| {
    let ident_token = parser.expect(TokenKind::Identifier)?;
    let ident = match &ident_token.data {
        TokenData::Identifier(ident) => ident.to_owned(),
        _ => panic!("Failed to read identifier from token {:?}", ident_token),
    };

    parser.expect(TokenKind::ColonEquals)?;

    let start_expr = parser.parse_expression(Precedence::None)?;

    parser.expect(TokenKind::KwTo)?;

    let end_expr = parser.parse_expression(Precedence::None)?;

    parser.expect(TokenKind::KwDo)?;

    let body = parser.parse_expression(Precedence::None)?;

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
};

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

        let arg = parser.parse_expression(Precedence::None)?;
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
        _ => panic!(
            "Unexpected token data {:?} in parse_record_subscript()",
            subscript_token
        ),
    };

    let data = SubscriptData::Record(value);
    Ok(Subscript { data, span })
}

fn parse_array_subscript(parser: &mut Parser) -> Result<Subscript, ParseError> {
    let start_span = parser.expect(TokenKind::OpenBrack)?.span;
    let subscript = parser.parse_expression(Precedence::None)?;
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

    if parser.matches(TokenKind::ColonEquals) {
        return parse_assignment(parser, lvalue);
    }

    let data = ExpressionData::Lvalue(lvalue);
    Ok(Expression { data, span })
}

fn parse_array_or_subscript(
    parser: &mut Parser,
    type_or_var_name: String,
    start_span: Span,
) -> Result<Expression, ParseError> {
    let subscript_start = parser.expect(TokenKind::OpenBrack)?.span;
    let subscript_or_size = parser.parse_expression(Precedence::None)?;
    let subscript_end = parser.expect(TokenKind::CloseBrack)?.span;

    if parser.matches(TokenKind::KwOf) {
        parser.expect(TokenKind::KwOf)?;

        let init = parser.parse_expression(Precedence::None)?;
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

fn parse_record_init(
    parser: &mut Parser,
    record_type: String,
    start_span: Span,
) -> Result<Expression, ParseError> {
    parser.expect(TokenKind::OpenBrace)?;

    let mut fields = IndexMap::new();

    if !parser.matches(TokenKind::CloseBrace) {
        loop {
            let (field_name, field_token) = expect_identifier(parser)?;
            if fields.contains_key(&field_name) {
                return Err(ParseError::DuplicateField(field_name, field_token.span));
            }

            parser.expect(TokenKind::Colon)?;

            let init = parser.parse_expression(Precedence::None)?;
            fields.insert(field_name, init);

            if parser.matches(TokenKind::CloseBrace) {
                break;
            }

            parser.expect(TokenKind::Comma)?;
        }
    }

    let end_span = parser.expect(TokenKind::CloseBrace)?.span;

    let data = ExpressionData::Record {
        name: record_type,
        fields,
    };
    let span = start_span.extend(end_span);

    Ok(Expression { data, span })
}

fn parse_assignment(parser: &mut Parser, lvalue: Lvalue) -> Result<Expression, ParseError> {
    parser.expect(TokenKind::ColonEquals)?;

    let value = parser.parse_expression(Precedence::None)?;
    let span = lvalue.span.extend(value.span);
    let data = ExpressionData::Assign(lvalue, value.into());
    Ok(Expression { data, span })
}

const PARSE_IDENTIFIER: PrefixParser = |parser, token| {
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

    if parser.matches(TokenKind::OpenBrace) {
        return parse_record_init(parser, value, token.span);
    }

    let lvalue_data = LvalueData {
        name: value,
        subscripts: Vec::new(),
    };
    let lvalue = Lvalue {
        data: lvalue_data,
        span: token.span,
    };

    if parser.matches(TokenKind::ColonEquals) {
        return parse_assignment(parser, lvalue);
    }

    let data = ExpressionData::Lvalue(lvalue);
    Ok(Expression {
        data,
        span: token.span,
    })
};

fn expect_identifier(parser: &mut Parser) -> Result<(String, Token), ParseError> {
    let token = parser.expect(TokenKind::Identifier)?;

    let identifier = match &token.data {
        TokenData::Identifier(ident) => ident.to_owned(),
        _ => panic!(
            "Unexpected token {:?} in expect_identifier()",
            token.kind::<TokenKind>()
        ),
    };

    Ok((identifier, token))
}

type DeclarationParser = ParserFunction<Declaration>;

const PARSE_VAR_DECL: DeclarationParser = |parser, token| {
    let (var_name, _) = expect_identifier(parser)?;

    let type_name = if parser.matches(TokenKind::Colon) {
        parser.expect(TokenKind::Colon)?;
        Some(expect_identifier(parser)?.0)
    } else {
        None
    };

    parser.expect(TokenKind::ColonEquals)?;

    let init = parser.parse_expression(Precedence::None)?;

    let span = token.span.extend(init.span);
    let data = DeclarationData::Variable(Variable {
        span,
        data: VariableData {
            name: var_name,
            ty_dec: type_name,
            init,
        },
    });

    Ok(Declaration { data, span })
};

type TypeParser = ParserFunction<Type>;

const PARSE_TYPE_ALIAS: TypeParser = |_, token| {
    let value = match &token.data {
        TokenData::Identifier(ident) => ident.to_owned(),
        _ => panic!("Unexpected non-identifier token in type alias parser"),
    };

    let span = token.span;
    let data = TypeData::Identifier(value);
    Ok(Type { data, span })
};

const PARSE_RECORD_TYPE: TypeParser = |parser, token| {
    let mut fields = IndexMap::new();

    if !parser.matches(TokenKind::CloseBrace) {
        loop {
            let (field, field_token) = expect_identifier(parser)?;
            if fields.contains_key(&field) {
                return Err(ParseError::DuplicateField(field, field_token.span));
            }

            parser.expect(TokenKind::Colon)?;
            let (value, _) = expect_identifier(parser)?;

            fields.insert(field, value);

            if parser.matches(TokenKind::CloseBrace) {
                break;
            }

            parser.expect(TokenKind::Comma)?;
        }
    }

    let end_span = parser.expect(TokenKind::CloseBrace)?.span;

    let data = TypeData::Record(fields);
    let span = token.span.extend(end_span);
    Ok(Type { data, span })
};

const PARSE_ARRAY_TYPE: TypeParser = |parser, token| {
    parser.expect(TokenKind::KwOf)?;

    let (other_type, other_token) = expect_identifier(parser)?;

    let span = token.span.extend(other_token.span);
    let data = TypeData::Array(other_type);
    Ok(Type { data, span })
};

fn type_parser_map() -> EnumMap<TokenKind, Option<TypeParser>> {
    let mut map = EnumMap::default();

    map[TokenKind::Identifier] = Some(PARSE_TYPE_ALIAS);
    map[TokenKind::OpenBrace] = Some(PARSE_RECORD_TYPE);
    map[TokenKind::KwArray] = Some(PARSE_ARRAY_TYPE);

    map
}

const PARSE_TYPE_DECL: DeclarationParser = |parser, token| {
    let mut declarations = Vec::new();

    let mut current_start_span = token.span;
    let mut current_end_span;

    let map = type_parser_map();

    loop {
        let (name, _) = expect_identifier(parser)?;
        parser.expect(TokenKind::Equals)?;

        let current = parser.current()?;
        let type_parser = match map[current.kind()] {
            None => return Err(ParseError::UnexpectedToken(current)),
            Some(parser) => parser,
        };

        parser.expect(current.kind::<TokenKind>())?;
        let type_def = type_parser(parser, &current)?;
        current_end_span = type_def.span;

        let span = current_start_span.extend(current_end_span);
        let data = TypeDeclData {
            name,
            decl: type_def,
        };
        declarations.push(TypeDecl { data, span });

        if !parser.matches(TokenKind::KwVar) {
            break;
        }

        current_start_span = parser.expect(TokenKind::KwVar)?.span;
    }

    let span = current_start_span.extend(current_end_span);
    let data = DeclarationData::Type(declarations);

    Ok(Declaration { data, span })
};

const PARSE_FUNC_DECL: DeclarationParser = |parser, token| {
    let mut declarations = Vec::new();
    let mut current_start_span = token.span;
    let mut current_end_span;

    loop {
        let mut fields = IndexMap::new();

        let (name, _) = expect_identifier(parser)?;
        parser.expect(TokenKind::OpenParen)?;

        if !parser.matches(TokenKind::CloseParen) {
            loop {
                let (field_name, field_token) = expect_identifier(parser)?;
                if fields.contains_key(&field_name) {
                    return Err(ParseError::DuplicateArgument(field_name, field_token.span));
                }

                parser.expect(TokenKind::Colon)?;
                let (type_name, type_token) = expect_identifier(parser)?;

                fields.insert(
                    field_name,
                    Spanned {
                        data: type_name,
                        span: field_token.span.extend(type_token.span),
                    },
                );

                if parser.matches(TokenKind::CloseParen) {
                    break;
                }

                parser.expect(TokenKind::Comma)?;
            }
        }

        parser.expect(TokenKind::CloseParen)?;

        let ret_type = if parser.matches(TokenKind::Colon) {
            parser.expect(TokenKind::Colon)?;
            Some(expect_identifier(parser)?.0)
        } else {
            None
        };

        parser.expect(TokenKind::Equals)?;

        let body = parser.parse_expression(Precedence::None)?;
        current_end_span = body.span;

        let span = current_start_span.extend(body.span);
        let data = FunctionData {
            name,
            fields,
            ret_type,
            body,
        };

        declarations.push(Function { data, span });

        if !parser.matches(TokenKind::KwFunction) {
            break;
        }

        current_start_span = parser.expect(TokenKind::KwFunction)?.span;
    }

    let span = token.span.extend(current_end_span);
    let data = DeclarationData::Function(declarations);
    Ok(Declaration { data, span })
};

fn declaration_token_map() -> EnumMap<TokenKind, Option<DeclarationParser>> {
    let mut map = EnumMap::default();

    map[TokenKind::KwType] = Some(PARSE_TYPE_DECL);
    map[TokenKind::KwVar] = Some(PARSE_VAR_DECL);
    map[TokenKind::KwFunction] = Some(PARSE_FUNC_DECL);

    map
}

const PARSE_LET_IN: PrefixParser = |parser, token| {
    let decl_map = declaration_token_map();

    let mut declarations = Vec::new();

    loop {
        if parser.matches(TokenKind::KwIn) {
            break;
        }

        let current = parser.current()?;
        let decl_parser = match decl_map[current.kind()] {
            None => return Err(ParseError::UnexpectedToken(current)),
            Some(func) => func,
        };

        parser.expect(current.kind::<TokenKind>())?;
        declarations.push(decl_parser(parser, &current)?);
    }

    parser.expect(TokenKind::KwIn)?;

    let mut body = Vec::new();

    if !parser.matches(TokenKind::KwEnd) {
        loop {
            body.push(parser.parse_expression(Precedence::None)?);
            if parser.matches(TokenKind::Semicolon) {
                parser.expect(TokenKind::Semicolon)?;
                continue;
            }

            if parser.matches(TokenKind::KwEnd) {
                break;
            }
        }
    }

    let end_span = parser.expect(TokenKind::KwEnd)?.span;

    let data = ExpressionData::Let(declarations, body);
    let span = token.span.extend(end_span);
    Ok(Expression { data, span })
};

const PARSE_IF: PrefixParser = |parser, token| {
    let predicate = parser.parse_expression(Precedence::None)?;

    parser.expect(TokenKind::KwThen)?;

    let consequent = parser.parse_expression(Precedence::None)?;

    let alternative = if parser.matches(TokenKind::KwElse) {
        parser.expect(TokenKind::KwElse)?;
        let alternative = parser.parse_expression(Precedence::None)?;
        Some(Box::new(alternative))
    } else {
        None
    };

    let span = token.span.extend(match &alternative {
        Some(expr) => expr.span,
        None => consequent.span,
    });

    let data = ExpressionData::If {
        predicate: predicate.into(),
        consequent: consequent.into(),
        alternative,
    };

    Ok(Expression { data, span })
};

pub(crate) fn prefix_token_map() -> EnumMap<TokenKind, Option<PrefixParser>> {
    let mut map = EnumMap::default();

    map[TokenKind::Minus] = Some(PARSE_NEGATION);
    map[TokenKind::KwNil] = Some(PARSE_NIL);
    map[TokenKind::KwBreak] = Some(PARSE_BREAK);
    map[TokenKind::String] = Some(PARSE_STRING);
    map[TokenKind::Integer] = Some(PARSE_INTEGER);
    map[TokenKind::OpenParen] = Some(PARSE_PARENS);
    map[TokenKind::KwWhile] = Some(PARSE_WHILE);
    map[TokenKind::KwFor] = Some(PARSE_FOR);
    map[TokenKind::Identifier] = Some(PARSE_IDENTIFIER);
    map[TokenKind::KwLet] = Some(PARSE_LET_IN);
    map[TokenKind::KwIf] = Some(PARSE_IF);

    map
}
