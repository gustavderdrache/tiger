use enum_map::EnumMap;

use crate::{
    expression::*,
    token::{Token, TokenKind},
};

use super::{Associativity, ParseError, Parser, Precedence};

#[derive(Debug, Clone, Copy)]
pub struct InfixParser {
    pub operator: BinaryOperatorData,
    pub precedence: Precedence,
    pub associativity: Associativity,
}

impl InfixParser {
    pub fn parse(
        &self,
        parser: &mut Parser,
        lhs: Expression,
        token: &Token,
    ) -> Result<Expression, ParseError> {
        let rhs = parser.parse_expression(self.precedence)?;

        let span = lhs.span.extend(rhs.span);
        let data = ExpressionData::BinaryOperation {
            lhs: lhs.into(),
            rhs: rhs.into(),
            op: BinaryOperator {
                data: self.operator,
                span: token.span,
            },
        };

        Ok(Expression { data, span })
    }
}

pub fn infix_token_map() -> EnumMap<TokenKind, Option<InfixParser>> {
    let mut map = EnumMap::default();

    macro_rules! op {
        ($kind:tt, $op:tt, $assoc:tt, $prec:tt) => {
            map[TokenKind::$kind] = Some(InfixParser {
                operator: BinaryOperatorData::$op,
                associativity: Associativity::$assoc,
                precedence: Precedence::$prec,
            });
        };
    }

    op!(VerticalBar, Or, Left, Or);

    op!(Ampersand, And, Left, And);

    op!(Equals, Equal, None, Equality);
    op!(NotEquals, NotEqual, None, Equality);
    op!(GreaterThan, Greater, None, Equality);
    op!(GreaterThanEquals, GreaterEqual, None, Equality);
    op!(LessThan, Less, None, Equality);
    op!(LessThanEquals, LessEqual, None, Equality);

    op!(Plus, Add, Left, Additive);
    op!(Minus, Subtract, Left, Additive);

    op!(Star, Multiply, Left, Multiplicative);
    op!(Slash, Divide, Left, Multiplicative);

    map
}
