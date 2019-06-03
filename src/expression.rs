use indexmap::IndexMap;

use crate::pos::Spanned;

#[derive(Debug, Clone)]
pub enum TypeData {
    Identifier(String),
    Record(IndexMap<String, String>),
    Array(String),
}

pub type Type = Spanned<TypeData>;

#[derive(Debug, Clone)]
pub struct VariableData {
    name: String,
    ty_dec: Option<String>,
}

pub type Variable = Spanned<VariableData>;

#[derive(Debug, Clone)]
pub struct FunctionData {
    name: String,
    fields: IndexMap<String, String>,
    ret_type: Option<String>,
}

pub type Function = Spanned<FunctionData>;

#[derive(Debug, Clone)]
pub enum DeclarationData {
    Type(Vec<Type>),
    Variable(Variable),
    Function(Vec<Function>),
}

pub type Declaration = Spanned<DeclarationData>;

#[derive(Debug, Clone)]
pub enum BinaryOperatorData {
    Multiply,
    Divide,
    Add,
    Subtract,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    LessThan,
    LessThanEqual,
    And,
    Or,
}

pub type BinaryOperator = Spanned<BinaryOperatorData>;

#[derive(Debug, Clone)]
pub enum UnaryOperatorData {
    Negate,
}

pub type UnaryOperator = Spanned<UnaryOperatorData>;

#[derive(Debug, Clone)]
pub enum SubscriptData {
    Record(String),
    Array(Expression),
}

pub type Subscript = Spanned<SubscriptData>;

#[derive(Debug, Clone)]
pub struct LvalueData {
    name: String,
    subscripts: Vec<Subscript>,
}

pub type Lvalue = Spanned<LvalueData>;

#[derive(Debug, Clone)]
pub enum ExpressionData {
    Nil,
    Break,
    Void,
    Integer(isize),
    String(String),
    Let(Vec<Declaration>, Vec<Expression>),
    Sequence(Vec<Expression>),
    FunctionCall(String, Vec<Expression>),
    Array {
        name: String,
        size: Box<Expression>,
        init: Box<Expression>,
    },
    BinaryOperation {
        op: BinaryOperator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    UnaryOperation(UnaryOperator, Box<Expression>),
    While {
        predicate: Box<Expression>,
        body: Box<Expression>,
    },
    If {
        predicate: Box<Expression>,
        consequent: Box<Expression>,
        alternative: Option<Box<Expression>>,
    },
    For {
        binding: String,
        lower: Box<Expression>,
        upper: Box<Expression>,
        body: Box<Expression>,
    },
    Lvalue(Lvalue),
}

pub type Expression = Spanned<ExpressionData>;
