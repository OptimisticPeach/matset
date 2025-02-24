use crate::interp::value::Value;

pub struct NodeId(pub usize);

pub struct Expr {
    pub nodes: Vec<ExprNode>,
    pub root: NodeId,
}

pub enum ExprNode {
    Constant(Value),
    Ident(IdentId),
    Binary(NodeId, NodeId, BinaryOp),
    Unary(NodeId, UnaryOp),
    Parens {
        prefix: Option<NodeId>,
        args: Vec<NodeId>,
    },
}

pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    NthRoot,
    Mod,
    Min,
    Max,
}

pub enum UnaryOp {
    Sqrt,
    Neg,
    Sin,
    Cos,
    Tan,
}

pub struct IdentId(pub usize);

pub enum ParsedExpr {
    /// `f(x) = 2x`
    Function(FunctionDef),
    /// `x = 2`
    Variable(VariableDef),
    /// `f(2) + 2`
    Evaluate(Expr),
    Empty,
}

pub struct VariableDef {
    name: IdentId,
    value: Expr,
}

pub struct FunctionDef {
    name: IdentId,
    params: Vec<IdentId>,
    body: Expr,
}
