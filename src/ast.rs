use serde::{Deserialize, Serialize};

use crate::interp::value::Value;

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub struct NodeId(pub usize);

#[derive(Debug, Serialize, Deserialize)]
pub struct Expr {
    pub nodes: Vec<ExprNode>,
    pub root: NodeId,
}

impl Expr {
    fn format_into(&self, id: &NodeId, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let node = &self.nodes[id.0];

        match node {
            ExprNode::Constant(value) => write!(f, "{value:?}"),
            ExprNode::Ident(IdentId(id)) => write!(f, "@{id}"),
            ExprNode::Binary(lhs, rhs, op) => match op {
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul => {
                    self.format_into(lhs, f)?;
                    let s = match op {
                        BinaryOp::Add => " + ",
                        BinaryOp::Sub => " - ",
                        BinaryOp::Mul => " * ",
                        _ => panic!("Truly unreachable"),
                    };
                    write!(f, "{s}")?;
                    self.format_into(rhs, f)
                }
                BinaryOp::Div | BinaryOp::Pow | BinaryOp::Mod => {
                    write!(f, "(")?;
                    self.format_into(lhs, f)?;
                    let s = match op {
                        BinaryOp::Div => "/",
                        BinaryOp::Pow => "^",
                        BinaryOp::Mod => " % ",
                        _ => panic!("Truly unreachable"),
                    };
                    write!(f, "){s}(")?;
                    self.format_into(rhs, f)?;
                    write!(f, ")")
                }
                BinaryOp::NthRoot => {
                    write!(f, "nthroot(")?;
                    self.format_into(lhs, f)?;
                    write!(f, ", ")?;
                    self.format_into(rhs, f)?;
                    write!(f, ")")
                }
                BinaryOp::Min => todo!(),
                BinaryOp::Max => todo!(),
            },
            ExprNode::Unary(term, op) => {
                if let UnaryOp::Neg = op {
                    write!(f, "-")?;
                    self.format_into(term, f)
                } else {
                    let s = match op {
                        UnaryOp::Sqrt => "sqrt",
                        UnaryOp::Sin => "sin",
                        UnaryOp::Cos => "cos",
                        UnaryOp::Tan => "tan",
                        _ => unreachable!(),
                    };

                    write!(f, "{s}(")?;

                    self.format_into(term, f)?;
                    write!(f, ")")
                }
            }
            ExprNode::Parens { prefix, args } => {
                if let Some(prefix) = prefix {
                    self.format_into(prefix, f)?;
                }

                write!(f, "(")?;
                for x in &args[..args.len() - 1] {
                    self.format_into(x, f)?;
                    write!(f, ", ")?;
                }
                self.format_into(args.last().unwrap(), f)?;
                write!(f, ")")
            }
            ExprNode::Matrix { rows, cols, elems } => {
                write!(f, "(")?;

                for row in 0..*rows {
                    for col in 0..*cols {
                        self.format_into(&elems[col * rows + row], f)?;
                        if col != cols - 1 {
                            write!(f, ", ")?;
                        }
                    }

                    if row != rows - 1 {
                        write!(f, "; ")?;
                    }
                }

                write!(f, ")")
            }
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.format_into(&self.root, f)
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum ExprNode {
    Constant(Value),
    Ident(IdentId),
    Binary(NodeId, NodeId, BinaryOp),
    Unary(NodeId, UnaryOp),
    Matrix {
        rows: usize,
        cols: usize,

        elems: Vec<NodeId>,
    },
    Parens {
        prefix: Option<NodeId>,
        args: Vec<NodeId>,
    },
}

#[derive(Debug, Serialize, Deserialize)]
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

#[derive(Debug, Serialize, Deserialize)]
pub enum UnaryOp {
    Sqrt,
    Neg,
    Sin,
    Cos,
    Tan,
}

#[derive(Debug, Hash, Eq, PartialEq, PartialOrd, Serialize, Deserialize, Copy, Clone)]
pub struct IdentId(pub usize);

#[derive(Debug)]
pub enum ParsedExpr {
    /// `f(x) = 2x`
    Function(FunctionDef),
    /// `x = 2`
    Variable(VariableDef),
}

impl std::fmt::Display for ParsedExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(FunctionDef { name, params, body }) => {
                write!(f, "{name:?}({params:?}) = {body}")
            }
            Self::Variable(VariableDef { name, value }) => {
                write!(f, "{name:?} = {value}")
            }
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct VariableDef {
    pub(crate) name: IdentId,
    pub(crate) value: Expr,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FunctionDef {
    pub(crate) name: IdentId,
    pub(crate) params: Vec<IdentId>,
    pub(crate) body: Expr,
}
