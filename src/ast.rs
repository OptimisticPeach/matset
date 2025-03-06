use std::collections::HashMap;

use anyhow::Result;

use serde::{Deserialize, Serialize};

use crate::interp::value::{Value, function::Function, rational::Rational, real::Real};

#[derive(Debug, PartialEq, Copy, Clone, Serialize, Deserialize)]
pub struct NodeId(pub usize);

impl NodeId {
    pub fn format_into(
        &self,
        nodes: &[ExprNode],
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let node = &nodes[self.0];

        match node {
            ExprNode::Constant(value) => write!(f, "{value:?}"),
            ExprNode::Ident(IdentId(id)) => write!(f, "@{id}"),
            ExprNode::Binary(lhs, rhs, op) => match op {
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul => {
                    lhs.format_into(nodes, f)?;
                    let s = match op {
                        BinaryOp::Add => " + ",
                        BinaryOp::Sub => " - ",
                        BinaryOp::Mul => " * ",
                        _ => panic!("Truly unreachable"),
                    };
                    write!(f, "{s}")?;
                    rhs.format_into(nodes, f)
                }
                BinaryOp::Div | BinaryOp::Pow | BinaryOp::Mod => {
                    write!(f, "(")?;
                    lhs.format_into(nodes, f)?;
                    let s = match op {
                        BinaryOp::Div => "/",
                        BinaryOp::Pow => "^",
                        BinaryOp::Mod => " % ",
                        _ => panic!("Truly unreachable"),
                    };
                    write!(f, "){s}(")?;
                    rhs.format_into(nodes, f)?;
                    write!(f, ")")
                }
                BinaryOp::NthRoot => {
                    write!(f, "nthroot(")?;
                    lhs.format_into(nodes, f)?;
                    write!(f, ", ")?;
                    rhs.format_into(nodes, f)?;
                    write!(f, ")")
                }
            },
            ExprNode::Unary(term, op) => {
                if let UnaryOp::Neg = op {
                    write!(f, "-")?;
                    term.format_into(nodes, f)
                } else {
                    let s = match op {
                        UnaryOp::Sqrt => "sqrt",
                        _ => unreachable!(),
                    };

                    write!(f, "{s}(")?;

                    term.format_into(nodes, f)?;
                    write!(f, ")")
                }
            }
            ExprNode::Parens { prefix, args } => {
                if let Some(prefix) = prefix {
                    prefix.format_into(nodes, f)?;
                }

                write!(f, "(")?;
                for x in &args[..args.len() - 1] {
                    x.format_into(nodes, f)?;
                    write!(f, ", ")?;
                }
                args.last().unwrap().format_into(nodes, f)?;
                write!(f, ")")
            }
            ExprNode::Matrix { rows, cols, elems } => {
                write!(f, "(")?;

                for row in 0..*rows {
                    for col in 0..*cols {
                        elems[col * rows + row].format_into(nodes, f)?;
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
            ExprNode::MakeClosure { params, body, .. } => {
                write!(f, "(")?;
                for (idx, param) in params.iter().enumerate() {
                    write!(f, "@{}", param.0)?;
                    if idx != params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") => ")?;
                body.format_into(nodes, f)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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
    MakeClosure {
        params: Vec<IdentId>,
        captures: Vec<IdentId>,
        body: NodeId,
    },
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize, Deserialize)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    NthRoot,
    Mod,
}

impl BinaryOp {
    pub fn apply(self, lhs: Value, rhs: Value) -> Result<Value> {
        match self {
            BinaryOp::Add => Ok(lhs + rhs),
            BinaryOp::Sub => Ok(lhs - rhs),
            BinaryOp::Mul => Ok(lhs * rhs),
            BinaryOp::Div => Ok(lhs / rhs),
            BinaryOp::Mod => Ok(lhs % rhs),
            BinaryOp::Pow => Ok(lhs.pow(rhs)?),
            BinaryOp::NthRoot => Ok(lhs.pow(rhs.inverse())?),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum UnaryOp {
    Sqrt,
    Neg,
}

impl UnaryOp {
    pub fn apply(self, term: Value) -> Result<Value> {
        match self {
            UnaryOp::Sqrt => term.pow(Value::from(Real::Rational(Rational { num: 1, denom: 2 }))),
            UnaryOp::Neg => Ok(-term),
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, PartialOrd, Serialize, Deserialize, Copy, Clone)]
pub struct IdentId(pub usize);

#[derive(Debug)]
pub enum ParsedExpr {
    /// `f(x) = 2x`
    Function(FunctionDef),
    /// `x = 2`
    Variable(VariableDef),
    None,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct VariableDef {
    pub(crate) name: IdentId,
    pub(crate) value: NodeId,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FunctionDef {
    pub(crate) name: IdentId,
    pub(crate) params: Vec<IdentId>,
    pub(crate) body: NodeId,
}

impl FunctionDef {
    pub fn to_lambda(self) -> Value {
        Value::Function(Function::Closure {
            parameters: self.params,
            captures: HashMap::new(),
            body: self.body,
        })
    }
}
