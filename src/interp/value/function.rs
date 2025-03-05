use std::collections::HashMap;

use anyhow::{Result, bail};
use serde::{Deserialize, Serialize};

use crate::{
    ast::{BinaryOp, IdentId, NodeId, UnaryOp},
    interp::{EvalContext, eval::eval_inner},
};

use super::{Value, complex::Complex, real::Real};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum BuiltinUnary {
    Sin,
    Cos,
    Tan,
}

impl BuiltinUnary {
    pub fn apply_on(self, params: &[Value]) -> Result<Value> {
        let [param] = params else {
            bail!("Unexpected number of args to unary functions {self:?}!")
        };

        match param {
            Value::Real(real) => Ok(self.apply_real(*real)),
            Value::Complex(complex) => Ok(self.apply_complex(*complex)),
            Value::Mat(_) => unimplemented!(),
            Value::Function(_) => unimplemented!(),
        }
    }

    pub fn apply_real(self, val: Real) -> Value {
        match self {
            BuiltinUnary::Sin => val.sin(),
            BuiltinUnary::Cos => val.cos(),
            BuiltinUnary::Tan => val.tan(),
        }
        .into()
    }

    pub fn apply_complex(self, val: Complex<Real>) -> Value {
        match self {
            BuiltinUnary::Sin => val.sin(),
            BuiltinUnary::Cos => val.cos(),
            BuiltinUnary::Tan => val.tan(),
        }
        .into()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum BuiltinBinary {}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Function {
    BuiltinUnary(BuiltinUnary),
    BuiltinBinary(BuiltinBinary),
    Closure {
        parameters: Vec<IdentId>,
        captures: HashMap<IdentId, Value>,
        body: NodeId,
    },
    CompositeBinary {
        lhs: Box<Function>,
        rhs: Box<Function>,
        op: BinaryOp,
    },
    CompositeUnary {
        term: Box<Function>,
        op: UnaryOp,
    },
    Composition {
        inner: Box<Function>,
        outer: Box<Function>,
    },
    Constant(Box<Value>),
}

impl Function {
    pub fn eval_on(self, params: &[Value], ctx: &EvalContext) -> Result<Value> {
        match self.arity() {
            None => {}
            Some(x) => {
                if x != params.len() {
                    bail!("Wrong number of parameters!")
                }
            }
        }

        match self {
            Function::BuiltinUnary(op) => op.apply_on(params),
            Function::BuiltinBinary(_) => unimplemented!(),
            Function::Closure {
                parameters,
                captures,
                body,
            } => {
                let mut locals = captures.clone();
                for (val, name) in params.iter().zip(parameters.iter()) {
                    locals.insert(*name, val.clone());
                }
                eval_inner(body, ctx, &locals)
            }
            Function::CompositeBinary { lhs, rhs, op } => {
                op.apply(lhs.eval_on(params, ctx)?, rhs.eval_on(params, ctx)?)
            }
            Function::CompositeUnary { term, op } => op.apply(term.eval_on(params, ctx)?),
            Function::Composition { inner, outer } => {
                outer.eval_on(&[inner.eval_on(params, ctx)?], ctx)
            }
            Function::Constant(value) => Ok(*value),
        }
    }

    fn arity(&self) -> Option<usize> {
        match self {
            Function::BuiltinUnary(_) => Some(1),
            Function::BuiltinBinary(_) => Some(2),
            Function::Closure { parameters, .. } => Some(parameters.len()),
            Function::CompositeBinary { lhs, .. } => lhs.arity(),
            Function::CompositeUnary { term, .. } => term.arity(),
            Function::Composition { inner, .. } => inner.arity(),
            Function::Constant(_) => None,
        }
    }

    pub fn ensure_arity(&self, other: &Self) -> Result<()> {
        let my_arity = self.arity();
        let other_arity = other.arity();

        match (my_arity, other_arity) {
            (Some(x), Some(y)) => {
                if x != y {
                    bail!("Cannot combine functions of different arities: {x}, {y}");
                }

                Ok(())
            }

            (Some(_), None) | (None, Some(_)) => Ok(()),

            (None, None) => {
                bail!("Cannot combine two constants!")
            }
        }
    }

    pub fn bin_op_lhs(self, rhs: Value, op: BinaryOp) -> Result<Self> {
        match rhs {
            Value::Function(rhs) => {
                self.ensure_arity(&rhs)?;

                Ok(Function::CompositeBinary {
                    lhs: self.into(),
                    rhs: rhs.into(),
                    op,
                })
            }

            x => Ok(Function::CompositeBinary {
                lhs: self.into(),
                rhs: Function::Constant(x.into()).into(),
                op,
            }),
        }
    }

    pub fn bin_op_rhs(self, lhs: Value, op: BinaryOp) -> Result<Self> {
        match lhs {
            Value::Function(lhs) => {
                self.ensure_arity(&lhs)?;

                Ok(Function::CompositeBinary {
                    lhs: lhs.into(),
                    rhs: self.into(),
                    op,
                })
            }

            x => Ok(Function::CompositeBinary {
                lhs: Function::Constant(x.into()).into(),
                rhs: self.into(),
                op,
            }),
        }
    }

    pub fn unary_op(self, op: UnaryOp) -> Self {
        Function::CompositeUnary {
            term: self.into(),
            op,
        }
    }

    pub fn composition_of(self, inner: Function) -> Result<Self> {
        match self.arity() {
            None => panic!("This should not happen!"),
            Some(1) => {}
            _ => bail!("Arity of lhs of composition should be 1."),
        }

        Ok(Function::Composition {
            inner: inner.into(),
            outer: self.into(),
        })
    }
}
