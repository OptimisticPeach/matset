use std::collections::HashMap;

use super::{EvalContext, value::Value};
use crate::ast::{Expr, ExprNode, FunctionDef, IdentId, NodeId};

use anyhow::{Context, Result, bail};

pub fn eval(expr: &Expr, ctx: &EvalContext) -> Result<Value> {
    eval_inner(&expr.nodes, expr.root, ctx, &HashMap::new())
}

fn eval_inner(
    nodes: &[ExprNode],
    root: NodeId,
    ctx: &EvalContext,
    locals: &HashMap<IdentId, Value>,
) -> Result<Value> {
    let node = &nodes[root.0];

    match node {
        ExprNode::Constant(value) => Ok(value.clone()),
        ExprNode::Ident(ident_id) => ctx
            .variables
            .get(ident_id)
            .map(|x| eval_inner(&x.value.nodes, x.value.root, ctx, locals))
            .or_else(|| locals.get(ident_id).cloned().map(Ok))
            .with_context(|| {
                if ctx.functions.get(ident_id).is_some() {
                    format!(
                        "Name is a function, not a variable: {}",
                        ctx.idents.reverse[ident_id.0]
                    )
                } else {
                    "Name is undefined!".to_string()
                }
            })?,
        ExprNode::Binary(lhs, rhs, op) => {
            let lhs = eval_inner(nodes, *lhs, ctx, locals)?;
            let rhs = eval_inner(nodes, *rhs, ctx, locals)?;

            match op {
                crate::ast::BinaryOp::Add => Ok(lhs + rhs),
                crate::ast::BinaryOp::Sub => Ok(lhs - rhs),
                crate::ast::BinaryOp::Mul => Ok(lhs * rhs),
                crate::ast::BinaryOp::Div => Ok(lhs / rhs),
                crate::ast::BinaryOp::Pow => todo!(),
                crate::ast::BinaryOp::NthRoot => todo!(),
                crate::ast::BinaryOp::Mod => todo!(),
                crate::ast::BinaryOp::Min => todo!(),
                crate::ast::BinaryOp::Max => todo!(),
            }
        }
        ExprNode::Unary(term, op) => {
            let term = eval_inner(nodes, *term, ctx, locals)?;

            match op {
                crate::ast::UnaryOp::Sqrt => todo!(),
                crate::ast::UnaryOp::Neg => Ok(-term),
                crate::ast::UnaryOp::Sin => todo!(),
                crate::ast::UnaryOp::Cos => todo!(),
                crate::ast::UnaryOp::Tan => todo!(),
            }
        }
        ExprNode::Parens { prefix, args } => {
            if let Some(prefix) = prefix {
                let node = &nodes[prefix.0];

                match node {
                    ExprNode::Ident(id) => {
                        if let Some(var) = ctx
                            .variables
                            .get(id)
                            .map(|x| eval(&x.value, ctx))
                            .or_else(|| locals.get(id).cloned().map(Ok))
                        {
                            let lhs = var?;

                            let [arg] = &**args else {
                                bail!("Points are not supported!")
                            };

                            Ok(lhs * eval_inner(nodes, *arg, ctx, locals)?)
                        } else if let Some(FunctionDef { params, body, .. }) = ctx.functions.get(id)
                        {
                            if params.len() != args.len() {
                                bail!(
                                    "Number of parameters is not equal to number of arguments provided!"
                                )
                            }

                            let args = args
                                .iter()
                                .zip(params.iter())
                                .map(|(node, id)| {
                                    let val = eval_inner(nodes, *node, ctx, locals)?;

                                    Ok((*id, val))
                                })
                                .collect::<Result<HashMap<IdentId, Value>>>()?;

                            let val = eval_inner(&body.nodes, body.root, ctx, &args)?;

                            Ok(val)
                        } else {
                            bail!("Function not found!")
                        }
                    }
                    _ => {
                        let lhs = eval_inner(nodes, *prefix, ctx, locals)?;
                        let [arg] = &**args else {
                            bail!("Points are not supported!")
                        };

                        Ok(lhs * eval_inner(nodes, *arg, ctx, locals)?)
                    }
                }
            } else {
                let [arg] = &**args else {
                    bail!("Points are not supported!")
                };

                eval_inner(nodes, *arg, ctx, locals)
            }
        }
        ExprNode::Matrix { rows, cols, elems } => {
            let new_elems = elems
                .iter()
                .map(|x| eval_inner(nodes, *x, ctx, locals))
                .map(|x| {
                    x.and_then(|y| match y {
                        Value::Real(r) => Ok(r.into()),
                        Value::Complex(c) => Ok(c),
                        Value::Mat(_) => bail!("Unexpected matrix in matrix!"),
                    })
                })
                .collect::<Result<Vec<_>>>()?;

            Ok(Value::Mat(super::value::mat::Matrix {
                rows: *rows as _,
                cols: *cols as _,
                data: new_elems,
            }))
        }
    }
}
