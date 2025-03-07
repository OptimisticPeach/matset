use std::collections::HashMap;

use super::{
    EvalContext,
    value::{Value, function::Function},
};
use crate::ast::{ExprNode, IdentId, NodeId};

use anyhow::{Context, Result, bail};

pub fn eval(expr: NodeId, ctx: &EvalContext) -> Result<Value> {
    eval_inner(expr, ctx, &HashMap::new())
}

pub fn eval_inner(
    root: NodeId,
    ctx: &EvalContext,
    locals: &HashMap<IdentId, Value>,
) -> Result<Value> {
    let node = &ctx.node_store.0[root.0];

    match node {
        ExprNode::Constant(value) => Ok(value.clone()),
        ExprNode::Ident(ident_id) => ctx
            .variables
            .get(&ident_id)
            .map(|x| eval_inner(*x, ctx, locals))
            .or_else(|| locals.get(&ident_id).cloned().map(Ok))
            .context("Name is undefined!")?,
        ExprNode::Binary(lhs, rhs, op) => {
            let lhs = eval_inner(*lhs, ctx, locals)?;
            let rhs = eval_inner(*rhs, ctx, locals)?;
            op.apply(lhs, rhs)
        }
        ExprNode::Unary(term, op) => {
            let term = eval_inner(*term, ctx, locals)?;

            op.apply(term)
        }
        ExprNode::Parens { prefix, args } => {
            if let Some(prefix) = prefix {
                match eval_inner(*prefix, ctx, locals)? {
                    Value::Function(function) => function.eval_on(
                        &args
                            .iter()
                            .map(|x| eval_inner(*x, ctx, locals))
                            .collect::<Result<Vec<_>>>()?,
                        ctx,
                    ),
                    x => {
                        if args.len() != 1 {
                            bail!("Points not supported!")
                        }

                        Ok(x * eval_inner(args[0], ctx, locals)?)
                    }
                }
            } else {
                let [arg] = &**args else {
                    bail!("Points are not supported!")
                };

                eval_inner(*arg, ctx, locals)
            }
        }
        ExprNode::Matrix { rows, cols, elems } => {
            let new_elems = elems
                .iter()
                .map(|x| eval_inner(*x, ctx, locals))
                .map(|x| {
                    x.and_then(|y| match y {
                        Value::Real(r) => Ok(r.into()),
                        Value::Complex(c) => Ok(c),
                        Value::Mat(_) => bail!("Unexpected matrix in matrix!"),
                        Value::Function(_) => bail!("Unexpected function in matrix!"),
                    })
                })
                .collect::<Result<Vec<_>>>()?;

            Ok(Value::Mat(super::value::mat::Matrix {
                rows: *rows as _,
                cols: *cols as _,
                data: new_elems,
            }))
        }
        ExprNode::MakeClosure {
            params,
            captures,
            body,
        } => {
            let mut captured = HashMap::new();

            for capture in captures {
                if let Some(val) = locals.get(capture) {
                    // todo: make an arena/gc ish thing
                    captured.insert(*capture, val.clone());
                }
            }

            Ok(Value::Function(Function::Closure {
                parameters: params.clone(),
                captures: captured,
                body: *body,
            }))
        }
    }
}
