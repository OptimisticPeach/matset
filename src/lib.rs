use anyhow::Context;
use ast::{NodeId, ParsedExpr};
use interp::{EvalContext, NodeStore, eval, value::Value};
use util::NameCache;
use wasm_minimal_protocol::*;

initiate_protocol!();

pub mod ast;
pub mod interp;
pub mod parse;
pub mod typst_ast;
pub mod util;

fn parse_eqn_inner(
    buf: &[u8],
    cache: &mut NameCache,
    nodes: &mut NodeStore,
) -> anyhow::Result<ParsedExpr> {
    cache.clear_current_locals();
    let parsed = serde_json::from_slice::<typst_ast::TypstAst>(buf)?;

    let Some(intermediate) = parsed.get_intermediate()? else {
        return Ok(ParsedExpr::None);
    };

    let ast = parse::parse_statement(&intermediate, cache, nodes)?;

    Ok(ast)
}

fn parse_expr_inner(
    buf: &[u8],
    cache: &mut NameCache,
    nodes: &mut NodeStore,
) -> anyhow::Result<NodeId> {
    cache.clear_current_locals();
    let parsed = serde_json::from_slice::<typst_ast::TypstAst>(buf)?;

    let intermediate = parsed
        .get_intermediate()?
        .context("Empty Intermediate AST!")?;

    let ast = parse::parse_expr(&intermediate, cache, nodes)?;

    Ok(ast)
}

#[cfg_attr(target_arch = "wasm32", wasm_func)]
pub fn insert(existing: &[u8], eqn: &[u8]) -> Result<Vec<u8>, String> {
    let mut existing = if let [] = existing {
        EvalContext::default()
    } else {
        bincode::deserialize(existing).map_err(|x| x.to_string())?
    };

    let expr = match parse_eqn_inner(eqn, &mut existing.idents, &mut existing.node_store) {
        Ok(x) => x,
        Err(e) => Err(e.to_string())?,
    };

    existing.insert(expr);

    bincode::serialize(&existing).map_err(|x| x.to_string())
}

fn eval_inner(existing: &[u8], expr: &[u8]) -> anyhow::Result<Value> {
    let mut existing = if let [] = existing {
        EvalContext::default()
    } else {
        bincode::deserialize(existing)?
    };

    let expr = parse_expr_inner(expr, &mut existing.idents, &mut existing.node_store)?;

    let result = eval::eval(expr, &existing)?;

    Ok(result)
}

#[cfg_attr(target_arch = "wasm32", wasm_func)]
pub fn evaluate(existing: &[u8], expr: &[u8]) -> Result<Vec<u8>, String> {
    eval_inner(existing, expr)
        .map_err(|x| x.to_string())
        .and_then(|x| serde_json::to_vec(&x).map_err(|x| x.to_string()))
}

#[cfg_attr(target_arch = "wasm32", wasm_func)]
pub fn floateval(existing: &[u8], expr: &[u8]) -> Result<Vec<u8>, String> {
    eval_inner(existing, expr)
        .map(|mut x| {
            x.floatify();
            x.realify()
        })
        .map_err(|x| x.to_string())
        .and_then(|x| serde_json::to_vec(&x).map_err(|x| x.to_string()))
}

#[cfg_attr(target_arch = "wasm32", wasm_func)]
pub fn debug_ctx(ctx: &[u8]) -> Result<Vec<u8>, String> {
    let existing = if let [] = ctx {
        EvalContext::default()
    } else {
        bincode::deserialize(ctx).map_err(|x| x.to_string())?
    };

    let s = format!("{existing}");

    Ok(s.into())
}

#[cfg(test)]
mod tests {
    use crate::{interp::EvalContext, parse_eqn_inner};

    #[test]
    fn parse_basic_func() {
        let mut ctx = EvalContext::default();

        let bytes = include_bytes!("./function_def.json");
        parse_eqn_inner(&bytes[..], &mut ctx.idents, &mut ctx.node_store).unwrap();
    }

    #[test]
    fn basic_eval() {
        let func_bytes = include_bytes!("./function_def.json");
        let reply = super::insert(&[], func_bytes).unwrap();

        let expr_bytes = include_bytes!("./expr_test.json");
        let _reply = super::evaluate(&reply, expr_bytes).unwrap();
    }

    #[test]
    fn decimals() {
        let eval_bytes = include_bytes!("./decimal_test.json");
        let _reply = super::evaluate(&[], eval_bytes).unwrap();
    }

    #[test]
    fn matrix() {
        let eval_bytes = include_bytes!("./mat_test.json");
        let _reply = super::evaluate(&[], eval_bytes).unwrap();
    }
}
