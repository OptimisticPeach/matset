use std::collections::HashSet;

use crate::{
    ast::{self, Expr, ExprNode, IdentId, NodeId},
    interp::value::Value,
    typst_ast::IntermediateAST,
    util::{NameCache, SymbolClass},
};

use anyhow::{Context, Result, bail};

struct ParseContext<'names> {
    names: &'names mut NameCache,
}

pub fn parse(ast: &IntermediateAST<'_>, names: &mut NameCache) -> Result<ast::ParsedExpr> {
    let mut ctx = ParseContext { names };

    parse_inner(ast, &mut ctx)
}

fn parse_inner(ast: &IntermediateAST<'_>, ctx: &mut ParseContext<'_>) -> Result<ast::ParsedExpr> {}

fn parse_variable_ident(
    lhs_ast: &IntermediateAST<'_>,
    name_processor: impl FnOnce(String) -> Result<ast::IdentId>,
) -> Result<ast::IdentId> {
    match lhs_ast {
        IntermediateAST::Text {
            text,
            class: SymbolClass::Ident,
        } => name_processor(text.to_string()),
        IntermediateAST::Subscript { base, subscript } => {
            let IntermediateAST::Text {
                text: base_text,
                class: SymbolClass::Ident,
            } = &**base
            else {
                bail!("Unexpected variable ident!")
            };

            let IntermediateAST::Text {
                text: subscr_text,
                class: SymbolClass::Ident | SymbolClass::MixedNumberAlpha | SymbolClass::Number,
            } = &**subscript
            else {
                bail!("Subscript is invalid for {base_text}!")
            };

            let new_name = format!("{base_text}_{subscr_text}");

            name_processor(new_name)
        }
        _ => bail!("Unexpected variable ident!"),
    }
}

fn parse_function_signature(
    lhs_ast: &[IntermediateAST<'_>],
    ctx: &mut ParseContext<'_>,
) -> Result<(ast::IdentId, Vec<ast::IdentId>)> {
    let [
        name,
        IntermediateAST::LeftRight {
            left,
            right,
            children,
        },
    ] = lhs_ast
    else {
        bail!("Unexpected function signature: {lhs_ast:?}!")
    };

    if ("(", ")") != (left, right) {
        bail!("Unexpected parentheses: `{left}` and `{right}`")
    };

    let name = parse_variable_ident(name, |x| ctx.names.create_global_id(x))?;

    let Some(children) = children else {
        return Ok((name, vec![]));
    };

    let children = match &**children {
        IntermediateAST::Sequence { children } => {
            let mut params = vec![];

            let mut iter = children.iter();

            let first =
                parse_variable_ident(iter.next().unwrap(), |x| ctx.names.create_local_id(x))?;

            params.push(first);

            loop {
                match iter.next() {
                    None => break,
                    Some(IntermediateAST::Comma) => {}
                    Some(x) => bail!("Unexpected symbol in parameters: {x:?}"),
                }

                let name = iter
                    .next()
                    .context("Cannot end parameter list with comma")?;

                let name = parse_variable_ident(name, |x| ctx.names.create_local_id(x))?;

                params.push(name);
            }

            params
        }
        x => vec![parse_variable_ident(x, |x| ctx.names.create_local_id(x))?],
    };

    Ok((name, children))
}

fn parse_expr(ast: &IntermediateAST<'_>, ctx: &mut ParseContext<'_>) -> Result<Expr> {
    let mut nodes = Vec::new();
}

fn parse_base_term(
    ast: &mut &[IntermediateAST<'_>],
    ctx: &mut ParseContext<'_>,
    nodes: &mut Vec<ExprNode>,
) -> Result<NodeId> {
    let [first, rest @ ..] = ast else {
        bail!("Unexpected end of token stream!")
    };

    match first {
        IntermediateAST::Sequence { children } => todo!(),
        IntermediateAST::Comma => bail!("Unexpected comma in expression!"),
        IntermediateAST::Text { text, class } => match class {
            SymbolClass::GreekOperator => unimplemented!(),
            SymbolClass::BuiltinFunction => unimplemented!(),
            SymbolClass::UnusedSymbol => bail!("Unrecognized symbol: `{text}`"),
            SymbolClass::Ident => {
                let ident = parse_variable_ident(first, |x| ctx.names.get_existing(&x))?;
                Ok(make_ident(ident, nodes))
            }
            SymbolClass::Number => todo!(),
            SymbolClass::MixedNumberAlpha => todo!(),
            SymbolClass::Constant => todo!(),
        },
        IntermediateAST::Frac { num, denom } => {
            parse_binary(ast::BinaryOp::Div, num, denom, ctx, nodes)
        }
        IntermediateAST::Subscript { base, subscript } => todo!(),
        IntermediateAST::Power { base, power } => {
            parse_binary(ast::BinaryOp::Pow, base, power, ctx, nodes)
        }
        IntermediateAST::Prime { .. } => unimplemented!(),
        IntermediateAST::LeftRight {
            left,
            right,
            children,
        } => todo!(),
        IntermediateAST::Root { index, radicand } => {
            if let Some(index) = index {
                parse_binary(ast::BinaryOp::NthRoot, index, radicand, ctx, nodes)
            } else {
                parse_unary(ast::UnaryOp::Sqrt, radicand, ctx, nodes)
            }
        }
        IntermediateAST::Binomial { .. } => unimplemented!(),
    }
}

fn parse_binary(
    op: ast::BinaryOp,
    lhs: &IntermediateAST<'_>,
    rhs: &IntermediateAST<'_>,
    ctx: &mut ParseContext<'_>,
    nodes: &mut Vec<ExprNode>,
) -> Result<NodeId> {
    let lhs = parse_base_term(&mut std::slice::from_ref(lhs), ctx, nodes)?;
    let rhs = parse_base_term(&mut std::slice::from_ref(rhs), ctx, nodes)?;

    let node = ExprNode::Binary(lhs, rhs, op);

    let id = nodes.len();

    nodes.push(node);

    Ok(NodeId(id))
}

fn parse_unary(
    op: ast::UnaryOp,
    ast: &IntermediateAST<'_>,
    ctx: &mut ParseContext<'_>,
    nodes: &mut Vec<ExprNode>,
) -> Result<NodeId> {
    let ast = parse_base_term(&mut std::slice::from_ref(ast), ctx, nodes)?;

    let node = ExprNode::Unary(ast, op);

    let id = nodes.len();

    nodes.push(node);

    Ok(NodeId(id))
}

fn make_ident(id: IdentId, nodes: &mut Vec<ExprNode>) -> NodeId {
    let len = nodes.len();

    nodes.push(ExprNode::Ident(id));

    NodeId(len)
}
