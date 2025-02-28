use crate::{
    ast::{self, Expr, ExprNode, FunctionDef, IdentId, NodeId, VariableDef},
    interp::value::Value,
    typst_ast::IntermediateAST,
    util::{NameCache, SymbolClass},
};

use anyhow::{Result, bail};

struct ParseContext<'names> {
    names: &'names mut NameCache,
}

pub fn parse_statement(
    ast: &IntermediateAST<'_>,
    names: &mut NameCache,
) -> Result<ast::ParsedExpr> {
    let mut ctx = ParseContext { names };

    parse_inner(ast, &mut ctx)
}

pub fn parse_expr(ast: &IntermediateAST<'_>, names: &mut NameCache) -> Result<Expr> {
    let mut ctx = ParseContext { names };

    parse_expr_inner(std::slice::from_ref(ast), &mut ctx)
}

fn parse_inner(ast: &IntermediateAST<'_>, ctx: &mut ParseContext<'_>) -> Result<ast::ParsedExpr> {
    match ast {
        IntermediateAST::Sequence { children } => match &**children {
            [] => bail!("Empty sequence unexpected!"),
            [name, IntermediateAST::Text { text: "≔", .. }, rest @ ..] => {
                let ident = parse_variable_ident(name, |x| ctx.names.create_global_id(x))?;

                let expr = parse_expr_inner(rest, ctx)?;

                Ok(ast::ParsedExpr::Variable(VariableDef {
                    name: ident,
                    value: expr,
                }))
            }

            [
                _name,
                _paren,
                IntermediateAST::Text { text: "≔", .. },
                rest @ ..,
            ] => {
                let (name, params) = parse_function_signature(&children[..2], ctx)?;

                let expr = parse_expr_inner(rest, ctx)?;

                Ok(ast::ParsedExpr::Function(FunctionDef {
                    name,
                    params,
                    body: expr,
                }))
            }
            _ => bail!("Unexpected expression in statement position!"),
        },
        _ => bail!("Unexpected expression in statement position!"),
    }
}

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
                bail!("Unexpected variable ident: {lhs_ast:?}!")
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
        _ => bail!("Unexpected variable: {lhs_ast:?}!"),
    }
}

fn parse_comma_separated<T>(
    ast: &[IntermediateAST<'_>],
    mut parser: impl FnMut(&[IntermediateAST<'_>]) -> Result<T>,
) -> Result<Vec<T>> {
    let mut elems = vec![];

    if ast.len() == 0 {
        return Ok(elems);
    }

    let mut start = 0;

    for end in 0..ast.len() {
        if let IntermediateAST::Comma = ast[end] {
            elems.push(parser(&ast[start..end])?);

            start = end + 1;
        }
    }

    elems.push(parser(&ast[start..])?);

    Ok(elems)
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
        IntermediateAST::Sequence { children } => parse_comma_separated(children, |names| {
            let name = match names {
                [] => bail!("Parameter list cannot end in comma"),
                [name] => name,
                _ => bail!("Paramter list must consist of identifiers!"),
            };

            parse_variable_ident(name, |x| ctx.names.create_local_id(x))
        })?,
        x => vec![parse_variable_ident(x, |x| ctx.names.create_local_id(x))?],
    };

    Ok((name, children))
}

fn parse_expr_inner(ast: &[IntermediateAST<'_>], ctx: &mut ParseContext<'_>) -> Result<Expr> {
    let mut nodes = Vec::new();

    let root = parse_sequence(ast, ctx, &mut nodes)?;

    Ok(Expr { nodes, root })
}

fn parse_impl_prod(
    ast: &mut &[IntermediateAST<'_>],
    ctx: &mut ParseContext<'_>,
    nodes: &mut Vec<ExprNode>,
) -> Result<NodeId> {
    let [first, rest @ ..] = ast else {
        bail!("Unexpected end of ast stream!")
    };

    let mut rest = rest;

    let mut term = parse_base_term(first, ctx, nodes)?;

    loop {
        let [second, new_rest @ ..] = rest else {
            break;
        };

        let Ok(next) = parse_base_term(second, ctx, nodes) else {
            break;
        };

        rest = new_rest;

        if let ExprNode::Parens {
            prefix: prefix @ None,
            ..
        } = &mut nodes[next.0]
        {
            *prefix = Some(term);
            term = next;
        } else {
            let id = nodes.len();

            nodes.push(ExprNode::Binary(term, next, ast::BinaryOp::Mul));

            term = NodeId(id);
        }
    }

    *ast = rest;

    Ok(term)
}

fn parse_expl_prod(
    ast: &mut &[IntermediateAST<'_>],
    ctx: &mut ParseContext<'_>,
    nodes: &mut Vec<ExprNode>,
) -> Result<NodeId> {
    let mut term = parse_impl_prod(ast, ctx, nodes)?;

    while let [plus_minus, rest @ ..] = ast {
        let next_term = match plus_minus {
            IntermediateAST::Text {
                text: "*" | "⋅" | "∗",
                ..
            } => {
                *ast = rest;

                parse_impl_prod(ast, ctx, nodes)?
            }
            _ => return Ok(term),
        };

        let id = nodes.len();

        nodes.push(ExprNode::Binary(term, next_term, ast::BinaryOp::Mul));

        term = NodeId(id);
    }

    Ok(term)
}

fn parse_neg(
    ast: &mut &[IntermediateAST<'_>],
    ctx: &mut ParseContext<'_>,
    nodes: &mut Vec<ExprNode>,
) -> Result<NodeId> {
    let mut is_neg = false;

    while let [
        IntermediateAST::Text {
            text: sign @ ("+" | "-" | "−"),
            ..
        },
        rest @ ..,
    ] = ast
    {
        *ast = rest;
        if *sign != "+" {
            is_neg = !is_neg;
        }
    }

    let expl_prod = parse_expl_prod(ast, ctx, nodes)?;

    if !is_neg {
        return Ok(expl_prod);
    }

    let id = nodes.len();

    nodes.push(ExprNode::Unary(expl_prod, ast::UnaryOp::Neg));

    Ok(NodeId(id))
}

fn parse_sum(
    ast: &mut &[IntermediateAST<'_>],
    ctx: &mut ParseContext<'_>,
    nodes: &mut Vec<ExprNode>,
) -> Result<NodeId> {
    println!("Got here!");
    let mut term = parse_neg(ast, ctx, nodes)?;
    println!("Got here 2 : {ast:?}!");

    while let [plus_minus, rest @ ..] = ast {
        let next_term = match plus_minus {
            IntermediateAST::Text { text: "+", .. } => {
                *ast = rest;

                println!("Parsing plus: {ast:?}");
                parse_neg(ast, ctx, nodes)?
            }
            IntermediateAST::Text {
                text: "-" | "−", ..
            } => parse_neg(ast, ctx, nodes)?,
            _ => return Ok(term),
        };

        let id = nodes.len();

        nodes.push(ExprNode::Binary(term, next_term, ast::BinaryOp::Add));

        term = NodeId(id);
    }

    println!("Got here 3!");

    Ok(term)
}

fn parse_sequence(
    mut ast: &[IntermediateAST<'_>],
    ctx: &mut ParseContext<'_>,
    nodes: &mut Vec<ExprNode>,
) -> Result<NodeId> {
    let result = parse_sum(&mut ast, ctx, nodes)?;

    assert!(ast.is_empty(), "Ast was not entirely consumed: {ast:?}");

    Ok(result)
}

fn parse_base_term(
    ast: &IntermediateAST<'_>,
    ctx: &mut ParseContext<'_>,
    nodes: &mut Vec<ExprNode>,
) -> Result<NodeId> {
    match ast {
        IntermediateAST::Sequence { children } => parse_sequence(children, ctx, nodes),
        IntermediateAST::Comma => bail!("Unexpected comma in expression!"),
        IntermediateAST::Text { text, class } => match class {
            SymbolClass::GreekOperator => unimplemented!(),
            SymbolClass::UnusedSymbol => bail!("Unrecognized symbol: `{text}`"),
            SymbolClass::InfixOperator => bail!("Unexpected infix operator `{text}`"),
            SymbolClass::BuiltinFunction => {
                let ident = parse_variable_ident(ast, |x| ctx.names.make_use_of(x))?;
                let ident = make_ident(ident, nodes);

                Ok(ident)
            }
            SymbolClass::Ident | SymbolClass::Number => {
                if let Some(val) = Value::parse(text) {
                    Ok(make_const(val, nodes))
                } else {
                    let ident = parse_variable_ident(ast, |x| ctx.names.make_use_of(x))?;
                    Ok(make_ident(ident, nodes))
                }
            }
            SymbolClass::MixedNumberAlpha => unimplemented!(),
            SymbolClass::Constant => {
                if let Some(val) = Value::parse(text) {
                    Ok(make_const(val, nodes))
                } else {
                    unimplemented!()
                }
            }
        },
        IntermediateAST::Frac { num, denom } => {
            parse_binary(ast::BinaryOp::Div, num, denom, ctx, nodes)
        }
        IntermediateAST::Subscript { .. } => {
            let ident = parse_variable_ident(ast, |x| ctx.names.make_use_of(x))?;
            Ok(make_ident(ident, nodes))
        }
        IntermediateAST::Power { base, power } => {
            parse_binary(ast::BinaryOp::Pow, base, power, ctx, nodes)
        }
        IntermediateAST::Prime { .. } => unimplemented!(),
        IntermediateAST::LeftRight {
            left,
            right,
            children,
        } => {
            if ("(", ")") != (left, right) {
                bail!("Unexpected parentheses: `{left}`, `{right}`")
            }

            let Some(children) = children else {
                bail!("Empty parentheses are not permitted!")
            };

            let children = match &**children {
                IntermediateAST::Sequence { children } => {
                    parse_comma_separated(children, |x| parse_sequence(x, ctx, nodes))?
                }
                _ => {
                    let children = std::slice::from_ref(&**children);

                    vec![parse_sequence(children, ctx, nodes)?]
                }
            };

            let node = ExprNode::Parens {
                prefix: None,
                args: children,
            };

            let id = nodes.len();

            nodes.push(node);

            Ok(NodeId(id))
        }
        IntermediateAST::Root { index, radicand } => {
            if let Some(index) = index {
                parse_binary(ast::BinaryOp::NthRoot, index, radicand, ctx, nodes)
            } else {
                parse_unary(ast::UnaryOp::Sqrt, radicand, ctx, nodes)
            }
        }
        IntermediateAST::Binomial { .. } => unimplemented!(),
        IntermediateAST::Matrix { rows } => {
            if rows.len() == 0 {
                bail!("Cannot have empty matrix!")
            }

            if rows.iter().any(|x| x.len() != rows[0].len()) {
                bail!("Rows of matrix must all have same length!")
            }

            let col_n = rows[0].len();
            let row_n = rows.len();

            let mut elems = Vec::with_capacity(col_n * row_n);

            for c in 0..col_n {
                for r in 0..row_n {
                    elems.push(parse_base_term(&rows[r][c], ctx, nodes)?)
                }
            }

            let id = nodes.len();

            nodes.push(ExprNode::Matrix {
                rows: row_n,
                cols: col_n,
                elems,
            });

            Ok(NodeId(id))
        }
    }
}

fn parse_binary(
    op: ast::BinaryOp,
    lhs: &IntermediateAST<'_>,
    rhs: &IntermediateAST<'_>,
    ctx: &mut ParseContext<'_>,
    nodes: &mut Vec<ExprNode>,
) -> Result<NodeId> {
    let lhs = parse_base_term(lhs, ctx, nodes)?;
    let rhs = parse_base_term(rhs, ctx, nodes)?;

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
    let ast = parse_base_term(ast, ctx, nodes)?;

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

fn make_const(val: Value, nodes: &mut Vec<ExprNode>) -> NodeId {
    let len = nodes.len();

    nodes.push(ExprNode::Constant(val));

    NodeId(len)
}
