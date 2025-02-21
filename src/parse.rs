use std::collections::HashSet;

use crate::{interp::value::Value, typst_ast::TypstAst};

type BExprAst = Box<ExprAstNode>;

pub enum ExprAstNode {
    Constant(Value),
    Name(String),
    Sum(Vec<ExprAstNode>),
    Product(Vec<ExprAstNode>),
    Neg(BExprAst),
    Fraction {
        num: BExprAst,
        denom: BExprAst,
    },
    FunctionApplication {
        function: String,
        params: Vec<ExprAstNode>,
    },
    Power {
        base: BExprAst,
        exponent: BExprAst,
    },
    Radical {
        index: Option<BExprAst>,
        radicand: BExprAst,
    },
    Binomial {
        upper: BExprAst,
        lower: Vec<ExprAstNode>,
    },
    Factorial(BExprAst),
    List(Vec<ExprAstNode>),
    Prime {
        expr: BExprAst,
        count: usize,
    },
}

pub fn parse<'a>(typst_ast: &TypstAst<'a>) -> Result<Option<ExprAstNode>, String> {
    match typst_ast {
        TypstAst::Equation { body, .. } => parse(body),
        TypstAst::Sequence { children } => todo!(),
        TypstAst::Symbol { text } => Ok(Some(
            Value::parse(text)
                .map(ExprAstNode::Constant)
                .unwrap_or_else(|| ExprAstNode::Name(text.to_string())),
        )),
        TypstAst::Text { text } => Ok(Some(
            Value::parse(text)
                .map(ExprAstNode::Constant)
                .unwrap_or_else(|| ExprAstNode::Name(text.to_string())),
        )),
        TypstAst::Space => Ok(None),
        TypstAst::Frac { num, denom } => Ok(Some(ExprAstNode::Fraction {
            num: parse(num)?.ok_or("Fractions need a numerator!")?.into(),
            denom: parse(denom)?.ok_or("Fractions need a denominator!")?.into(),
        })),
        TypstAst::Attach { base, t, tr, b, .. } => {
            if let Some(b) = b {
                if matches!(&**b, TypstAst::Text { .. })
                    && !matches!(&**base, TypstAst::Text { .. })
                {
                    Err("Unexpected subscript!")?;
                } else if !matches!(&**b, TypstAst::Text { .. }) {
                    Err("Unexpected expression in subscript!")?;
                }
            }

            let mut base =
                if let (TypstAst::Text { text: base_text }, Some(TypstAst::Text { text })) =
                    (&**base, b.as_ref().map(|x| &**x))
                {
                    let new_name = format!("{base_text}_{text}");

                    ExprAstNode::Name(new_name)
                } else {
                    parse(base)?.ok_or("Exponents need bases!")?
                };

            if let Some(tr) = tr {
                if let TypstAst::Primes { count } = &**tr {
                    base = ExprAstNode::Prime {
                        expr: base.into(),
                        count: *count,
                    };
                } else {
                    Err("Unexpected object in TR of Attach")?;
                }
            }

            if let Some(t) = t {
                let superscript = parse(t)?.ok_or("Cannot have empty superscript!")?;

                base = ExprAstNode::Power {
                    base: base.into(),
                    exponent: superscript.into(),
                };
            }

            Ok(Some(base))
        }
        TypstAst::LeftRight { body } => {
            let TypstAst::Sequence { children } = &**body else {
                Err("Expected Sequence in left/right node")?
            };

            if children.len() < 3 {
                Err("Left/right node has fewer than three elements, are your parentheses empty?")?
            }

            // let [left, inner @ .., right] = &children[..];

            // let (TypstAst::Text { text: left } | TypstAst::Symbol { text: left }) = left else {
            //     Err("Unexpected left element in left/right node")?
            // };

            // let (TypstAst::Text { text: right } | TypstAst::Symbol { text: right }) = right else {
            //     Err("Unexpected right element in left/right node")?
            // };

            // if left != "(" || right != ")" {
            //     Err("Unsupported parentheses!")?
            // }
            //
            todo!()
        }
        TypstAst::Primes { .. } => Err("Free-floating primes don't make sense!")?,
        TypstAst::Root { index, radicand } => Ok(Some(ExprAstNode::Radical {
            index: index
                .as_ref()
                .map(|x| parse(x))
                .transpose()?
                .flatten()
                .map(Into::into),
            radicand: parse(radicand)?.ok_or("Radicals need a radicand!")?.into(),
        })),
        TypstAst::Binomial { upper, lower } => Ok(Some(ExprAstNode::Binomial {
            upper: parse(upper)?
                .ok_or("Binomials need an upper element!")?
                .into(),
            lower: {
                let result = lower
                    .iter()
                    .map(|x| parse(x))
                    .filter(|x| !matches!(x, Ok(None)))
                    .collect::<Result<Option<Vec<ExprAstNode>>, String>>()?
                    .unwrap();
                if result.len() == 0 {
                    Err("Binomials need a lower element!")?;
                }

                result
            },
        })),
        TypstAst::Operator { .. } => unimplemented!(),
    }
}
