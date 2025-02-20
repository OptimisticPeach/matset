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

pub fn parse<'a>(
    typst_ast: &TypstAst<'a>,
    symbols: &mut HashSet<String>,
) -> Result<Option<ExprAstNode>, String> {
    match typst_ast {
        TypstAst::Equation { body, .. } => parse(body, symbols),
        TypstAst::Sequence { children } => parse_sequence(children, symbols),
        TypstAst::Text { text } => Ok(Some(
            Value::parse(text)
                .map(ExprAstNode::Constant)
                .unwrap_or_else(|| {
                    symbols.insert(text.to_string());

                    ExprAstNode::Name(text.to_string())
                }),
        )),
        TypstAst::Space => Ok(None),
        TypstAst::Frac { num, denom } => Ok(Some(ExprAstNode::Fraction {
            num: parse(num, symbols)?
                .ok_or("Fractions need a numerator!")?
                .into(),
            denom: parse(denom, symbols)?
                .ok_or("Fractions need a denominator!")?
                .into(),
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

                    symbols.insert(new_name.clone());

                    ExprAstNode::Name(new_name)
                } else {
                    parse(base, symbols)?.ok_or("Exponents need bases!")?
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
                let superscript = parse(t, symbols)?.ok_or("Cannot have empty superscript!")?;

                base = ExprAstNode::Power {
                    base: base.into(),
                    exponent: superscript.into(),
                };
            }

            Ok(Some(base))
        }
        TypstAst::LeftRight { body } => todo!(),
        TypstAst::Primes { .. } => Err("Free-floating primes don't make sense!")?,
        TypstAst::Root { index, radicand } => Ok(Some(ExprAstNode::Radical {
            index: index
                .as_ref()
                .map(|x| parse(x, symbols))
                .transpose()?
                .flatten()
                .map(Into::into),
            radicand: parse(radicand, symbols)?
                .ok_or("Radicals need a radicand!")?
                .into(),
        })),
        TypstAst::Binomial { upper, lower } => Ok(Some(ExprAstNode::Binomial {
            upper: parse(upper, symbols)?
                .ok_or("Binomials need an upper element!")?
                .into(),
            lower: {
                let result = lower
                    .iter()
                    .map(|x| parse(x, symbols))
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

fn parse_sequence(
    seq: &[TypstAst<'_>],
    symbols: &mut HashSet<String>,
) -> Result<Option<ExprAstNode>, String> {
}
