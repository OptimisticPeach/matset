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
        function: BExprAst,
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

pub fn parse(typst_ast: &TypstAst<'_>) -> Result<Option<ExprAstNode>, String> {
    match typst_ast {
        TypstAst::Equation { body, .. } => parse(body),
        TypstAst::Sequence { children } => todo!(),
        TypstAst::Text { text } => todo!(),
        TypstAst::Space => Ok(None),
        TypstAst::Frac { num, denom } => Ok(Some(ExprAstNode::Fraction {
            num: parse(num)?.ok_or("Fractions need a numerator!")?.into(),
            denom: parse(denom)?.ok_or("Fractions need a denominator!")?.into(),
        })),
        TypstAst::Attach {
            base,
            t,
            tr,
            br,
            b,
            bl,
            tl,
        } => todo!(),
        TypstAst::LeftRight { body } => todo!(),
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
                    .map(parse)
                    .filter(|x| !matches!(x, Ok(None)))
                    .collect::<Result<Option<Vec<ExprAstNode>>, String>>()?
                    .unwrap();
                if result.len() == 0 {
                    Err("Binomials need a lower element!")?;
                }

                result
            },
        })),
    }
}
