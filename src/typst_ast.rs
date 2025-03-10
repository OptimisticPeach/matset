use std::fmt::{Display, Write};

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};

use crate::util::{self, SymbolClass};

type BAst<'a> = Box<TypstAst<'a>>;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
#[serde(tag = "func")]
pub enum TypstAst<'a> {
    #[serde(rename = "equation")]
    Equation { block: bool, body: BAst<'a> },

    #[serde(rename = "sequence")]
    Sequence { children: Vec<TypstAst<'a>> },

    #[serde(rename = "text")]
    Text { text: &'a str },

    #[serde(rename = "space")]
    Space,

    #[serde(rename = "frac")]
    Frac { num: BAst<'a>, denom: BAst<'a> },

    #[serde(rename = "attach")]
    Attach {
        base: BAst<'a>,
        t: Option<BAst<'a>>,
        tr: Option<BAst<'a>>,
        br: Option<BAst<'a>>,
        b: Option<BAst<'a>>,
        bl: Option<BAst<'a>>,
        tl: Option<BAst<'a>>,
    },

    #[serde(rename = "lr")]
    LeftRight { body: BAst<'a> },

    #[serde(rename = "primes")]
    Primes { count: usize },

    #[serde(rename = "root")]
    Root {
        index: Option<BAst<'a>>,
        radicand: BAst<'a>,
    },

    #[serde(rename = "binom")]
    Binomial {
        upper: BAst<'a>,
        lower: Vec<TypstAst<'a>>,
    },

    #[serde(rename = "op")]
    Operator { text: BAst<'a>, limits: bool },

    #[serde(rename = "symbol")]
    Symbol { text: &'a str },

    #[serde(rename = "mat")]
    Matrix {
        rows: Vec<Vec<TypstAst<'a>>>,
        delim: Option<[&'a str; 2]>,
    },

    #[serde(rename = "vec")]
    Vector {
        children: Vec<TypstAst<'a>>,
        delim: Option<[&'a str; 2]>,
    },

    #[serde(rename = "overline")]
    Bar { body: BAst<'a> },
}

impl<'a> Display for TypstAst<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.format_into(f)
    }
}

impl<'a> TypstAst<'a> {
    fn format_into<W: Write>(&self, out: &mut W) -> std::fmt::Result {
        match self {
            TypstAst::Equation { body, .. } => body.format_into(out),
            TypstAst::Sequence { children } => children
                .iter()
                .map(|x| x.format_into(out))
                .collect::<std::fmt::Result>(),
            TypstAst::Text { text } => write!(out, "{text}"),
            TypstAst::Space => write!(out, " "),
            TypstAst::Frac { num, denom } => write!(out, "frac({num}, {denom})"),
            TypstAst::Attach {
                base,
                t,
                tr,
                br,
                b,
                bl,
                tl,
            } => {
                assert!(br.is_none() && bl.is_none() && tl.is_none());

                let write_subscript = |out: &mut W| -> std::fmt::Result {
                    if let Some(b) = b {
                        let (TypstAst::Text { text } | TypstAst::Symbol { text }) = &**b else {
                            panic!("Unsupported subscript!");
                        };

                        write!(out, "subscript({base}, {text})")
                    } else {
                        write!(out, "{base}")
                    }
                };

                let write_base = |out: &mut W| -> std::fmt::Result {
                    if let Some(tr) = tr {
                        let TypstAst::Primes { count } = &**tr else {
                            panic!("Unexpected value in tr of attach!");
                        };

                        write!(out, "primes(")?;
                        write_subscript(out)?;
                        write!(out, ", {count})")
                    } else {
                        write_subscript(out)
                    }
                };

                if let Some(t) = t {
                    write!(out, "pow(")?;
                    write_base(out)?;
                    write!(out, ", {t})")
                } else {
                    write_base(out)
                }
            }
            TypstAst::LeftRight { body } => body.format_into(out),
            TypstAst::Primes { .. } => panic!("Free-floating primes are not supported!"),
            TypstAst::Root { index, radicand } => {
                if let Some(index) = index {
                    write!(out, "nthroot({radicand}, {index})")
                } else {
                    write!(out, "sqrt({radicand})")
                }
            }
            TypstAst::Binomial { upper, lower } => {
                write!(out, "binom({upper}; ")?;
                assert!(lower.len() > 0, "Binomials need a lower part!");
                for elem in lower[0..lower.len() - 1].iter() {
                    write!(out, "{elem}, ")?;
                }

                write!(out, "{})", lower.last().unwrap())
            }
            TypstAst::Operator { text, .. } => write!(out, "{text}"),
            TypstAst::Symbol { text } => write!(out, "{text}"),
            TypstAst::Matrix { rows, delim } => {
                let delim = delim.unwrap_or(["(", ")"]);

                write!(out, "{}", delim[0])?;

                for row in &rows[..rows.len() - 1] {
                    for elem in &row[..row.len() - 1] {
                        write!(out, "{elem}, ")?;
                    }
                    write!(out, "{}; ", row[row.len() - 1])?;
                }

                let last_row = &rows[rows.len() - 1];

                for elem in &last_row[..last_row.len() - 1] {
                    write!(out, "{elem}, ")?;
                }
                write!(out, "{}{}", last_row[last_row.len() - 1], delim[1])
            }
            TypstAst::Vector { children, delim } => {
                let delim = delim.unwrap_or(["(", ")"]);

                write!(out, "{}", delim[0])?;

                for row in &children[..children.len() - 1] {
                    write!(out, "{row}; ")?;
                }

                let last_row = &children[children.len() - 1];

                write!(out, "{}{}", last_row, delim[1])
            }
            TypstAst::Bar { body } => write!(out, "conj({body})"),
        }
    }

    pub fn get_intermediate(&self) -> Result<Option<IntermediateAST<'a>>> {
        match self {
            TypstAst::Equation { body, .. } => body.get_intermediate(),
            TypstAst::Sequence { children } => intermediate_sequence(children),
            TypstAst::Text { text } | TypstAst::Symbol { text } => {
                let text = text.trim();

                if !text.is_empty() {
                    if text == "," {
                        Ok(Some(IntermediateAST::Comma))
                    } else {
                        Ok(Some(IntermediateAST::Text {
                            text,
                            class: util::is_reserved(text),
                        }))
                    }
                } else {
                    Ok(None)
                }
            }
            TypstAst::Space => Ok(None),
            TypstAst::Frac { num, denom } => {
                let num = num
                    .get_intermediate()?
                    .context("Fractions must have non-empty numerators!")?;
                let denom = denom
                    .get_intermediate()?
                    .context("Fractions must have non-empty denominators!")?;

                Ok(Some(IntermediateAST::Frac {
                    num: num.into(),
                    denom: denom.into(),
                }))
            }
            TypstAst::Attach {
                base,
                t,
                tr,
                br,
                b,
                bl,
                tl,
            } => {
                if bl.is_some() || tl.is_some() || br.is_some() {
                    bail!("Unexpected Attach values in bl, tl, or br!");
                }

                let mut base = base
                    .get_intermediate()?
                    .context("Attach must have a base!")?;

                // Precedence: subscript, prime, power

                if let Some(b) = b
                    .as_ref()
                    .map(|x| x.get_intermediate())
                    .transpose()?
                    .flatten()
                {
                    base = IntermediateAST::Subscript {
                        base: base.into(),
                        subscript: b.into(),
                    };
                }

                if let Some(tr) = tr {
                    match &**tr {
                        TypstAst::Primes { count } => {
                            base = IntermediateAST::Prime {
                                base: base.into(),
                                count: *count,
                            }
                        }
                        _ => bail!("Unexpected value in tr of attach!"),
                    }
                }

                if let Some(t) = t
                    .as_ref()
                    .map(|x| x.get_intermediate())
                    .transpose()?
                    .flatten()
                {
                    base = IntermediateAST::Power {
                        base: base.into(),
                        power: t.into(),
                    };
                }

                Ok(Some(base))
            }
            TypstAst::LeftRight { body } => {
                let body = match &**body {
                    TypstAst::Sequence { children } => &**children,
                    _ => bail!("Unexpected value in lr!"),
                };

                let [left, inner @ .., right] = body else {
                    bail!("lr must have at least two elements in it!")
                };

                let Ok(Some(IntermediateAST::Text { text: left, .. })) = left.get_intermediate()
                else {
                    bail!("Unexpected left side on lr!")
                };

                let Ok(Some(IntermediateAST::Text { text: right, .. })) = right.get_intermediate()
                else {
                    bail!("Unexpected left side on lr!")
                };

                Ok(Some(IntermediateAST::LeftRight {
                    left,
                    right,
                    children: intermediate_sequence(inner)?.map(Box::new),
                }))
            }
            TypstAst::Primes { .. } => bail!("Unexpected free-floating primes!"),
            TypstAst::Root { index, radicand } => Ok(Some(IntermediateAST::Root {
                index: index
                    .as_ref()
                    .map(|x| x.get_intermediate())
                    .transpose()?
                    .flatten()
                    .map(Box::new),
                radicand: radicand
                    .get_intermediate()?
                    .map(Box::new)
                    .context("Cannot have empty radicand!")?,
            })),
            TypstAst::Binomial { upper, lower } => {
                let upper = upper
                    .get_intermediate()?
                    .map(Box::new)
                    .context("Binomials need a top part!")?;

                if lower.len() != 1 {
                    bail!("Lower part of binomials must be precisely one element!")
                }

                let lower = lower[0]
                    .get_intermediate()?
                    .map(Box::new)
                    .context("Binomials need a bottom part!")?;

                Ok(Some(IntermediateAST::Binomial { upper, lower }))
            }
            TypstAst::Operator { text, .. } => {
                let text @ IntermediateAST::Text { .. } = text
                    .get_intermediate()?
                    .context("Operators cannot be empty!")?
                else {
                    bail!("Unexpected object in operator context!")
                };

                Ok(Some(text))
            }
            TypstAst::Matrix { rows, .. } => Ok(Some(IntermediateAST::Matrix {
                rows: rows
                    .iter()
                    .map(|x| {
                        x.iter()
                            .map(|y| {
                                y.get_intermediate()
                                    .and_then(|x| x.context("Matrix elements cannot be empty!"))
                            })
                            .collect::<Result<Vec<_>>>()
                    })
                    .collect::<Result<Vec<Vec<_>>>>()?,
            })),
            TypstAst::Vector { children, .. } => Ok(Some(IntermediateAST::Matrix {
                rows: children
                    .iter()
                    .map(|x| {
                        Ok(vec![
                            x.get_intermediate()
                                .and_then(|y| y.context("Vector row cannot be empty!"))?,
                        ])
                    })
                    .collect::<Result<Vec<_>>>()?,
            })),

            TypstAst::Bar { body } => Ok(Some(IntermediateAST::Conjugate {
                body: body
                    .get_intermediate()?
                    .context("Conjugates may not be empty!")?
                    .into(),
            })),
        }
    }
}

fn intermediate_sequence<'a>(seq: &[TypstAst<'a>]) -> Result<Option<IntermediateAST<'a>>> {
    let mut new_children = Vec::with_capacity(seq.len() / 2);

    for child in seq {
        let intermediate = child.get_intermediate()?;

        if let Some(int) = intermediate {
            if let IntermediateAST::Sequence { children } = int {
                new_children.extend(children.into_iter());
            } else {
                new_children.push(int);
            }
        }
    }

    if new_children.len() > 1 {
        Ok(Some(IntermediateAST::Sequence {
            children: new_children,
        }))
    } else if new_children.len() == 1 {
        Ok(Some(new_children.pop().unwrap()))
    } else {
        Ok(None)
    }
}

type IAst<'a> = Box<IntermediateAST<'a>>;

#[derive(Debug)]
pub enum IntermediateAST<'a> {
    Sequence {
        children: Vec<IntermediateAST<'a>>,
    },
    Comma,
    Text {
        text: &'a str,
        class: SymbolClass,
    },
    Frac {
        num: IAst<'a>,
        denom: IAst<'a>,
    },
    Subscript {
        base: IAst<'a>,
        subscript: IAst<'a>,
    },
    Power {
        base: IAst<'a>,
        power: IAst<'a>,
    },
    Prime {
        base: IAst<'a>,
        count: usize,
    },
    LeftRight {
        left: &'a str,
        right: &'a str,
        children: Option<IAst<'a>>,
    },
    Root {
        index: Option<IAst<'a>>,
        radicand: IAst<'a>,
    },
    Binomial {
        upper: IAst<'a>,
        lower: IAst<'a>,
    },
    Matrix {
        rows: Vec<Vec<IntermediateAST<'a>>>,
    },
    Conjugate {
        body: IAst<'a>,
    },
}

#[cfg(test)]
mod tests {
    use super::TypstAst;

    static TESTING_SRC: &'static str = include_str!("../tests/typst_ast_test_input.json");

    #[test]
    fn parses_correctly() {
        let src = TESTING_SRC;
        let parsed = serde_json::from_str::<TypstAst>(src).unwrap();

        use TypstAst::*;

        let correct = Equation {
            block: true,
            body: Sequence {
                children: vec![
                    Attach {
                        base: Symbol { text: "x" }.into(),
                        t: Some(
                            Binomial {
                                upper: Text { text: "a" }.into(),
                                lower: vec![Symbol { text: "b" }],
                            }
                            .into(),
                        ),
                        tr: None,
                        br: None,
                        b: None,
                        bl: None,
                        tl: None,
                    },
                    Space,
                    Symbol { text: "=" },
                    Space,
                    Attach {
                        base: Frac {
                            num: Attach {
                                base: Text { text: "54" }.into(),
                                tr: Some(Primes { count: 2 }.into()),
                                t: None,
                                br: None,
                                b: None,
                                bl: None,
                                tl: None,
                            }
                            .into(),
                            denom: LeftRight {
                                body: Sequence {
                                    children: vec![
                                        Symbol { text: "‖" },
                                        Symbol { text: "y" },
                                        Symbol { text: "‖" },
                                    ],
                                }
                                .into(),
                            }
                            .into(),
                        }
                        .into(),
                        t: None,
                        tr: None,
                        br: None,
                        b: None,
                        bl: None,
                        tl: None,
                    },
                    Space,
                    LeftRight {
                        body: Sequence {
                            children: vec![
                                Symbol { text: "⟨" },
                                Space,
                                Root {
                                    index: Some(Symbol { text: "α" }.into()),
                                    radicand: Text { text: "12" }.into(),
                                },
                                Symbol { text: "," },
                                Root {
                                    index: None,
                                    radicand: Sequence {
                                        children: vec![
                                            Operator {
                                                text: Text { text: "sin" }.into(),
                                                limits: false,
                                            },
                                            LeftRight {
                                                body: Sequence {
                                                    children: vec![
                                                        Symbol { text: "(" },
                                                        Symbol { text: "x" },
                                                        Symbol { text: ")" },
                                                    ],
                                                }
                                                .into(),
                                            },
                                        ],
                                    }
                                    .into(),
                                },
                                Space,
                                Symbol { text: "⟩" },
                            ],
                        }
                        .into(),
                    },
                ],
            }
            .into(),
        };

        assert_eq!(parsed, correct);
    }

    #[test]
    fn stringifies_correctly() {
        let src = TESTING_SRC;
        let parsed = serde_json::from_str::<TypstAst>(src).unwrap();

        assert_eq!(
            format!("{parsed}"),
            "pow(x, binom(a; b)) = frac(primes(54, 2), ‖y‖) ⟨ nthroot(12, α),sqrt(sin(x)) ⟩"
        );
    }
}
