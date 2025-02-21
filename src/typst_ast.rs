use std::fmt::{Display, Write};

use serde::{Deserialize, Serialize};

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
        }
    }
}

#[cfg(test)]
mod tests {
    use super::TypstAst;

    static TESTING_SRC: &'static str = include_str!("./typst_ast_test_input.json");

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
