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
}

#[cfg(test)]
mod tests {
    use super::TypstAst;

    #[test]
    fn parses_correctly() {
        let src = include_str!("./typst_ast_test_input.json");
        let parsed = serde_json::from_str::<TypstAst>(src).unwrap();

        use TypstAst::*;

        let correct = Equation {
            block: true,
            body: Sequence {
                children: vec![
                    Attach {
                        base: Text { text: "x" }.into(),
                        t: Some(
                            Binomial {
                                upper: Text { text: "a" }.into(),
                                lower: vec![Text { text: "b" }],
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
                    Text { text: "=" },
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
                                        Text { text: "‖" },
                                        Text { text: "y" },
                                        Text { text: "‖" },
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
                                Text { text: "⟨" },
                                Space,
                                Root {
                                    index: Some(Text { text: "α" }.into()),
                                    radicand: Text { text: "12" }.into(),
                                },
                                Text { text: "," },
                                Root {
                                    index: None,
                                    radicand: Text { text: "x" }.into(),
                                },
                                Space,
                                Text { text: "⟩" },
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
}
