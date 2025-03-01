use std::{collections::HashMap, fmt::Display};

use serde::{Deserialize, Serialize};

use crate::{
    ast::{FunctionDef, IdentId, ParsedExpr, VariableDef},
    util::NameCache,
};

pub mod eval;
pub mod value;

#[derive(Serialize, Deserialize, Default, Debug)]
pub struct EvalContext {
    pub functions: HashMap<IdentId, FunctionDef>,
    pub variables: HashMap<IdentId, VariableDef>,
    pub idents: NameCache,
}

impl Display for EvalContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (_, FunctionDef { name, params, body }) in &self.functions {
            writeln!(
                f,
                "func: {{@{}({}) = {body}}}, ",
                name.0,
                params
                    .iter()
                    .map(|x| format!("@{}", x.0))
                    .collect::<String>()
            )?;
        }

        for (_, VariableDef { name, value }) in &self.variables {
            writeln!(f, "var: {{@{} = {value}}}, ", name.0)?;
        }

        write!(f, "names: {:#?}", self.idents)
    }
}

impl EvalContext {
    pub fn insert(&mut self, parsed: ParsedExpr) {
        match parsed {
            ParsedExpr::Function(function_def) => {
                self.functions.insert(function_def.name, function_def);
            }
            ParsedExpr::Variable(variable_def) => {
                self.variables.insert(variable_def.name, variable_def);
            }
            ParsedExpr::None => {}
        }
    }
}
