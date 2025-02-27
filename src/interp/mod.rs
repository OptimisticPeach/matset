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
            write!(f, "func: {{{name:?}({params:?}) = {body}}}, ")?;
        }

        for (_, VariableDef { name, value }) in &self.variables {
            write!(f, "var: {{{name:?} = {value}}}, ")?;
        }

        write!(f, "names: {:?}", self.idents)
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
        }
    }
}
