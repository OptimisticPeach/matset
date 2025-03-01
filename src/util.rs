use std::{
    collections::{HashMap, HashSet},
    sync::OnceLock,
};

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};

use crate::ast::IdentId;

#[derive(Default, Serialize, Deserialize, Debug)]
pub struct NameCache {
    names: HashMap<String, usize>,
    globals: HashSet<usize>,
    all_locals: HashSet<usize>,
    current_locals: HashSet<usize>,
    pub reverse: Vec<String>,
    count: usize,
}

impl NameCache {
    /// Used in creating the names of function parameters
    pub fn create_local_id(&mut self, s: String) -> Result<IdentId> {
        let id = match self.names.entry(s) {
            std::collections::hash_map::Entry::Occupied(x) => {
                let id = x.get();

                if self.globals.contains(id) {
                    bail!("Name {} is already used in a global context!", x.key());
                }

                if self.current_locals.contains(id) {
                    bail!(
                        "Cannot have multiple parameters with the same name: {}",
                        x.key()
                    )
                }

                *id
            }
            std::collections::hash_map::Entry::Vacant(x) => {
                let new_id = self.count;
                self.count += 1;

                self.reverse.push(x.key().clone());

                x.insert(new_id);

                new_id
            }
        };

        self.all_locals.insert(id);
        self.current_locals.insert(id);

        Ok(IdentId(id))
    }

    pub fn clear_current_locals(&mut self) {
        self.current_locals.clear();
    }

    /// Used in the creation of global variables or function names
    pub fn create_global_id(&mut self, s: String) -> Result<IdentId> {
        let id = match self.names.entry(s) {
            std::collections::hash_map::Entry::Occupied(x) => {
                let id = x.get();

                if self.globals.contains(id) {
                    bail!("Name {} is already used in a global context", x.key());
                }

                if self.all_locals.contains(id) {
                    bail!(
                        "Name {} is already used in a local context somewhere",
                        x.key()
                    )
                }

                self.globals.insert(*id);

                *id
            }
            std::collections::hash_map::Entry::Vacant(x) => {
                let new_id = self.count;
                self.count += 1;

                self.reverse.push(x.key().clone());

                x.insert(new_id);

                new_id
            }
        };

        self.globals.insert(id);

        Ok(IdentId(id))
    }

    pub fn get_existing(&self, s: &str) -> Result<IdentId> {
        self.names
            .get(s)
            .with_context(|| format!("Name {s} does not exist!"))
            .map(|&x| IdentId(x))
    }

    pub fn make_use_of(&mut self, s: String) -> Result<IdentId> {
        match self.names.entry(s) {
            std::collections::hash_map::Entry::Occupied(x) => Ok(IdentId(*x.get())),
            std::collections::hash_map::Entry::Vacant(x) => {
                let new_id = self.count;
                self.count += 1;

                self.reverse.push(x.key().clone());

                x.insert(new_id);

                Ok(IdentId(new_id))
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum SymbolClass {
    GreekOperator,
    BuiltinFunction,
    InfixOperator,
    Constant,
    UnusedSymbol,
    Ident,
    Number,
    MixedNumberAlpha,
}

pub fn is_reserved(s: &str) -> SymbolClass {
    assert_ne!(s.len(), 0);

    static DATA: OnceLock<HashMap<&'static str, SymbolClass>> = OnceLock::new();

    const RESERVED_SYMBOLS: &[(&'static str, SymbolClass)] = &[
        ("∑", SymbolClass::GreekOperator),
        ("∏", SymbolClass::GreekOperator),
        ("∫", SymbolClass::GreekOperator),
        ("π", SymbolClass::Constant),
        ("τ", SymbolClass::Constant),
        ("e", SymbolClass::Constant),
        ("i", SymbolClass::Constant),
        ("sin", SymbolClass::BuiltinFunction),
        ("cos", SymbolClass::BuiltinFunction),
        ("tan", SymbolClass::BuiltinFunction),
        ("csc", SymbolClass::BuiltinFunction),
        ("sec", SymbolClass::BuiltinFunction),
        ("cot", SymbolClass::BuiltinFunction),
        ("sinh", SymbolClass::BuiltinFunction),
        ("cosh", SymbolClass::BuiltinFunction),
        ("tanh", SymbolClass::BuiltinFunction),
        ("csch", SymbolClass::BuiltinFunction),
        ("sech", SymbolClass::BuiltinFunction),
        ("tanh", SymbolClass::BuiltinFunction),
        ("arcsin", SymbolClass::BuiltinFunction),
        ("arccos", SymbolClass::BuiltinFunction),
        ("arctan", SymbolClass::BuiltinFunction),
        ("arsinh", SymbolClass::BuiltinFunction),
        ("arcosh", SymbolClass::BuiltinFunction),
        ("artanh", SymbolClass::BuiltinFunction),
        ("arccsc", SymbolClass::BuiltinFunction),
        ("arcsec", SymbolClass::BuiltinFunction),
        ("arccot", SymbolClass::BuiltinFunction),
        ("arcsch", SymbolClass::BuiltinFunction),
        ("arsech", SymbolClass::BuiltinFunction),
        ("arcoth", SymbolClass::BuiltinFunction),
        ("+", SymbolClass::InfixOperator),
        ("−", SymbolClass::InfixOperator),
        ("-", SymbolClass::InfixOperator),
        ("*", SymbolClass::InfixOperator),
        ("⋅", SymbolClass::InfixOperator),
        ("∗", SymbolClass::InfixOperator),
    ];

    let data = DATA.get_or_init(|| RESERVED_SYMBOLS.iter().copied().collect());

    if let Some(x) = data.get(s) {
        return *x;
    }

    for c in s.chars() {
        if !c.is_alphanumeric() && c != '_' && c != '.' {
            return SymbolClass::UnusedSymbol;
        }
    }

    let mut chars = s.chars();

    let first = chars.next().unwrap();

    if first.is_ascii_digit() {
        if chars.any(|x| !x.is_ascii_digit() && x != '.') {
            SymbolClass::MixedNumberAlpha
        } else {
            SymbolClass::Number
        }
    } else {
        SymbolClass::Ident
    }
}
