use std::{
    collections::{HashMap, HashSet},
    sync::OnceLock,
};

use anyhow::{Context, Result, bail};

use crate::ast::IdentId;

#[derive(Default)]
pub struct NameCache {
    names: HashMap<String, usize>,
    globals: HashSet<usize>,
    all_locals: HashSet<usize>,
    current_locals: HashSet<usize>,
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

                unreachable!("Id not registered to either all_locals nor globals")
            }
            std::collections::hash_map::Entry::Vacant(x) => {
                let new_id = self.count;
                self.count += 1;

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
}

#[derive(Debug, Copy, Clone)]
pub enum SymbolClass {
    GreekOperator,
    Operator,
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
        ("π", SymbolClass::GreekOperator),
        ("τ", SymbolClass::GreekOperator),
        ("∫", SymbolClass::Operator),
        ("e", SymbolClass::Operator),
        ("sin", SymbolClass::Operator),
        ("cos", SymbolClass::Operator),
        ("tan", SymbolClass::Operator),
        ("csc", SymbolClass::Operator),
        ("sec", SymbolClass::Operator),
        ("cot", SymbolClass::Operator),
        ("sinh", SymbolClass::Operator),
        ("cosh", SymbolClass::Operator),
        ("tanh", SymbolClass::Operator),
        ("csch", SymbolClass::Operator),
        ("sech", SymbolClass::Operator),
        ("tanh", SymbolClass::Operator),
        ("arcsin", SymbolClass::Operator),
        ("arccos", SymbolClass::Operator),
        ("arctan", SymbolClass::Operator),
        ("arsinh", SymbolClass::Operator),
        ("arcosh", SymbolClass::Operator),
        ("artanh", SymbolClass::Operator),
        ("arccsc", SymbolClass::Operator),
        ("arcsec", SymbolClass::Operator),
        ("arccot", SymbolClass::Operator),
        ("arcsch", SymbolClass::Operator),
        ("arsech", SymbolClass::Operator),
        ("arcoth", SymbolClass::Operator),
    ];

    let data = DATA.get_or_init(|| RESERVED_SYMBOLS.iter().copied().collect());

    if let Some(x) = data.get(s) {
        return *x;
    }

    for c in s.chars() {
        if !c.is_alphanumeric() && c != '_' {
            return SymbolClass::UnusedSymbol;
        }
    }

    let mut chars = s.chars();

    let first = chars.next().unwrap();

    if first.is_ascii_digit() {
        if chars.any(|x| !x.is_ascii_digit()) {
            SymbolClass::MixedNumberAlpha
        } else {
            SymbolClass::Number
        }
    } else {
        SymbolClass::Ident
    }
}
