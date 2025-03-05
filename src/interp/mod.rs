use std::{collections::HashMap, fmt::Display};

use serde::{Deserialize, Serialize};
use value::{
    Value,
    complex::Complex,
    function::{BuiltinUnary, Function},
    real::Real,
};

use crate::{
    ast::{BinaryOp, ExprNode, IdentId, NodeId, ParsedExpr, UnaryOp},
    util::NameCache,
};

pub mod eval;
pub mod value;

#[derive(Serialize, Deserialize, Debug)]
pub struct EvalContext {
    pub variables: HashMap<IdentId, NodeId>,
    pub idents: NameCache,
    pub node_store: NodeStore,
}

impl Default for EvalContext {
    fn default() -> Self {
        let mut initial = Self {
            variables: HashMap::new(),
            idents: NameCache::default(),
            node_store: NodeStore::default(),
        };

        use BuiltinUnary::*;
        for (name, value) in [
            ("π", Value::Real(Real::Float(std::f64::consts::PI))),
            ("τ", Value::Real(Real::Float(std::f64::consts::TAU))),
            ("e", Value::Real(Real::Float(std::f64::consts::E))),
            ("i", Value::Complex(Complex::I)),
            ("sin", Value::Function(Function::BuiltinUnary(Sin))),
            ("cos", Value::Function(Function::BuiltinUnary(Cos))),
            ("tan", Value::Function(Function::BuiltinUnary(Tan))),
        ] {
            initial.insert_constant(name, value);
        }

        initial
    }
}

impl Display for EvalContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (name, value) in &self.variables {
            write!(f, "@{} := ", name.0)?;
            value.format_into(&self.node_store.0, f)?;
            writeln!(f, ", ")?;
        }

        write!(f, "names: {:#?}", self.idents)
    }
}

impl EvalContext {
    pub fn insert(&mut self, parsed: ParsedExpr) {
        match parsed {
            ParsedExpr::Function(function_def) => {
                self.variables.insert(
                    function_def.name,
                    self.node_store.make_constant(function_def.to_lambda()),
                );
            }
            ParsedExpr::Variable(variable_def) => {
                self.variables.insert(variable_def.name, variable_def.value);
            }
            ParsedExpr::None => {}
        }
    }

    fn insert_constant(&mut self, name: &str, value: Value) {
        let id = self.idents.create_global_id(name.to_string()).unwrap();
        let val_id = self.node_store.make_constant(value);
        self.variables.insert(id, val_id);
    }
}

#[derive(Debug, Serialize, Deserialize, Default)]
pub struct NodeStore(pub Vec<ExprNode>);

impl NodeStore {
    pub fn make_constant(&mut self, value: Value) -> NodeId {
        self.make(ExprNode::Constant(value))
    }

    pub fn make_ident(&mut self, ident: IdentId) -> NodeId {
        self.make(ExprNode::Ident(ident))
    }

    pub fn make_bin_op(&mut self, lhs: NodeId, rhs: NodeId, op: BinaryOp) -> NodeId {
        self.make(ExprNode::Binary(lhs, rhs, op))
    }

    pub fn make_un_op(&mut self, term: NodeId, op: UnaryOp) -> NodeId {
        self.make(ExprNode::Unary(term, op))
    }

    pub fn make(&mut self, node: ExprNode) -> NodeId {
        let id = self.0.len();

        self.0.push(node);

        NodeId(id)
    }
}
