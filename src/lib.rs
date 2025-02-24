use wasm_minimal_protocol::*;

initiate_protocol!();

pub mod ast;
pub mod interp;
pub mod parse;
pub mod typst_ast;
pub mod util;
