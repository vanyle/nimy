use std::rc::Rc;

use super::{
    symbols::Symbol,
    types::{NimProcType, NimType},
};

#[derive(Debug)]
pub struct NimProc {
    pub sym: Symbol,
    pub nimtype: NimProcType,
}

#[derive(Debug)]
pub enum NimValue {
    Int(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Nil,
    Other, // value is unrepresentable by nimy because it is too complex.
    Unknown,
}
#[derive(Debug)]
pub enum ValueMutability {
    Const(Rc<NimValue>),
    Let,
    Var,
}

#[derive(Debug)]
pub struct NimVariable {
    // var, let or const
    pub sym: Symbol,
    pub mutability: ValueMutability,
    pub nimtype: Rc<NimType>,
    pub raw_value: String, // value definition in the code
}
