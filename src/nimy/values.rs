use std::rc::Rc;

use super::{symbols::Symbol, types::NimType};

#[derive(Debug, PartialEq, Clone)]
pub enum NimValue {
    Int(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Nil,
    Other, // value is unrepresentable by nimy because it is too complex.
    Unknown,
}

impl NimValue {
    pub fn is_truthy(&self) -> Option<bool> {
        match self {
            NimValue::Boolean(b) => Some(*b),
            NimValue::Nil => Some(false),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum ValueMutability {
    Const(Rc<NimValue>), // the content is the compile time evaluated value
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

/// Converts a string containing a Nim literal to its value
pub fn parse_literal(s: &str) -> Option<NimValue> {
    if s == "true" {
        return Some(NimValue::Boolean(true));
    } else if s == "false" {
        return Some(NimValue::Boolean(false));
    } else if s == "nil" {
        return Some(NimValue::Nil);
    }
    if let Ok(i) = s.parse::<i64>() {
        return Some(NimValue::Int(i));
    }
    if let Ok(f) = s.parse::<f64>() {
        return Some(NimValue::Float(f));
    }
    if s.starts_with('"') && s.ends_with('"') {
        // See https://nim-lang.org/docs/manual.html#lexical-analysis-string-literals
        // for the specs. There are multiple kinds of escape sequences.
        return Some(NimValue::String("TODO: string literals".to_string()));
    }
    None
}
