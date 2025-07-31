use std::rc::Rc;

use crate::nimy::{generics::GenericType, symbols::Symbol, types::NimType};

#[derive(Debug, Clone)]
pub enum MaybeGenericType {
    GenericType(Rc<GenericType>),
    RegularType(Rc<NimType>),
}

/// A nim type with a name
#[derive(Debug)]
pub struct NamedType {
    pub content: MaybeGenericType,
    pub sym: Symbol,
}

#[derive(Debug)]
pub struct NamedRegularType {
    pub content: Rc<NimType>,
    pub sym: Symbol,
}

#[derive(Debug)]
pub struct NamedGenericType {
    pub content: Rc<GenericType>,
    pub sym: Symbol,
}

impl std::fmt::Display for NamedRegularType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let export_star = if self.sym.is_exported { "*" } else { "" };
        write!(
            f,
            "type {}{} = {}",
            &self.sym.name, export_star, self.content
        )
    }
}

impl std::fmt::Display for NamedGenericType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let export_star = if self.sym.is_exported { "*" } else { "" };
        write!(
            f,
            "type {}{}[{}] = {}",
            &self.sym.name,
            export_star,
            self.content
                .generics
                .iter()
                .map(|t| match &t.subtype_constraint {
                    None => t.name.to_string(),
                    Some(subtype) => format!("{}: {}", t.name, subtype),
                })
                .collect::<Vec<_>>()
                .join(", "),
            self.content.underlying_type
        )
    }
}

impl std::fmt::Display for NamedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.content {
            MaybeGenericType::GenericType(t) => write!(
                f,
                "{}",
                &NamedGenericType {
                    content: t.clone(),
                    sym: self.sym.clone(),
                },
            ),
            MaybeGenericType::RegularType(t) => write!(
                f,
                "{}",
                &NamedRegularType {
                    content: t.clone(),
                    sym: self.sym.clone(),
                }
            ),
        }
    }
}
