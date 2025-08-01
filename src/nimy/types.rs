use std::{iter, rc::Rc};

use crate::nimy::{trees::ParseNode, type_constraints::NimTypeClass};

use super::symbols::Symbol;

#[derive(Debug, Eq, PartialEq)]
pub enum NimType {
    Int,
    Bool,
    Float,
    String,
    Typedesc,
    Typed,
    TypeOfNil,
    Untyped,
    Alias(Rc<str>), // not a symbol here as an alias is not yet resolved
    AliasGeneric(Rc<str>, Vec<Rc<NimType>>), // an alias to a generic instanciation, like `Bluck[int]`
    Magic(Rc<Symbol>), // represents a magic defined type, like int8 or cstring
    MagicGeneric(Rc<str>, Vec<Rc<NimType>>), // represents a magic defined type that is generic, like sink
    Undefined(Symbol),
    Ptr(Rc<NimType>),
    Ref(Rc<NimType>),
    Var(Rc<NimType>),
    Distinct(Rc<NimType>),
    OpenArray(Rc<NimType>),
    Range(usize, usize),
    // first type must satisfy the range constraint to be valid
    Array(Rc<NimType>, Rc<NimType>),
    Varargs(Rc<NimType>, bool), // bool indicates `$`.
    Seq(Rc<NimType>),
    Set(Rc<NimType>),
    Proc(NimProcType),
    Enum(NimEnumType),
    Object(NimObjectType),
    ObjectVariant(NimObjectVariantType),
    Tuple(NimTupleType),
    GenericParameter(Rc<GenericParameterType>),
    TypeClass(NimTypeClass),
}

impl std::fmt::Display for NimType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NimType::Int => write!(f, "int"),
            NimType::Bool => write!(f, "bool"),
            NimType::Float => write!(f, "float"),
            NimType::String => write!(f, "string"),
            NimType::Typedesc => write!(f, "typedesc"),
            NimType::Typed => write!(f, "typed"),
            NimType::TypeOfNil => write!(f, "nil"),
            NimType::Untyped => write!(f, "untyped"),
            NimType::Alias(inner) => write!(f, "alias({inner})"),
            NimType::AliasGeneric(name, args) => {
                write!(
                    f,
                    "alias({}[{}])",
                    name,
                    args.iter()
                        .map(|a| format!("{a}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            NimType::Magic(inner) => write!(f, "{}", inner.name),
            NimType::MagicGeneric(name, args) => {
                write!(
                    f,
                    "{}({})",
                    name,
                    args.iter()
                        .map(|a| format!("{a}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            NimType::Undefined(sym) => {
                write!(f, "???({})", &sym.name)
            }
            NimType::Ptr(inner) => write!(f, "ptr({inner})"),
            NimType::Ref(inner) => write!(f, "ref({inner})"),
            NimType::Var(inner) => write!(f, "var({inner})"),
            NimType::Distinct(inner) => write!(f, "distinct({inner})"),
            NimType::OpenArray(inner) => write!(f, "openarray({inner})"),
            NimType::Range(start, end) => write!(f, "range[{start}..{end}]"),
            NimType::Array(range, inner) => write!(f, "array[{range}, {inner}]"),
            NimType::Varargs(inner, is_dollar) => {
                if *is_dollar {
                    write!(f, "varargs[{inner}, `$`]")
                } else {
                    write!(f, "varargs[{inner}]")
                }
            }
            NimType::Seq(inner) => write!(f, "seq[{inner}]"),
            NimType::Set(inner) => write!(f, "set[{inner}]"),
            NimType::Proc(proc_type) => write!(
                f,
                "proc({}): {}",
                proc_type
                    .arguments
                    .iter()
                    .map(|t| format!("{t}"))
                    .collect::<Vec<_>>()
                    .join(", "),
                proc_type
                    .return_type
                    .as_ref()
                    .map_or("void".to_string(), |t| format!("{t}"))
            ),
            NimType::Enum(enum_type) => write!(f, "{enum_type}"),
            NimType::Tuple(tuple_type) => write!(
                f,
                "tuple({})",
                tuple_type
                    .fields
                    .iter()
                    .map(|f| format!("{f}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            NimType::Object(object_type) => write!(f, "{object_type}"),
            NimType::ObjectVariant(variant_type) => write!(
                f,
                "object({}, case {}: {} of {})",
                &variant_type
                    .other_fields
                    .iter()
                    .map(|f| format!("{f}"))
                    .collect::<Vec<_>>()
                    .join(", "),
                &variant_type.discriminator_name,
                variant_type.discriminator,
                variant_type
                    .branches
                    .iter()
                    .map(|(names, fields)| {
                        format!(
                            "{}: {{{}}}",
                            names.join(", "),
                            fields
                                .iter()
                                .map(|f| format!("{f}"))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    }) // TODO: display other_fields
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
            NimType::GenericParameter(param) => write!(
                f,
                "{}{}",
                &param.name,
                param
                    .subtype_constraint
                    .as_ref()
                    .map_or("".to_string(), |t| format!(": {t}"))
            ),
            NimType::TypeClass(type_class) => write!(f, "{type_class}"),
        }
    }
}

impl NimType {
    pub fn children<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Rc<NimType>> + 'a> {
        match self {
            NimType::Ptr(inner) => Box::new(iter::once(inner)),
            NimType::Ref(inner) => Box::new(iter::once(inner)),
            NimType::Var(inner) => Box::new(iter::once(inner)),
            NimType::Distinct(inner) => Box::new(iter::once(inner)),
            NimType::OpenArray(inner) => Box::new(iter::once(inner)),
            NimType::Range(_, _) => Box::new(iter::empty()),
            NimType::Array(range, inner) => Box::new(iter::once(range).chain(iter::once(inner))),
            NimType::Varargs(inner, _) => Box::new(iter::once(inner)),
            NimType::Seq(inner) => Box::new(iter::once(inner)),
            NimType::Set(inner) => Box::new(iter::once(inner)),
            NimType::Proc(proc_type) => Box::new(
                proc_type
                    .arguments
                    .iter()
                    .chain(proc_type.return_type.iter()),
            ),
            NimType::AliasGeneric(_, args) => Box::new(args.iter()),
            NimType::MagicGeneric(_, args) => Box::new(args.iter()),
            NimType::Tuple(tuple_type) => Box::new(tuple_type.fields.iter().map(|f| &f.field_type)),
            NimType::Object(object_type) => Box::new(
                object_type
                    .fields
                    .iter()
                    .map(|f| &f.field_type)
                    .chain(object_type.parent.iter()),
            ),
            NimType::ObjectVariant(variant_type) => Box::new(
                variant_type
                    .branches
                    .iter()
                    .flat_map(|(_, fields)| fields.iter().map(|f| &f.field_type))
                    .chain(iter::once(&variant_type.discriminator))
                    .chain(variant_type.other_fields.iter().map(|f| &f.field_type)),
            ),
            NimType::GenericParameter(param) => Box::new(param.subtype_constraint.iter()),
            NimType::Int
            | NimType::Bool
            | NimType::Enum(_)
            | NimType::Float
            | NimType::Typedesc
            | NimType::Typed
            | NimType::TypeOfNil
            | NimType::Alias(..)
            | NimType::Undefined(..)
            | NimType::Magic(..)
            | NimType::Untyped
            | NimType::String => Box::new(iter::empty()),
            NimType::TypeClass(NimTypeClass::Union(lhr, rhs)) => {
                Box::new(iter::once(lhr).chain(iter::once(rhs)))
            }
            NimType::TypeClass(_) => Box::new(iter::empty()),
        }
    }
}

/// A type should never contain a cycle and if this returns true, it means that somebody messed up.
/// A lot of our functions (including the display trait) assume acyclicity.
pub fn is_type_cyclic(nim_type: Rc<NimType>) -> bool {
    let mut seen = vec![];
    let mut stack = vec![nim_type];
    while let Some(current) = stack.pop() {
        if seen.contains(&current) {
            return true;
        }
        for child in current.children() {
            stack.push((*child).clone());
        }
        seen.push(current);
    }
    false
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NimEnumType {
    pub variants: Vec<Rc<str>>,
}

impl std::fmt::Display for NimEnumType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "enum({})", self.variants.join(", "))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct NimTupleType {
    pub fields: Vec<NimTupleField>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NimObjectType {
    pub fields: Vec<NimObjectField>,
    // normally, parent must be a Rc<NimType::NimObjectType(...)>
    // but this type doesn't play nice with dealing with other NimTypes, so we keep this.
    pub parent: Option<Rc<NimType>>,
}

impl std::fmt::Display for NimObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "object of {}({})",
            self.parent
                .as_ref()
                .map_or("root".to_string(), |p| format!("{p}").to_string()),
            self.fields
                .iter()
                .map(|f| format!("{f}"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct NimTupleField {
    pub sym: Option<Symbol>,
    pub field_type: Rc<NimType>,
}

impl std::fmt::Display for NimTupleField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(sym) = &self.sym {
            write!(f, "{}: {}", sym.name, self.field_type)
        } else {
            write!(f, "{}", self.field_type)
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NimObjectField {
    pub sym: Symbol,
    pub field_type: Rc<NimType>,
}

impl std::fmt::Display for NimObjectField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", &self.sym.name, self.field_type)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct NimObjectVariantType {
    pub discriminator_name: String,
    pub discriminator: Rc<NimType>, // should be an enum to be valid
    pub branches: Vec<(Vec<String>, Vec<NimObjectField>)>,
    pub other_fields: Vec<NimObjectField>,
}
#[derive(Debug, Eq, PartialEq)]
pub struct NimProcType {
    pub arguments: Vec<Rc<NimType>>,
    pub return_type: Option<Rc<NimType>>,
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct GenericParameterType {
    pub name: String,
    pub subtype_constraint: Option<Rc<NimType>>,
}

pub fn to_undefined_type(node: &ParseNode) -> NimType {
    NimType::Undefined(Symbol::new(
        node.to_str(),
        node.start_byte(),
        node.end_byte(),
        None,
        Rc::new(node.path.to_path_buf()),
        false,
    ))
}

thread_local! {
    pub static INT: Rc<NimType> = Rc::new(NimType::Int);
    pub static BOOL: Rc<NimType> = Rc::new(NimType::Bool);
    pub static FLOAT: Rc<NimType> = Rc::new(NimType::Float);
    pub static STRING: Rc<NimType> = Rc::new(NimType::String);
    pub static TYPEDESC: Rc<NimType> = Rc::new(NimType::Typedesc);
    pub static TYPED: Rc<NimType> = Rc::new(NimType::Typed);
}

/// Converts some string with well-known type names to their given type
pub fn str_to_type(s: &str) -> Option<Rc<NimType>> {
    match s {
        "int" => Some(INT.with(|f| f.clone())),
        "bool" => Some(BOOL.with(|f| f.clone())),
        "float" => Some(FLOAT.with(|f| f.clone())),
        "string" => Some(STRING.with(|f| f.clone())),
        "typedesc" => Some(TYPEDESC.with(|f| f.clone())),
        "typed" => Some(TYPED.with(|f| f.clone())),
        _ => None,
    }
}
