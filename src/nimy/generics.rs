use std::{cell::LazyCell, rc::Rc};

use crate::nimy::{
    trees::{NodeKind, ParseNode},
    type_constraints::{self, NimTypeClass},
    types::{self, GenericParameterType, NimType},
};

#[derive(Debug)]
pub struct GenericType {
    pub underlying_type: Rc<NimType>,
    pub generics: Vec<Rc<GenericParameterType>>,
}

pub struct GenericInstanciation {
    pub arguments: Vec<Rc<NimType>>,
}

fn build_generic_pair(
    argument_name: String,
    type_constraint: Option<Rc<NimType>>,
) -> (Rc<NimType>, Rc<GenericParameterType>) {
    let generic_param = Rc::new(GenericParameterType {
        name: argument_name,
        subtype_constraint: type_constraint,
    });
    let generic_type = Rc::new(NimType::GenericParameter(generic_param.clone()));
    (generic_type, generic_param)
}

thread_local! {
    static SEQ: LazyCell<Rc<GenericType>> = LazyCell::new(|| {
        let (generic_type, generic_param) = build_generic_pair("T".to_string(), None);
        Rc::new(GenericType {
            underlying_type: Rc::new(NimType::Seq(generic_type)),
            generics: vec![generic_param],
        })
    });
    static SET: LazyCell<Rc<GenericType>> = LazyCell::new(|| {
        let (generic_type, generic_param) = build_generic_pair("T".to_string(), None);
        Rc::new(GenericType {
            underlying_type: Rc::new(NimType::Set(generic_type)),
            generics: vec![generic_param],
        })
    });
    static OPEN_ARRAY: LazyCell<Rc<GenericType>> = LazyCell::new(|| {
        let (generic_type, generic_param) = build_generic_pair("T".to_string(), None);
        Rc::new(GenericType {
            underlying_type: Rc::new(NimType::OpenArray(generic_type)),
            generics: vec![generic_param],
        })
    });
    static ARRAY: LazyCell<Rc<GenericType>> = LazyCell::new(|| {
        let (generic_type, generic_param) = build_generic_pair("T".to_string(), None);
        let (range_type, range_param) = build_generic_pair("R".to_string(), Some(type_constraints::RANGE_CLASS.with(|f| (*f).clone())));
        Rc::new(GenericType {
            underlying_type: Rc::new(NimType::Array(range_type, generic_type)),
            generics: vec![generic_param, range_param],
        })
    });
}

pub fn str_to_generic_type(s: &str) -> Option<Rc<GenericType>> {
    match s {
        "seq" => Some(SEQ.with(|f| (*f).clone())),
        "set" => Some(SET.with(|f| (*f).clone())),
        "openarray" | "openArray" => Some(OPEN_ARRAY.with(|f| (*f).clone())),
        "array" => Some(ARRAY.with(|f| (*f).clone())),
        _ => None,
    }
}

/// Input node should be a "generic_parameter_list"
/// The closure is passed a "type_expression" node.
pub fn parse_generic_param_list<F>(
    node: ParseNode,
    mut type_parser: F,
) -> Vec<Rc<GenericParameterType>>
where
    F: FnMut(ParseNode) -> Option<Rc<NimType>>,
{
    assert_eq!(node.kind, NodeKind::GenericParameterList);
    let mut generics = Vec::new();
    node.children()
        .filter(|child| child.kind == NodeKind::ParameterDeclaration)
        .for_each(|child| {
            let symbol_declaration_list = child.child(0).unwrap();
            let maybe_type_constraint = child.child(2);
            symbol_declaration_list
                .children()
                .filter(|c| c.kind == NodeKind::SymbolDeclaration)
                .map(|c| c.to_str())
                .for_each(|name| {
                    generics.push(Rc::new(GenericParameterType {
                        name,
                        subtype_constraint: maybe_type_constraint.and_then(&mut type_parser),
                    }));
                });
        });
    generics
}

/// Search the type tree of the type provided and replace generic type variables
/// with the ones provided in the arguments array.
/// This can panic if `type_with_generic_param` contains generic variables that
/// are not inside `argument_names`.
fn find_and_replace_generics(
    type_with_generic_param: Rc<NimType>,
    argument_names: &[Rc<GenericParameterType>],
    arguments: &[Rc<NimType>],
) -> Rc<NimType> {
    assert_eq!(argument_names.len(), arguments.len());

    match type_with_generic_param.as_ref() {
        NimType::Int
        | NimType::Float
        | NimType::Nil
        | NimType::String
        | NimType::Untyped
        | NimType::Alias(..)
        | NimType::AliasGeneric(..)
        | NimType::Magic(..)
        | NimType::Undefined(..)
        | NimType::Enum(..)
        | NimType::Range(..)
        | NimType::Typedesc
        | NimType::Typed => type_with_generic_param,
        NimType::GenericParameter(param_type) => argument_names
            .iter()
            .zip(arguments)
            .find_map(|(generic_param, arg)| {
                if generic_param.name == param_type.name {
                    Some(arg.clone())
                } else {
                    None
                }
            })
            .unwrap_or_else(|| type_with_generic_param.clone()),
        NimType::Array(r, t) => {
            let new_r = find_and_replace_generics(r.clone(), argument_names, arguments);
            let new_t = find_and_replace_generics(t.clone(), argument_names, arguments);
            Rc::new(NimType::Array(new_r, new_t))
        }
        NimType::Seq(t) => {
            let new_t = find_and_replace_generics(t.clone(), argument_names, arguments);
            Rc::new(NimType::Seq(new_t))
        }
        NimType::Ptr(t) => {
            let new_t = find_and_replace_generics(t.clone(), argument_names, arguments);
            Rc::new(NimType::Ptr(new_t))
        }
        NimType::Set(t) => {
            let new_t = find_and_replace_generics(t.clone(), argument_names, arguments);
            Rc::new(NimType::Set(new_t))
        }
        NimType::OpenArray(t) => {
            let new_t = find_and_replace_generics(t.clone(), argument_names, arguments);
            Rc::new(NimType::OpenArray(new_t))
        }
        NimType::Distinct(t) => {
            let new_t = find_and_replace_generics(t.clone(), argument_names, arguments);
            Rc::new(NimType::Distinct(new_t))
        }
        NimType::Ref(t) => {
            let new_t = find_and_replace_generics(t.clone(), argument_names, arguments);
            Rc::new(NimType::Ref(new_t))
        }
        NimType::Var(t) => {
            let new_t = find_and_replace_generics(t.clone(), argument_names, arguments);
            Rc::new(NimType::Var(new_t))
        }
        NimType::MagicGeneric(name, args) => {
            let new_args = args
                .iter()
                .map(|a| find_and_replace_generics(a.clone(), argument_names, arguments))
                .collect();
            Rc::new(NimType::MagicGeneric(name.clone(), new_args))
        }
        NimType::Proc(proc_type) => {
            let new_args = proc_type
                .arguments
                .iter()
                .map(|a| find_and_replace_generics(a.clone(), argument_names, arguments))
                .collect();
            let new_return_type = proc_type
                .return_type
                .as_ref()
                .map(|t| find_and_replace_generics(t.clone(), argument_names, arguments));
            Rc::new(NimType::Proc(types::NimProcType {
                arguments: new_args,
                return_type: new_return_type,
            }))
        }
        NimType::Tuple(tuple_type) => {
            let new_fields = tuple_type
                .fields
                .iter()
                .map(|field| types::NimTupleField {
                    sym: field.sym.clone(),
                    field_type: find_and_replace_generics(
                        field.field_type.clone(),
                        argument_names,
                        arguments,
                    ),
                })
                .collect();
            Rc::new(NimType::Tuple(types::NimTupleType { fields: new_fields }))
        }
        NimType::Object(obj_type) => {
            let new_fields = obj_type
                .fields
                .iter()
                .map(|field| types::NimObjectField {
                    sym: field.sym.clone(),
                    field_type: find_and_replace_generics(
                        field.field_type.clone(),
                        argument_names,
                        arguments,
                    ),
                })
                .collect();
            let parent = obj_type
                .parent
                .as_ref()
                .map(|p| find_and_replace_generics(p.clone(), argument_names, arguments));
            Rc::new(NimType::Object(types::NimObjectType {
                fields: new_fields,
                parent,
            }))
        }
        NimType::Varargs(arg_type, is_stringifiable) => {
            let new_arg_type =
                find_and_replace_generics(arg_type.clone(), argument_names, arguments);
            Rc::new(NimType::Varargs(new_arg_type, *is_stringifiable))
        }
        NimType::ObjectVariant(variant) => {
            Rc::new(NimType::ObjectVariant(types::NimObjectVariantType {
                discriminator_name: variant.discriminator_name.clone(),
                discriminator: variant.discriminator.clone(), // generics not allowed in the discriminator field.
                other_fields: variant
                    .other_fields
                    .iter()
                    .map(|f| types::NimObjectField {
                        sym: f.sym.clone(),
                        field_type: find_and_replace_generics(
                            f.field_type.clone(),
                            argument_names,
                            arguments,
                        ),
                    })
                    .collect(),
                branches: variant
                    .branches
                    .iter()
                    .map(|(name, fields)| {
                        (
                            name.clone(),
                            fields
                                .iter()
                                .map(|f| types::NimObjectField {
                                    sym: f.sym.clone(),
                                    field_type: find_and_replace_generics(
                                        f.field_type.clone(),
                                        argument_names,
                                        arguments,
                                    ),
                                })
                                .collect(),
                        )
                    })
                    .collect(),
            }))
        }
        NimType::TypeClass(NimTypeClass::Union(lhs, rhs)) => {
            let new_lhs = find_and_replace_generics(lhs.clone(), argument_names, arguments);
            let new_rhs = find_and_replace_generics(rhs.clone(), argument_names, arguments);
            Rc::new(NimType::TypeClass(NimTypeClass::Union(new_lhs, new_rhs)))
        }
        NimType::TypeClass(_) => type_with_generic_param,
    }
}

/// Create a new type by instanciating the generic.
/// If the instanciation is not possible (bad number of arguments or the arguments don't match the constraints),
/// returns None.
pub fn instanciate_generic_type(
    generic_type: &GenericType,
    inst: &GenericInstanciation,
) -> Option<Rc<NimType>> {
    if generic_type.generics.len() != inst.arguments.len() {
        return None;
    }
    let are_constraints_satisfied =
        generic_type
            .generics
            .iter()
            .zip(&inst.arguments)
            .all(|(generic, arg)| {
                let Some(subtype_constraint) = &generic.subtype_constraint else {
                    return true; // no constraint
                };
                type_constraints::is_subtype(arg, subtype_constraint)
            });
    if !are_constraints_satisfied {
        return None;
    }
    // Build a new type by deep-cloning generic_type.underlying_type while
    // replacing the GenericParameter with the matching name by the types inside `inst`.
    Some(find_and_replace_generics(
        generic_type.underlying_type.clone(),
        &generic_type.generics,
        &inst.arguments,
    ))
}
