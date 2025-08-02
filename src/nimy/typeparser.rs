use std::rc::Rc;

use crate::nimy::{
    cpunit::CompilationUnit,
    generics::{self, GenericType},
    namedtypes::{MaybeGenericType, NamedType},
    scope::InnerScope,
    symbols::Symbol,
    trees::{self, NodeKind},
    type_constraints::NimTypeClass,
    types::{self, GenericParameterType, NimEnumType, NimTupleType, NimType},
};

fn parse_type_symbol_declaration(node: &trees::ParseNode) -> Symbol {
    let is_exported = node.extract_by_kind(NodeKind::ExportedSymbol).is_some();
    let name = node
        .extract_by_kind(NodeKind::Identifier)
        .map(|n| n.to_str())
        .unwrap_or_else(|| node.to_str());
    Symbol::new(
        name,
        node.start_byte(),
        node.end_byte(),
        None,
        Rc::new(node.path.to_path_buf()),
        is_exported,
    )
}

fn parse_symbol_declaration_list(node: &trees::ParseNode) -> Vec<Symbol> {
    assert_eq!(node.kind, NodeKind::SymbolDeclarationList);
    node.children()
        .filter(|n| n.kind == NodeKind::SymbolDeclaration)
        .map(|n| parse_type_symbol_declaration(&n))
        .collect()
}

fn parse_fields(
    fields: &trees::ParseNode,
    cpunit: &CompilationUnit,
    scope: &InnerScope,
    generic_parameters: &[Rc<GenericParameterType>],
) -> impl Iterator<Item = (Symbol, Rc<NimType>)> {
    let kind = fields.kind;
    assert!(
        fields.kind == NodeKind::FieldDeclarationList || fields.kind == NodeKind::TupleType,
        "Unexpected kind {kind:?}"
    );
    fields
        .children()
        .filter(|n| n.kind == NodeKind::FieldDeclaration)
        .filter_map(|n| {
            let symbols = n.get_by_kind(NodeKind::SymbolDeclarationList)?;
            let type_expression = n.get_by_kind(NodeKind::TypeExpression)?;
            let type_expression =
                parse_type_expression(&type_expression, cpunit, scope, generic_parameters)?;
            Some(
                parse_symbol_declaration_list(&symbols)
                    .iter()
                    .map(|s| (s.clone(), type_expression.clone()))
                    .collect::<Vec<_>>(),
            )
        })
        .flatten()
}

fn parse_fields_from_tuple_construction(
    node: &trees::ParseNode,
    cpunit: &CompilationUnit,
    scope: &InnerScope,
    generic_parameters: &[Rc<GenericParameterType>],
) -> Vec<types::NimTupleField> {
    assert_eq!(node.kind, NodeKind::TupleType);
    // tuples can directly contain the list of fields with no field_list node
    let fields = node
        .get_by_kind(NodeKind::FieldDeclarationList)
        .unwrap_or(*node);

    let fields = parse_fields(&fields, cpunit, scope, generic_parameters);
    fields
        .map(|(sym, field_type)| types::NimTupleField {
            sym: Some(sym),
            field_type,
        })
        .collect()
}

fn parse_fields_from_object_construction(
    node: &trees::ParseNode,
    cpunit: &CompilationUnit,
    scope: &InnerScope,
    generic_parameters: &[Rc<GenericParameterType>],
) -> Vec<types::NimObjectField> {
    let Some(fields) = node.get_by_kind(NodeKind::FieldDeclarationList) else {
        return vec![];
    };
    let fields = parse_fields(&fields, cpunit, scope, generic_parameters);
    fields
        .map(|(sym, field_type)| types::NimObjectField { sym, field_type })
        .collect()
}

fn resolve_type_using_aliases(
    type_name: &str,
    generic_parameters: &[Rc<GenericParameterType>],
) -> Rc<NimType> {
    let t = types::str_to_type(type_name);
    if let Some(t) = t {
        return t;
    }
    for generic_parameter in generic_parameters {
        if type_name == generic_parameter.name {
            return Rc::new(NimType::GenericParameter(generic_parameter.clone()));
        }
    }
    Rc::new(NimType::Alias(Rc::from(type_name.to_string())))
}

fn resolve_generic_type(
    generic_name: &str,
    _cpunit: &CompilationUnit,
    scope: &InnerScope,
    // generic parameters are not generic types themselves.
    // This is because generic instanciation has to finish and be non-turing complete
) -> Option<Rc<GenericType>> {
    // Check for well known names
    if let Some(t) = generics::str_to_generic_type(generic_name) {
        return Some(t);
    }
    // Check for locally defined names
    for generic in &scope.generic_types {
        if generic.sym.name == generic_name {
            return Some(generic.content.clone());
        }
    }
    // Check for globally defined names
    if let Some(parent_scope) = scope.parent.upgrade() {
        return resolve_generic_type(generic_name, _cpunit, &parent_scope.borrow());
    }
    None
}

/// parses nodes of the form a[T, B]
/// Provides a specialization of a generic type
fn parse_type_from_bracket_expression(
    cpunit: &CompilationUnit,
    node: &trees::ParseNode,
    scope: &InnerScope,
    generic_parameters: &[Rc<GenericParameterType>],
) -> Option<Rc<NimType>> {
    assert_eq!(node.kind, NodeKind::BracketExpression);
    let generic_name = node.child(0).expect("Expected generic name").to_str();
    let generic_type = resolve_generic_type(&generic_name, cpunit, scope)?;
    let arguments = node.get_by_kind(NodeKind::ArgumentList)?;

    let arguments = arguments
        .children()
        .filter(|n| n.kind != NodeKind::Comma)
        .filter_map(|id| parse_type_expression(&id, cpunit, scope, generic_parameters))
        .collect::<Vec<_>>();

    let inst = generics::GenericInstanciation { arguments };
    generics::instanciate_generic_type(&generic_type, &inst)
}

fn parse_infix_operator_for_types(
    cpunit: &CompilationUnit,
    scope: &InnerScope,
    left: &trees::ParseNode,
    right: &trees::ParseNode,
    operator: &str,
    generic_parameters: &[Rc<GenericParameterType>],
) -> Option<Rc<NimType>> {
    match operator {
        "|" => {
            let left = parse_type_expression(left, cpunit, scope, generic_parameters);
            let right = parse_type_expression(right, cpunit, scope, generic_parameters);
            if let Some(left) = left
                && let Some(right) = right
            {
                Some(Rc::new(NimType::TypeClass(NimTypeClass::Union(
                    left, right,
                ))))
            } else {
                None
            }
        }
        // MARK: TODO Ranges
        ".." => Some(Rc::new(NimType::Range(0, 1))),
        _ => None,
    }
}

fn parse_variant_object_type(
    cpunit: &CompilationUnit,
    node: &trees::ParseNode,
    scope: &InnerScope,
    generic_parameters: &[Rc<GenericParameterType>],
) -> Option<Rc<NimType>> {
    assert_eq!(node.kind, NodeKind::ObjectDeclaration);
    let fields = node.get_by_kind(NodeKind::FieldDeclarationList)?;
    let regular_fields = parse_fields(&fields, cpunit, scope, generic_parameters)
        .map(|(sym, field_type)| types::NimObjectField { sym, field_type })
        .collect();
    let variant_node = fields.get_by_kind(NodeKind::VariantDeclaration)?;
    let discriminator_node = variant_node.get_by_kind(NodeKind::VariantDiscriminatorDeclaration)?;
    let discriminator_name = discriminator_node
        .child(0)?
        .extract_by_kind(NodeKind::Identifier)?
        .to_str();

    let discriminator = parse_type_expression(
        &discriminator_node.child(2)?,
        cpunit,
        scope,
        generic_parameters,
    )?;

    let branches = variant_node
        .children()
        .filter(|n| n.kind == NodeKind::OfBranch)
        .filter_map(|branch_node| {
            let identifiers = branch_node
                .get_by_kind(NodeKind::ExpressionList)?
                .children()
                .filter(|n| n.kind == NodeKind::Identifier)
                .map(|n| n.to_str().to_string())
                .collect::<Vec<_>>();
            let fields = parse_fields_from_object_construction(
                &branch_node,
                cpunit,
                scope,
                generic_parameters,
            );
            Some((identifiers, fields))
        })
        .collect::<Vec<_>>();

    Some(Rc::new(NimType::ObjectVariant(
        types::NimObjectVariantType {
            other_fields: regular_fields,
            discriminator_name,
            discriminator,
            branches,
        },
    )))
}

fn parse_object_type(
    cpunit: &CompilationUnit,
    node: &trees::ParseNode,
    scope: &InnerScope,
    generic_parameters: &[Rc<GenericParameterType>],
) -> Option<Rc<NimType>> {
    let inherits = node
        .child(1)
        .map(|n| n.kind == NodeKind::Of)
        .unwrap_or(false);
    let parent_type = if inherits {
        node.child(2)
            .and_then(|n| parse_type_expression(&n, cpunit, scope, generic_parameters))
    } else {
        None
    };
    Some(Rc::new(NimType::Object(types::NimObjectType {
        fields: parse_fields_from_object_construction(node, cpunit, scope, generic_parameters),
        parent: parent_type,
    })))
}

pub fn parse_type_expression(
    node: &trees::ParseNode,
    cpunit: &CompilationUnit,
    scope: &InnerScope,
    generic_parameters: &[Rc<GenericParameterType>],
) -> Option<Rc<NimType>> {
    match node.kind {
        NodeKind::EnumDeclaration => {
            let enum_fields = node
                .children()
                .filter(|n| n.kind == NodeKind::EnumFieldDeclaration);
            Some(Rc::new(NimType::Enum(NimEnumType {
                variants: enum_fields.map(|n| n.to_str().into()).collect(),
            })))
        }
        NodeKind::TupleConstruction => {
            // TODO: TupleConstruction not implemented
            None
        }
        NodeKind::ObjectDeclaration => {
            let is_variant = node.extract_by_kind(NodeKind::VariantDeclaration).is_some();
            if is_variant {
                parse_variant_object_type(cpunit, node, scope, generic_parameters)
            } else {
                parse_object_type(cpunit, node, scope, generic_parameters)
            }
        }
        // Resolve identifier as "Alias" to present cyclic types
        NodeKind::Identifier => Some(resolve_type_using_aliases(
            &node.to_str(),
            generic_parameters,
        )),
        NodeKind::TypeExpression => {
            parse_type_expression(&node.child(0).unwrap(), cpunit, scope, generic_parameters)
        }
        NodeKind::BracketExpression => {
            parse_type_from_bracket_expression(cpunit, node, scope, generic_parameters)
        }
        NodeKind::DistinctType => {
            let content = node.child(1);
            content
                .and_then(|node| parse_type_expression(&node, cpunit, scope, generic_parameters))
                .map(|t| Rc::new(types::NimType::Distinct(t)))
        }
        NodeKind::InfixExpression => {
            let left = node.child(0).unwrap();
            let right = node.child(2).unwrap();
            let operator = node.child(1).unwrap().to_str();
            parse_infix_operator_for_types(
                cpunit,
                scope,
                &left,
                &right,
                &operator,
                generic_parameters,
            )
        }
        NodeKind::TupleType => Some(Rc::new(NimType::Tuple(NimTupleType {
            fields: parse_fields_from_tuple_construction(node, cpunit, scope, generic_parameters),
        }))),
        NodeKind::VarType => {
            let in_var =
                parse_type_expression(&node.child(1).unwrap(), cpunit, scope, generic_parameters);
            in_var.map(|t| Rc::new(NimType::Var(t)))
        }
        NodeKind::PointerType => {
            let in_ptr =
                parse_type_expression(&node.child(1).unwrap(), cpunit, scope, generic_parameters);
            in_ptr.map(|t| Rc::new(NimType::Ptr(t)))
        }
        NodeKind::RefType => {
            let in_ref =
                parse_type_expression(&node.child(1).unwrap(), cpunit, scope, generic_parameters);
            in_ref.map(|t| Rc::new(NimType::Ref(t)))
        }
        NodeKind::OutType => {
            let in_out =
                parse_type_expression(&node.child(1).unwrap(), cpunit, scope, generic_parameters);
            in_out.map(|t| Rc::new(NimType::MagicGeneric(Rc::from("out"), vec![t])))
        }
        NodeKind::Call => {
            // For calls, the argument list only contains one element
            let argument = node
                .get_by_kind(NodeKind::ArgumentList)
                .unwrap_or_else(|| panic!("No argument list in call (sink, lent, ...)"))
                .child(0)
                .unwrap_or_else(|| {
                    panic!("Argument list is empty for type call (sink, lent, ...)")
                });
            let argument_type = parse_type_expression(&argument, cpunit, scope, generic_parameters);
            argument_type.map(|t| {
                let call_str = node.child(0).unwrap().to_str();
                Rc::new(NimType::MagicGeneric(Rc::from(call_str.as_str()), vec![t]))
            })
        }
        _ => {
            eprintln!(
                "{:?} not implemented while parsing {}",
                node.kind,
                node.loc()
            );
            None
        }
    }
}

fn extract_doc(node: &trees::ParseNode) -> String {
    let mut result = String::new();
    if node.kind == NodeKind::CommentContent {
        result.push_str(&node.to_str());
    } else {
        for child in node.children() {
            result.push_str(&extract_doc(&child));
        }
    }
    result
}

pub fn parse_type_declaration(
    cpunit: &CompilationUnit,
    node: &trees::ParseNode,
    scope: &mut InnerScope,
) -> NamedType {
    assert_eq!(node.kind, NodeKind::TypeDeclaration);

    let type_symbol_declaration = node.child(0).unwrap();
    let maybe_type_definition = node.child(2);

    let generic_param_list = type_symbol_declaration.get_by_kind(NodeKind::GenericParameterList);
    let mut sym = parse_type_symbol_declaration(&type_symbol_declaration);

    if let Some(type_def) = maybe_type_definition {
        let doc = extract_doc(&type_def);
        if !doc.is_empty() {
            sym.doc = Some(doc);
        }
    }

    if let Some(generic_param_list) = generic_param_list {
        let generic_arguments = generics::parse_generic_param_list(generic_param_list, |n| {
            parse_type_expression(&n, cpunit, scope, &[])
        });
        let underlying_type = maybe_type_definition
            .and_then(|def| parse_type_expression(&def, cpunit, scope, &generic_arguments))
            .unwrap_or_else(|| Rc::new(types::to_undefined_type(&type_symbol_declaration)));
        let generic = Rc::new(GenericType {
            underlying_type,
            generics: generic_arguments,
        });
        NamedType {
            sym,
            content: MaybeGenericType::GenericType(generic),
        }
    } else {
        let type_val =
            maybe_type_definition.and_then(|def| parse_type_expression(&def, cpunit, scope, &[]));

        let type_val =
            type_val.unwrap_or_else(|| Rc::new(types::to_undefined_type(&type_symbol_declaration)));

        NamedType {
            sym,
            content: MaybeGenericType::RegularType(type_val),
        }
    }
}
