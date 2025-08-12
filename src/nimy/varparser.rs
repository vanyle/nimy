use std::{path::PathBuf, rc::Rc};

use crate::nimy::{
    cpunit::CompilationUnit,
    scope::InnerScope,
    sourcefiles::FileReferences,
    symbols::Symbol,
    trees::{self, NodeKind},
    typeparser::parse_type_expression,
    types::{self, NimType},
    values::{NimVariable, ValueMutability},
};

/// Handles variable section parsing (var/let/const sections).
pub fn handle_var_section(
    cpunit: &CompilationUnit,
    node: &trees::ParseNode,
    scope: &mut InnerScope,
    _imports: &mut FileReferences,
    _includes: &mut FileReferences,
    current_file_path: &Rc<PathBuf>,
    mutability: ValueMutability,
) {
    assert!(matches!(
        node.kind,
        NodeKind::VarSection | NodeKind::LetSection
    ));

    for child in node.children() {
        if child.kind == NodeKind::VariableDeclaration {
            handle_variable_declaration(cpunit, &child, scope, current_file_path, &mutability);
        }
    }
}

fn handle_variable_declaration(
    cpunit: &CompilationUnit,
    node: &trees::ParseNode,
    scope: &mut InnerScope,
    current_file_path: &Rc<PathBuf>,
    mutability: &ValueMutability,
) {
    assert_eq!(node.kind, NodeKind::VariableDeclaration);

    // Extract variable names from symbol_declaration_list
    let symbol_declaration_list = node.get_by_kind(NodeKind::SymbolDeclarationList);
    let variable_names = if let Some(symbol_list) = symbol_declaration_list {
        symbol_list
            .children()
            .filter(|n| n.kind == NodeKind::SymbolDeclaration)
            .map(|symbol_decl| {
                // Get the identifier from the symbol declaration
                if let Some(identifier) = symbol_decl.get_by_kind(NodeKind::Identifier) {
                    Symbol::new(
                        identifier.to_str(),
                        identifier.start_byte(),
                        identifier.end_byte(),
                        None,
                        current_file_path.clone(),
                        false, // TODO: check for export markers
                    )
                } else {
                    // Fallback to using the symbol declaration itself if no identifier found
                    Symbol::new(
                        symbol_decl.to_str(),
                        symbol_decl.start_byte(),
                        symbol_decl.end_byte(),
                        None,
                        current_file_path.clone(),
                        false,
                    )
                }
            })
            .collect::<Vec<_>>()
    } else {
        return; // No variable names found
    };

    // Try to extract explicit type from type_expression
    let explicit_type = node
        .get_by_kind(NodeKind::TypeExpression)
        .and_then(|type_expr| parse_type_expression(&type_expr, cpunit, scope, &[]));

    // Try to extract value for type inference
    let value_node = node.children().find(|n| {
        matches!(
            n.kind,
            NodeKind::IntegerLiteral
                | NodeKind::InterpretedStringLiteral
                | NodeKind::RawStringLiteral
                | NodeKind::LongStringLiteral
                | NodeKind::FloatLiteral
                | NodeKind::NilLiteral
                | NodeKind::Identifier
                | NodeKind::Call
                | NodeKind::BracketExpression
        )
    });

    // Determine the type
    let inferred_type = if let Some(explicit_type) = explicit_type {
        explicit_type
    } else if let Some(value) = &value_node {
        infer_type_from_value_node(value, current_file_path)
    } else {
        // No type and no value - use unknown type
        Rc::new(NimType::Undefined(Symbol::new(
            "unknown".to_string(),
            node.start_byte(),
            node.end_byte(),
            None,
            current_file_path.clone(),
            false,
        )))
    };

    // Create variables for each name and add to scope
    for var_symbol in variable_names {
        let variable_mutability = match mutability {
            ValueMutability::Var => ValueMutability::Var,
            ValueMutability::Let => ValueMutability::Let,
            ValueMutability::Const(value) => ValueMutability::Const(value.clone()),
        };

        // Extract raw value - try to get the actual value representation
        let raw_value = if let Some(value) = &value_node {
            match value.kind {
                NodeKind::InterpretedStringLiteral => {
                    // For string literals, try to extract the string content if available
                    if let Some(string_content) = value.get_by_kind(NodeKind::StringContent) {
                        string_content.to_str()
                    } else {
                        value.to_str()
                    }
                }
                _ => value.to_str(),
            }
        } else {
            String::new()
        };

        let variable = Rc::new(NimVariable {
            sym: var_symbol.clone(),
            mutability: variable_mutability,
            nimtype: inferred_type.clone(),
            raw_value,
        });

        scope.variables.push(variable);
    }
}

// Given a value, return its type if possible.
// Return the unknown type when inference fails.
fn infer_type_from_value_node(
    value_node: &trees::ParseNode,
    current_file_path: &Rc<PathBuf>,
) -> Rc<NimType> {
    match value_node.kind {
        NodeKind::IntegerLiteral => types::INT.with(|t| t.clone()),
        NodeKind::FloatLiteral => types::FLOAT.with(|t| t.clone()),
        NodeKind::InterpretedStringLiteral
        | NodeKind::RawStringLiteral
        | NodeKind::LongStringLiteral => types::STRING.with(|t| t.clone()),
        NodeKind::NilLiteral => Rc::new(NimType::TypeOfNil),
        _ => {
            // For unknown node types, create an undefined type
            Rc::new(NimType::Undefined(Symbol::new(
                "unknown".to_string(),
                value_node.start_byte(),
                value_node.end_byte(),
                None,
                current_file_path.clone(),
                false,
            )))
        }
    }
}

pub fn handle_const_section(
    _cpunit: &CompilationUnit,
    _node: &trees::ParseNode,
    _scope: &mut InnerScope,
    _imports: &mut FileReferences,
    _includes: &mut FileReferences,
    _current_file_path: &Rc<PathBuf>,
) {
}
