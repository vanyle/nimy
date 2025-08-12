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
                | NodeKind::ArrayConstruction
                | NodeKind::InfixExpression
                | NodeKind::PrefixExpression
        )
    });

    // Determine the type
    let inferred_type = if let Some(explicit_type) = explicit_type {
        explicit_type
    } else if let Some(value) = &value_node {
        infer_type_from_value_node(value, cpunit, scope, current_file_path)
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

    let variable_mutability = match mutability {
        ValueMutability::Var => ValueMutability::Var,
        ValueMutability::Let => ValueMutability::Let,
        ValueMutability::Const(value) => ValueMutability::Const(value.clone()),
    };

    // Create variables for each name and add to scope
    for var_symbol in variable_names {
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
            mutability: variable_mutability.clone(),
            nimtype: inferred_type.clone(),
            raw_value,
        });

        scope.variables.push(variable);
    }
}

// Helper function to find a variable in the current scope or parent scopes
fn find_variable_in_scope(scope: &InnerScope, variable_name: &str) -> Option<Rc<NimVariable>> {
    // Check current scope
    for variable in &scope.variables {
        if variable.sym.name == variable_name {
            return Some(variable.clone());
        }
    }

    // Check parent scope if it exists
    if let Some(parent_scope) = scope.parent.upgrade() {
        let parent_scope = parent_scope.borrow();
        return find_variable_in_scope(&parent_scope, variable_name);
    }

    None
}

// Given a value, return its type if possible.
// Return the unknown type when inference fails.
fn infer_type_from_value_node(
    value_node: &trees::ParseNode,
    cpunit: &CompilationUnit,
    scope: &InnerScope,
    current_file_path: &Rc<PathBuf>,
) -> Rc<NimType> {
    match value_node.kind {
        NodeKind::IntegerLiteral => types::INT.with(|t| t.clone()),
        NodeKind::FloatLiteral => types::FLOAT.with(|t| t.clone()),
        NodeKind::InterpretedStringLiteral
        | NodeKind::RawStringLiteral
        | NodeKind::LongStringLiteral => types::STRING.with(|t| t.clone()),
        NodeKind::NilLiteral => Rc::new(NimType::TypeOfNil),
        NodeKind::Call => {
            // Handle procedure calls
            infer_type_from_call(value_node, cpunit, scope, current_file_path)
        }
        NodeKind::InfixExpression => {
            // Handle infix expressions like @["aa"] * @["bb"]
            infer_type_from_infix_expression(value_node, cpunit, scope, current_file_path)
        }
        NodeKind::PrefixExpression => {
            // Handle prefix expressions like @["aa"]
            infer_type_from_prefix_expression(value_node, cpunit, scope, current_file_path)
        }
        NodeKind::BracketExpression => {
            // Handle array literals like @["aa"]
            infer_type_from_bracket_expression(value_node, cpunit, scope, current_file_path)
        }
        NodeKind::ArrayConstruction => {
            // Handle array construction like ["aa"] - treat same as bracket expression
            infer_type_from_bracket_expression(value_node, cpunit, scope, current_file_path)
        }
        NodeKind::Identifier => {
            // Handle variable references - look up the variable in scope
            let identifier_name = value_node.to_str();

            // Look for the variable in the current scope and parent scopes
            if let Some(variable) = find_variable_in_scope(scope, &identifier_name) {
                variable.nimtype.clone()
            } else {
                // Variable not found, return undefined type
                Rc::new(NimType::Undefined(Symbol::new(
                    format!("unknown variable: {identifier_name}"),
                    value_node.start_byte(),
                    value_node.end_byte(),
                    None,
                    current_file_path.clone(),
                    false,
                )))
            }
        }
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

/// Infer the type from a procedure call
fn infer_type_from_call(
    call_node: &trees::ParseNode,
    cpunit: &CompilationUnit,
    scope: &InnerScope,
    current_file_path: &Rc<PathBuf>,
) -> Rc<NimType> {
    // Get the function name
    let function_name = if let Some(identifier) = call_node.get_by_kind(NodeKind::Identifier) {
        identifier.to_str()
    } else {
        // Could be an operator call or complex expression
        return Rc::new(NimType::Undefined(Symbol::new(
            "unknown".to_string(),
            call_node.start_byte(),
            call_node.end_byte(),
            None,
            current_file_path.clone(),
            false,
        )));
    };

    // Get argument types
    let argument_types = if let Some(arg_list) = call_node.get_by_kind(NodeKind::ArgumentList) {
        arg_list
            .children()
            .filter(|child| {
                // Filter out non-argument nodes like parentheses and commas
                !matches!(
                    child.kind,
                    NodeKind::LParen | NodeKind::RParen | NodeKind::Comma
                )
            })
            .map(|arg| infer_type_from_value_node(&arg, cpunit, scope, current_file_path))
            .collect::<Vec<_>>()
    } else {
        Vec::new()
    };

    // Resolve the procedure
    if let Some(proc) =
        crate::nimy::scope::resolve_proc(&function_name, &argument_types, cpunit, scope)
    {
        if let Some(return_type) = &proc.nimtype.return_type {
            return_type.clone()
        } else {
            // Procedure doesn't return anything (void)
            Rc::new(NimType::Undefined(Symbol::new(
                "void".to_string(),
                call_node.start_byte(),
                call_node.end_byte(),
                None,
                current_file_path.clone(),
                false,
            )))
        }
    } else {
        // Procedure not found
        Rc::new(NimType::Undefined(Symbol::new(
            "unknown".to_string(),
            call_node.start_byte(),
            call_node.end_byte(),
            None,
            current_file_path.clone(),
            false,
        )))
    }
}

/// Infer the type from an infix expression (binary operator)
fn infer_type_from_infix_expression(
    infix_node: &trees::ParseNode,
    cpunit: &CompilationUnit,
    scope: &InnerScope,
    current_file_path: &Rc<PathBuf>,
) -> Rc<NimType> {
    // Get the operator
    let operator = if let Some(op_node) = infix_node.get_by_kind(NodeKind::Operator) {
        op_node.to_str()
    } else {
        return Rc::new(NimType::Undefined(Symbol::new(
            "unknown".to_string(),
            infix_node.start_byte(),
            infix_node.end_byte(),
            None,
            current_file_path.clone(),
            false,
        )));
    };

    // Get left and right operands
    let children: Vec<_> = infix_node.children().collect();
    if children.len() != 3 {
        return Rc::new(NimType::Undefined(Symbol::new(
            "unknown".to_string(),
            infix_node.start_byte(),
            infix_node.end_byte(),
            None,
            current_file_path.clone(),
            false,
        )));
    }

    let left_operand = &children[0];
    let right_operand = &children[2]; // Skip the operator in the middle

    let left_type = infer_type_from_value_node(left_operand, cpunit, scope, current_file_path);
    let right_type = infer_type_from_value_node(right_operand, cpunit, scope, current_file_path);

    // Look for an operator procedure with these argument types
    let argument_types = vec![left_type, right_type];
    if let Some(proc) = crate::nimy::scope::resolve_proc(&operator, &argument_types, cpunit, scope)
    {
        if let Some(return_type) = &proc.nimtype.return_type {
            return_type.clone()
        } else {
            Rc::new(NimType::Undefined(Symbol::new(
                "void".to_string(),
                infix_node.start_byte(),
                infix_node.end_byte(),
                None,
                current_file_path.clone(),
                false,
            )))
        }
    } else {
        Rc::new(NimType::Undefined(Symbol::new(
            "unknown".to_string(),
            infix_node.start_byte(),
            infix_node.end_byte(),
            None,
            current_file_path.clone(),
            false,
        )))
    }
}

/// Infer the type from a bracket expression (like array literals)
fn infer_type_from_bracket_expression(
    bracket_node: &trees::ParseNode,
    cpunit: &CompilationUnit,
    scope: &InnerScope,
    current_file_path: &Rc<PathBuf>,
) -> Rc<NimType> {
    // For bracket expressions like ["string"], return the element type
    // The @ prefix operator will handle creating the seq[element_type]
    if let Some(arg_list) = bracket_node.get_by_kind(NodeKind::ArgumentList) {
        if let Some(first_arg) = arg_list.children().find(|child| {
            // Filter out parentheses and commas, just like in call argument parsing
            !matches!(
                child.kind,
                NodeKind::LParen | NodeKind::RParen | NodeKind::Comma
            )
        }) {
            return infer_type_from_value_node(&first_arg, cpunit, scope, current_file_path);
        }
    } else {
        // Maybe the ArrayConstruction doesn't use ArgumentList
        if let Some(first_arg) = bracket_node.children().find(|child| {
            matches!(
                child.kind,
                NodeKind::InterpretedStringLiteral
                    | NodeKind::RawStringLiteral
                    | NodeKind::LongStringLiteral
                    | NodeKind::IntegerLiteral
                    | NodeKind::FloatLiteral
            )
        }) {
            return infer_type_from_value_node(&first_arg, cpunit, scope, current_file_path);
        }
    }

    // Default to unknown
    Rc::new(NimType::Undefined(Symbol::new(
        "unknown".to_string(),
        bracket_node.start_byte(),
        bracket_node.end_byte(),
        None,
        current_file_path.clone(),
        false,
    )))
}

/// Infer the type from a prefix expression (unary operator)
fn infer_type_from_prefix_expression(
    prefix_node: &trees::ParseNode,
    cpunit: &CompilationUnit,
    scope: &InnerScope,
    current_file_path: &Rc<PathBuf>,
) -> Rc<NimType> {
    // Get the operator and operand
    let children: Vec<_> = prefix_node.children().collect();
    if children.len() != 2 {
        return Rc::new(NimType::Undefined(Symbol::new(
            "unknown".to_string(),
            prefix_node.start_byte(),
            prefix_node.end_byte(),
            None,
            current_file_path.clone(),
            false,
        )));
    }

    let operator = &children[0];
    let operand = &children[1];

    // Special case for @ operator which creates sequences
    if operator.to_str() == "@" {
        // The operand should be a bracket expression containing the element type
        let operand_type = infer_type_from_value_node(operand, cpunit, scope, current_file_path);

        // If the operand is already a sequence type (from bracket expression), return it
        // Otherwise, create seq[operand_type]
        match operand_type.as_ref() {
            types::NimType::Seq(_) => operand_type,
            _ => Rc::new(types::NimType::Seq(operand_type)),
        }
    } else {
        // For other prefix operators, try to resolve as a procedure call
        let operand_type = infer_type_from_value_node(operand, cpunit, scope, current_file_path);
        let argument_types = vec![operand_type];

        if let Some(proc) =
            crate::nimy::scope::resolve_proc(&operator.to_str(), &argument_types, cpunit, scope)
        {
            if let Some(return_type) = &proc.nimtype.return_type {
                return_type.clone()
            } else {
                Rc::new(NimType::Undefined(Symbol::new(
                    "void".to_string(),
                    prefix_node.start_byte(),
                    prefix_node.end_byte(),
                    None,
                    current_file_path.clone(),
                    false,
                )))
            }
        } else {
            Rc::new(NimType::Undefined(Symbol::new(
                "unknown".to_string(),
                prefix_node.start_byte(),
                prefix_node.end_byte(),
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
