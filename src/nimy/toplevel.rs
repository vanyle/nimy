use std::{path::PathBuf, rc::Rc};

use crate::nimy::{
    compiletimeeval::evaluate_compile_time_expression,
    cpunit::CompilationUnit,
    installation,
    namedtypes::MaybeGenericType,
    namedtypes::{NamedGenericType, NamedRegularType},
    scope::InnerScope,
    sourcefiles::FileReferences,
    trees::{self, NodeKind},
    typeparser,
};

fn get_expression_list_from_import<'a, 'b>(
    node: trees::ParseNode<'a, 'b>,
) -> trees::ParseNode<'a, 'b> {
    assert_eq!(node.kind, NodeKind::ImportStatement);
    node.children().nth(1).expect("Expected at least one child")
}

pub fn extract_import_paths(node: &trees::ParseNode, current_path: &str, result: &mut Vec<String>) {
    match node.kind {
        NodeKind::Identifier => {
            if current_path.is_empty() {
                result.push(node.to_str());
            } else {
                let path_to_add = current_path.to_string();
                result.push(path_to_add + "/" + &node.to_str());
            }
        }
        NodeKind::InfixExpression => {
            let base = node.child(0).unwrap();
            let path_to_add = if current_path.is_empty() {
                base.to_str()
            } else {
                current_path.to_string() + "/" + &base.to_str()
            };
            for child in node.children().skip(1) {
                extract_import_paths(&child, &path_to_add, result);
            }
        }
        NodeKind::ArrayConstruction | NodeKind::ExpressionList => {
            for child in node.children() {
                extract_import_paths(&child, current_path, result);
            }
        }
        _ => (),
    }
}

/// We assume that expression reordering is not allowed, aka that types, functions and imports
/// are declared top to bottom and that a previous type is not allowed to refer to a later type.
pub fn handle_top_level(
    cpunit: &CompilationUnit,
    node: &trees::ParseNode,
    scope: &mut InnerScope,
    imports: &mut FileReferences,
    includes: &mut FileReferences,
    current_file_path: &Rc<PathBuf>,
) {
    for child in node.children() {
        match child.kind {
            NodeKind::ImportStatement => {
                let expr_list = get_expression_list_from_import(child);
                let mut import_paths = Vec::new();
                extract_import_paths(&expr_list, "", &mut import_paths);
                import_paths
                    .iter()
                    .filter_map(|s| installation::get_lib_path(s))
                    .for_each(|path| {
                        cpunit.query_file(&path, None);
                        if !imports.0.contains(&path) {
                            imports.0.push(path);
                        }
                    });
            }
            NodeKind::IncludeStatement => {
                crate::nimy::includes::handle_include_statement(
                    cpunit,
                    &child,
                    scope,
                    includes,
                    current_file_path,
                );
            }
            NodeKind::TypeSection => {
                handle_type_section(cpunit, &child, scope);
            }
            NodeKind::When => {
                handle_conditional(&child, cpunit, scope, imports, includes, current_file_path)
            }
            _ => {}
        }
    }
}

fn handle_conditional(
    node: &trees::ParseNode,
    cpunit: &CompilationUnit,
    scope: &mut InnerScope,
    imports: &mut FileReferences,
    includes: &mut FileReferences,
    current_file_path: &Rc<PathBuf>,
) {
    assert!(matches!(
        node.kind,
        NodeKind::When | NodeKind::ElifBranch | NodeKind::ElseBranch
    ));
    // When = "when" <compile-time-condition> ":" <statement-list> [elif-branch] [else]
    // elif-branch = "elif" <compile-time-condition> ":" <statement-list>
    // else-branch = "else" <compile-time-condition> ":" <statement-list>
    let children = node.children_without_comments().collect::<Vec<_>>();
    if children.is_empty() {
        return; // This is "when", the keyword, not the branch in the AST.
    }

    let Some(compile_time_expression) = children.get(1) else {
        // This is an error.
        return;
    };

    // The Nim compiler does not perform any checks on when branches that are not taken, which is
    // fair, but should we do the same? Typing and other static analysis for 'when' branches
    // can be helpful! We need to take a careful approach to avoid false positives.
    // As when branch can rely on types defined in other branches, some things like
    // references to undefined types inside 'when false' branches should be allowed,
    // but other things like syntax errors should not.
    // Worst case scenario, the user can add a comment/pragma to disables the checks for the branch.

    let when_val = evaluate_compile_time_expression(compile_time_expression, cpunit, scope);
    let is_truthy = when_val.is_truthy();
    // There can be "elif" branches in whens
    if is_truthy.unwrap_or(false) {
        let statements = children.get(3);
        if let Some(statements) = statements {
            handle_top_level(
                cpunit,
                statements,
                scope,
                imports,
                includes,
                current_file_path,
            );
        }
    } else {
        // else-if and else logic
        for when_child in &children {
            match when_child.kind {
                NodeKind::ElifBranch => {
                    let compile_time_expression = when_child.child(1).unwrap();
                    let elif_val =
                        evaluate_compile_time_expression(&compile_time_expression, cpunit, scope);
                    if elif_val.is_truthy().unwrap_or(false) {
                        let statements = when_child.child(3);
                        if let Some(statements) = statements {
                            handle_top_level(
                                cpunit,
                                &statements,
                                scope,
                                imports,
                                includes,
                                current_file_path,
                            );
                        }
                        return; // We found a truthy elif branch, no need to check else.
                    }
                }
                NodeKind::ElseBranch => {
                    let statements = when_child.child(2);
                    if let Some(statements) = statements {
                        handle_top_level(
                            cpunit,
                            &statements,
                            scope,
                            imports,
                            includes,
                            current_file_path,
                        );
                    }
                    return;
                }
                _ => continue,
            }
        }
    }
}

/// Inside a type block, types are allowed to refer to one another.
/// We need 2 pass. In the first pass, we build the types with placeholders and in the second pass,
/// we resolve the "undefined" placeholders.
fn handle_type_section(cpunit: &CompilationUnit, node: &trees::ParseNode, scope: &mut InnerScope) {
    let named_types = node
        .children()
        .filter(|n| n.kind == NodeKind::TypeDeclaration)
        .map(|child| typeparser::parse_type_declaration(cpunit, &child, scope))
        .collect::<Vec<_>>();

    // Add types from within the block
    for t in named_types {
        match t.content {
            MaybeGenericType::GenericType(generic) => {
                scope.generic_types.push(Rc::new(NamedGenericType {
                    sym: t.sym,
                    content: generic,
                }));
            }
            MaybeGenericType::RegularType(regular) => {
                scope.types.push(Rc::new(NamedRegularType {
                    sym: t.sym,
                    content: regular,
                }));
            }
        }
    }
}
