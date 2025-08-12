use std::{path::PathBuf, rc::Rc};

use crate::nimy::{
    compiletimeeval::evaluate_compile_time_expression,
    cpunit::CompilationUnit,
    generics::{self, GenericType},
    includes::handle_include_statement,
    installation,
    namedtypes::{MaybeGenericType, NamedGenericType, NamedRegularType},
    procparser::handle_proc_declaration,
    scope::InnerScope,
    sourcefiles::FileReferences,
    trees::{self, NodeKind},
    typeparser::{self},
    types::NimType,
    varparser::handle_var_section,
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
        // println!("Top level node: \n{}", &child.dbg_str());
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
            NodeKind::VarSection => {
                handle_var_section(cpunit, &child, scope, imports, includes, current_file_path);
            }
            NodeKind::IncludeStatement => {
                handle_include_statement(cpunit, &child, scope, includes, current_file_path);
            }
            NodeKind::TypeSection => {
                handle_type_section(cpunit, &child, scope);
            }
            NodeKind::When => {
                handle_when(&child, cpunit, scope, imports, includes, current_file_path)
            }
            NodeKind::ProcDeclaration => handle_proc_declaration(&child, scope, cpunit),
            _ => {}
        }
    }
}

fn handle_when(
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
/// we resolve the alias generics.
fn handle_type_section(cpunit: &CompilationUnit, node: &trees::ParseNode, scope: &mut InnerScope) {
    let named_types = node
        .children()
        .filter(|n| n.kind == NodeKind::TypeDeclaration)
        .map(|child| typeparser::parse_type_declaration(cpunit, &child, scope))
        .collect::<Vec<_>>();

    // Separate regular and generic types for processing
    let mut regular_types = Vec::new();
    let mut generic_types = Vec::new();

    for t in named_types {
        match t.content {
            MaybeGenericType::GenericType(generic) => {
                generic_types.push(NamedGenericType {
                    sym: t.sym,
                    content: generic,
                });
            }
            MaybeGenericType::RegularType(regular) => {
                regular_types.push(NamedRegularType {
                    sym: t.sym,
                    content: regular,
                });
            }
        }
    }

    // Second pass: resolve deferred generic instantiations only for types in this type block
    let (resolved_regular_types, resolved_generic_types) =
        resolve_deferred_generics(cpunit, scope, regular_types, generic_types);

    // Add resolved types to the scope
    for regular_type in resolved_regular_types {
        scope.types.push(Rc::new(regular_type));
    }

    for generic_type in resolved_generic_types {
        scope.generic_types.push(Rc::new(generic_type));
    }
}

/// Resolves AliasGeneric types for a specific list of types (i.e., within a type block)
fn resolve_deferred_generics(
    cpunit: &CompilationUnit,
    scope: &InnerScope,
    regular_types: Vec<NamedRegularType>,
    generic_types: Vec<NamedGenericType>,
) -> (Vec<NamedRegularType>, Vec<NamedGenericType>) {
    let mut current_regular_types = regular_types;
    let mut current_generic_types = generic_types;

    // Keep track of resolved types to detect cycles
    let mut resolved_any = true;
    let mut max_iterations = current_regular_types.len() + current_generic_types.len() + 10;

    while resolved_any && max_iterations > 0 {
        resolved_any = false;
        max_iterations -= 1;

        // Resolve regular types
        let new_regular_types: Vec<NamedRegularType> = current_regular_types
            .iter()
            .map(|regular_type| {
                if let Some(new_content) = resolve_alias_generics_in_type_with_lists(
                    &regular_type.content,
                    cpunit,
                    scope,
                    &current_generic_types,
                ) {
                    resolved_any = true;
                    NamedRegularType {
                        sym: regular_type.sym.clone(),
                        content: new_content,
                    }
                } else {
                    regular_type.clone()
                }
            })
            .collect();
        current_regular_types = new_regular_types;

        // Resolve generic types
        let new_generic_types: Vec<NamedGenericType> = current_generic_types
            .iter()
            .map(|generic_type| {
                if let Some(new_underlying) = resolve_alias_generics_in_type_with_lists(
                    &generic_type.content.underlying_type,
                    cpunit,
                    scope,
                    &current_generic_types,
                ) {
                    resolved_any = true;
                    NamedGenericType {
                        sym: generic_type.sym.clone(),
                        content: Rc::new(GenericType {
                            underlying_type: new_underlying,
                            generics: generic_type.content.generics.clone(),
                        }),
                    }
                } else {
                    generic_type.clone()
                }
            })
            .collect();
        current_generic_types = new_generic_types;
    }

    if max_iterations == 0 {
        //eprintln!(
        //    "Warning: Max iterations reached in resolve_deferred_generics. Possible cyclic dependencies."
        //);
    }

    (current_regular_types, current_generic_types)
}

/// Recursively resolves AliasGeneric types within a given type using type lists
fn resolve_alias_generics_in_type_with_lists(
    nim_type: &Rc<NimType>,
    cpunit: &CompilationUnit,
    scope: &InnerScope,
    generic_types: &[NamedGenericType],
) -> Option<Rc<NimType>> {
    match nim_type.as_ref() {
        NimType::AliasGeneric(generic_name, args) => {
            // Try to resolve the generic type now
            if let Some(generic_type) =
                resolve_generic_type_with_lists(generic_name, cpunit, scope, generic_types)
            {
                // First, recursively resolve any AliasGeneric types in the arguments
                let resolved_args: Vec<Rc<NimType>> = args
                    .iter()
                    .map(|arg| {
                        resolve_alias_generics_in_type_with_lists(arg, cpunit, scope, generic_types)
                            .unwrap_or_else(|| arg.clone())
                    })
                    .collect();

                let inst = generics::GenericInstanciation {
                    arguments: resolved_args,
                };
                generics::instanciate_generic_type(&generic_type, &inst)
            } else {
                // Still can't resolve, keep as AliasGeneric but try to resolve arguments
                let resolved_args: Vec<Rc<NimType>> = args
                    .iter()
                    .map(|arg| {
                        resolve_alias_generics_in_type_with_lists(arg, cpunit, scope, generic_types)
                            .unwrap_or_else(|| arg.clone())
                    })
                    .collect();

                if resolved_args
                    .iter()
                    .zip(args.iter())
                    .any(|(new_arg, old_arg)| !Rc::ptr_eq(new_arg, old_arg))
                {
                    Some(Rc::new(NimType::AliasGeneric(
                        generic_name.clone(),
                        resolved_args,
                    )))
                } else {
                    None // No change
                }
            }
        }
        _ => {
            // Use the map function to recursively resolve children
            let mapped_type = nim_type.map(|child_type| {
                resolve_alias_generics_in_type_with_lists(child_type, cpunit, scope, generic_types)
                    .unwrap_or_else(|| child_type.clone())
            });

            // Check if anything changed by comparing pointer equality
            if Rc::ptr_eq(nim_type, &mapped_type) {
                None // No change
            } else {
                Some(mapped_type)
            }
        }
    }
}

/// Helper function to resolve generic types using type lists instead of scope
fn resolve_generic_type_with_lists(
    generic_name: &str,
    _cpunit: &CompilationUnit,
    scope: &InnerScope,
    generic_types: &[NamedGenericType],
) -> Option<Rc<GenericType>> {
    // Check for well known names
    if let Some(t) = generics::str_to_generic_type(generic_name) {
        return Some(t);
    }

    // Check in the provided generic types list (current type block)
    for generic in generic_types {
        if generic.sym.name == generic_name {
            return Some(generic.content.clone());
        }
    }

    // Check for locally defined names in scope (parent scopes)
    for generic in &scope.generic_types {
        if generic.sym.name == generic_name {
            return Some(generic.content.clone());
        }
    }

    None
}
