use std::{path::PathBuf, rc::Rc};

use crate::nimy::{
    cpunit::CompilationUnit,
    installation,
    sourcefiles::FileReferences,
    trees::{self, NodeKind},
    typer::InnerScope,
};

/// Handle an include statement by extracting the paths and processing the files
pub fn handle_include_statement(
    cpunit: &CompilationUnit,
    node: &trees::ParseNode,
    scope: &mut InnerScope,
    includes: &mut FileReferences,
    current_file_path: &Rc<PathBuf>,
) {
    assert_eq!(node.kind, NodeKind::IncludeStatement);

    let include_paths = extract_include_paths(node, current_file_path);
    for include_path in include_paths {
        process_include_file(cpunit, &include_path, scope, includes);
    }
}

/// Extract the file paths from an include statement node
/// This returns a list of paths since include can have multiple paths: include "pathA", "pathB"
fn extract_include_paths(
    node: &trees::ParseNode,
    current_file_path: &Rc<PathBuf>,
) -> Vec<PathBuf> {
    let mut paths = Vec::new();
    
    // Include statements are typically: include "path/to/file" or include path/to/file
    // Get the second child which should contain the path expression(s)
    let path_expr = match node.children().nth(1) {
        Some(expr) => expr,
        None => return paths,
    };

    // Handle different forms of path expressions
    match path_expr.kind {
        NodeKind::ExpressionList => {
            // Handle multiple paths like: include "pathA", "pathB" or include pathA, pathB
            for child in path_expr.children() {
                if let Some(path) = extract_single_path(&child, current_file_path) {
                    paths.push(path);
                }
            }
        }
        _ => {
            // Handle single path
            if let Some(path) = extract_single_path(&path_expr, current_file_path) {
                paths.push(path);
            }
        }
    }

    paths
}

/// Extract a single path from a path expression node
fn extract_single_path(
    node: &trees::ParseNode,
    current_file_path: &Rc<PathBuf>,
) -> Option<PathBuf> {
    // Extract the string content from the path expression
    let path_str = match node.kind {
        NodeKind::InterpretedStringLiteral
        | NodeKind::LongStringLiteral
        | NodeKind::RawStringLiteral => {
            // For string literals, we need to extract the content between quotes
            // The string literal node should have a string_content child
            if let Some(content_node) = node.children().find(|child| child.kind == NodeKind::StringContent) {
                content_node.to_str()
            } else {
                // Fallback: remove quotes manually
                let raw_path_str = node.to_str();
                if (raw_path_str.starts_with('"') && raw_path_str.ends_with('"'))
                    || (raw_path_str.starts_with('\'') && raw_path_str.ends_with('\''))
                {
                    raw_path_str[1..raw_path_str.len() - 1].to_string()
                } else {
                    raw_path_str
                }
            }
        }
        NodeKind::Identifier => node.to_str(),
        _ => {
            return None;
        }
    };

    resolve_include_path(&path_str, current_file_path)
}

/// Resolve an include path relative to the current file or in standard library
fn resolve_include_path(path_str: &str, current_file_path: &Rc<PathBuf>) -> Option<PathBuf> {
    // Try to resolve the path relative to the current file first
    let current_dir = current_file_path.parent()?;
    let relative_path = current_dir.join(path_str);

    // Add .nim extension if not present
    let relative_path = if relative_path.extension().is_none() {
        relative_path.with_extension("nim")
    } else {
        relative_path
    };

    if relative_path.exists() {
        return Some(relative_path);
    }

    // If not found relative to current file, try using the standard library resolution
    installation::get_lib_path(path_str)
}

/// Process an include file by parsing it and merging its content into the current scope
fn process_include_file(
    cpunit: &CompilationUnit,
    include_path: &PathBuf,
    scope: &mut InnerScope,
    includes: &mut FileReferences,
) {
    // Track the include relationship
    if !includes.0.contains(include_path) {
        includes.0.push(include_path.clone());
    }

    // Check if we've already processed this include in this scope context
    if let Some(cached_tree) = scope.include_cache.get(include_path).cloned() {
        // Use cached tree - clone it to avoid borrowing issues
        process_include_tree(cpunit, &cached_tree, scope, includes, include_path);
    } else {
        // Read and parse the include file
        match std::fs::read(include_path) {
            Ok(content) => {
                // Parse the content using tree-sitter
                let tree = {
                    let mut parser = cpunit.parser.borrow_mut();
                    parser.parse(&content, None)
                };

                if let Some(tree) = tree {
                    // Process the tree first (parser borrow is now dropped)
                    process_include_tree(cpunit, &tree, scope, includes, include_path);

                    // Cache the parsed tree after processing
                    scope.include_cache.insert(include_path.clone(), tree);
                }
            }
            Err(_) => {
                // File not found or read error - silently ignore for now
                // In a real implementation, we might want to report this error
            }
        }
    }
}

/// Process the parsed tree of an included file and merge its symbols into the current scope
fn process_include_tree(
    cpunit: &CompilationUnit,
    tree: &tree_sitter::Tree,
    scope: &mut InnerScope,
    includes: &mut FileReferences,
    include_path: &PathBuf,
) {
    // Read the file content again to create ParseNode (tree-sitter needs the content)
    if let Ok(content) = std::fs::read(include_path) {
        let include_path_rc = Rc::new(include_path.clone());
        let root_node = trees::ParseNode::new(tree.root_node(), &content, &include_path_rc);

        // Process the included content as if it were part of the current file
        // This directly merges the symbols into the current scope
        super::typer::handle_top_level(
            cpunit,
            &root_node,
            scope,
            &mut FileReferences(Vec::new()),
            includes,
            &include_path_rc,
        );
    }
}
