use core::panic;
use std::{
    cell::RefCell,
    ops::Deref,
    path::PathBuf,
    rc::{Rc, Weak},
};

use crate::nimy::{
    compiletimeeval::evaluate_compile_time_expression,
    generics::{self, GenericType},
    installation,
    namedtypes::MaybeGenericType,
    sourcefiles::{self, FileReferences},
    trees::NodeKind,
    type_constraints::NimTypeClass,
    types::{self, GenericParameterType, NimEnumType, NimTupleType},
};

use crate::nimy::{
    cpunit::CompilationUnit,
    namedtypes::{NamedGenericType, NamedRegularType, NamedType},
    symbols::Symbol,
    trees,
    types::NimType,
    values::{NimProc, NimVariable},
};

#[derive(Debug)]
pub struct Scope(Rc<RefCell<InnerScope>>);

impl Deref for Scope {
    type Target = Rc<RefCell<InnerScope>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Clone for Scope {
    fn clone(&self) -> Self {
        Scope(self.0.clone())
    }
}

#[derive(Debug)]
pub struct InnerScope {
    pub start: usize,
    pub end: usize,
    pub variables: Vec<Rc<NimVariable>>,
    pub types: Vec<Rc<NamedRegularType>>,
    pub generic_types: Vec<Rc<NamedGenericType>>,
    pub procs: Vec<Rc<NimProc>>,

    pub parent: Weak<RefCell<InnerScope>>,
    pub children: Vec<Scope>,
}

impl<'a> Scope {
    // Perform type inference on the tree and return the resulting scope.
    // Also perform typing on imported files if needed and add them to the cache.
    pub fn new(
        cpunit: &CompilationUnit,
        node: &tree_sitter::Node<'a>,
        parent: Option<Scope>,
        imports: &mut sourcefiles::FileReferences,
        _includes: &mut sourcefiles::FileReferences,
        path: &Rc<PathBuf>,
        content: &[u8],
    ) -> Self {
        // println!("fs: {}", path.display());
        let root = trees::ParseNode::new(*node, content, path);

        let start = node.start_byte();
        let end = node.end_byte();
        let children = Vec::new();

        let inner_scope = InnerScope {
            start,
            end,
            variables: Vec::new(),
            types: Vec::new(),
            generic_types: Vec::new(),
            procs: Vec::new(),
            parent: parent.map(|p| Rc::downgrade(&p.0)).unwrap_or_default(),
            children,
        };

        let result = Scope::from_inner(inner_scope);
        handle_top_level(cpunit, &root, &mut result.borrow_mut(), imports);
        result
    }

    /// Return a list of the names of the concrete types defined in the scope
    pub fn defined_type_names(&self) -> Vec<String> {
        self.borrow().defined_type_names().collect()
    }
    pub fn defined_types(&self) -> Vec<Rc<NamedRegularType>> {
        self.borrow().defined_types().collect()
    }

    /// Return a list of the names of the generic types defined in the scope
    pub fn defined_generic_names(&self) -> Vec<String> {
        self.borrow().defined_generic_names().collect()
    }
    pub fn defined_generics(&self) -> Vec<Rc<NamedGenericType>> {
        self.borrow().defined_generics().collect()
    }

    fn from_inner(inner: InnerScope) -> Self {
        Scope(Rc::new(RefCell::new(inner)))
    }

    pub fn resolve_type(&self, type_name: &str, cpunit: &CompilationUnit) -> Option<Rc<NimType>> {
        let scope = self.borrow();
        resolve_type(type_name, cpunit, &scope, &[])
    }
}

impl InnerScope {
    fn defined_type_names(&self) -> impl Iterator<Item = String> {
        self.types.iter().map(|t| t.sym.name.clone())
    }
    fn defined_types(&self) -> impl Iterator<Item = Rc<NamedRegularType>> {
        self.types.iter().cloned()
    }
    fn defined_generic_names(&self) -> impl Iterator<Item = String> {
        self.generic_types.iter().map(|t| t.sym.name.clone())
    }
    fn defined_generics(&self) -> impl Iterator<Item = Rc<NamedGenericType>> {
        self.generic_types.iter().cloned()
    }
}

fn get_expression_list_from_import<'a, 'b>(
    node: trees::ParseNode<'a, 'b>,
) -> trees::ParseNode<'a, 'b> {
    assert_eq!(node.kind, NodeKind::ImportStatement);
    node.children().nth(1).expect("Expected at least one child")
}

fn extract_import_paths(node: &trees::ParseNode, current_path: &str, result: &mut Vec<String>) {
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
fn handle_top_level(
    cpunit: &CompilationUnit,
    node: &trees::ParseNode,
    scope: &mut InnerScope,
    imports: &mut FileReferences,
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
            NodeKind::TypeSection => {
                handle_type_section(cpunit, &child, scope);
            }
            NodeKind::When => handle_conditional(&child, cpunit, scope, imports),
            _ => {}
        }
    }
}

fn handle_conditional(
    node: &trees::ParseNode,
    cpunit: &CompilationUnit,
    scope: &mut InnerScope,
    imports: &mut FileReferences,
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
            handle_top_level(cpunit, statements, scope, imports);
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
                            handle_top_level(cpunit, &statements, scope, imports);
                        }
                        return; // We found a truthy elif branch, no need to check else.
                    }
                }
                NodeKind::ElseBranch => {
                    let statements = when_child.child(2);
                    if let Some(statements) = statements {
                        handle_top_level(cpunit, &statements, scope, imports);
                    }
                    return;
                }
                _ => continue,
            }
        }
    }
}

// Inside a type block, types are allowed to refer to one another.
// We need 2 pass. In the first pass, we build the types with placeholders and in the second pass,
// we resolve the "undefined" placeholders.
fn handle_type_section(cpunit: &CompilationUnit, node: &trees::ParseNode, scope: &mut InnerScope) {
    let named_types = node
        .children()
        .filter(|n| n.kind == NodeKind::TypeDeclaration)
        .map(|child| parse_type_declaration(cpunit, &child, scope))
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

/// Given an identifier, return the type it represents.
/// If the identifier does not represent a valid type, return None
pub fn resolve_type(
    type_name: &str,
    _cpunit: &CompilationUnit,
    scope: &InnerScope,
    generic_parameters: &[Rc<GenericParameterType>],
) -> Option<Rc<NimType>> {
    // assert_eq!(type_name_node.kind, NodeKind::Identifier);
    // let type_name = type_name_node.to_str();
    let t = types::str_to_type(type_name);
    if let Some(t) = t {
        return Some(t);
    }
    // Generic type name take precedence
    for generic_parameter in generic_parameters {
        if type_name == generic_parameter.name {
            return Some(Rc::new(NimType::GenericParameter(
                generic_parameter.clone(),
            )));
        }
    }
    for t in &scope.types {
        if t.sym.name == type_name {
            return Some(t.content.clone());
        }
    }
    if let Some(parent_scope) = scope.parent.upgrade() {
        // Might be already borrowed issue here?
        let parent_scope = parent_scope.borrow();
        let resulting_type = resolve_type(type_name, _cpunit, &parent_scope, generic_parameters);
        return resulting_type;
    };
    // Try to resolve type based on imports
    // MARK: TODO Imports T

    None
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

fn parse_type_expression(
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
            println!(
                "TupleConstruction not implemented while parsing {}",
                node.loc()
            );
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

fn parse_type_declaration(
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
