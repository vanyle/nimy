use std::{
    cell::RefCell,
    collections::HashMap,
    ops::Deref,
    path::PathBuf,
    rc::{Rc, Weak},
};

use crate::nimy::{
    cpunit::CompilationUnit,
    generics::GenericProc,
    namedprocs::NimProc,
    namedtypes::{NamedGenericType, NamedRegularType},
    sourcefiles::{self, SourceFile},
    trees,
    types::{GenericParameterType, NimType},
    values::NimVariable,
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
    pub generic_procs: Vec<Rc<GenericProc>>,

    pub parent: Weak<RefCell<InnerScope>>,
    pub children: Vec<Scope>,

    // Cache for included files - maps include path to parsed tree
    pub include_cache: HashMap<PathBuf, tree_sitter::Tree>,
}

impl<'a> Scope {
    /// Perform type inference on the tree and return the resulting scope.
    /// Also perform typing on imported files if needed and add them to the cache.
    pub fn new(
        cpunit: &CompilationUnit,
        node: &tree_sitter::Node<'a>,
        parent: Option<Scope>,
        imports: &mut sourcefiles::FileReferences,
        includes: &mut sourcefiles::FileReferences,
        path: &Rc<PathBuf>,
        content: &[u8],
    ) -> Self {
        println!("fs: {}", path.display());
        let root: trees::ParseNode<'_, '_> = trees::ParseNode::new(*node, content, path);

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
            generic_procs: Vec::new(),
            parent: parent.map(|p| Rc::downgrade(&p.0)).unwrap_or_default(),
            children,
            include_cache: HashMap::new(),
        };

        let result = Scope::from_inner(inner_scope);
        crate::nimy::toplevel::handle_top_level(
            cpunit,
            &root,
            &mut result.borrow_mut(),
            imports,
            includes,
            path,
        );
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

/// Given an identifier, return the type it represents.
/// If the identifier does not represent a valid type, return None
pub fn resolve_type(
    type_name: &str,
    cpunit: &CompilationUnit,
    scope: &InnerScope,
    generic_parameters: &[Rc<GenericParameterType>],
) -> Option<Rc<NimType>> {
    resolve_type_with_imports(type_name, cpunit, scope, generic_parameters, None)
}

/// Resolve a procedure by name and argument types, considering overloading
pub fn resolve_proc(
    proc_name: &str,
    arg_types: &[Rc<NimType>],
    _cpunit: &CompilationUnit,
    scope: &InnerScope,
) -> Option<Rc<NimProc>> {
    // Look for procedures with matching name
    for proc in &scope.procs {
        if proc.sym.name == proc_name && proc.nimtype.arguments.len() == arg_types.len() {
            // Check if argument types match
            let types_match = proc.nimtype.arguments.iter().zip(arg_types.iter()).all(
                |(param_type, arg_type)| {
                    crate::nimy::types::types_are_compatible(param_type, arg_type)
                },
            );

            if types_match {
                return Some(proc.clone());
            }
        }
    }

    // Check parent scope
    if let Some(parent_scope) = scope.parent.upgrade() {
        let parent_scope = parent_scope.borrow();
        return resolve_proc(proc_name, arg_types, _cpunit, &parent_scope);
    }

    None
}

/// Given an identifier, return the type it represents, checking imports if source_file is provided.
/// If the identifier does not represent a valid type, return None
pub fn resolve_type_with_imports(
    type_name: &str,
    cpunit: &CompilationUnit,
    scope: &InnerScope,
    generic_parameters: &[Rc<GenericParameterType>],
    source_file: Option<&SourceFile>,
) -> Option<Rc<NimType>> {
    use crate::nimy::types;

    // Check if it's a basic type first
    let t = types::str_to_type(type_name);
    if let Some(t) = t {
        return Some(t);
    }

    // Check if it's a generic parameter
    for generic_param in generic_parameters {
        if generic_param.name == type_name {
            return Some(Rc::new(NimType::GenericParameter(generic_param.clone())));
        }
    }

    // Look for regular types in current scope
    for named_type in &scope.types {
        if named_type.sym.name == type_name {
            return Some(named_type.content.clone());
        }
    }

    // Look for generic types in current scope
    for generic_type in &scope.generic_types {
        if generic_type.sym.name == type_name {
            return Some(Rc::new(NimType::AliasGeneric(
                generic_type.sym.name.clone().into(),
                vec![],
            )));
        }
    }

    // Check parent scope
    if let Some(parent_scope) = scope.parent.upgrade() {
        let parent_scope = parent_scope.borrow();
        if let Some(result) = resolve_type_with_imports(
            type_name,
            cpunit,
            &parent_scope,
            generic_parameters,
            source_file,
        ) {
            return Some(result);
        }
    }

    // Try to resolve type based on imports
    if let Some(source_file) = source_file {
        // Check available types from imports
        for available_type in source_file.available_types(cpunit) {
            if available_type.sym.name == type_name {
                return Some(available_type.content.clone());
            }
        }

        // Check available generic types from imports
        for available_generic in source_file.available_generics(cpunit) {
            if available_generic.sym.name == type_name {
                return Some(Rc::new(NimType::AliasGeneric(
                    available_generic.sym.name.clone().into(),
                    vec![],
                )));
            }
        }
    }

    None
}
