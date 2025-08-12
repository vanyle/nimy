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
    sourcefiles, trees,
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
    _cpunit: &CompilationUnit,
    scope: &InnerScope,
    generic_parameters: &[Rc<GenericParameterType>],
) -> Option<Rc<NimType>> {
    use crate::nimy::types;

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
        let parent_scope = parent_scope.borrow();
        let resulting_type = resolve_type(type_name, _cpunit, &parent_scope, generic_parameters);
        return resulting_type;
    };
    // Try to resolve type based on imports
    // MARK: TODO Imports T

    None
}
