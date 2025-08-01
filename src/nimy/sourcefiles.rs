use std::cell::RefCell;
use std::iter;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::nimy::namedtypes::{NamedGenericType, NamedRegularType};
use crate::nimy::{cpunit::CompilationUnit, trees::ParseTree, typer::Scope};

/// Represents a parsed Nim file
#[derive(Debug)]
pub struct SourceFile {
    pub path: Rc<PathBuf>,
    pub last_updated: std::time::SystemTime,

    pub tree: Option<tree_sitter::Tree>,

    pub includes: FileReferences,
    pub imports: FileReferences,

    pub root_scope: Scope,
}

#[derive(Debug)]
pub struct FileReferences(pub Vec<PathBuf>);

impl SourceFile {
    /// Creates a new NimFile from a given path, using the content provided.
    /// Assumes that the file might not exist on the filesystem.
    pub fn new(cpunit: &CompilationUnit, path: PathBuf, content: &[u8]) -> Self {
        let path = Rc::new(path);
        let tree = parse_content(cpunit, content, &path);

        let mut imports = FileReferences(Vec::new());
        let mut includes = FileReferences(Vec::new());

        let root_scope = Scope::new(
            cpunit,
            &tree.root_node(),
            None,
            &mut imports,
            &mut includes,
            &path,
            content,
        );
        SourceFile {
            path,
            last_updated: std::time::SystemTime::now(),
            tree: None,
            imports,
            includes,
            root_scope,
        }
    }

    pub fn new_from_file(cpunit: &CompilationUnit, path: PathBuf) -> Self {
        let content = std::fs::read(&path)
            .unwrap_or_else(|err| panic!("Failed to read Nim file {}: {}", path.display(), err));
        SourceFile::new(cpunit, path, &content)
    }

    pub fn needs_update_according_to_last_modified(&self) -> bool {
        self.last_updated < get_last_modified(&self.path)
    }

    /// Use the filesystem to update the file and its definitions
    pub fn update_from_fs(&mut self, cpunit: &CompilationUnit) {
        self.last_updated = get_last_modified(&self.path);
        let content = std::fs::read(self.path.as_ref()).unwrap_or_else(|err| {
            panic!("Failed to read Nim file {}: {}", self.path.display(), err)
        });

        self.update(cpunit, Some(&content));
    }

    /// Update the imports, exports, etc... of the file
    /// You need to call this when the content of a file changes to get up-to-date information.
    /// If you do not provide the new_content, we try to read the file.
    pub fn update(&mut self, cpunit: &CompilationUnit, new_content: Option<&[u8]>) {
        let Some(new_content) = new_content else {
            return self.update_from_fs(cpunit);
        };

        let tree = parse_content(cpunit, new_content, &self.path);
        let root = tree.root_node();

        // Prints the kinds of all the nodes in the tree. Useful to get a feel of how a full tree looks like.
        // println!("{:?}", &root.node.to_sexp());
        self.root_scope = Scope::new(
            cpunit,
            &root,
            None,
            &mut self.imports,
            &mut self.includes,
            &self.path,
            new_content,
        );

        self.tree = Some(tree.tree);
    }

    /// Return a list of concrete types defined inside the file
    pub fn defined_type_names(&self) -> Vec<String> {
        self.root_scope.defined_type_names()
    }

    pub fn defined_generic_names(&self) -> Vec<String> {
        self.root_scope.defined_generic_names()
    }

    pub fn defined_types(&self) -> Vec<Rc<NamedRegularType>> {
        self.root_scope.defined_types()
    }

    pub fn defined_generics(&self) -> Vec<Rc<NamedGenericType>> {
        self.root_scope.defined_generics()
    }

    fn files_needed_to_resolve_current_file<'a>(
        &'a self,
        cpunit: &'a CompilationUnit,
    ) -> Box<dyn Iterator<Item = Rc<RefCell<SourceFile>>> + 'a> {
        let system_lib = cpunit.get_system_path();
        let files: Box<dyn Iterator<Item = Rc<RefCell<SourceFile>>>> = Box::new(
            self.imports
                .0
                .iter()
                .map(|path| cpunit.query_file(path, None)),
        );
        if let Some(system_lib) = system_lib {
            Box::new(files.chain(iter::once(cpunit.query_file(system_lib, None))))
        } else {
            files
        }
    }

    pub fn available_types(&self, cpunit: &CompilationUnit) -> Vec<Rc<NamedRegularType>> {
        let files = self.files_needed_to_resolve_current_file(cpunit);
        let types = files
            .flat_map(|file| {
                file.borrow()
                    .defined_types()
                    .into_iter()
                    .filter(|t| t.sym.is_exported)
            })
            .chain(self.defined_types());

        types.collect::<Vec<_>>()
    }

    pub fn available_type_names(&self, cpunit: &CompilationUnit) -> Vec<String> {
        self.available_types(cpunit)
            .iter()
            .map(|t| t.sym.name.clone())
            .collect()
    }

    pub fn available_generics(&self, cpunit: &CompilationUnit) -> Vec<Rc<NamedGenericType>> {
        let files = self.files_needed_to_resolve_current_file(cpunit);
        files
            .flat_map(|file| {
                file.borrow()
                    .defined_generics()
                    .into_iter()
                    .filter(|t| t.sym.is_exported)
            })
            .chain(self.defined_generics())
            .collect::<Vec<_>>()
    }

    pub fn available_generic_names(&self, cpunit: &CompilationUnit) -> Vec<String> {
        self.available_generics(cpunit)
            .iter()
            .map(|g| g.sym.name.clone())
            .collect()
    }
}

fn parse_content<'a>(
    cpunit: &CompilationUnit,
    content: &'a [u8],
    file_parsed: &'a Path,
) -> ParseTree<'a> {
    ParseTree::new(&mut cpunit.parser.borrow_mut(), content, file_parsed)
        .unwrap_or_else(|| panic!("Parsing of Nim file {} failed", file_parsed.display()))
}

fn get_last_modified(path: &PathBuf) -> std::time::SystemTime {
    // We can deal with files that do not exist yet as they are only in the IDE buffer.
    // In that case, we consider that the last modified time is the current time.
    std::fs::metadata(path)
        .and_then(|metadata| metadata.modified())
        .unwrap_or_else(|_| std::time::SystemTime::now())
}

impl std::fmt::Display for SourceFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NimFile({})", self.path.display())
    }
}
