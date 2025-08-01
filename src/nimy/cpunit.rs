use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;
use std::{collections::HashMap, path::PathBuf};

use super::installation;
use super::sourcefiles::SourceFile;

/// Represents an execution of the nim compiler.
/// Can be reused to perform type-checks as files get edited.
pub struct CompilationUnit {
    pub parser: RefCell<tree_sitter::Parser>,
    pub filecache: RefCell<HashMap<PathBuf, Rc<RefCell<SourceFile>>>>,
    pub compilation_flags: HashMap<String, String>,

    system_path: Option<PathBuf>,
}

impl CompilationUnit {
    /// Creates a new compilation unit with an empty file cache.
    pub fn new(with_system: bool) -> Self {
        let lang = tree_sitter_nim::language();
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&lang)
            .expect("Failed to create Nim parser");
        // let parser = type_sitter::Parser::new(&lang).expect("Failed to create Nim parser");

        let cu = CompilationUnit {
            parser: RefCell::new(parser),
            filecache: RefCell::new(HashMap::new()),
            compilation_flags: HashMap::new(),
            system_path: if with_system {
                installation::get_system_lib_path()
            } else {
                None
            },
        };
        if with_system {
            if let Some(system_path) = &cu.system_path {
                cu.query_file(system_path, None);
            }
        }
        cu
    }

    pub fn get_system_path(&self) -> Option<&PathBuf> {
        self.system_path.as_ref()
    }

    pub fn new_with_flags(with_system: bool, compilation_flags: HashMap<String, String>) -> Self {
        let mut cu = CompilationUnit::new(with_system);
        cu.compilation_flags = compilation_flags;
        cu
    }

    /// Get a reference to a NimFile which contains the exported symbols at the path provided.
    /// If no content is provided, the file is read to obtain the content.
    pub fn query_file(&self, path: &Path, content: Option<&[u8]>) -> Rc<RefCell<SourceFile>> {
        let nim_file = self.filecache.borrow_mut().get(path).cloned();
        match nim_file {
            None => {
                let new_file = Rc::new(RefCell::new(match content {
                    Some(content) => SourceFile::new(self, path.to_path_buf(), content),
                    None => SourceFile::new_from_file(self, path.to_path_buf()),
                }));
                self.filecache
                    .borrow_mut()
                    .insert(path.to_path_buf(), new_file.clone());
                new_file
            }
            Some(nim_file) => {
                {
                    let mut nim_file = nim_file.borrow_mut();
                    if nim_file.needs_update_according_to_last_modified() {
                        nim_file.update(self, content);
                    }
                }
                nim_file.clone()
            }
        }
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_compilation_unit_creation() {
        let env = env!("CARGO_MANIFEST_DIR");
        println!("{}, {}", file!(), env);
    }
}
