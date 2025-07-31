use std::{path::PathBuf, rc::Rc};

use crate::nimy::trees;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub start_byte: usize,
    pub end_byte: usize,
    pub doc: Option<String>,
    pub file: Rc<PathBuf>,
    pub is_exported: bool,
}

impl Symbol {
    pub fn new(
        name: String,
        start_byte: usize,
        end_byte: usize,
        doc: Option<String>,
        file: Rc<PathBuf>,
        is_exported: bool,
    ) -> Self {
        Symbol {
            name,
            start_byte,
            end_byte,
            doc,
            file,
            is_exported,
        }
    }

    pub fn from_node(node: &trees::ParseNode, is_exported: bool) -> Self {
        Symbol {
            name: node.to_str(),
            start_byte: node.start_byte(),
            end_byte: node.end_byte(),
            doc: None, // Documentation extraction can be added later
            file: Rc::new(node.path.to_path_buf()),
            is_exported,
        }
    }
}
