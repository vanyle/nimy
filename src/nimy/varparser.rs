use std::{path::PathBuf, rc::Rc};

use crate::nimy::{
    cpunit::CompilationUnit,
    scope::InnerScope,
    sourcefiles::FileReferences,
    trees::{self, NodeKind},
};

pub fn handle_var_section(
    _cpunit: &CompilationUnit,
    node: &trees::ParseNode,
    _scope: &mut InnerScope,
    _imports: &mut FileReferences,
    _includes: &mut FileReferences,
    _current_file_path: &Rc<PathBuf>,
) {
    assert!(matches!(node.kind, NodeKind::VarSection));
    // Uncomment this line to see how a node looks like.
    // println!("Var node: \n{}", &node.dbg_str());

    // let variable_ref = Rc::new(NimVariable {
    //     mutability: ValueMutability::Var,
    //     ... compute the proper fields
    // });
    // scope.variables.push(variable_ref);
}
