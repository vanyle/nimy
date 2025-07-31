use std::{env, path::Path};

use nimy::nimy::cpunit::CompilationUnit;

#[test]
fn it_resolves_imports() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(file!())
        .join("../basic_imports_test.nim")
        .canonicalize()
        .expect("Failed to canonicalize path");
    let file = include_str!("basic_imports_test.nim");

    let compilation_unit = CompilationUnit::new(false);
    let nim_file = compilation_unit.query_file(&path, Some(file.as_bytes()));

    let nim_file = nim_file.borrow();

    assert_eq!(nim_file.imports.0.len(), 4, "The file contains 4 imports");
    assert!(nim_file.imports.0[0].exists(), "The imported file exist");
}
