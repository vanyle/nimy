use nimy::nimy::cpunit::CompilationUnit;
use std::{env, path::Path};

#[test]
fn it_resolves_imports() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(Path::new(file!()).parent().unwrap())
        .join("basic_imports_test.nim");

    let path = path
        .canonicalize()
        .unwrap_or_else(|_| panic!("Failed to canonicalize {}", path.display()));
    let file = include_str!("basic_imports_test.nim");

    let compilation_unit = CompilationUnit::new(false);
    let nim_file = compilation_unit.query_file(&path, Some(file.as_bytes()));

    let nim_file = nim_file.borrow();

    assert_eq!(nim_file.imports.0.len(), 4, "The file contains 4 imports");
    assert!(nim_file.imports.0[0].exists(), "The imported file exist");
}
