// use nimy::nimy::nimtype::{NimType, NimVariable, Scope};

use std::{env, path::Path};

use nimy::nimy::{cpunit::CompilationUnit, types};

#[test]
fn it_resolves_simple_types() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(Path::new(file!()).parent().unwrap())
        .join("basic_type_test.nim")
        .canonicalize()
        .expect("Failed to canonicalize path");
    let file = include_str!("basic_type_test.nim");

    let compilation_unit = CompilationUnit::new(false);
    let nim_file = compilation_unit.query_file(&path, Some(file.as_bytes()));
    let nim_file = nim_file.borrow();
    let root_scope = &nim_file.root_scope;

    let st = root_scope.resolve_type("SimpleType", &compilation_unit);
    assert!(st.is_some(), "Failed to resolve SimpleType");
    let st = st.unwrap();
    assert!(types::INT.with(|i| i == &st));

    /*
    println!("Parsed types:");
    for t in &root_scope.types {
        println!("  {t}");
    }
    println!("Parsed generics:");
    for t in &root_scope.generic_types {
        println!("  {t}");
    }
    */
}

#[test]
fn it_parses_reports_type_errors() {
    // assert_eq!(false, true, "This test is not implemented yet.");
}
