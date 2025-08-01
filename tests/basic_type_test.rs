use nimy::nimy::{cpunit::CompilationUnit, types};
use std::{env, path::Path};

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

    let st = root_scope.resolve_type("NestedGeneric", &compilation_unit);
    assert!(st.is_some(), "Failed to resolve NestedGeneric");

    let st = root_scope.resolve_type("MoreType", &compilation_unit);
    assert!(st.is_some(), "Failed to resolve MoreType");

    let generics = root_scope.defined_generic_names();
    assert!(
        generics.contains(&"Pair".to_string()),
        "Pair generic not found"
    );

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
