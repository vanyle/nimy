use nimy::nimy::{cpunit::CompilationUnit, types};
use std::{env, path::Path};

#[test]
fn it_resolves_cyclic_types() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(Path::new(file!()).parent().unwrap())
        .join("when_type_test.nim")
        .canonicalize()
        .expect("Failed to canonicalize path");
    let file = include_str!("when_type_test.nim");

    let compilation_unit = CompilationUnit::new(false);
    let nim_file = compilation_unit.query_file(&path, Some(file.as_bytes()));
    let nim_file = nim_file.borrow();
    let root_scope = &nim_file.root_scope;

    let st = root_scope.resolve_type("B", &compilation_unit);
    assert!(
        st.is_none(),
        "B should not exist as a type. There is an issue with comparisons of compile-time values."
    );

    let type_a = root_scope
        .resolve_type("A", &compilation_unit)
        .expect("Failed to resolve A");
    assert!(
        matches!(*type_a, types::NimType::Int),
        "A is incorrectly typed, the content of a when true block should be evaluated."
    );

    let type_ab = root_scope
        .resolve_type("AB", &compilation_unit)
        .expect("Failed to resolve AB");
    assert!(
        matches!(*type_ab, types::NimType::Bool),
        "AB is incorrectly typed, there was an issue with nested when blocks."
    );

    root_scope
        .resolve_type("C", &compilation_unit)
        .expect("Unable to resolve C, There was an issue with when/else-if/else");
}
