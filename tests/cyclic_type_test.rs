use nimy::nimy::{cpunit::CompilationUnit, types};
use std::{env, path::Path};

#[test]
fn it_resolves_cyclic_types() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(Path::new(file!()).parent().unwrap())
        .join("cyclic_type_test.nim")
        .canonicalize()
        .expect("Failed to canonicalize path");
    let file = include_str!("cyclic_type_test.nim");

    let compilation_unit = CompilationUnit::new(false);
    let nim_file = compilation_unit.query_file(&path, Some(file.as_bytes()));
    let nim_file = nim_file.borrow();
    let root_scope = &nim_file.root_scope;

    let st = root_scope.resolve_type("A", &compilation_unit);
    assert!(st.is_some(), "Failed to resolve A");
    let type_a = st.unwrap();
    eprintln!("A: {}", &type_a);

    assert!(matches!(*type_a, types::NimType::Object(..)));
    let types::NimType::Object(obj) = type_a.as_ref() else {
        panic!("Expected Object type");
    };

    assert_eq!(obj.fields.len(), 2, "Object A should have 2 fields");
}
