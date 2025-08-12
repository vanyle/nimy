use nimy::nimy::cpunit::CompilationUnit;
use std::{env, path::Path};

#[test]
fn it_lists_and_resolves_the_types_of_variables() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(Path::new(file!()).parent().unwrap())
        .join("var_type_test.nim")
        .canonicalize()
        .expect("Failed to canonicalize path");
    let file = include_str!("var_type_test.nim");

    let compilation_unit = CompilationUnit::new(false);
    let nim_file = compilation_unit.query_file(&path, Some(file.as_bytes()));
    let nim_file = nim_file.borrow();
    let root_scope = &nim_file.root_scope.borrow_mut().variables;

    let variable_names = root_scope
        .iter()
        .map(|var| var.sym.name.clone())
        .collect::<Vec<_>>();
    assert_eq!(
        variable_names,
        vec!["my_integer_var", "some_list", "other_variable", "a_string"],
        "Variable names should match"
    );
}
