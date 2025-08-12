use nimy::nimy::cpunit::CompilationUnit;
use std::{env, path::Path};

#[test]
fn it_resolves_the_types_of_variables_from_expressions() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(Path::new(file!()).parent().unwrap())
        .join("var_from_proc_test.nim")
        .canonicalize()
        .expect("Failed to canonicalize path");
    let file = include_str!("var_from_proc_test.nim");

    let compilation_unit = CompilationUnit::new(false);
    let nim_file = compilation_unit.query_file(&path, Some(file.as_bytes()));
    let nim_file = nim_file.borrow();
    let root_scope_ref = &nim_file.root_scope;
    let root_scope = root_scope_ref.borrow();

    let variables = &root_scope.variables;

    let get_var_by_name = |name: &str| {
        variables
            .iter()
            .find(|v| v.sym.name == name)
            .unwrap_or_else(|| panic!("{name} should exist"))
    };

    // Check individual variable types
    let var1 = get_var_by_name("var1");
    assert_eq!(
        var1.nimtype.to_string(),
        "seq[string]",
        "var1 should be a seq[string]"
    );

    let var2 = get_var_by_name("var2");
    assert_eq!(var2.nimtype.to_string(), "float", "var2 should be a float");

    let var3 = get_var_by_name("var3");
    assert_eq!(var3.nimtype.to_string(), "int", "var3 should be an int");

    let var4 = get_var_by_name("var4");
    assert_eq!(var4.nimtype.to_string(), "float", "var4 should be a float");
}
