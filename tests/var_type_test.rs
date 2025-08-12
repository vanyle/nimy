use nimy::nimy::cpunit::CompilationUnit;
use nimy::nimy::values::ValueMutability;
use std::{env, path::Path};

#[test]
fn it_lists_and_resolves_the_types_of_variables() {
    // This test verifies that the variable parser correctly:
    // 1. Finds all variable declarations in var/let sections
    // 2. Infers types from literal values (int from 2, string from "text")
    // 3. Parses explicit type annotations (seq[bool], int)
    // 4. Extracts raw values correctly
    // 5. Handles different mutability levels (var vs let)

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

    let get_var_by_name = |name: &str| {
        root_scope
            .iter()
            .find(|v| v.sym.name == name)
            .unwrap_or_else(|| panic!("{name} should exist"))
    };

    // Check individual variable types
    let my_integer_var = get_var_by_name("my_integer_var");

    assert_eq!(
        my_integer_var.nimtype.to_string(),
        "int",
        "my_integer_var should be int, got: {}",
        my_integer_var.nimtype
    );
    assert_eq!(
        my_integer_var.raw_value, "2",
        "my_integer_var should have raw value '2'"
    );
    assert!(
        matches!(my_integer_var.mutability, ValueMutability::Var),
        "my_integer_var should be var"
    );

    let my_other_integer_var = get_var_by_name("my_other_integer_var");
    assert_eq!(
        my_other_integer_var.nimtype.to_string(),
        "int",
        "my_other_integer_var should be int, got: {}",
        my_other_integer_var.nimtype
    );

    let some_list = get_var_by_name("some_list");
    assert_eq!(
        format!("{}", some_list.nimtype),
        "seq[bool]",
        "some_list should be seq[bool], got: {}",
        some_list.nimtype
    );
    assert_eq!(
        some_list.raw_value, "",
        "some_list should have no raw value (explicit type, no assignment)"
    );
    assert!(
        matches!(some_list.mutability, ValueMutability::Var),
        "some_list should be var"
    );

    let other_variable = get_var_by_name("other_variable");
    assert_eq!(
        format!("{}", other_variable.nimtype),
        "int",
        "other_variable should be explicitly typed as int, got: {}",
        other_variable.nimtype
    );
    assert_eq!(
        other_variable.raw_value, "3",
        "other_variable should have raw value '3'"
    );
    assert!(
        matches!(other_variable.mutability, ValueMutability::Let),
        "other_variable should be let"
    );

    let a_string = get_var_by_name("a_string");
    assert_eq!(
        format!("{}", a_string.nimtype),
        "string",
        "a_string should be inferred as string, got: {}",
        a_string.nimtype
    );
    assert_eq!(
        a_string.raw_value, "This is a piece of text.",
        "a_string should have the correct string content"
    );
    assert!(
        matches!(a_string.mutability, ValueMutability::Var),
        "a_string should be var"
    );
}
