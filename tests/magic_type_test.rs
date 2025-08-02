use nimy::nimy::{
    cpunit::CompilationUnit,
    types::{self, NimType},
};
use std::{env, path::Path};

#[test]
fn it_parses_magic_types() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(Path::new(file!()).parent().unwrap())
        .join("magic_type_test.nim")
        .canonicalize()
        .expect("Failed to canonicalize path");
    let file = include_str!("magic_type_test.nim");

    let compilation_unit = CompilationUnit::new(false);
    let nim_file = compilation_unit.query_file(&path, Some(file.as_bytes()));
    let nim_file = nim_file.borrow();
    let root_scope = &nim_file.root_scope;

    // Test that magic types are parsed correctly
    let int_type = root_scope.resolve_type("int", &compilation_unit);
    assert!(int_type.is_some(), "Failed to resolve int magic type");
    let int_type = int_type.unwrap();

    // For known magic types like int, they should be converted to their standard type
    assert!(
        types::INT.with(|i| i == &int_type),
        "int magic type should be converted to NimType::Int"
    );

    let string_type = root_scope.resolve_type("string", &compilation_unit);
    assert!(string_type.is_some(), "Failed to resolve string magic type");
    let string_type = string_type.unwrap();
    assert!(
        types::STRING.with(|s| s == &string_type),
        "string magic type should be converted to NimType::String"
    );

    // For unknown magic types, they should remain as Magic variants
    let custom_magic_type = root_scope.resolve_type("CustomMagic", &compilation_unit);
    assert!(
        custom_magic_type.is_some(),
        "Failed to resolve CustomMagic magic type"
    );
    let custom_magic_type = custom_magic_type.unwrap();

    match custom_magic_type.as_ref() {
        NimType::Magic(sym) => {
            assert_eq!(
                sym.name, "CustomMagic",
                "Magic type should preserve the symbol name"
            );
        }
        _ => panic!("CustomMagic should be parsed as NimType::Magic, got: {custom_magic_type:?}"),
    }

    // Test int8 - should also be Magic since it's not in str_to_type
    let int8_type = root_scope.resolve_type("int8", &compilation_unit);
    assert!(int8_type.is_some(), "Failed to resolve int8 magic type");
    let int8_type = int8_type.unwrap();

    match int8_type.as_ref() {
        NimType::Magic(sym) => {
            assert_eq!(
                sym.name, "int8",
                "int8 magic type should preserve the symbol name"
            );
        }
        _ => panic!("int8 should be parsed as NimType::Magic, got: {int8_type:?}"),
    }

    /*
    println!("Parsed types:");
    for t in &root_scope.types {
        println!("  {t}");
    }
    */
}
