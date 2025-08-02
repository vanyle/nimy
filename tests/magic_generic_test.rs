use nimy::nimy::{cpunit::CompilationUnit, types::NimType};
use std::{env, path::Path};

#[test]
fn it_parses_magic_generic_types() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(Path::new(file!()).parent().unwrap())
        .join("magic_generic_test.nim")
        .canonicalize()
        .expect("Failed to canonicalize path");
    let file = include_str!("magic_generic_test.nim");

    let compilation_unit = CompilationUnit::new(false);
    let nim_file = compilation_unit.query_file(&path, Some(file.as_bytes()));
    let nim_file = nim_file.borrow();
    let root_scope = &nim_file.root_scope;

    // Test that magic generics are properly defined
    let defined_generics = root_scope.defined_generic_names();

    // Test known magic generics
    assert!(
        defined_generics.contains(&"seq".to_string()),
        "seq magic generic should be defined"
    );
    assert!(
        defined_generics.contains(&"ptr".to_string()),
        "ptr magic generic should be defined"
    );
    assert!(
        defined_generics.contains(&"ref".to_string()),
        "ref magic generic should be defined"
    );
    assert!(
        defined_generics.contains(&"range".to_string()),
        "range magic generic should be defined"
    );
    assert!(
        defined_generics.contains(&"array".to_string()),
        "array magic generic should be defined"
    );

    // Test unknown magic generic
    assert!(
        defined_generics.contains(&"CustomMagicGeneric".to_string()),
        "CustomMagicGeneric should be defined"
    );

    // Test that the generics have the correct structure
    let defined_generics_objects = root_scope.defined_generics();

    // Find the array generic and verify it has 2 parameters
    let array_generic = defined_generics_objects
        .iter()
        .find(|g| g.sym.name == "array")
        .expect("array generic should be found");

    assert_eq!(
        array_generic.content.generics.len(),
        2,
        "array generic should have 2 type parameters"
    );

    // Test that known magic generics are converted to their standard types
    let seq_generic = defined_generics_objects
        .iter()
        .find(|g| g.sym.name == "seq")
        .expect("seq generic should be found");

    // seq should be converted to the standard seq type, not remain as Undefined or MagicGeneric
    match seq_generic.content.underlying_type.as_ref() {
        NimType::Seq(_) => {
            // This is what we want - the magic generic was converted to a proper seq type
        }
        NimType::Undefined(_) => {
            panic!(
                "seq magic generic should not be Undefined, it should be converted to NimType::Seq"
            );
        }
        NimType::MagicGeneric(name, _) => {
            if name.as_ref() == "seq" {
                // If it's a known magic generic like seq, it should be converted to the standard type
                panic!(
                    "Known magic generic 'seq' should be converted to NimType::Seq, not remain as MagicGeneric"
                );
            }
        }
        other => {
            panic!("seq magic generic has unexpected type: {other:?}");
        }
    }

    // Test that unknown magic generics remain as MagicGeneric
    let custom_generic = defined_generics_objects
        .iter()
        .find(|g| g.sym.name == "CustomMagicGeneric")
        .expect("CustomMagicGeneric should be found");

    match custom_generic.content.underlying_type.as_ref() {
        NimType::MagicGeneric(name, _) => {
            assert_eq!(
                name.as_ref(),
                "CustomMagicGeneric",
                "MagicGeneric should preserve the name"
            );
        }
        NimType::Undefined(_) => {
            panic!("CustomMagicGeneric should be parsed as MagicGeneric, not Undefined");
        }
        other => {
            panic!("CustomMagicGeneric should be MagicGeneric, got: {other:?}");
        }
    }

    /*
    println!("Defined generic names: {defined_generics:?}");

    // Let's check the underlying type of some generics to see if they're MagicGeneric
    let defined_generics_objects = root_scope.defined_generics();
    for generic in &defined_generics_objects {
        println!("Generic {}: {}", generic.sym.name, generic.content.underlying_type);
    }
    */
}
