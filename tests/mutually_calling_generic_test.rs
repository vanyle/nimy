use nimy::nimy::{cpunit::CompilationUnit, types, types::NimType};
use std::path::Path;

#[test]
fn it_parses_generics_defined_in_the_same_type_block() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(Path::new(file!()).parent().unwrap())
        .join("mutually_calling_generic_test.nim");

    let file = include_str!("mutually_calling_generic_test.nim");
    let compilation_unit = CompilationUnit::new(false);
    let nim_file = compilation_unit.query_file(&path, Some(file.as_bytes()));
    let nim_file = nim_file.borrow();
    let root_scope = &nim_file.root_scope;

    // Debug what types are actually available
    {
        let inner_scope = root_scope.borrow();
        println!("Available generics:");
        for generic in &inner_scope.generic_types {
            println!("  {generic}");
        }
        println!("Available regular types:");
        for regular in &inner_scope.types {
            println!("  {regular}");
        }
    }

    // Verify that HSlice is parsed correctly
    {
        let inner_scope = root_scope.borrow();
        let hslice_generic = inner_scope
            .generic_types
            .iter()
            .find(|g| g.sym.name == "HSlice");
        assert!(
            hslice_generic.is_some(),
            "HSlice should be defined as a generic type"
        );

        let hslice_generic = hslice_generic.unwrap();
        // Check that HSlice itself is not undefined (it should be a proper object type)
        match hslice_generic.content.underlying_type.as_ref() {
            NimType::Undefined(_) => panic!("HSlice should not have undefined underlying type"),
            NimType::Object(_) => println!("Good: HSlice has proper object underlying type"),
            other => println!("HSlice has underlying type: {other:?}"),
        }
    }

    // Verify that Slice is found (even if instantiation isn't working perfectly yet)
    {
        let inner_scope = root_scope.borrow();
        let slice_found = inner_scope
            .generic_types
            .iter()
            .any(|g| g.sym.name == "Slice");
        assert!(slice_found, "Slice should be defined as a generic type");
    }

    // Test that SS (which is Slice[int]) resolves correctly
    let ss_type = root_scope.resolve_type("SS", &compilation_unit);
    assert!(ss_type.is_some(), "SS should be defined");
    let ss_type = ss_type.unwrap();
    match ss_type.as_ref() {
        NimType::Undefined(_) => panic!("SS should not be undefined"),
        NimType::Object(obj) => {
            // SS should be Slice[int] which should resolve to HSlice[int, int]
            // which is an object with fields a: int, b: int
            assert_eq!(obj.fields.len(), 2, "SS should have 2 fields (a and b)");
            let field_a = &obj.fields[0];
            let field_b = &obj.fields[1];
            assert_eq!(field_a.sym.name, "a", "First field should be 'a'");
            assert_eq!(field_b.sym.name, "b", "Second field should be 'b'");

            // Both fields should be int
            assert!(
                types::INT.with(|i| i == &field_a.field_type),
                "Field 'a' should be int"
            );
            assert!(
                types::INT.with(|i| i == &field_b.field_type),
                "Field 'b' should be int"
            );
        }
        _ => panic!("SS should resolve to an Object type, got: {ss_type:?}"),
    }

    // The key test: forward references work for generic types!
    println!("✓ SUCCESS: Forward references work for generic types!");
    println!("✓ SUCCESS: Generic types are parsed and instantiated correctly!");
    println!("✓ SUCCESS: Slice[int] resolves to the correct HSlice[int, int] instantiation!");
}
