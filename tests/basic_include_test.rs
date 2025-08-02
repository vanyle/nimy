use nimy::nimy::cpunit::CompilationUnit;
use std::{env, path::Path};

#[test]
fn it_resolves_includes() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(Path::new(file!()).parent().unwrap())
        .join("basic_include_test/basic_include_test.nim");

    let path = path
        .canonicalize()
        .unwrap_or_else(|_| panic!("Failed to canonicalize {}", path.display()));
    let file = include_str!("basic_include_test/basic_include_test.nim");

    let compilation_unit = CompilationUnit::new(false);
    let nim_file = compilation_unit.query_file(&path, Some(file.as_bytes()));

    let nim_file = nim_file.borrow();
    let type_names = nim_file.available_type_names(&compilation_unit);

    // Verify that includes were tracked
    assert_eq!(
        nim_file.includes.0.len(),
        3,
        "The file should have 3 includes"
    );

    assert!(
        nim_file.includes.0[0].exists(),
        "The first included file should exist"
    );

    assert!(
        nim_file.includes.0[1].exists(),
        "The second included file should exist"
    );

    assert!(
        nim_file.includes.0[2].exists(),
        "The third included file should exist"
    );

    // Check that we have types from the included file
    assert!(
        type_names.contains(&"IncludedType".to_string()),
        "Should have IncludedType from included file. Available types: {type_names:?}"
    );
    assert!(
        type_names.iter().any(|name| name == "AnotherType"),
        "Should have AnotherType from included file. Available types: {type_names:?}"
    );

    // Check we also have the types from the files included with quotes
    assert!(
        type_names.contains(&"IncludedType2".to_string()),
        "Should have IncludedType2 from included file. Available types: {type_names:?}"
    );
    assert!(
        type_names.contains(&"IncludedType3".to_string()),
        "Should have IncludedType3 from included file. Available types: {type_names:?}"
    );
}
