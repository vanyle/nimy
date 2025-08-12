use nimy::nimy::cpunit::CompilationUnit;
use std::{env, path::Path};

#[test]
fn it_lists_available_procs() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(Path::new(file!()).parent().unwrap())
        .join("when_type_test.nim")
        .canonicalize()
        .expect("Failed to canonicalize path");
    let file = include_str!("proc_test.nim");

    let compilation_unit = CompilationUnit::new(false);
    let nim_file = compilation_unit.query_file(&path, Some(file.as_bytes()));
    let nim_file = nim_file.borrow();
    let root_scope = &nim_file.root_scope.borrow();

    let proc_names = root_scope
        .procs
        .iter()
        .map(|proc| proc.sym.name.clone())
        .collect::<Vec<_>>();

    assert_eq!(
        proc_names,
        vec!["defined", "hello2"],
        "The procedure names are called defined and hello2"
    );

    // Test that defined has the correct signature.
    let defined_proc = root_scope
        .procs
        .iter()
        .find(|proc| proc.sym.name == "defined")
        .expect("There should be a defined proc");

    assert_eq!(
        format!("{}", defined_proc.nimtype),
        "proc(untyped): bool",
        "the defined proc does not match the expected signature."
    );

    let hello_proc = root_scope
        .generic_procs
        .iter()
        .find(|proc| proc.underlying_proc.sym.name == "hello")
        .expect("There should be a generic procedure named hello inside proc_test");

    // Note: might be brittle if we change how types are displayed,
    // but at least, it makes the test very readable.
    assert_eq!(
        format!("{hello_proc}"),
        "hello[A] := proc(seq[A], A): void",
        "the hello proc does not match the expected signature."
    );
}
