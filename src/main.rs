use std::path::Path;

use ::nimy::nimy::cpunit::CompilationUnit;

pub mod nimy;

fn main() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(Path::new(file!()).parent().unwrap())
        .join("../scratchpad.nim")
        .canonicalize()
        .expect("Failed to canonicalize path");
    let file = include_str!("../scratchpad.nim");

    let compilation_unit = CompilationUnit::new(false);
    let nim_file = compilation_unit.query_file(&path, Some(file.as_bytes()));

    let nim_file = nim_file.borrow();
    let root_scope = &nim_file.root_scope.borrow();
    println!("Parsed types:");
    for t in &root_scope.types {
        println!("  {t}");
    }

    println!("Parsed generics:");
    for t in &root_scope.generic_types {
        println!("  {t}");
    }
}
