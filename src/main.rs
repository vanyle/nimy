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

    let compilation_unit = CompilationUnit::new(true);
    let nim_file = compilation_unit.query_file(&path, Some(file.as_bytes()));

    let nim_file = nim_file.borrow();
    println!("Available types:");
    for t in &nim_file.available_types(&compilation_unit) {
        println!("  {t}");
    }

    println!("Available generics:");
    for t in &nim_file.available_generics(&compilation_unit) {
        println!("  {t}");
    }
}
