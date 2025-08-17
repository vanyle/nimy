use std::path::Path;

use crate::nimy::cpunit::CompilationUnit;

pub mod nimy;

pub fn show_available_items() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(Path::new(file!()).parent().unwrap())
        .join("../scratchpad.nim")
        .canonicalize()
        .expect("Failed to canonicalize path");
    let file = include_str!("../scratchpad.nim");

    let compilation_unit = CompilationUnit::new(false);
    let nim_file = compilation_unit.query_file(&path, Some(file.as_bytes()));

    let nim_file = nim_file.borrow();

    println!("Available procs");
    for proc in &nim_file.available_procs(&compilation_unit) {
        println!("  {proc}");
    }

    println!("Available generic procs:");
    for t in &nim_file.available_generic_procs(&compilation_unit) {
        println!("  {t}");
    }

    println!("Available types:");
    for t in &nim_file.available_types(&compilation_unit) {
        println!("  {t}");
    }

    println!("Available generics:");
    for t in &nim_file.available_generics(&compilation_unit) {
        println!("  {t}");
    }
}
