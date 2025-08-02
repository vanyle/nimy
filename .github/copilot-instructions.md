# Nimy - Fast Nim Type Checker and Language Server

Nimy is a Rust-based type checker, linter, and suggestion engine for the Nim programming language. It provides incremental compilation and fast type checking by interpreting a subset of Nim rather than full compilation.

## Architecture Overview

### Core Components

- **CompilationUnit** (`src/nimy/cpunit.rs`): Central orchestrator managing file caching, Nim installation detection, and compilation flags. Entry point for all type checking operations.
- **SourceFile** (`src/nimy/sourcefiles.rs`): Represents parsed Nim files with import/include tracking, incremental updates, and scope management.
- **Scope & Typer** (`src/nimy/typer.rs`): Hierarchical scope system for symbol resolution and type inference. Handles variables, types, generics, and procedures.
- **NimType System** (`src/nimy/types.rs`): Comprehensive type representation including generics, object variants, type classes, and Nim's complex type system.

### Data Flow

1. `CompilationUnit::query_file()` -> Parse/cache Nim files
2. `SourceFile::new()` -> Tree-sitter parsing -> `Scope::new()`
3. Inside `typer.rs`: Type inference walks AST, resolves imports, builds symbol tables
4. Results cached for incremental updates via file modification timestamps

## Development Workflows

### Building & Testing

```bash
cargo build                    # Standard build
cargo test -- --nocapture      # Full test output
```

You can write Nim code in `scratchpad.nim` and run the parser on it with:

```bash
cargo run
```

### Key Commands

- Uses `scratchpad.nim` as default test file in `main.rs`
- Tests follow pattern: `{test_name}.nim` + `{test_name}.rs` pairs

## Project-Specific Patterns

### File Organization

- `src/nimy/` contains all core modules (not `src/lib.rs` style)
- Each test has both `.nim` and `.rs` files for input/validation
- `nim_implementation/` contains bundled Nim stdlib as a reference and for documentation
- `vendor/` contains tree-sitter grammar definitions, also as a reference

### Type System Architecture

```rust
// Core pattern: Rc<RefCell<T>> for shared mutable state
let compilation_unit = CompilationUnit::new(true); // true = include system libs
let file = compilation_unit.query_file(&path, Some(content));
let available_types = file.borrow().available_types(&compilation_unit);
```

### Scope Management

- Hierarchical scopes with `parent: Weak<RefCell<InnerScope>>` to avoid cycles
- Symbol resolution walks scope chain upward
- Import resolution handled during scope construction

### Incremental Updates

- File modification time tracking for cache invalidation
- `SourceFile::needs_update_according_to_last_modified()`
- Content can be provided directly or read from filesystem

## Critical Dependencies

### Tree-sitter Integration

- Custom Nim grammar: `tree-sitter-nim` (git dependency)
- Node traversal via `ParseNode` wrapper in `src/nimy/trees.rs`
- AST -> type system conversion in `typer.rs`

### Nim Installation Detection

- `installation.rs` finds Nim stdlib paths automatically
- System library bundled in `nim_implementation/` for fallback
- Compilation flags support for conditional compilation

## Testing Conventions

### Test Structure

```rust
#[test]
fn it_resolves_imports() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(Path::new(file!()).parent().unwrap())
        .join("basic_imports_test.nim");

    let compilation_unit = CompilationUnit::new(false); // false = no system libs for speed
    let nim_file = compilation_unit.query_file(&path, Some(file.as_bytes()));

    // Test assertions on nim_file.borrow().imports, types, etc.
}
```

### Test Categories

- `basic_imports_test`: Import resolution and file dependencies
- `basic_type_test`: Type definitions, generics, object variants
- `cyclic_type_test`: Circular type references
- `when_type_test`: Conditional compilation (`when` statements)

## Key Implementation Details

- **RefCell + Rc pattern**: Enables shared mutable access to cached files and scopes
- **Weak references**: Prevent cycles in parent-child scope relationships
- **Tree-sitter parsing**: Single parser instance per CompilationUnit (RefCell wrapped)
- **Incremental design**: File cache + timestamp checking for performance
- **Nim subset interpretation**: Focuses on type checking over full compilation

## Development Notes

- Use `include_str!()` macro for embedding test Nim files in Rust tests
- Path canonicalization required for cross-platform file caching
- LSP integration planned but not yet implemented (`lsp-types` dependency)
- VS Code extension structure exists in `client/` but currently empty
