# Nimy, A type-checker and suggestion engine for Nim

A **fast** type-checker, linter, suggestion engine and VSCode extension for [Nim](https://nim-lang.org/).

## Why?

Nimsuggest is slow, especially on large projects. This is because Nim can perform
arbitrarly complicated operations at compile-time and does not have incremental compilation.

Nimy interprets a subset of Nim to provide a good compromise between performance and usability.
Nimy also features incremental checks.

Moreover, Nimy tries to sort suggestions by relevance and take types into account when suggesting.

## Working on Nimy

- You'll need a working Nim Installation in your PATH, at least v2.
- Build the project locally with `cargo build`
- Start bacon to enable watch mode for tests: `bacon test-nowarn`
- Start coding!

## TODOs

- [x] List types defined in a file
- [x] List generics types and evaluate generics
- [x] Imports API
- [x] `include` support
- [x] Inference of dependent generic types defined in the same type block, even out-of-order
- [ ] Support `when` with:
  - [x] Simple expressions (true and false)
  - [x] Basic comparison operators (==)
  - [ ] `defined`
  - [ ] Complex boolean operators
  - [ ] More comparison operators
  - [ ] Compilation flags
  - [ ] current_platform / current_os
- [x] List procedures defined in a file
- [x] Type inference for variables
  - [x] Variables with explicit type hint
  - [x] Variables with no type hint using basic type inference from literals
  - [x] Support for var/let declarations
  - [x] Variable defined as an expression (like `var a = some_function(b, c)`)
  - [ ] Support for const declarations
- [ ] Suggestions:
  - [ ] Suggest types
  - [ ] Suggest procedures based on their names
  - [ ] Suggest procedures in a method call syntax context using the type of the first argument
  - [ ] Suggest variables
- [ ] LSP integration for suggestions
- [ ] VSCode integration
- [ ] Generating errors
- [ ] Specific imports (import from, import except)
- [ ] Indexing and caching
- [ ] Parallelism
