pub mod cpunit;
/// Checks that Nim is installed, where it is installed, what version is installed, etc...
/// Anything related to the Nim installation.
pub mod installation;
pub mod sourcefiles;
/// Defines datastructures to represent Nim Types and variables as well as traits to display them
pub mod types;

/// Provides a way to evaluate expressions at compile time
pub mod compiletimeeval;

/// Defines type classes and sub-typing relations
pub mod type_constraints;

/// Algorithms for representing and dealing with generics.
pub mod generics;

pub mod namedprocs;
pub mod namedtypes;

/// Defines a symbol, which is a piece of string with a location in a file
pub mod symbols;

/// Defines nim values
pub mod values;

/// Scope management and symbol resolution
pub mod scope;

/// Top-level file processing and orchestration  
pub mod toplevel;

/// Type expression parsing and construction
pub mod typeparser;

/// Handles include statement processing and file inclusion
pub mod includes;

// Wrappers around tree_sitter
pub mod trees;
