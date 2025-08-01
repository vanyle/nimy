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

pub mod namedtypes;

/// Defines a symbol, which is a piece of string with a location in a file
pub mod symbols;

/// Defines nim values
pub mod values;

/// Contains functions needed to convert from the tree-sitter AST
/// to our own representation and datastructures
pub mod typer;

// Wrappers around tree_sitter
pub mod trees;
