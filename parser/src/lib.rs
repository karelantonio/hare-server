//! Functions and structures to parse Hare source files and process them
//! For lex-specific info see the [`lex`] module
//! For parse-specific info, see the [`ast`] module

/// Lex functions and structures
pub mod lex;

/// Ast functions and structures
pub mod ast;

#[cfg(test)]
mod tests;
