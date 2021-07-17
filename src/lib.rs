//! Crate `hcl` is a Rust library for working with [HCL](https://github.com/hashicorp/hcl).

#![deny(missing_debug_implementations)]

#[cfg_attr(test, macro_use)]
extern crate pest;

mod ast;
mod parse;

pub use ast::*;
pub use parse::parse as parse_pest;
use parse::HclParser;

#[cfg(debug_assertions)]
const _GRAMMAR: &'static str = include_str!("hcl.pest");

pub fn parse(input: &str) -> Result<ast::ConfigFile, Box<dyn std::error::Error>> {
    Ok(HclParser::config_file(parse_pest(input)?.single()?)?)
}
