extern crate pest;

#[macro_use]
extern crate pest_derive;

#[derive(Parser)]
#[grammar = "astro.pest"]
pub struct AstroParser;
