use astroparser::{AstroParser, Rule};
use astrotypechecker::build_and_check_ast;
use immutable_map::TreeMap;
use pest::Parser;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();

    let unparsed_file = fs::read_to_string(&args[1]).expect("cannot read file");

    let file = AstroParser::parse(Rule::Chunk, &unparsed_file)
        .unwrap_or_else(|e| panic!("{}", e))
        .next()
        .unwrap();

    let ast = build_and_check_ast(file, TreeMap::new());
    println!("{:#?}", ast)
}
