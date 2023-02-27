use astroparser::{AstroParser, Rule};
use pest::Parser;
use std::fs;
use std::env;


fn main() {
    let args: Vec<String> = env::args().collect();


    let unparsed_file = fs::read_to_string(&args[1])
        .expect("cannot read file");

    let file = AstroParser::parse(Rule::Chunk, &unparsed_file).expect("fail to parse")
        .next()
        .unwrap();

        println!(
            "Parsed: {:#?}",
            file.into_inner()
        );


}
