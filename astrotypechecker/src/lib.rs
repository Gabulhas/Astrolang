pub mod types;

mod checker;
use astroparser::Rule;
use pest::iterators::Pair;

pub fn typecheck(program: Pair<Rule>){
    checker::typecheck_program(program)
}







