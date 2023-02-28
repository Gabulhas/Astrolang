mod checker;
use astroast::Program;
use astroparser::Rule;
use immutable_map::TreeMap;
use pest::iterators::Pair;

// This "known_types" come from previously loaded files that are compiled together with this current program
pub fn build_and_check_ast(
    program: Pair<Rule>,
    known_types: TreeMap<&str, types::Type>,
) -> Program {
    checker::build_program_ast(program, known_types)
}
