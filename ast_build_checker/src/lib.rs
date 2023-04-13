mod checker;
use astroast::Program;
use astroparser::Rule;
use immutable_map::TreeMap;
use pest::iterators::Pair;
use types::{string_type, unit_type, Type};

pub fn temp_known_types() -> Vec<(&'static str, types::Type)> {
    vec![("print", Type::Function(vec![string_type(), unit_type()]))]
}

// This "known_types" come from previously loaded files that are compiled together with this current program
pub fn build_and_check_ast(
    program: Pair<Rule>,
    known_types: TreeMap<&str, types::Type>,
) -> Program {
    //TODO: this is temporary

    let mut known_types = known_types.clone();
    for (key, val) in temp_known_types() {
        known_types = known_types.insert(key, val.clone());
    }
    checker::build_program_ast(program, known_types)
}
