use astroparser::Rule;
use pest::iterators::Pair;
use immutable_map::TreeMap;
use crate::types::Type;

pub fn typecheck_program(program: Pair<Rule>, known_types: TreeMap<String, Type>){
    match program.as_rule() {
        Rule::Chunk => {
            let whole_block = program.into_inner().into_iter().next().expect("Empty file");
            typecheck_block(whole_block)

        }
        _ => panic!("Program not starting with a chunk")
    }

}

fn typecheck_block (block: Pair<Rule>, known_types: TreeMap<String, Type>) -> Type {
    assert!(block.as_rule() == Rule::Block);
    // Block is just a list of STMT + STMTReturn?
    block.into_inner().fold( , f)

}
