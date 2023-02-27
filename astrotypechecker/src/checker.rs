use astroparser::Rule;
use pest::iterators::Pair;
use crate::types;

pub fn typecheck_program(program: Pair<Rule>){
    match program.as_rule() {
        Rule::Chunk => {
            let whole_block = program.into_inner().into_iter().next().expect("Empty file");
            typecheck_block(whole_block)

        }
        _ => panic!("Program not starting with a chunk")
    }

}

fn typecheck_block (block: Pair<Rule>) {
    assert!(block.as_rule() == Rule::Block);
    // Block is just a list of STMT + STMTReturn?




}
