use astroparser::Rule;
use pest::iterators::{Pair, Pairs};
use immutable_map::TreeMap;
use types::{Type, BitSize, PrimitiveType};
use astroast::{Program, Block, Statement};

pub fn build_program_ast(program: Pair<Rule>, known_types: TreeMap<&str, Type>) -> Program {
    match program.as_rule() {
        Rule::Chunk => {
            let whole_block = program.into_inner().into_iter().next().expect("Empty file");
            Program{
                contents: typecheck_block(whole_block, known_types)    
            }
        }
        _ => panic!("Program not starting with a chunk")
    }

}

fn build_block (block: Pair<Rule>, known_types: TreeMap<&str, Type>) -> Block {
    assert!(block.as_rule() == Rule::Block);
    // Block is just a list of STMT + STMTReturn?
    let result_statement = Vec::new();
    for statement in block.into_inner(){
        match statement.as_rule() {
            Rule::StmtDefine => build_stmtdefine(statement.into_inner(), known_types)
            _ => panic!("Impossible statement") 
        }
    }
    Block {
        statements: result_statement
    }

}

fn build_stmtdefine(definitions_and_exps: Pairs<Rule>, known_types: TreeMap<&str, Type>) -> (Vec<Statement>, TreeMap<&str, Type>) {
    
    let definitions = definitions_and_exps.next().unwrap().into_inner();

    let r = Type::Primitive(PrimitiveType::Unit);
    let a = TreeMap::new().insert("key", r);

    while let Some(name) =  definitions.next() {
        if let Some(vtype) = definitions.next() {
            known_types = known_types.is_empty

        } else {
            panic!("Variable name without type")
        }

    }

    

     
}

fn infer_text() {

}
