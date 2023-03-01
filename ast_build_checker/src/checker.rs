use astroast::{Block, Program, Statement, DefineStmt};
use astroparser::Rule;
use core::panicking::panic;
use immutable_map::TreeMap;
use pest::iterators::{Pair, Pairs};
use std::iter;
use types::type_parsing::{parse_simpletype, parse_type};
use types::{same_type, BitSize, PrimitiveType, Type};

pub fn build_program_ast(program: Pair<Rule>, known_types: TreeMap<&str, Type>) -> Program {
    match program.as_rule() {
        Rule::Chunk => {
            let whole_block = program.into_inner().into_iter().next().expect("Empty file");
            Program {
                contents: typecheck_block(whole_block, known_types),
            }
        }
        _ => panic!("Program not starting with a chunk"),
    }
}

fn build_block(block: Pair<Rule>, known_types: TreeMap<&str, Type>) -> Block {
    assert!(block.as_rule() == Rule::Block);
    // Block is just a list of STMT + STMTReturn?
    let result_statement = Vec::new();
    for statement in block.into_inner() {
        match statement.as_rule() {
            Rule::StmtDefine => build_stmtdefine(statement.into_inner(), known_types),
            _ => panic!("Impossible statement"),
        }
    }
    Block {
        statements: result_statement,
    }
}

fn build_stmtdefine(
    definitions_and_exps: Pairs<Rule>,
    known_types: TreeMap<&str, Type>,
) -> (Vec<Statement>, TreeMap<&str, Type>) {
    let definitions = definitions_and_exps.nth(0).unwrap().into_inner();
    let exps = match definitions_and_exps.nth(1) {
        Some(a) => Some(a.into_inner()),
        None => None,
    };

    let mut known_types = known_types;
    // This is used to check if the order of the types are correct
    let mut result_definitions = Vec::new();

    let mut was_any_exp_defined = false;

    let mut current_definition = 0;

    while let Some(name) = definitions.next() {
        if let Some(vtype) = definitions.next() {
            let new_type = parse_type(vtype);
            known_types = known_types.insert(name.as_span().as_str(), new_type);

            let definition_matching_exp = match exps {
                None => None,
                Some(matching_exps) => match matching_exps.nth(current_definition) {
                    Some(exp) => Some(build_exp(exp, known_types)),
                    None => panic!("Variable definion of {} has no matching expression", name.to_string())
                },
            };

            if let Some(exp) = definition_matching_exp {
                if exp.exptype != new_type {
                    panic("The expression")
                }
            }

            result_definitions.push(Statement::Define(DefineStmt {
                var: name.as_span().as_str().to_string(),
                exp: definition_matching_exp,
            }));

            current_definition = current_definition + 1;
        } else {
            panic!("Variable name without type")
        }
    }

    //TODO, still check if there are more expressions than definitions

    (definitions, known_types)
}

fn build_exp(exp: Pair<Rule>, known_types: TreeMap<&str, Type>) -> Exp {}
