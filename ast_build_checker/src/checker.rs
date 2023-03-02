use astroast::{Block, DefineStmt, Exp, FunctionBody, Program, Statement, Value, FunctionCall};
use astroparser::Rule;
use immutable_map::TreeMap;
use pest::iterators::Pair;
use types::type_parsing::parse_type;
use types::{
    array_type, boolean_type, char_type, integer_type, same_type, BitSize, IntegerSign,
    PrimitiveType, Type,
};

pub fn build_program_ast(program: Pair<Rule>, known_types: TreeMap<&str, Type>) -> Program {
    match program.as_rule() {
        Rule::Chunk => {
            let whole_block = program.into_inner().into_iter().next().expect("Empty file");
            Program {
                contents: build_block(whole_block, &known_types),
            }
        }
        _ => panic!("Program not starting with a chunk"),
    }
}

fn build_block(block: Pair<Rule>, known_types: &TreeMap<&str, Type>) -> Block {
    assert!(block.as_rule() == Rule::Block);
    // Block is just a list of STMT + STMTReturn?
    let result_statement = Vec::new();
    for statement in block.into_inner() {
        match statement.as_rule() {
            Rule::StmtDefine => build_stmtdefine(statement, known_types),
            _ => panic!("Impossible statement"),
        };
    }
    Block {
        statements: result_statement,
    }
}

fn build_stmtdefine<'a>(
    stmt: Pair<'a, Rule>,
    known_types: &TreeMap<&'a str, Type>,
) -> (Vec<Statement>, TreeMap<&'a str, Type>) {
    let mut definitions_and_exps = stmt.into_inner();
    let mut definitions = definitions_and_exps.next().unwrap().into_inner();
    let mut exps = definitions_and_exps.nth(1).map(|a| a.into_inner());
    let mut known_types = known_types.clone();
    // This is used to check if the order of the types are correct
    let mut result_definitions = Vec::new();

    let mut current_definition = 0;

    while let Some(name) = definitions.next() {
        if let Some(vtype) = definitions.next() {
            let new_type = parse_type(vtype);
            known_types = known_types.insert(name.as_span().as_str(), new_type.clone());

            let definition_matching_exp = match exps {
                None => None,
                Some(ref mut matching_exps) => match matching_exps.nth(current_definition) {
                    Some(exp) => Some(build_exp(exp, known_types.clone())),
                    None => panic!("Variable definition of {} has no matching expression", name),
                },
            };

            if let Some(ref exp) = definition_matching_exp {
                if !same_type(&exp.exptype, &new_type) {
                    panic!("The expression doesn't match the type defined")
                }
            }

            result_definitions.push(Statement::Define(DefineStmt {
                var: name.as_span().as_str().to_string(),
                exp: definition_matching_exp,
            }));

            current_definition += 1;
        } else {
            panic!("Variable name without type")
        }
    }

    //TODO, still check if there are more expressions than definitions

    (result_definitions, known_types)
}

fn build_exp(exp_pair: Pair<Rule>, known_types: TreeMap<&str, Type>) -> Exp {
    match exp_pair.as_rule() {
        Rule::LiteralNil => Exp {
            contents: Box::new(astroast::ExpContent::Nil),
            exptype: Type::Primitive(PrimitiveType::Void),
        },
        Rule::LiteralTrue => Exp {
            contents: Box::new(astroast::ExpContent::True),
            exptype: boolean_type(),
        },
        Rule::LiteralFalse => Exp {
            contents: Box::new(astroast::ExpContent::False),
            exptype: boolean_type(),
        },
        Rule::LiteralNumber => Exp {
            contents: Box::new(astroast::ExpContent::Number(exp_pair.to_string())),
            exptype: integer_type(IntegerSign::Signed, BitSize::B32),
        }, // This is the default string. If not otherwise stated, a integer is always i32
        Rule::LiteralString => Exp {
            contents: Box::new(astroast::ExpContent::EString(exp_pair.to_string())),
            exptype: array_type(char_type()),
        },
        Rule::AnonFuncDef => {
            let built_function = build_function_body(exp_pair, &known_types.clone());
            let function_type = built_function.function_type.clone();
            Exp {
                contents: Box::new(astroast::ExpContent::AnonFuncDef(built_function)),
                exptype: function_type,
            }
        }
        Rule::Value => {
            let (value, value_type)  =  build_value(exp_pair, &known_types);
            Exp {
                contents: Box::new(astroast::ExpContent::Value(value)),
                exptype: value_type
            }
        }

        _ => panic!("Impossible Expression"),
    }
}

//Used to recursivly change the expression types, as they default to i32, and might defined as u64 or something
//fn change_exp_type(exp: Exp, previous_type: Type, new_type: Type) {}
fn build_function_body(exp_pair: Pair<Rule>, known_types: &TreeMap<&str, Type>) -> FunctionBody {
    let parlist_type_and_block = exp_pair.into_inner();
    let mut parameter_list = Vec::new();
    let mut types = Vec::new();
    let mut block = Block {
        statements: Vec::new(),
    };
    let mut return_type = Type::Primitive(PrimitiveType::Void);
    let mut known_types = known_types.clone();

    for pair in parlist_type_and_block {
        match pair.as_rule() {
            Rule::Block => block = build_block(pair, &known_types),
            Rule::ParList => {
                //Add stuff to known types
                let mut parlist_inners = pair.into_inner();
                while let Some(name) = parlist_inners.next() {
                    if let Some(vtype) = parlist_inners.next() {
                        let new_type = parse_type(vtype);
                        known_types = known_types.insert(name.as_span().as_str(), new_type.clone());
                        types.push(new_type);
                        parameter_list.push(name.as_str().to_string());
                    }
                }
            }
            _ => {
                return_type = parse_type(pair);
            }
        }
    }
    types.push(return_type);
    FunctionBody {
        parameter_list,
        function_type: Type::Function(types),
        block,
    }
}

fn build_value(value_pair: Pair<Rule>, known_types: &TreeMap<&str, Type>) -> (Value, Type) {
    match value_pair.as_rule() {

        Rule::FunctionCall => build_value
    }
}


fn build_function_call(function_call_pair: Pair<Rule>,  known_types: &TreeMap<&str, Type>) -> FunctionCall {

}

fn build_var(var_pair: Pair<Rule>, known_types: &TreeMap<&str, Type>) -> Var {

}
