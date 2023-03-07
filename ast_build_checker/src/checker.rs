use astroast::{Block, DefineStmt, Exp, FunctionBody, Program, Statement, Value, FunctionCall, Index, AtomicExp, Var, VarAux, Call, ExpContent, UnopSign, BinopSign};
use astroparser::Rule;
use immutable_map::TreeMap;
use pest::iterators::Pair;
use types::type_parsing::parse_type;
use types::{
    array_type, boolean_type, char_type, integer_type, same_type, BitSize, IntegerSign,
    PrimitiveType, Type, undefined_integer, undefined_type
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
                    Some(exp) => Some(build_exp(exp, &known_types)),
                    None => panic!("Variable definition of {:?} has no matching expression", name.as_str()),
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

fn get_unary_op(unary_op_str: &str) -> (UnopSign, Type)  {
    match unary_op_str {
        "-" =>   (UnopSign::Neg, undefined_integer()),
        "not" => (UnopSign::Not, boolean_type()),
        "#" =>   (UnopSign::Hash, array_type(undefined_type())),
        _ => panic!("Impossible unary op")
        
    }
    
}

// the sign, the left and right types, and the return type
fn get_binary_op(binary_op_str: &str) -> (BinopSign, Type, Type) {
    match binary_op_str {
        ">>"  => (DoubleGreat, undefined_integer(), undefined_integer()),
        "<<"  => (DoubleLess, undefined_integer(), undefined_integer()),
        "<="  => (LessEquals, undefined_integer(), boolean_type())
        ">="  => (GreaterEquals, undefined_integer(), boolean_type())
        "=="  => (Equals, 
        "~="  => (Different,
        "//"  => (DoubleSlash,
        "+"   => (Plus,
        "-"   => (Minus,
        "*"   => (Mult,
        "/"   => (SingleSlash,
        "^"   => (Carrot,
        "%"   => (Percentile,
        "&"   => (Appersand,
        "~"   => (Tilda,
        "|"   => (Bar,
        ".."  => (DoubleDot,
        "<"   => (Less,
        ">"   => (Greater,
        "and" => (And,
        "or"  => (Or,
    }

}


fn build_exp(exp_pair: Pair<Rule>, known_types: &TreeMap<&str, Type>) -> Exp {

    let (expcontents, exptype) = match exp_pair.as_rule() {
        Rule::LiteralNil => (astroast::ExpContent::Nil, Type::Primitive(PrimitiveType::Unit)),
        Rule::LiteralTrue => ( astroast::ExpContent::True, boolean_type()),
        Rule::LiteralFalse => (astroast::ExpContent::False, boolean_type()),
        Rule::LiteralNumber => (astroast::ExpContent::Number(exp_pair.to_string()), integer_type(IntegerSign::Undefined, BitSize::Undefined)),
        Rule::LiteralString => (astroast::ExpContent::EString(exp_pair.to_string()), array_type(char_type())),
        Rule::AnonFuncDef => {
            let built_function = build_function_body(exp_pair, &known_types.clone());
            let function_type = built_function.function_type.clone();
            (astroast::ExpContent::AnonFuncDef(built_function), function_type)
        }
        Rule::Value => {
            let (value, value_type)  =  build_value(exp_pair, known_types);
            (astroast::ExpContent::Value(value), value_type)
        }

        Rule::UnaryOpExp => {
            let mut inners = exp_pair.into_inner();
            let (unary_op, unaryop_type) = get_unary_op(inners.next().unwrap().as_str());
            let exp = build_exp(inners.next().unwrap(), known_types);
            let exp_clone = exp.exptype.clone();
            if same_type(&unaryop_type, &(exp.exptype)) {
                (ExpContent::Unop { sign: unary_op, exp}, exp_clone)

            } else {
                panic!("Non matching sign and exp")
            }
             
        }

        Rule::BinaryOpExp {
            let mut inners = exp_pair.into_inner();
            let first = inners.next().unwrap();





        }

        Rule::TupleExp => {
            panic!("Not implemented yet")
            //TODO: transform tuple to composite type, since every element of the tuple can be of different type and this will help indexing it in the same way composite types are indexed
        }

        _ => panic!("Impossible Expression"),
    };

    Exp {
        contents: Box::new(expcontents),
        exptype
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
    let mut return_type = Type::Primitive(PrimitiveType::Unit);
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
    //TODO: Do typecheck
    match value_pair.as_rule() {

        Rule::Var => {
            let var = build_var(value_pair, known_types);
            let var_type = var.vartype.clone();
            (Value::Var(var), var_type)
        }
        Rule::FunctionCall => {
            let func = build_function_call(value_pair, known_types);
            let func_type  = func.calltype.clone();
            (Value::FunctionCall(func), func_type)
        }
        _ => panic!("Impossible Var")
    }
}


fn build_function_call(function_call_pair: Pair<Rule>,  known_types: &TreeMap<&str, Type>) -> FunctionCall {
    let mut inners =  function_call_pair.into_inner();
    let call_var = build_var(inners.next().unwrap(), known_types);
    let mut previous_type = call_var.vartype.clone();
    let mut calls = Vec::new();
    for call in inners{
        if let Type::Function(function_args_and_return) = previous_type  {
            let (call, calltype) = build_call(call, known_types, &function_args_and_return);
            calls.push(call);
            previous_type = calltype

        } else {
            panic!("You are calling a non function")
        }
    }

    FunctionCall { var: call_var, call: calls, calltype: previous_type }
     
}

fn build_var(var_pair: Pair<Rule>, known_types: &TreeMap<&str, Type>) -> Var {
    let mut inners = var_pair.into_inner();
    let (atomic_exp, atomic_type) = build_atomic_exp(inners.next().unwrap(), known_types);
    let mut previous_type = atomic_type;
    let mut var_calls_and_indexes = Vec::new();
    for varaux in inners {
        match varaux.as_rule() {
            Rule::Call => {
                if let Type::Function(function_args_and_return) = previous_type {
                    let (call, return_type) = build_call(varaux, known_types, &function_args_and_return);
                    var_calls_and_indexes.push(VarAux::Call(call));
                    previous_type = return_type;
                } else {
                    panic!("Trying to call a type that is not a function ")
                }
            } 
            Rule::Index => {
                if let Type::Composite(_) = previous_type {

                    let (indexed, attribute_type) = build_index(varaux, known_types, &previous_type);
                    var_calls_and_indexes.push(VarAux::Index(indexed));
                    previous_type = attribute_type;
                } else {
                    panic!("Trying to call a type that is not a function ")
                }

            } 
            _ => panic!("Impossible var")
        }  
    }
        //TODO: this needs some thinking:
        /*

    we need to know what function is being called and if the parameters are correct
    cause the previous index dictates the type of the arguments in the call, and the call returns a type

    like: list[4].potato(a, b, c)(4,5).some_value
    list[4] must have the attribute potato
    potato attribute must be a function (with the 3 arguments in the call)
    those arguments must be correct (as types)
    the result of that function is another function
    those arguments must be correct (as types)
    the return type of that function must contain the atribute "some_value"


*/
    Var{
        atomic_exp,
        rest: var_calls_and_indexes,
        vartype: previous_type
    }
}

/*TODO: Figure out what to do with oop calls
    Can just assume that, for example, object.method(arg1), is in fact method(object, arg1)
    also, figure out later what to do in case we code closures
*/
fn build_call(call_pair: Pair<Rule>, known_types: &TreeMap<&str, Type>, function_type: &[Type]) -> (Call, Type) {
    let call_inners = call_pair.into_inner().next().unwrap();

    match call_inners.as_rule() {
        Rule::ExpList => {
            let args_list =  call_inners.into_inner().map(|exp_inner| build_exp(exp_inner, known_types));
            let mut expected_types = function_type.iter();
            while let Some(arg) = args_list.clone().next() {
                if let Some(expected_type) = expected_types.next() {
                    if !same_type(&arg.exptype, expected_type)  {
                        panic!("Unexpected Type")
                    }
                } else {
                    panic!("Unexpected argument")
                }
            }
            let leftover_types: Vec<Type> = expected_types.cloned().collect();
            if leftover_types.len() != 1 {
                panic!("There are missing arguments")
            } else {
                (Call::ArgList(args_list.collect()), leftover_types.first().unwrap().clone())
            }

        }
        Rule::TableConstructor | Rule::LiteralString => panic!("Not implemented, and idk if i should add this"),
        _ => panic!("Impossible Call")

    }

    
} 


fn build_atomic_exp(atomic_exp: Pair<Rule>, known_types: &TreeMap<&str, Type>) -> (AtomicExp, Type)  {
    let atomic_exp = atomic_exp.into_inner().next().unwrap();
    match atomic_exp.as_rule() {
        Rule::Ident => {
            let known_type = match known_types.get(atomic_exp.as_str()) {
                Some(t) => t,
                None => panic!("Undefined ident {}", atomic_exp.as_str())


            };
            (AtomicExp::Ident(atomic_exp.to_string()), known_type.clone())
        }
        Rule::Exp => {
            let result_exp = build_exp(atomic_exp, known_types);
            let result_type = result_exp.exptype.clone();
            (AtomicExp::Exp(result_exp), result_type)
        }
        _ => panic!("Impossible atomic exp")
    } 
}

//Indexed type is the type of the variable that is actually being indexed
// returns the type of what is being idexed, for example, an array containing numbers, it should return type integer or something
fn build_index(index_pair: Pair<Rule>, known_types: &TreeMap<&str, Type>, indexed_type: &Type) -> (Index, Type) {
    //TODO: Check if index is even possible, and return the type of such indexing
    let mut index_inners = index_pair.into_inner();
    let index_inner = index_inners.next().unwrap();
    match index_inner.as_rule() {
        //a["exp"]
        //This should work on maps, tuples and arrays
        Rule::Exp => {
            let inner_exp = build_exp(index_inner, known_types);
            match indexed_type {
                //If an array or 
                Type::Array(value_type) => {
                    match inner_exp.exptype {
                        Type::Primitive(PrimitiveType::Integer(IntegerSign::Undefined, BitSize::Undefined)) => {
                            let inner_exp = update_integer_recursively(
                                &undefined_integer(),
                                &Type::Primitive( PrimitiveType::Integer(IntegerSign::Unsigned, BitSize::Size)), 
                                inner_exp);
                            (Index::Square(inner_exp), *((value_type).clone()))
                        },
                        _ => panic!("You can only index an array using a integer")
                    }
                    
                }
                //if a map, it can be indexed by string, like, a["key"]
                Type::Map(key_type, value_type) => {
                    let key_type_ref =*(key_type.clone());
                    if same_type(&inner_exp.exptype, &key_type_ref) {
                        match inner_exp.exptype {
                            Type::Primitive(PrimitiveType::Integer(IntegerSign::Undefined, BitSize::Undefined)) => {
                            let inner_exp = update_integer_recursively(
                                &undefined_integer(),
                                &Type::Primitive( PrimitiveType::Integer(IntegerSign::Unsigned, BitSize::Size)), 
                                inner_exp);
                                (Index::Square(inner_exp), *((value_type).clone()))
                                
                            }
                            _ => panic!("Indexing expression doens't have the same type as the map keys")
                        }

                    } else {
                            (Index::Square(inner_exp), *((value_type).clone()))
                    } 
                }
                Type::Tuple(_) => panic!("TODO: this arm is right, just havent' figured out how it works, Maybe do it like rust, where it can be dot indexed and where it acts more like a map indexed by dot"),
                _ => panic!("Impossible square indexing")
                //if a map, it can be indexed by string, like, a["key"]
            }

            
        },
        //a.ident
        Rule::Ident => {
            match indexed_type {
                Type::Composite(composite) => {
                    let ident = index_inner.as_str();   
                    let index_type = composite.types.get(ident).unwrap();
                    (Index::Dot(ident.to_string()), index_type.clone())
                }
                _ => panic!("There are no indexes in non composite types")
            }

        },
        _ => panic!("Impossible Index")
    }
}


fn update_integer_recursively(previous_integer: &Type, new_integer: &Type, exp: Exp) -> Exp {
    let exp_type = exp.exptype;
    let exp_contents = *exp.contents;
    let new_contents = match exp_contents {
        ExpContent::Number(ref n) => {
            if same_type(previous_integer, &exp_type) {
                ExpContent::Number(n.to_string())
            } else {
                exp_contents
            }
        }
        ExpContent::Unop { sign, exp } => {
            let new_exp = update_integer_recursively(previous_integer, new_integer, exp);
            ExpContent::Unop {
                sign,
                exp: new_exp,
            }
        }
        ExpContent::Value(_) => panic!("Not implemented yet"),
        ExpContent::AnonFuncDef(_) => panic!("Not implemented yet"),
        ExpContent::TableConstructor(_) => panic!("Not implemented yet"),
        ExpContent::Tuple(_) => panic!("Not implemented yet"),
        ExpContent::Binop { left, sign, right } => {
            let new_left = update_integer_recursively(previous_integer, new_integer, left);
            let new_right = update_integer_recursively(previous_integer, new_integer, right);
            ExpContent::Binop {
                left: new_left,
                sign,
                right: new_right,
            }
        }
        _ => exp_contents,
    };
    Exp {
        exptype: if same_type(previous_integer, &exp_type) {
            new_integer.clone()
        } else {
            exp_type
        },
        contents: Box::new(new_contents),
    }
}
