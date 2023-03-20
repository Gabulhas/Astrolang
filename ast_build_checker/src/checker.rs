use std::collections::HashMap;

use astroast::{Block, DefineStmt, Exp, FunctionBody, Program, Statement, Value, FunctionCall, Index, AtomicExp, Var, VarAux, Call, ExpContent, UnopSign, BinopSign, FunctionVariant, FuncName};
use astroparser::Rule;
use immutable_map::TreeMap;
use pest::iterators::Pair;
use types::type_parsing::parse_type;
use types::{
    array_type, boolean_type, char_type, integer_type, same_type, BitSize, IntegerSign,
    PrimitiveType, Type, undefined_integer, undefined_type, undefined_number, Composite
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
    let mut result_statement = Vec::new();
    let mut known_types = known_types.clone();
    for statement in block.into_inner() {
        match statement.as_rule() {
            Rule::StmtDefine => {
                let (new_stmt, new_known_types) =  build_stmtdefine(statement, &known_types);
                known_types = new_known_types;
                for stmt in new_stmt {
                    result_statement.push(stmt);
                }
            }
            Rule::StmtAssign => {
                let new_stmt = build_stmtassign(statement, &known_types);
                for stmt in new_stmt {
                    result_statement.push(stmt)
                }
            }
            Rule::StmtFuncCall => 
                result_statement.push(Statement::FunctionCall(build_function_call(statement.into_inner().next().unwrap(), &known_types))),
            Rule::StmtBreak => 
                result_statement.push(Statement::Break),
            Rule::StmtWhile => 
                result_statement.push(build_stmtwhile(statement, &known_types)),
            Rule::StmtIf => 
                result_statement.push(build_stmtif(statement, &known_types)),
            Rule::StmtForIndex => 
                result_statement.push(build_forindex(statement, &known_types)),
            Rule::StmtForEach =>
                result_statement.push(build_foreach(statement, &known_types)),
            Rule::StmtDefineType => {
                let (new_stmt, new_known_types) =  build_define_type(statement, known_types);
                known_types = new_known_types;
                result_statement.push(new_stmt)
            }
            Rule::StmtFuncDef => {
                let (new_stmt, new_known_types) =  build_functiondefinition(statement, known_types);
                known_types = new_known_types;
                result_statement.push(new_stmt)
            }
            _ => panic!("Impossible statement {:#?}", statement),
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


fn build_stmtassign<'a>(
    stmt: Pair<'a, Rule>,
    known_types: &TreeMap<&'a str, Type>,
) -> Vec<Statement> {
    let mut assignments_and_exps = stmt.into_inner();
    let assignments = assignments_and_exps.next().unwrap().into_inner();
    let mut exps = assignments_and_exps.next().unwrap().into_inner();

    let mut result_assignments = Vec::new();

    for var_pair in assignments {
        if let Some(exp_pair) = exps.next() {
            let var = build_var(var_pair, known_types);
            let exp = build_exp(exp_pair, known_types);
            if same_type(&var.vartype, &exp.exptype) {
                result_assignments.push(Statement::Assign(astroast::AssignStmt { var, exp }));
            } else  {
                match (&var.vartype, &exp.exptype) {
                    (Type::Primitive(PrimitiveType::Integer(_, _)), Type::Primitive(PrimitiveType::Integer(IntegerSign::Undefined, BitSize::Undefined))) 
                        | (Type::Primitive(PrimitiveType::Integer(_, _)), Type::UndefinedNumber ) => {

                        let exp = update_integer_recursively(&exp.exptype, &var.vartype, &exp);
                        result_assignments.push(Statement::Assign(astroast::AssignStmt { var, exp }));

                    }
                    _ => panic!("Var type {:?} ({:?}) doesn't match the Exp type {:?} ({:?})", var.vartype, var ,exp.exptype, exp)

                }

            }


        } else {
            panic!("Non matching exp")
        }
    }

    result_assignments
}





fn build_stmtwhile (
    stmt: Pair<Rule>,
    known_types: &TreeMap<& str, Type>,
) -> Statement {
        let mut inners = stmt.into_inner();
        let exp = build_exp(inners.next().unwrap(), known_types);
        if same_type(&exp.exptype, &boolean_type()) {
            let block = build_block(inners.next().unwrap(), known_types);
            Statement::While { condition: exp, block: Box::new(block) }
        } else {
            panic!("While statement has a non-boolean expression {:#?}", exp )
        }
}

fn build_stmtif (
    stmt: Pair<Rule>,
    known_types: &TreeMap<& str, Type>,
) -> Statement {
    let mut inners = stmt.into_inner();
    let if_exp = build_exp(inners.next().unwrap(), known_types);

    if same_type(&if_exp.exptype, &boolean_type()) {
        let block = build_block(inners.next().unwrap(), known_types);
        let mut elseifs = Vec::new();
        let mut else_block = None;
        while let Some(elif_or_elseblock_pair) = inners.next() {
            //since the parser checks stuff, we don't need to check if we reached the end or not
            match elif_or_elseblock_pair.as_rule() {
                Rule::Block => else_block = Some(build_block(elif_or_elseblock_pair, known_types)),
                Rule::Exp => {
                    let elif_exp = build_exp(elif_or_elseblock_pair, known_types);
                    if same_type(&elif_exp.exptype, &boolean_type()) {
                        let elif_block = build_block(inners.next().unwrap(), known_types);
                        elseifs.push((elif_exp, elif_block))
                    } else {
                        panic!("Elif expression isn't a boolean")
                    }
                }
                _ => panic!("Impossible if section")
            }

        }
        Statement::If { condition: if_exp, then_block: Box::new(block), elseifs, else_block }

    } else {
        panic!("If statement has a non-boolean expression")
    }

}

fn build_forindex(stmt: Pair<Rule>, known_types: &TreeMap<&str, Type>) -> Statement{
    let mut inners = stmt.into_inner();
    let assign_or_define = inners.next().unwrap();
    let mut known_types = known_types.clone();
    let define_type_variant = match assign_or_define.as_rule() {
        Rule::StmtAssign => {
            let mut assign = build_stmtassign(assign_or_define, &known_types);
            if assign.len() != 1 {
                panic!("More than one assignment in for");
            }
            assign.pop().unwrap()
        }
        Rule::StmtDefine => {
            let (mut define, new_known_types) = build_stmtdefine(assign_or_define, &known_types);
            if define.len() != 1 {
                panic!("More than one definition in for");
            }
            known_types = new_known_types;
            define.pop().unwrap()
        }
        _ => panic!("Impossible Statement in For")
    };


    let condition = build_exp(inners.next().unwrap(), &known_types);
    if !same_type(&condition.exptype, &boolean_type()) {
        panic!("For Index condition is not a boolean") 
    }
    let lastexp_or_block = inners.next().unwrap();
    let (lastexp_opt, forblock) = match lastexp_or_block.as_rule() {
        Rule::Exp => (Some(build_exp(lastexp_or_block, &known_types)), build_block(inners.next().unwrap(), &known_types)),
        Rule::Block =>  (None,build_block(lastexp_or_block, &known_types)),
        _ => panic!("Impossible For element")
    };

    Statement::ForIndex { starting_value: Box::new(define_type_variant), condition, exp: lastexp_opt, block: Box::new(forblock) }
    
}

fn build_foreach(stmt: Pair<Rule>, known_types: &TreeMap<&str, Type>) -> Statement{
    let mut inners = stmt.into_inner();
    let identlist = inners.next().unwrap().into_inner().map(|f| f.as_str().to_string()).collect();
    let explist = inners.next().unwrap().into_inner().map(|f| build_exp(f, known_types)).collect();
    let block = build_block(inners.next().unwrap(), known_types) ;
    Statement::ForEach { variable_names: identlist, expression_list: explist, block:Box::new(block) }
}

fn build_define_type<'a>(stmt: Pair<'a, Rule>, known_types: TreeMap<&'a str, Type>) -> (Statement, TreeMap<&'a str, Type>)  {
    let mut inners = stmt.into_inner();
    let known_types = known_types.clone();
    //TODO: add other variants
    let name = inners.next().unwrap().as_str().to_string(); 
    //This is for the stmt
    let mut type_definitions = Vec::new();
    //This is for the type
    let mut types = HashMap::new();
    while let Some(attribute_name) = inners.next() {
        if let Some(attribute_type) = inners.next() {
            let new_type = parse_type(attribute_type);
            types.insert(attribute_name.as_str().to_string(), new_type.clone());
            type_definitions.push((attribute_name.as_str().to_string(), new_type));

             
        } else {
            panic!("Impossible attribute type")
        }
    }

    //TODO: do something for defined types, not for known types of variables
    known_types.insert(&name, Type::Composite(Composite{name: Some(name.clone()), types}));
    //change variant later
    (Statement::DefineType { variant: astroast::DefineTypeVariant::Struct, name, type_definitions }, known_types)

    // Adicionar novo tipo à known_types
}


fn build_functiondefinition<'a>(stmt: Pair<'a, Rule>, known_types: TreeMap<&'a str, Type>) -> (Statement, TreeMap<&'a str, Type>)  {
    //TODO: add more
    let mut inners = stmt.into_inner();
    let variant = FunctionVariant::Function;
    //HAcky

    let name_pair = inners.next().unwrap();
    let name = build_funcname(name_pair.clone());
    let body = build_function_body(inners.next().unwrap(), &known_types);
    let body_type = body.function_type.clone();
    let known_types = known_types.insert(name_pair.as_span().as_str(), body_type);
    (Statement::FunctionDefinition { variant, name, body }, known_types)
    
}

fn build_funcname(funcname_pair: Pair<Rule>) -> FuncName {
    let mut inners = funcname_pair.into_inner();
    let name = inners.next().unwrap().to_string();
    let mut dot = Vec::new();
    let mut colon = None;
    for dot_or_colon in inners {
        match dot_or_colon.as_rule() {
            Rule::FuncDot => {
                dot.push(dot_or_colon.as_str().to_string())
            }
            Rule::FuncColon => {
                colon = Some(dot_or_colon.as_str().to_string())
            }
            _ => panic!("Impossible funcname")
        }

    }
    FuncName { name, dot, colon }
    
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
fn get_binary_op(binary_op_str: &str) -> (BinopSign, i32 ,Type, Type) {
    match binary_op_str {
        //TODO: fix these precedence values
        ">>"  => (BinopSign::DoubleGreat,   4, undefined_integer(), undefined_integer()),
        "<<"  => (BinopSign::DoubleLess,    4, undefined_integer(), undefined_integer()),
        "<="  => (BinopSign::LessEquals,    2, undefined_integer(), boolean_type()),
        ">="  => (BinopSign::GreaterEquals, 2, undefined_integer(), boolean_type()),
        "=="  => (BinopSign::Equals,        2, undefined_type(), boolean_type()),
        "~="  => (BinopSign::Different,     2, undefined_type(), boolean_type()),
        "//"  => (BinopSign::DoubleSlash,   3, undefined_integer(), undefined_integer()),
        "+"   => (BinopSign::Plus,          2, undefined_number(), undefined_number()),
        "-"   => (BinopSign::Minus,         2, undefined_number(), undefined_number()),
        "*"   => (BinopSign::Mult,          3, undefined_number(), undefined_number()),
        "/"   => (BinopSign::SingleSlash,   3, undefined_number(), undefined_number()),
        "^"   => (BinopSign::Carrot,        4, undefined_number(), undefined_number()),
        "%"   => (BinopSign::Percentile,    3, undefined_number(), undefined_number()),
        "&"   => (BinopSign::Appersand,     2, boolean_type(), boolean_type()),
        "~"   => (BinopSign::Tilda,         2, boolean_type(), boolean_type()),
        "|"   => (BinopSign::Bar,           2, boolean_type(), boolean_type()),
        ".."  => (BinopSign::DoubleDot,     0, undefined_type(), undefined_type()),
        "<"   => (BinopSign::Less,          2, undefined_number(), boolean_type()),
        ">"   => (BinopSign::Greater,       2, undefined_number(), boolean_type()),
        "and" => (BinopSign::And,           1, boolean_type(), boolean_type()),
        "or"  => (BinopSign::Or,            1, boolean_type(), boolean_type()),
        _ => panic!("Impossible binary op str")
    }

}


fn build_exp(exp_pair: Pair<Rule>, known_types: &TreeMap<&str, Type>) -> Exp {

    let (expcontents, exptype) = match exp_pair.as_rule() {
        Rule::LiteralNil => (astroast::ExpContent::Nil, Type::Primitive(PrimitiveType::Unit)),
        Rule::LiteralTrue => ( astroast::ExpContent::True, boolean_type()),
        Rule::LiteralFalse => (astroast::ExpContent::False, boolean_type()),
        Rule::LiteralNumber => (astroast::ExpContent::Number(exp_pair.as_str().to_string()), integer_type(IntegerSign::Undefined, BitSize::Undefined)),
        Rule::LiteralString => (astroast::ExpContent::EString(exp_pair.as_str().to_string()), array_type(char_type())),
        Rule::AnonFuncDef => {
            let built_function = build_function_body(exp_pair, &known_types.clone());
            let function_type = built_function.function_type.clone();
            (astroast::ExpContent::AnonFuncDef(built_function), function_type)
        }
        Rule::Value => {
            let (value, value_type) = build_value(exp_pair, known_types);
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

        Rule::BinaryOpExp => {
            let exp = build_binaryexp(exp_pair, known_types);
            let exp_clone = exp.exptype.clone();
            (*exp.contents, exp_clone)

        }

        Rule::TupleExp => {
            panic!("Not implemented yet")
            //TODO: transform tuple to composite type, since every element of the tuple can be of different type and this will help indexing it in the same way composite types are indexed
        }

        _ => panic!("Impossible Expression {:#?}", exp_pair),
    };

    Exp {
        contents: Box::new(expcontents),
        exptype
    }
}

fn build_binaryexp(exp_pair: Pair<Rule>, known_types: &TreeMap<&str, Type>) -> Exp {
    let mut exps = Vec::new();
    let mut ops_parsed = Vec::new();
    for exp_or_op in exp_pair.into_inner() {
        match exp_or_op.as_rule() {
            Rule::BinaryOp => ops_parsed.push(get_binary_op(exp_or_op.as_str())),
            _ => exps.push(build_exp(exp_or_op, known_types))

        }
    }


/*
convert the token sequence into an AST:
Traverse the token sequence from left to right, building up the AST.
When encountering an operator token, compare its precedence with the precedence of the operator at the top of the operator stack.

If the new operator has higher precedence, push it onto the operator stack. 
If the new operator has lower or equal precedence, pop operators from the operator stack until the top operator has lower precedence or the stack is empty, and add each popped operator to the AST.
Then push the new operator onto the stack.

When the token sequence is finished, pop all remaining operators from the operator stack and add them to the AST in the order they were popped.

Return the root node of the AST.
*/
    let mut ops_iterator = ops_parsed.iter();

    let mut output_stack = Vec::new();
    let mut op_stack = Vec::new();

    fn pop_to_ast(output_stack: &mut Vec<Exp>, sign: &BinopSign, left_right_exps: &Type, result_type: &Type) {

        let (a_exp, b_exp) = handle_two_different_numbers(output_stack.pop().unwrap(), output_stack.pop().unwrap());
        let a_type = a_exp.exptype.clone();
        let b_type = b_exp.exptype.clone();

        if same_type(&a_type, left_right_exps) {
           let new_binop_exp = Exp {
               exptype: result_type.clone(),
               contents: Box::new(
                   ExpContent::Binop { left: a_exp, sign: sign.clone(), right: b_exp }
                )
           };
           output_stack.push(new_binop_exp)

        } else {
            match (&a_type, left_right_exps) {
                (Type::Primitive(PrimitiveType::Integer(_, _)), Type::UndefinedNumber) 
                    | (Type::Primitive(PrimitiveType::Float(BitSize::Undefined)), Type::UndefinedNumber)=> {
                    let result_type = if same_type(result_type, &undefined_number()) {
                        a_type.clone()
                    } else if same_type(result_type, &boolean_type()){
                        boolean_type() 
                    } else {
                        result_type.clone()
                    };

                   let new_binop_exp = Exp {
                       exptype: result_type,
                       contents: Box::new(
                           ExpContent::Binop { left: a_exp, sign: sign.clone(), right: b_exp }
                        )
                   };
                   output_stack.push(new_binop_exp)
                }
                _ => panic!("The expected types in the binop aren't correct: left {:?}, right {:?}, expected {:?}", a_type, b_type, left_right_exps)
                 
            }

            
        }

    }


    for exp in exps {
        output_stack.push(exp);
        let op = match ops_iterator.next()  {
            Some(a) => a,
            None => break
        };

        loop {
            match op_stack.last() {
                None => {
                    op_stack.push(op);
                    break
                },
                Some(top)  => {
                    let (_, op_prec, _, _ ) = op;
                    let (_, top_prec, _, _) = top;
                    if op_prec > top_prec {
                        op_stack.push(op);
                        break
                    } else {
                        let (sign, _, left_right_exps, result_type) = op_stack.pop().unwrap();
                        pop_to_ast(&mut output_stack, sign ,left_right_exps, result_type)
                    }

                }

            }
        }

    }

    for (sign, _, left_right_exps, result_type) in op_stack {
        pop_to_ast(&mut output_stack, sign, left_right_exps, result_type)
    }
    output_stack.pop().unwrap()

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
    let value_pair = value_pair.into_inner().next().unwrap();
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
        _ => panic!("Impossible Value {:#?} ", value_pair)
    }
}


fn build_function_call(function_call_pair: Pair<Rule>,  known_types: &TreeMap<&str, Type>) -> FunctionCall {
    println!("Function call {:#?}", function_call_pair);
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
            panic!("You are calling a non function {:#?}", call)
        }
    }

    FunctionCall { var: call_var, call: calls, calltype: previous_type }
     
}

fn build_var(var_pair: Pair<Rule>, known_types: &TreeMap<&str, Type>) -> Var {
    println!("var pair {:#?}",  var_pair);
    let mut inners = var_pair.into_inner();
    let atomic_exp = inners.next().unwrap();
    println!("var atomic exp {:#?}", atomic_exp);
    let (atomic_exp, atomic_type) = build_atomic_exp(atomic_exp, known_types);
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


fn build_atomic_exp(atomic_exp_pair: Pair<Rule>, known_types: &TreeMap<&str, Type>) -> (AtomicExp, Type)  {
    let atomic_exp = atomic_exp_pair.clone().into_inner().next().unwrap();
    match atomic_exp.as_rule() {
        Rule::Ident => {
            let known_type = match known_types.get(atomic_exp.as_str()) {
                Some(t) => t,
                None => &Type::Undefined,
            };
            (AtomicExp::Ident(atomic_exp.as_str().to_string()), known_type.clone())
        }
        Rule::Exp => {
            let result_exp = build_exp(atomic_exp, known_types);
            let result_type = result_exp.exptype.clone();
            (AtomicExp::Exp(result_exp), result_type)
        }
        _ => panic!("Impossible atomic exp {:#?} {:#?}", atomic_exp_pair, atomic_exp)
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
                                &inner_exp);
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
                                &inner_exp);
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

fn handle_two_different_numbers(a: Exp, b: Exp) -> (Exp, Exp) {
    if same_type(&a.exptype, &b.exptype) {
        (a, b)
    } else {
        match (&a.exptype, &b.exptype) {
            (Type::Primitive(PrimitiveType::Integer(IntegerSign::Undefined, BitSize::Undefined)), Type::Primitive(PrimitiveType::Integer(_, _))) => {
                (update_integer_recursively(&a.exptype, &b.exptype, &a), b)
            }
            (Type::Primitive(PrimitiveType::Integer(_, _)), Type::Primitive(PrimitiveType::Integer(IntegerSign::Undefined, BitSize::Undefined))) => {
                (update_integer_recursively(&b.exptype, &a.exptype, &a), b)
            }
            //TODO: there are missing ones for flaots
        _ => panic!("Not possible to join both types")
        }
    }

}


fn update_integer_recursively(previous_integer: &Type, new_integer: &Type, exp: &Exp) -> Exp {
    let exp_type = &exp.exptype;
    let exp_contents = &*exp.contents;
    let new_contents = match exp_contents {
        ExpContent::Number(ref n) => {
            if same_type(previous_integer, exp_type) {
                ExpContent::Number(n.to_string())
            } else {
                exp_contents.to_owned()
            }
        }
        ExpContent::Unop { sign, exp } => {
            let new_exp = update_integer_recursively(previous_integer, new_integer, exp);
            ExpContent::Unop {
                sign: sign.clone(),
                exp: new_exp,
            }
        }
        ExpContent::Binop {left, sign, right} => {
            let left = update_integer_recursively(previous_integer, new_integer, left);
            let right = update_integer_recursively(previous_integer, new_integer, right);
            ExpContent::Binop { left, sign: sign.clone(), right }

        }
        ExpContent::AnonFuncDef(_) =>       panic!("Not implemented yet {:?}", exp_contents),
        ExpContent::TableConstructor(_) =>  panic!("Not implemented yet {:?}", exp_contents),
        ExpContent::Tuple(_) =>             panic!("Not implemented yet {:?}", exp_contents),
        //ExpContent::Binop { left, sign, right } => {
        //    let new_left = update_integer_recursively(previous_integer, new_integer, left);
        //    let new_right = update_integer_recursively(previous_integer, new_integer, right);
        //    ExpContent::Binop {
        //        left: new_left,
        //        sign,
        //        right: new_right,
        //    }
        //}
        _ => exp_contents.clone(),
    };
    Exp {
        exptype: if same_type(previous_integer, exp_type) {
            new_integer.clone()
        } else {
            exp_type.clone()
        },
        contents: Box::new(new_contents),
    }
}
