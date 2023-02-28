use types::*;

pub enum FunctionBody {
    ParameterList(Vec<String>),
    Type(Type),
    Block(Block)
}

pub struct Block {
    statements: Vec<Statement>,
}


pub enum TableConstructor  {

}

pub enum Var {

}

pub struct  FuncName {

}

pub struct DefineStmt {
    var: String, exp: Option<Exp>
}

pub struct AssignStmt {
    var: Var, exp: Exp
}

pub enum DefineTypeVariant {
    Struct,
    Context,
    State, 
    Record
} 

pub enum FunctionVariant {
    Function, 
    Instruction
}


pub enum Statement {
    Define(DefineStmt),
    Assign(AssignStmt),
    FunctionCall{funcname: Var, call: Vec<Call>},
    Break,
    While{condition: Exp, block: Box<Block>},
    If{condition: Exp, then_block: Box<Block>, elseifs: Vec<(Exp, Block)>, else_block: Box<Block>},
    ForIndex{define: Option<DefineStmt>, stmt: Option<AssignStmt>, condition: Exp, exp: Option<Exp>, block: Box<Block>},
    DefineType{variant: DefineTypeVariant, name: String, type_definitions: Vec<(String, Type)>},
    FunctionDefinition{variant: FunctionVariant, name: FuncName, body: FunctionBody},
    Return{explist: Vec<Exp>}
}

pub enum Call {

}

pub enum PrefixExp {
    
}


pub enum Value {
    FunctionCall(),
    Var(Var)
}

pub struct Exp {
    contents: Box<ExpContent>,
    exptype: Type
}


pub enum ExpContent {
    Nil, 
    False, 
    True, 
    Number(String), 
    EString(String), 
    VarArg,
    AnonFuncDef(FunctionBody),
    Value(Value),
    TableConstructor(TableConstructor),
    Binop{left: Exp, sign:BinopSign, right:Exp},
    Unop{sign:UnopSign, exp:Exp},
    Tuple(Vec<Exp>)
}


pub enum BinopSign {
    DoubleGreat,
    DoubleLess,
    LessEquals,
    GreaterEquals,
    Equals,
    Different,
    DoubleSlash,
    Plus,
    Minus,
    Mult,
    SingleSlash,
    Carrot,
    Percentile,
    Appersand,
    Tilda,
    Bar,
    DoubleDot,
    Less,
    Greater,
    And,
    Or
}

pub enum UnopSign {
    Hash, //#
    Neg, //-
    Not, //not
    Tilda,
    TildaEquals
}

