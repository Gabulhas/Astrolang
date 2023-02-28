use types::*;

pub struct  Program {
    contents: Block
}

pub enum FunctionBody {
    ParameterList(Vec<String>),
    Type(Type),
    Block(Block)
}

pub struct Block {
    statements: Vec<Statement>,
}


pub enum Field {
    Square{key: Exp, exp: Exp},
    Dot{key: String, exp: Exp},
    Literal(Exp)
}

pub struct  TableConstructor  {
    fields: Vec<Field>
}

pub enum AtomicExp {
    Ident(String),
    Exp(Exp)
}

pub enum Index {
    Square(Exp),
    Dot(String)

}
pub enum VarAux {
    Complex(Vec<Call>, Index), 
    Simple(Index)
}
pub struct Var {
    atomic_exp: AtomicExp,
    rest: Vec<VarAux>
}

pub struct  FuncName {
    name: String,
    dot: Vec<String>,
    colon: Option<String>
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

/*This might be removed, this allows 
    potato(a)
    potato {a=5}
    potato a
*/

pub enum Call {
    ArgList(Vec<Exp>),
    TableConstructor(TableConstructor),
    LiteralString(String)
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

