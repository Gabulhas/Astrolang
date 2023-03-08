use types::*;

#[derive(Debug)]
pub struct Program {
    pub contents: Block,
}

#[derive(Debug)]
pub struct FunctionBody {
    pub parameter_list: Vec<String>,
    pub function_type: Type,
    pub block: Block,
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Field {
    Square { key: Exp, exp: Exp },
    Dot { key: String, exp: Exp },
    Literal(Exp),
}

#[derive(Debug)]
pub struct FunctionCall {
    pub var: Var,
    pub call: Vec<Call>,
    pub calltype: Type
}

#[derive(Debug)]
pub struct TableConstructor {
    pub fields: Vec<Field>,
}

#[derive(Debug)]
pub enum AtomicExp {
    Ident(String),
    Exp(Exp),
}

//This is for indexing a value, like a.potato, or a["key"], or a[4]
#[derive(Debug)]
pub enum Index {
    Square(Exp),
    Dot(String),
}

#[derive(Debug)]
pub enum VarAux {
    Call(Call),
    Index(Index),
}

#[derive(Debug)]
pub struct Var {
    pub atomic_exp: AtomicExp,
    pub rest: Vec<VarAux>,
    //TODO: this might be useless
    pub vartype: Type,
}

#[derive(Debug)]
pub struct FuncName {
    pub name: String,
    pub dot: Vec<String>,
    pub colon: Option<String>,
}

#[derive(Debug)]
pub struct DefineStmt {
    pub var: String,
    pub exp: Option<Exp>,
}

#[derive(Debug)]
pub struct AssignStmt {
    pub var: Var,
    pub exp: Exp,
}

#[derive(Debug)]
pub enum DefineTypeVariant {
    Struct,
    Context,
    State,
    Record,
}

#[derive(Debug)]
pub enum FunctionVariant {
    Function,
    Instruction,
}



#[derive(Debug)]
pub enum Statement {
    Define(DefineStmt),
    Assign(AssignStmt),
    FunctionCall(FunctionCall),
    Break,
    While {
        condition: Exp,
        block: Box<Block>,
    },
    If {
        condition: Exp,
        then_block: Box<Block>,
        elseifs: Vec<(Exp, Block)>,
        else_block: Option<Block>,
    },
    ForIndex {
        starting_value: Box<DefineStmt>,
        condition: Exp,
        exp: Option<Exp>,
        block: Box<Block>,
    },
    ForEach {
        variable_names: Vec<String>,
        expression_list: Vec<Exp>,
        block: Box<Block>
    },
    DefineType {
        variant: DefineTypeVariant,
        name: String,
        type_definitions: Vec<(String, Type)>,
    },
    FunctionDefinition {
        variant: FunctionVariant,
        name: FuncName,
        body: FunctionBody,
    },
    Return {
        explist: Vec<Exp>,
    },
}

/*This might be removed, this allows
    potato(a)
    potato {a=5}
    potato a
*/

#[derive(Debug)]
pub enum Call {
    ArgList(Vec<Exp>),
    TableConstructor(TableConstructor),
    LiteralString(String),
}

#[derive(Debug)]
pub enum Value {
    FunctionCall(FunctionCall),
    Var(Var),
}

#[derive(Debug)]
pub struct Exp {
    pub contents: Box<ExpContent>,
    pub exptype: Type,
}

#[derive(Debug)]
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
    Binop {
        first: Exp,
        chain: Vec<(BinopSign, Exp)>
    },
    Unop {
        sign: UnopSign,
        exp: Exp,
    },
    Tuple(Vec<Exp>),
}

#[derive(Debug)]
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
    Or,
}

#[derive(Debug)]
pub enum UnopSign {
    Hash, //#
    Neg,  //-
    Not,  //not
    Tilda,
    TildaEquals,
}
