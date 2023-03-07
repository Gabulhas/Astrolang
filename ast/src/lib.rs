use types::*;

pub struct Program {
    pub contents: Block,
}

pub struct FunctionBody {
    pub parameter_list: Vec<String>,
    pub function_type: Type,
    pub block: Block,
}

pub struct Block {
    pub statements: Vec<Statement>,
}

pub enum Field {
    Square { key: Exp, exp: Exp },
    Dot { key: String, exp: Exp },
    Literal(Exp),
}

pub struct FunctionCall {
    pub var: Var,
    pub call: Vec<Call>,
    pub calltype: Type
}

pub struct TableConstructor {
    pub fields: Vec<Field>,
}

pub enum AtomicExp {
    Ident(String),
    Exp(Exp),
}

//This is for indexing a value, like a.potato, or a["key"], or a[4]
pub enum Index {
    Square(Exp),
    Dot(String),
}

pub enum VarAux {
    Call(Call),
    Index(Index),
}

pub struct Var {
    pub atomic_exp: AtomicExp,
    pub rest: Vec<VarAux>,
    //TODO: this might be useless
    pub vartype: Type,
}

pub struct FuncName {
    pub name: String,
    pub dot: Vec<String>,
    pub colon: Option<String>,
}

pub struct DefineStmt {
    pub var: String,
    pub exp: Option<Exp>,
}

pub struct AssignStmt {
    pub var: Var,
    pub exp: Exp,
}

pub enum DefineTypeVariant {
    Struct,
    Context,
    State,
    Record,
}

pub enum FunctionVariant {
    Function,
    Instruction,
}

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
        else_block: Box<Block>,
    },
    ForIndex {
        define: Option<DefineStmt>,
        stmt: Option<AssignStmt>,
        condition: Exp,
        exp: Option<Exp>,
        block: Box<Block>,
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

pub enum Call {
    ArgList(Vec<Exp>),
    TableConstructor(TableConstructor),
    LiteralString(String),
}

pub enum Value {
    FunctionCall(FunctionCall),
    Var(Var),
}

pub struct Exp {
    pub contents: Box<ExpContent>,
    pub exptype: Type,
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

pub enum UnopSign {
    Hash, //#
    Neg,  //-
    Not,  //not
    Tilda,
    TildaEquals,
}
