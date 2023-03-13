use std::fmt;
use types::*;

#[derive(Debug, Clone)]
pub struct Program {
    pub contents: Block,
}

#[derive(Debug, Clone)]
pub struct FunctionBody {
    pub parameter_list: Vec<String>,
    pub function_type: Type,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Field {
    Square { key: Exp, exp: Exp },
    Dot { key: String, exp: Exp },
    Literal(Exp),
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub var: Var,
    pub call: Vec<Call>,
    pub calltype: Type
}

#[derive(Debug, Clone)]
pub struct TableConstructor {
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone)]
pub enum AtomicExp {
    Ident(String),
    Exp(Exp),
}

//This is for indexing a value, like a.potato, or a["key"], or a[4]
#[derive(Debug, Clone)]
pub enum Index {
    Square(Exp),
    Dot(String),
}

#[derive(Debug, Clone)]
pub enum VarAux {
    Call(Call),
    Index(Index),
}

#[derive(Debug, Clone)]
pub struct Var {
    pub atomic_exp: AtomicExp,
    pub rest: Vec<VarAux>,
    //TODO: this might be useless
    pub vartype: Type,
}

#[derive(Debug, Clone)]
pub struct FuncName {
    pub name: String,
    pub dot: Vec<String>,
    pub colon: Option<String>,
}

impl fmt::Display for FuncName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut result = self.name.clone();

        for dot_str in &self.dot {
            result.push_str(&format!(".{}", dot_str));
        }

        if let Some(colon_str) = &self.colon {
            result.push_str(&format!(":{}", colon_str));
        }

        write!(f, "{}", result)
    }
}

#[derive(Debug, Clone)]
pub struct DefineStmt {
    pub var: String,
    pub exp: Option<Exp>,
}

#[derive(Debug, Clone)]
pub struct AssignStmt {
    pub var: Var,
    pub exp: Exp,
}

#[derive(Debug, Clone)]
pub enum DefineTypeVariant {
    Struct,
    Context,
    State,
    Record,
}

#[derive(Debug, Clone)]
pub enum FunctionVariant {
    Function,
    Instruction,
}



#[derive(Debug, Clone)]
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
        starting_value: Box<Statement>,
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

#[derive(Debug, Clone)]
pub enum Call {
    ArgList(Vec<Exp>),
    TableConstructor(TableConstructor),
    LiteralString(String),
}

#[derive(Debug, Clone)]
pub enum Value {
    FunctionCall(FunctionCall),
    Var(Var),
}

#[derive(Debug, Clone)]
pub struct Exp {
    pub contents: Box<ExpContent>,
    pub exptype: Type,
}

#[derive(Debug, Clone)]
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
        left: Exp,
        sign: BinopSign,
        right: Exp

    },
    Unop {
        sign: UnopSign,
        exp: Exp,
    },
    Tuple(Vec<Exp>),
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum UnopSign {
    Hash, //#
    Neg,  //-
    Not,  //not
    Tilda,
    TildaEquals,
}
