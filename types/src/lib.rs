pub mod type_parsing;
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum IntegerSign {
    Signed,
    Unsigned,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum BitSize {
    B8,
    B16,
    B32,
    B64,
    B128,
    Size,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimitiveType {
    Integer(IntegerSign, BitSize),
    Float(BitSize),
    Char,
    Bool,
    Unit,
}

#[derive(Debug, Clone)]
pub enum Type {
    Primitive(PrimitiveType),
    Tuple(Vec<Type>),
    // A -> .... -> Return
    Function(Vec<Type>),
    Array(Box<Type>),
    Map(Box<Type>, Box<Type>),
    //It's a list of every type defined, even if the type is composite aswell
    UserDefined(Vec<Type>),
}


fn same_primitive(a: &PrimitiveType, b: &PrimitiveType) -> bool {
    match (a, b) {
        (PrimitiveType::Integer(a_sign, a_size), PrimitiveType::Integer(b_sign, b_size)) => {
            a_sign == b_sign && a_size == b_size
        }
        (PrimitiveType::Float(a), PrimitiveType::Float(b)) => a == b,
        (PrimitiveType::Char, PrimitiveType::Char) => true,
        (PrimitiveType::Bool, PrimitiveType::Bool) => true,
        (PrimitiveType::Unit, PrimitiveType::Unit) => true,
        _ => false,
    }
}

fn compare_multiple_type(a_type_list: &Vec<Type>, b_type_list: &Vec<Type>) -> bool {
    (a_type_list.len() == b_type_list.len())
        && a_type_list
            .iter()
            .zip(b_type_list)
            .filter(|&(a, b)| same_type(a, b))
            .count()
            == a_type_list.len()
}

pub fn same_type(a: &Type, b: &Type) -> bool {
    match (a, b) {
        (Type::Primitive(i), Type::Primitive(j)) => same_primitive(i, j),
        (Type::Tuple(a), Type::Tuple(b)) => compare_multiple_type(a, b),
        (Type::Function(a), Type::Function(b)) => compare_multiple_type(a, b),
        (Type::Array(a), Type::Array(b)) => same_type(a, b),
        (Type::Map(a, b), Type::Map(c, d)) => same_type(a, c) && same_type(b, d),
        (Type::UserDefined(a), Type::UserDefined(b)) => compare_multiple_type(a, b),
        _ => false,
    }
}
