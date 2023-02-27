#[derive(PartialEq, Eq)]
pub enum IntegerSign {
    Signed,
    Unsigned

}

#[derive(PartialEq, Eq)]
pub enum BitSize {
    B8,
    B16,
    B32,
    B64,
    B128,
    Size
}

#[derive(PartialEq, Eq)]
pub enum PrimitiveType {
    Integer(IntegerSign, BitSize),
    Float(BitSize),
    Char,
    Bool,
    Unit
}

pub enum Type{
    Primitive(PrimitiveType),
    Tuple(Vec<Type>),
    // A -> .... -> Return
    Function(Vec<Type>),
    Array(Box<Type>),
    Map(Box<Type>, Box<Type>),
    //It's a list of every type defined, even if the type is composite aswell
    UserDefined(Vec<Type>)
}


fn valid_float(float_size: BitSize) -> bool{
    match float_size {
       BitSize::B32 | BitSize::B64 => true,
       _ => false
    }
}


fn same_integer(a_sign: IntegerSign, a_size: BitSize, b_sign: IntegerSign, b_size: BitSize) -> bool{
    a_sign == b_sign && a_size == b_size
}

fn same_primitive(a: PrimitiveType, b: PrimitiveType) -> bool{
    match (a, b) {
        (PrimitiveType::Integer(a_sign, a_size), PrimitiveType::Integer(b_sign, b_size)) => same_integer(a_sign, a_size, b_sign, b_size),
        (PrimitiveType::Float(a), PrimitiveType::Float(b)) => a == b,
        (PrimitiveType::Char, PrimitiveType::Char) => true,
        (PrimitiveType::Bool, PrimitiveType::Bool) => true,
        (PrimitiveType::Unit, PrimitiveType::Unit) => true,
        _ => false
    }
}

fn compare_multiple_type(a_type_list: &Vec<Type>, b_type_list: &Vec<Type>) -> bool{
    (a_type_list.len() == b_type_list.len())  && a_type_list.into_iter().zip(b_type_list).filter(|&(a, b)| a == b).count() == a_type_list.len()

}

pub fn same_type(a: Type, b: Type) -> bool {
    match (a, b) {
        (Type::Primitive(i), Type::Primitive(j)) => same_primitive(i, j),
        (Type::Tuple(a), Type::Tuple(b))  => compare_multiple_type(&a, &b),
        (Type::Function(a), Type::Function(b))  => compare_multiple_type(&a, &b),
        (Type::UserDefined(a), Type::UserDefined(b))  => compare_multiple_type(&a, &b),
        
    }
}
