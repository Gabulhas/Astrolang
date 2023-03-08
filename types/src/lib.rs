pub mod type_parsing;
use std::collections::HashMap;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum IntegerSign {
    Signed,
    Unsigned,
    //This is used if the integer was not yet infered
    Undefined,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum BitSize {
    B8,
    B16,
    B32,
    B64,
    B128,
    Size,
    //This is used if the integer was not yet infered
    Undefined,
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
pub struct Composite {
    pub name: Option<String>,
    pub types: HashMap<String, Type>,
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
    Composite(Composite),
    Undefined,
    UndefinedNumber
}

pub fn integer_type(sign: IntegerSign, bitsize: BitSize) -> Type{
    Type::Primitive(PrimitiveType::Integer(sign, bitsize))
}

pub fn undefined_integer() -> Type {
    integer_type(IntegerSign::Undefined, BitSize::Undefined)
}

pub fn undefined_number() -> Type {
    Type::UndefinedNumber

}

pub fn float_type(bitsize: BitSize) -> Type {
    Type::Primitive(PrimitiveType::Float(bitsize))
}
pub fn char_type() -> Type {
    Type::Primitive(PrimitiveType::Char)
}
pub fn boolean_type() -> Type {
    Type::Primitive(PrimitiveType::Bool)
}
pub fn unit_type() -> Type {
    Type::Primitive(PrimitiveType::Unit)
}

pub fn array_type(inner_type: Type) -> Type {
    Type::Array(Box::new(inner_type))
}

pub fn map_type(key_type: Type, value_type: Type) -> Type {
    Type::Map(Box::new(key_type), Box::new(value_type))
}

pub fn string_type() -> Type {
    array_type(char_type())
}

pub fn undefined_type() -> Type {
    Type::Undefined
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

fn compare_multiple<T, F>(a_list: &[T], b_list: &[T], compare_fn: F) -> bool
where
    F: Fn(&T, &T) -> bool,
{
    (a_list.len() == b_list.len())
        && a_list
            .iter()
            .zip(b_list)
            .filter(|&(a, b)| compare_fn(a, b))
            .count()
            == a_list.len()
}

fn compare_multiple_type(a_type_list: &Vec<Type>, b_type_list: &Vec<Type>) -> bool {
    (a_type_list.len() == b_type_list.len())
        && compare_multiple(a_type_list, b_type_list, same_type)
}

fn compare_composite_type(a_composite: &Composite, b_composite: &Composite) -> bool {
    if a_composite.name == b_composite.name {
        let a_vec: Vec<(&String, &Type)> = Vec::from_iter(a_composite.types.iter());
        let b_vec: Vec<(&String, &Type)> = Vec::from_iter(b_composite.types.iter());
        compare_multiple(&a_vec, &b_vec, |&(a_name, a_type), &(b_name, b_type)| a_name == b_name && same_type(&a_type, &b_type) )
    } else {
        false
    }

}

pub fn same_type(a: &Type, b: &Type) -> bool {
    match (a, b) {
        (Type::Primitive(i), Type::Primitive(j)) => same_primitive(i, j),
        (Type::Tuple(a), Type::Tuple(b)) => compare_multiple_type(a, b),
        (Type::Function(a), Type::Function(b)) => compare_multiple_type(a, b),
        (Type::Array(a), Type::Array(b)) => same_type(a, b),
        (Type::Map(a, b), Type::Map(c, d)) => same_type(a, c) && same_type(b, d),
        (Type::Composite(a), Type::Composite(b)) => compare_composite_type(a, b),
        _ => false,
    }
}
