use crate::{BitSize, IntegerSign, PrimitiveType, Type};
use astroparser::Rule;
use pest::iterators::{Pair, Pairs};

//TODO: idk what to do with user defined, but will figure out
pub fn parse_type(type_pair: Pair<Rule>) -> Type {
    match type_pair.as_rule() {
        Rule::SimpleType => parse_simpletype(type_pair.as_str()),
        Rule::TupleType => Type::Tuple(parse_type_pairs(type_pair.into_inner())),
        Rule::ArrayType => Type::Array(Box::new(parse_simpletype(
            type_pair.into_inner().next().unwrap().as_str(),
        ))),
        Rule::MapType => {
            let mut inners = type_pair.into_inner();
            let first_type = parse_type(inners.next().unwrap());
            let second_type = parse_type(inners.next().unwrap());
            Type::Map(Box::new(first_type), Box::new(second_type))
        }
        Rule::FunctionType => Type::Function(parse_type_pairs(type_pair.into_inner())),
        _ => panic!("Invalid type"),
    }
}

//TODO: Add more types here
pub fn parse_simpletype(type_string: &str) -> Type {
    match type_string {
        "i8" => Type::Primitive(PrimitiveType::Integer(IntegerSign::Signed, BitSize::B8)),
        "i16" => Type::Primitive(PrimitiveType::Integer(IntegerSign::Signed, BitSize::B16)),
        "i32" => Type::Primitive(PrimitiveType::Integer(IntegerSign::Signed, BitSize::B32)),
        "i64" => Type::Primitive(PrimitiveType::Integer(IntegerSign::Signed, BitSize::B64)),
        "i128" => Type::Primitive(PrimitiveType::Integer(IntegerSign::Signed, BitSize::B128)),
        "isize" => Type::Primitive(PrimitiveType::Integer(IntegerSign::Signed, BitSize::Size)),
        "u8" => Type::Primitive(PrimitiveType::Integer(IntegerSign::Unsigned, BitSize::B8)),
        "u16" => Type::Primitive(PrimitiveType::Integer(IntegerSign::Unsigned, BitSize::B16)),
        "u32" => Type::Primitive(PrimitiveType::Integer(IntegerSign::Unsigned, BitSize::B32)),
        "u64" => Type::Primitive(PrimitiveType::Integer(IntegerSign::Unsigned, BitSize::B64)),
        "u128" => Type::Primitive(PrimitiveType::Integer(IntegerSign::Unsigned, BitSize::B128)),
        "usize" => Type::Primitive(PrimitiveType::Integer(IntegerSign::Unsigned, BitSize::Size)),
        "f32" => Type::Primitive(PrimitiveType::Float(BitSize::B32)),
        "f64" => Type::Primitive(PrimitiveType::Float(BitSize::B64)),
        "char" => Type::Primitive(PrimitiveType::Char),
        "bool" => Type::Primitive(PrimitiveType::Bool),
        "unit" => Type::Primitive(PrimitiveType::Unit),
        "string" => Type::Array(Box::new(Type::Primitive(PrimitiveType::Char))),
        _ => panic!("Invalid Type"),
    }
}

pub fn parse_type_pairs(type_pairs: Pairs<Rule>) -> Vec<Type> {
    type_pairs.map(parse_type).collect()
}

fn infer_from_text(text: &str) -> Type {
    
}


