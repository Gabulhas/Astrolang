pub enum IntegerSign {
    Signed,
    Unsigned

}

pub enum BitSize {
    B8,
    B16,
    B32,
    B64,
    B128,
    Size
}

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
