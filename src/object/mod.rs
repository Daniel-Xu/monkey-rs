use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Str(String),
    Null,
}

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);
pub const NULL: Object = Object::Null;

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Str(s) => write!(f, "{}", s),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(object) => write!(f, "return {}", object),
        }
    }
}

impl Object {
    pub fn get_bool(input: bool) -> Object {
        if input {
            return TRUE;
        }
        FALSE
    }

    pub fn debug_type(&self) -> String {
        match self {
            Object::Integer(_) => "Integer",
            Object::Boolean(_) => "Boolean",
            Object::Str(_) => "String",
            Object::Null => "Null",
            Object::ReturnValue(_) => "Return",
        }
        .to_string()
    }
}
