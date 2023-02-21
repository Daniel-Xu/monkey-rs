use crate::ast::{pretty_print, BlockStmt, Expr};
use crate::evaluator::EvalError;
use crate::object::environment::{Environment, SharedEnv};
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Str(String),
    Function(Vec<Expr>, BlockStmt, SharedEnv), // this is definition, not invocation
    BuiltIn(String, fn(Vec<Object>) -> Result<Object, EvalError>),
    Null,
}

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);
pub const NULL: Object = Object::Null;

pub mod builtin;
pub mod environment;

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Str(s) => write!(f, "{}", s),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(object) => write!(f, "return {}", object),
            Object::BuiltIn(name, body) => write!(f, "builtin"), // TODO
            Object::Function(params, body, env) => {
                write!(f, "fn({}) {}", pretty_print(params), body)
            }
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
            Object::Function(_, _, _) => "Function",
            Object::BuiltIn(_, _) => "Builtin",
        }
        .to_string()
    }
}
