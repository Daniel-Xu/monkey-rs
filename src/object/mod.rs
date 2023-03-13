use crate::ast::{pretty_print, BlockStmt, Expr};
use crate::evaluator::EvalError;
use crate::object::environment::SharedEnv;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Str(String),
    Function(Vec<Expr>, BlockStmt, SharedEnv), // this is definition, not invocation
    BuiltIn(String, fn(Vec<Object>) -> Result<Object, EvalError>),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
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
            Object::BuiltIn(name, _body) => write!(f, "builtin {}", name),
            Object::Array(objs) => write!(f, "[{}]", pretty_print(objs)),
            Object::Function(params, body, _env) => {
                write!(f, "fn({}) {}", pretty_print(params), body)
            }
            Object::Hash(_) => {
                write!(f, "{}", "todo")
            }
        }
    }
}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Object::Integer(i) => i.hash(state),
            Object::Boolean(b) => b.hash(state),
            Object::Str(s) => s.hash(state),
            _ => 0.hash(state),
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
            Object::Array(_) => "Array",
            Object::Hash(_) => "Hash",
        }
        .to_string()
    }
}
