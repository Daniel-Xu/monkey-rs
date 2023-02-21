use crate::evaluator::EvalError;
use crate::object::Object;

pub fn get(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Object::BuiltIn(name.to_string(), len)),
        _ => None,
    }
}

type BuiltinResult = std::result::Result<Object, EvalError>;
fn len(args: Vec<Object>) -> BuiltinResult {
    match &args[0] {
        Object::Str(content) => Ok(Object::Integer(content.len() as i32)),
        _ => Err(EvalError::TypeMismatch(
            format!("{}", args[0].debug_type()),
            "len".to_string(),
        )),
    }
}
