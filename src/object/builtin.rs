use crate::evaluator::EvalError;
use crate::object::Object;

pub fn get(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Object::BuiltIn(name.to_string(), len)),
        _ => None,
    }
}

type BuiltinResult = std::result::Result<Object, EvalError>;

fn assert_args(args: &[Object], n: usize, name: &str) -> Result<(), EvalError> {
    if args.len() != n {
        Err(EvalError::WrongNumberOfArguments(
            format!("parameters {}, arguments {}", n, args.len()),
            format!("builtin {}", name),
        ))
    } else {
        Ok(())
    }
}

fn len(args: Vec<Object>) -> BuiltinResult {
    assert_args(&args, 1, "len")?;
    match &args[0] {
        Object::Str(content) => Ok(Object::Integer(content.len() as i32)),
        Object::Array(elements) => Ok(Object::Integer(elements.len() as i32)),
        _ => Err(EvalError::TypeMismatch(
            format!("{}", args[0].debug_type()),
            "len".to_string(),
        )),
    }
}
