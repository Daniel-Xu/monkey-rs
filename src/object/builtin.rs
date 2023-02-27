use crate::evaluator::EvalError;
use crate::object::Object;
use crate::object::Object::Null;

pub fn get(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Object::BuiltIn(name.to_string(), len)),
        "first" => Some(Object::BuiltIn(name.to_string(), first)),
        "last" => Some(Object::BuiltIn(name.to_string(), last)),
        "rest" => Some(Object::BuiltIn(name.to_string(), rest)),
        "push" => Some(Object::BuiltIn(name.to_string(), push)),
        "puts" => Some(Object::BuiltIn(name.to_string(), puts)),
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

fn first(args: Vec<Object>) -> BuiltinResult {
    assert_args(&args, 1, "first")?;

    match &args[0] {
        Object::Array(elements) => {
            if elements.len() > 0 {
                Ok(elements[0].clone())
            } else {
                Ok(Null)
            }
        }
        _ => Err(EvalError::TypeMismatch(
            format!("{}", args[0].debug_type()),
            "first".to_string(),
        )),
    }
}

fn last(args: Vec<Object>) -> BuiltinResult {
    assert_args(&args, 1, "last")?;

    match &args[0] {
        Object::Array(elements) => {
            let n = elements.len();
            if n > 0 {
                Ok(elements[n - 1].clone())
            } else {
                Ok(Null)
            }
        }
        _ => Err(EvalError::TypeMismatch(
            format!("{}", args[0].debug_type()),
            "last".to_string(),
        )),
    }
}

//rest
fn rest(args: Vec<Object>) -> BuiltinResult {
    assert_args(&args, 1, "rest")?;

    match &args[0] {
        Object::Array(elements) => match elements.split_first() {
            Some((_, rest)) => Ok(Object::Array(rest.to_vec())),
            None => Ok(Null),
        },
        _ => Err(EvalError::TypeMismatch(
            format!("{}", args[0].debug_type()),
            "rest".to_string(),
        )),
    }
}

fn push(args: Vec<Object>) -> BuiltinResult {
    assert_args(&args, 2, "push")?;

    match (&args[0], &args[1]) {
        (Object::Array(elements), new_element) => {
            let mut new_elements = elements.clone();
            new_elements.push(new_element.clone());
            Ok(Object::Array(new_elements))
        }
        _ => Err(EvalError::TypeMismatch(
            format!("{}", args[0].debug_type()),
            "push".to_string(),
        )),
    }
}

fn puts(args: Vec<Object>) -> BuiltinResult {
    assert_args(&args, 1, "puts")?;
    for arg in &args {
        println!("{}", arg);
    }

    Ok(Null)
}
