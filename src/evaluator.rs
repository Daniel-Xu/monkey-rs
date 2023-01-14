use crate::ast::Expr::Integer;
use crate::ast::{BlockStmt, Expr, Program, Stmt};
use crate::evaluator::EvalError::NotImplemented;
use crate::object::Object::{self};
use crate::object::{FALSE, NULL, TRUE};
use crate::token::Token;

// pub fn eval_stmt(statement: Stmt) -> Object {}

pub type Result<T> = std::result::Result<T, EvalError>;

#[derive(Debug, PartialEq)]
pub enum EvalError {
    TypeMismatch(String, String),
    UnknownOperator(String, String),
    // SyntaxError(String, String),
    IdentifierNotFound(String, String),
    ExpectedIdentifier(String, String),
    WrongNumberOfArguments(String, String),
    IndexOutOfBounds(String, String),
    NotImplemented,
}
pub fn eval(program: Program) -> Result<Object> {
    let mut result = NULL;
    for stmt in &program.statements {
        result = eval_statement(stmt)?;

        if let Object::ReturnValue(value) = result {
            result = *value;
            break;
        }
    }

    Ok(result)
}

fn eval_block_stmts(block: &BlockStmt) -> Result<Object> {
    let mut result = NULL;
    for stmt in &block.statements {
        result = eval_statement(stmt)?;

        /**
         * we should still return ReturnValue so it can be handle correctly in eval()
         */
        if matches!(result, Object::ReturnValue(_)) {
            break;
        }
    }
    Ok(result)
}

pub fn eval_statement(stmt: &Stmt) -> Result<Object> {
    match stmt {
        Stmt::Expression(expr) => eval_expression(expr),
        Stmt::Return(expr) => Ok(Object::ReturnValue(Box::new(eval_expression(expr)?))),
        _ => Err(NotImplemented),
        // Let() => eval_let_stmt(),
    }
}

fn eval_expression(expr: &Expr) -> Result<Object> {
    match expr {
        Expr::Integer(n) => Ok(Object::Integer(*n)),
        Expr::Prefix(token, inner) => eval_prefix_expr(token, eval_expression(inner)?),

        // Infix(Box<Expr>, Token, Box<Expr>),          //left, token, right // TODO: change this to struct
        Expr::Infix(left, operator, right) => {
            eval_infix_expr(operator, eval_expression(left)?, eval_expression(right)?)
        }
        Expr::Boolean(bool_value) => Ok(Object::Boolean(*bool_value)), // copy happens here

        Expr::If(condition, if_block, else_block) => {
            eval_if_expression(condition, if_block, else_block)
        }

        Expr::Str(expr) => Ok(Object::Str(expr.clone())),
        _ => Err(NotImplemented),
    }
}

fn eval_if_expression(
    condition: &Box<Expr>,
    if_block: &BlockStmt,
    else_block: &Option<BlockStmt>,
) -> Result<Object> {
    let condition_obj = eval_expression(condition)?;
    if is_truthy(condition_obj) {
        eval_block_stmts(if_block)
    } else {
        match else_block {
            None => Ok(NULL),
            Some(blocks) => eval_block_stmts(blocks),
        }
    }
}

fn is_truthy(condition: Object) -> bool {
    match condition {
        NULL => false,
        TRUE => true,
        FALSE => false,
        _ => true,
    }
}

fn eval_infix_expr(operator: &Token, left: Object, right: Object) -> Result<Object> {
    match (left, right) {
        (Object::Integer(n1), Object::Integer(n2)) => eval_interger_infix_expr(operator, n1, n2),
        (Object::Boolean(b1), Object::Boolean(b2)) => eval_boolean_infix_expr(operator, b1, b2),
        (Object::Str(s1), Object::Str(s2)) => eval_string_infix_expr(operator, s1, s2),
        (_, _) => Err(NotImplemented),
    }
}

// do we need to use &str instead of String?
fn eval_string_infix_expr(operator: &Token, s1: String, s2: String) -> Result<Object> {
    match operator {
        Token::Plus => Ok(Object::Str(s1 + &s2)),
        Token::Eq => Ok(Object::Boolean(s1 == s2)),
        Token::NotEq => Ok(Object::Boolean(s1 != s2)),
        _ => Err(NotImplemented),
    }
}

fn eval_boolean_infix_expr(operator: &Token, b1: bool, b2: bool) -> Result<Object> {
    match operator {
        Token::NotEq => Ok(Object::Boolean(b1 != b2)),
        Token::Eq => Ok(Object::Boolean(b1 == b2)),
        _ => Err(NotImplemented),
    }
}

fn eval_interger_infix_expr(operator: &Token, n1: i32, n2: i32) -> Result<Object> {
    match operator {
        Token::Minus => Ok(Object::Integer(n1 - n2)),
        Token::Plus => Ok(Object::Integer(n1 + n2)),
        Token::Asterisk => Ok(Object::Integer(n1 * n2)),
        Token::Slash => Ok(Object::Integer(n1 / n2)),
        Token::Lt => Ok(Object::get_bool(n1 < n2)),
        Token::Gt => Ok(Object::get_bool(n1 > n2)),
        Token::Eq => Ok(Object::get_bool(n1 == n2)),
        Token::NotEq => Ok(Object::get_bool(n1 != n2)),
        _ => Err(NotImplemented),
    }
}

fn eval_prefix_expr(token: &Token, object: Object) -> Result<Object> {
    match token {
        Token::Minus => match object {
            Object::Integer(num) => Ok(Object::Integer(-num)),
            _ => Err(NotImplemented),
        },
        _ => Err(NotImplemented),
    }
}

fn eval_let_stmt() {
    todo!()
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;

    #[test]
    fn integer_expression() {
        let tests = vec![
            ("10;", Object::Integer(10)),
            ("5;", Object::Integer(5)),
            ("0;", Object::Integer(0)),
            ("-5;", Object::Integer(-5)),
            ("-10;", Object::Integer(-10)),
            ("1 + 2", Object::Integer(3)),
            ("1 + 2 * 3", Object::Integer(7)),
            ("1 - 2 * 3", Object::Integer(-5)),
            ("1 * 2 * 3", Object::Integer(6)),
            ("1 + 2 / 3", Object::Integer(1)),
            ("1 + 6 / -3", Object::Integer(-1)),
            ("2 / 2 + 5", Object::Integer(6)),
            ("-2 / 2 + 5", Object::Integer(4)),
            ("8 / 4 + 5", Object::Integer(7)),
            ("(10 + 5) * 3", Object::Integer(45)),
        ];

        for (input, expected) in tests {
            let program = Program::from_input(input);
            // TODO: restore environment
            let result = eval(program).unwrap();
            // let env = Environment::new();
            // assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
            assert_eq!(result, expected, "{}", input);
        }
    }

    #[test]
    fn boolean_expression() {
        let tests = vec![
            ("true", TRUE),
            ("false", FALSE),
            ("true;", TRUE),
            ("1 < 2", TRUE),
            ("1 > 2", FALSE),
            ("1 < 1", FALSE),
            ("1 > 1", FALSE),
            ("1 == 1", TRUE),
            ("1 != 1", FALSE),
            ("1 == 2", FALSE),
            ("1 != 2", TRUE),
            ("true == true", TRUE),
            ("false == false", TRUE),
            ("true == false", FALSE),
            ("true != false", TRUE),
            ("false != true", TRUE),
            ("(1 < 2) == true", TRUE),
            ("(1 < 2) == false", FALSE),
            ("(1 > 2) == true", FALSE),
            ("(1 > 2) == false", TRUE),
        ];

        for (input, expected) in tests {
            // let program = Program::new(input);
            // let env = Environment::new();
            // assert_eq!(eval(program, env).unwrap(), expected, "{}", input);

            let program = Program::from_input(input);
            let result = eval(program).unwrap();
            assert_eq!(result, expected, "{}", input);
        }
    }

    #[test]
    fn string_expression() {
        let tests = vec![
            ("\"hello\"", "hello"),
            ("\":)\"", ":)"),
            ("\"true\"", "true"),
            ("\"123\"", "123"),
        ];

        for (input, expected) in tests {
            // let program = Program::new(input);
            // let env = Environment::new();
            // assert_eq!(
            //     eval(program, env).unwrap(),
            //     Object::Str(expected.to_string()),
            //     "{}",
            //     input
            // );

            let program = Program::from_input(input);
            let result = eval(program).unwrap();
            assert_eq!(result, Object::Str(expected.to_string()), "{}", input);
        }
    }

    #[test]
    fn string_infix_expression() {
        let tests = vec![
            (
                "\"hello, \" + \"world!\"",
                Object::Str("hello, world!".to_string()),
            ),
            (
                "\"hello\" + \" \" + \":)\"",
                Object::Str("hello :)".to_string()),
            ),
            ("\"hello\" == \"hello\"", TRUE),
            ("\"hello\" == \":)\"", FALSE),
            ("\"hello\" != \":)\"", TRUE),
            ("\"hello\" != \"hello\"", FALSE),
        ];

        for (input, expected) in tests {
            // let program = Program::new(input);
            // let env = Environment::new();
            // assert_eq!(eval(program, env).unwrap(), expected, "{}", input);

            let program = Program::from_input(input);
            let result = eval(program).unwrap();
            assert_eq!(result, expected, "{}", input);
        }
    }

    // #[test]
    // fn array_literals() {
    //     let tests = vec![
    //         ("[]", Object::Array(vec![])),
    //         ("[1]", Object::Array(vec![Object::Integer(1)])),
    //         (
    //             "[1, 2]",
    //             Object::Array(vec![Object::Integer(1), Object::Integer(2)]),
    //         ),
    //     ];
    //
    //     for (input, expected) in tests {
    //         let program = Program::new(input);
    //         let env = Environment::new();
    //         assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
    //     }
    // }
    //
    // #[test]
    // fn array_index_expressions() {
    //     let tests = vec![
    //         ("[1,2,3][0]", Object::Integer(1)),
    //         ("[1,2,3][1]", Object::Integer(2)),
    //         ("[1,2,3][2]", Object::Integer(3)),
    //         ("let i = 0; [1,2,3][i]", Object::Integer(1)),
    //         ("[1,2,3][1+1]", Object::Integer(3)),
    //         ("let myArray = [1,2,3]; myArray[1]", Object::Integer(2)),
    //         (
    //             "let myArray = [1,2,3]; myArray[0] + myArray[1] + myArray[2]",
    //             Object::Integer(6),
    //         ),
    //         (
    //             "let myArray = [1,2,3]; let i = myArray[0]; myArray[i]",
    //             Object::Integer(2),
    //         ),
    //     ];
    //
    //     for (input, expected) in tests {
    //         let program = Program::new(input);
    //         let env = Environment::new();
    //         assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
    //     }
    // }
    //
    // #[test]
    // fn test_hash_literals() {
    //     let map_1 = HashMap::new();
    //     let mut map_2 = HashMap::new();
    //     map_2.insert(Object::Integer(2), Object::Str("two".to_string()));
    //     map_2.insert(TRUE, Object::Integer(3));
    //     map_2.insert(Object::Str("four".to_string()), Object::Integer(4));
    //     map_2.insert(Object::Str("five".to_string()), Object::Integer(5));
    //
    //     let tests = vec![
    //         ("{}", Object::Hash(map_1)),
    //         (
    //             "let five = \"five\";
    //           {
    //             2: \"two\",
    //             true: 3,
    //             \"fo\" + \"ur\": 4,
    //             five: 5,
    //           }",
    //             Object::Hash(map_2),
    //         ),
    //     ];
    //
    //     for (input, expected) in tests {
    //         let program = Program::new(input);
    //         let env = Environment::new();
    //         assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
    //     }
    // }
    //
    // #[test]
    // fn test_hash_index_expressions() {
    //     let tests = vec![
    //         ("{}[1]", NULL),
    //         ("{\"one\": 1}[\"one\"]", Object::Integer(1)),
    //         ("{2: true}[2]", TRUE),
    //         ("{false: 1, false: 2}[false]", Object::Integer(2)),
    //     ];
    //
    //     for (input, expected) in tests {
    //         let program = Program::new(input);
    //         let env = Environment::new();
    //         assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
    //     }
    // }
    //
    // #[test]
    // fn bang_operator() {
    //     let tests = vec![
    //         ("!true", Object::Boolean(false)),
    //         ("!false", Object::Boolean(true)),
    //         ("!!true", Object::Boolean(true)),
    //         ("!!false", Object::Boolean(false)),
    //     ];
    //
    //     for (input, expected) in tests {
    //         let program = Program::new(input);
    //         let env = Environment::new();
    //         assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
    //     }
    // }
    //
    #[test]
    fn if_expressions() {
        let tests = vec![
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", NULL),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", NULL),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
        ];

        for (input, expected) in tests {
            // let program = Program::new(input);
            // let env = Environment::new();
            // assert_eq!(eval(program, env).unwrap(), expected, "{}", input);

            let program = Program::from_input(input);
            let result = eval(program).unwrap();
            assert_eq!(result, expected, "{}", input);
        }
    }

    #[test]
    fn return_statements() {
        let tests = vec![
            // ("return 11;", Object::Integer(11)),
            ("return 12; 9;", Object::Integer(12)),
            // ("return 3 + (2 * 5); 9;", Object::Integer(13)),
            // ("9; return 2 * 7; 9;", Object::Integer(14)),
            // (
            //     "if (10 > 1) { if (true) { return 15; } return 1; } ",
            //     Object::Integer(15),
            // ),
        ];

        for (input, expected) in tests {
            // let program = Program::new(input);
            // let env = Environment::new();
            // assert_eq!(eval(program, env).unwrap(), expected, "{}", input);

            let program = Program::from_input(input);
            let result = eval(program).unwrap();
            assert_eq!(result, expected, "{}", input);
        }
    }

    // #[test]
    // fn error_handling() {
    //     let tests = vec![
    //         (
    //             "5 + true;",
    //             EvalError::TypeMismatch("Integer + Boolean".to_string(), "eval_infix_expression".to_string())
    //         ),
    //         (
    //             "5 + true; 5;",
    //             EvalError::TypeMismatch("Integer + Boolean".to_string(), "eval_infix_expression".to_string())
    //         ),
    //         ("-true", EvalError::UnknownOperator("-Boolean".to_string(), "eval_prefix_expression".to_string())),
    //         (
    //             "true + false",
    //             EvalError::UnknownOperator("+".to_string(), "eval_boolean_infix_expression".to_string())
    //         ),
    //         (
    //             "5; true + false; 5",
    //             EvalError::UnknownOperator("+".to_string(), "eval_boolean_infix_expression".to_string())
    //         ),
    //         (
    //             "if (2 > 1) { true + false; } else { 5 };",
    //             EvalError::UnknownOperator("+".to_string(), "eval_boolean_infix_expression".to_string())
    //         ),
    //         (
    //             "if (2 > 1) { if (2 < 1) { return 1; } else { return true + true; } } else { return 2; };",
    //             EvalError::UnknownOperator("+".to_string(), "eval_boolean_infix_expression".to_string())
    //         ),
    //         (
    //             "let add = fn(a, b) { a + b }; add(1, 2, 3);",
    //             EvalError::WrongNumberOfArguments("parameters 2, arguments 3".to_string(), "extend_function_environment".to_string())
    //         ),
    //         (
    //             "\"Hello\" - \"Hello\"",
    //             EvalError::UnknownOperator("-".to_string(), "eval_string_infix_expression".to_string())
    //         ),
    //         ("[1,2,3][3]", EvalError::IndexOutOfBounds("array length = 3, index = 3".to_string(), "eval_index_expression".to_string())),
    //         ("[1,2,3][-1]", EvalError::IndexOutOfBounds("negative indices not supported. index = -1".to_string(), "eval_index_expression".to_string())),
    //         ("{true: 2}[[1,2,3]]", EvalError::TypeMismatch("Array is not hashable".to_string(), "eval_index_expression".to_string())),
    //     ];
    //
    //     for (input, expected) in tests {
    //         // let program = Program::new(input);
    //         // let env = Environment::new();
    //         // assert_eq!(eval(program, env).unwrap_err(), expected, "{}", input);
    //
    //
    //         let program = Program::from_input(input);
    //         let result = eval(program).unwrap();
    //         assert_eq!(result, expected, "{}", input);
    //     }
    // }
    //
    // #[test]
    // fn let_statements() {
    //     let tests = vec![
    //         ("let a = 5; a;", Object::Integer(5)),
    //         ("let a = 5 * 5; a;", Object::Integer(25)),
    //         ("let a = 5; let b = a; b;", Object::Integer(5)),
    //         (
    //             "let a = 5; let b = a; let c = a + b + 5; c;",
    //             Object::Integer(15),
    //         ),
    //         (
    //             "let a = 5; let b = a; let c = a + b + 5; c * 3;",
    //             Object::Integer(45),
    //         ),
    //     ];
    //
    //     for (input, expected) in tests {
    //         let program = Program::new(input);
    //         let env = Environment::new();
    //         assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
    //     }
    // }
    //
    // #[test]
    // fn functions_objects_0() {
    //     let tests = vec![(
    //         "fn(x) { x + 2; };",
    //         Object::Function(
    //             vec![Expr::Identifier("x".to_string())],
    //             BlockStmt {
    //                 statements: vec![Stmt::Expression(Expr::Infix(
    //                     Box::new(Expr::Identifier("x".to_string())),
    //                     Token::Plus,
    //                     Box::new(Expr::Integer(2)),
    //                 ))],
    //             },
    //             Environment::new(),
    //         ),
    //     )];
    //
    //     for (input, expected) in tests {
    //         let program = Program::new(input);
    //         let env = Environment::new();
    //         assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
    //     }
    // }
    //
    // #[test]
    // fn functions_objects_1() {
    //     let tests = vec![("fn(x) { x + 2; };", "fn(x) (x + 2)".to_string())];
    //
    //     for (input, expected) in tests {
    //         let program = Program::new(input);
    //         let env = Environment::new();
    //         assert_eq!(
    //             eval(program, env).unwrap().to_string(),
    //             expected,
    //             "{}",
    //             input
    //         );
    //     }
    // }
    //
    // #[test]
    // fn function_application() {
    //     let tests = vec![
    //         ("let identity = fn(x) { x; }; identity(5);", Object::Integer(5)),
    //         ("let identity = fn(x) { return x; }; identity(5);", Object::Integer(5)),
    //         ("let double = fn(x) { x * 2; }; double(5);", Object::Integer(10)),
    //         ("let add = fn(x, y) { x + y; }; add(5, 5);", Object::Integer(10)),
    //         ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", Object::Integer(20)),
    //         ("fn(x) { x; }(5)", Object::Integer(5)),
    //         ("fn(x) { return x; }(5)", Object::Integer(5)),
    //         ("fn(x) { x; }(3 * 3)", Object::Integer(9)),
    //         ("let factorial = fn(n) { if (n == 0) { 1 } else { n * factorial(n - 1) } }; factorial(5);", Object::Integer(120)),
    //         ("let addThree = fn(x) { x + 3 }; let callTwoTimes = fn(x, func) { func(func(x)) }; callTwoTimes(3, addThree);", Object::Integer(9)),
    //         ("let fib = fn(i) { if (i < 2) { 1 } else { fib(i-1) + fib(i-2); } }; fib(9)", Object::Integer(55)),
    //         ("let fib = fn(i) { if (i==0) { return 1 } else { if (i==1) { return 1; } else { return fib(i-1) + fib(i-2); } } }; fib(9)", Object::Integer(55)),
    //         ("let n = 3; let add_n = fn(x) { x + n; }; add_n(2)", Object::Integer(5)),
    //         ("let n = 3; let add_n = fn(x) { x + n; }; let n = 1; add_n(2)", Object::Integer(3)), // not sure I like how environments behave
    //     ];
    //
    //     for (input, expected) in tests {
    //         let program = Program::new(input);
    //         let env = Environment::new();
    //         assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
    //     }
    // }
    //
    // #[test]
    // fn closures() {
    //     let tests = vec![(
    //         "let newAdder = fn(x) { fn(y) { x + y } }; let addTwo = newAdder(2); addTwo(2)",
    //         Object::Integer(4),
    //     )];
    //
    //     for (input, expected) in tests {
    //         let program = Program::new(input);
    //         let env = Environment::new();
    //         assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
    //     }
    // }
    //
    // #[test]
    // fn higher_order_functions() {
    //     let tests = vec![(
    //         "let add = fn(a, b) { a + b };
    //              let applyFunc = fn(a, b, func) { func(a, b) };
    //              applyFunc(2, 2, add);",
    //         Object::Integer(4),
    //     )];
    //
    //     for (input, expected) in tests {
    //         let program = Program::new(input);
    //         let env = Environment::new();
    //         assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
    //     }
    // }
    //
    // #[test]
    // fn functional() {
    //     let tests = vec![
    //         (
    //             "
    //         let map = fn(f, xs) {
    //             let iter = fn(accumulated, remaining) {
    //                 if (len(remaining) == 0) {
    //                     accumulated
    //                 } else {
    //                     iter(push(accumulated, f(first(remaining))), rest(remaining))
    //                 }
    //             };
    //             iter([], xs)
    //         };
    //         let square = fn(x) {
    //             x*x
    //         };
    //         let array = [1, 2, 3, 4];
    //         map(square, array)
    //         ",
    //             Object::Array(vec![
    //                 Object::Integer(1),
    //                 Object::Integer(4),
    //                 Object::Integer(9),
    //                 Object::Integer(16),
    //             ]),
    //         ),
    //         (
    //             "
    //         let reduce = fn(acc, f, xs) {
    //             let iter = fn(acc, xs) {
    //                 if (len(xs)==0) {
    //                     acc
    //                 } else {
    //                     iter(f(acc, first(xs)), rest(xs))
    //                 }
    //             };
    //             iter(acc, xs)
    //         }
    //         let array = [1, 2, 3, 4];
    //         reduce(0, fn(a, b){a + b}, array)
    //         ",
    //             Object::Integer(10),
    //         ),
    //     ];
    //
    //     for (input, expected) in tests {
    //         let program = Program::new(input);
    //         let env = Environment::new();
    //         assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
    //     }
    // }
    //
    // #[test]
    // fn builtin_functions() {
    //     let tests = vec![
    //         ("len(\"\")", Object::Integer(0)),
    //         ("len(\"four\")", Object::Integer(4)),
    //         ("len(\"hiiiiiiiii\")", Object::Integer(10)),
    //         ("len(\"hello\" + \" :)\")", Object::Integer(8)),
    //         ("len([1,2,3,4,5])", Object::Integer(5)),
    //         ("first([1,2,3,4,5])", Object::Integer(1)),
    //         ("last([1,2,3,4,5])", Object::Integer(5)),
    //         (
    //             "rest([1,2,3,4,5])",
    //             Object::Array(vec![
    //                 Object::Integer(2),
    //                 Object::Integer(3),
    //                 Object::Integer(4),
    //                 Object::Integer(5),
    //             ]),
    //         ),
    //         (
    //             "rest(rest([1,2,3,4,5]))",
    //             Object::Array(vec![
    //                 Object::Integer(3),
    //                 Object::Integer(4),
    //                 Object::Integer(5),
    //             ]),
    //         ),
    //         (
    //             "rest(rest(rest(rest(rest([1,2,3,4,5])))))",
    //             Object::Array(vec![]),
    //         ),
    //         (
    //             "push([1], 2)",
    //             Object::Array(vec![Object::Integer(1), Object::Integer(2)]),
    //         ),
    //         (
    //             "let x = [1]; let x = push(x, 2); let x = push(x, 3); x",
    //             Object::Array(vec![
    //                 Object::Integer(1),
    //                 Object::Integer(2),
    //                 Object::Integer(3),
    //             ]),
    //         ),
    //     ];
    //
    //     for (input, expected) in tests {
    //         let program = Program::new(input);
    //         let env = Environment::new();
    //         assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
    //     }
    // }
}
