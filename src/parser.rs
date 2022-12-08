use crate::ast::{Expr, Program, Stmt};
use crate::lexer::Lexer;
use crate::token::Token;

#[derive(Debug)]
enum ParserError {}

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<ParserError>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lexer,
            cur_token: Token::Eof,
            peek_token: Token::Eof,
            errors: vec![],
        };

        // cur_token  next_token
        // token(1)  token(2)      token(3)
        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    // X X X X
    // i j
    fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.cur_token != Token::Eof {
            let stmt = self.parse_stmt();

            match stmt {
                Some(stmt) => program.statements.push(stmt),
                _ => (),
            }

            self.next_token();
        }

        program
    }

    pub fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            _ => None,
        }
    }

    fn peek_token_is(&self, input: Token) -> bool {
        self.peek_token == input
    }

    fn cur_token_is(&self, input: Token) -> bool {
        self.cur_token == input
    }

    fn expect_peek(&mut self, input: Token) -> bool {
        return if self.peek_token_is(input) {
            self.next_token();
            true
        } else {
            false
        };
    }

    fn parse_let_statement(&mut self) -> Option<Stmt> {
        let id: String;
        // handle identifier
        if let Token::Ident(ident) = self.peek_token.clone() {
            id = ident;
            self.next_token();
        } else {
            return None;
        }

        // handle =

        if !self.expect_peek(Token::Assign) {
            return None;
        }

        // handle expr
        while !self.cur_token_is(Token::Semicolon) {
            self.next_token()
        }
        // return
        Some(Stmt::Let(id, Expr::Integer(31)))
    }

    // fn peek_error(&mut self) {
    //     self.errors.push();
    // }

    fn check_parser_errors(&self) {
        if self.errors.is_empty() {
            return;
        }

        eprintln!("Parser has {} errors:", self.errors.len());

        for error in &self.errors {
            eprintln!("the value is: {:?}", error);
        }
    }
}

// #[cfg(test)]
// mod test_precidence {
//     use super::*;
//
//     #[test]
//     fn test_ord() {
//         assert!(Precedence::Lowest < Precedence::Equals);
//         assert!(Precedence::Equals > Precedence::Lowest);
//     }
// }

#[cfg(test)]
mod test_parser_statements {
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = r"let x = 5;
                           let y = true;
                           let z = y;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected = vec![
            Stmt::Let("x".to_string(), Expr::Integer(5)),
            Stmt::Let("y".to_string(), Expr::Boolean(true)),
            Stmt::Let("z".to_string(), Expr::Identifier("y".to_string())),
        ];

        assert_eq!(program.statements, expected);
    }

    // #[test]
    // fn test_return_statements() {
    //     let input = r"return 5;
    //                        return 2 + 3;";
    //
    //     let lexer = Lexer::new(input.to_string());
    //     let mut parser = Parser::new(lexer);
    //     let program = parser.parse_program();
    //     parser.check_parser_errors();
    //
    //     let expected = vec![
    //         Stmt::Return(Expr::Integer(5)),
    //         Stmt::Return(Expr::Infix(
    //             Box::new(Expr::Integer(2)),
    //             Token::Plus,
    //             Box::new(Expr::Integer(3)),
    //         )),
    //     ];
    //
    //     assert_eq!(program.statements, expected);
    // }
}

// #[cfg(test)]
// mod test_parser_expressions {
//     use std::vec;
//
//     use super::*;
//
//     #[test]
//     fn test_identifier() {
//         let input = r"x";
//
//         let lexer = Lexer::new(input.to_string());
//         let mut parser = Parser::new(lexer);
//         let program = parser.parse_program();
//         parser.check_parser_errors();
//
//         let expected = vec![Stmt::Expression(Expr::Identifier("x".to_string()))];
//
//         assert_eq!(program.statements, expected);
//     }
//
//     #[test]
//     fn test_integer() {
//         let input = r"5";
//
//         let lexer = Lexer::new(input.to_string());
//         let mut parser = Parser::new(lexer);
//         let program = parser.parse_program();
//         parser.check_parser_errors();
//
//         let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::Integer(5))];
//
//         assert_eq!(program.statements, expected);
//     }
//
//     #[test]
//     fn test_string() {
//         let input = "\"hello world\"; \"hello world 2\"";
//
//         let lexer = Lexer::new(input.to_string());
//         let mut parser = Parser::new(lexer);
//         let program = parser.parse_program();
//         parser.check_parser_errors();
//
//         let expected: Vec<Stmt> = vec![
//             Stmt::Expression(Expr::Str("hello world".to_string())),
//             Stmt::Expression(Expr::Str("hello world 2".to_string())),
//         ];
//
//         assert_eq!(program.statements, expected);
//     }
//
//     #[test]
//     fn test_boolean() {
//         let input = r"true;
//                            false;
//                            let foobar = true;";
//
//         let lexer = Lexer::new(input.to_string());
//         let mut parser = Parser::new(lexer);
//         let program = parser.parse_program();
//         parser.check_parser_errors();
//
//         let expected: Vec<Stmt> = vec![
//             Stmt::Expression(Expr::Boolean(true)),
//             Stmt::Expression(Expr::Boolean(false)),
//             Stmt::Let("foobar".to_string(), Expr::Boolean(true)),
//         ];
//
//         assert_eq!(program.statements, expected);
//     }
//
//     #[test]
//     fn test_array() {
//         let input = "[];
//                           [1, 2];
//                           [3, 4+5];
//                           [6, 7, \"hello\"];";
//
//         let program = Program::new(input);
//
//         let expected: Vec<Stmt> = vec![
//             Stmt::Expression(Expr::Array(vec![])),
//             Stmt::Expression(Expr::Array(vec![Expr::Integer(1), Expr::Integer(2)])),
//             Stmt::Expression(Expr::Array(vec![
//                 Expr::Integer(3),
//                 Expr::Infix(
//                     Box::new(Expr::Integer(4)),
//                     Token::Plus,
//                     Box::new(Expr::Integer(5)),
//                 ),
//             ])),
//             Stmt::Expression(Expr::Array(vec![
//                 Expr::Integer(6),
//                 Expr::Integer(7),
//                 Expr::Str("hello".to_string()),
//             ])),
//         ];
//
//         assert_eq!(program.statements, expected);
//     }
//
//     #[test]
//     fn test_index() {
//         let input = "myArray[2]";
//
//         let program = Program::new(input);
//
//         let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::Index(
//             Box::new(Expr::Identifier("myArray".to_string())),
//             Box::new(Expr::Integer(2)),
//         ))];
//
//         assert_eq!(program.statements, expected);
//     }
//
//     #[test]
//     fn test_map_empty() {
//         let input = "{}";
//         let program = Program::new(input);
//
//         let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::Hash(vec![]))];
//
//         assert_eq!(program.statements, expected);
//     }
//
//     #[test]
//     fn test_map_strings() {
//         let input = "{\"one\": 1, \"two\": 2, \"three\": 3};
//                           {1: \"one\", 23: \"two\" + \"three\"}";
//
//         let program = Program::new(input);
//
//         let expected: Vec<Stmt> = vec![
//             Stmt::Expression(Expr::Hash(vec![
//                 (Expr::Str("one".to_string()), Expr::Integer(1)),
//                 (Expr::Str("two".to_string()), Expr::Integer(2)),
//                 (Expr::Str("three".to_string()), Expr::Integer(3)),
//             ])),
//             Stmt::Expression(Expr::Hash(vec![
//                 (Expr::Integer(1), Expr::Str("one".to_string())),
//                 (
//                     Expr::Integer(23),
//                     Expr::Infix(
//                         Box::new(Expr::Str("two".to_string())),
//                         Token::Plus,
//                         Box::new(Expr::Str("three".to_string())),
//                     ),
//                 ),
//             ])),
//         ];
//         assert_eq!(program.statements, expected);
//     }
//
//     #[test]
//     fn test_prefix_expression() {
//         let input = "!5;
//                           -15;
//                           !true;
//                           !false;";
//
//         let lexer = Lexer::new(input.to_string());
//         let mut parser = Parser::new(lexer);
//         let program = parser.parse_program();
//         parser.check_parser_errors();
//
//         let expected: Vec<Stmt> = vec![
//             Stmt::Expression(Expr::Prefix(Token::Bang, Box::new(Expr::Integer(5)))),
//             Stmt::Expression(Expr::Prefix(Token::Minus, Box::new(Expr::Integer(15)))),
//             Stmt::Expression(Expr::Prefix(Token::Bang, Box::new(Expr::Boolean(true)))),
//             Stmt::Expression(Expr::Prefix(Token::Bang, Box::new(Expr::Boolean(false)))),
//         ];
//
//         assert_eq!(program.statements, expected);
//     }
//
//     #[test]
//     fn test_infix_expressions() {
//         let input = "5 + 5;
//                           5 - 5;
//                           5 * 5;
//                           5 / 5;
//                           5 > 5;
//                           5 < 5;
//                           5 == 5;
//                           5 != 5;
//                           true == true;
//                           true != false;";
//
//         let lexer = Lexer::new(input.to_string());
//         let mut parser = Parser::new(lexer);
//         let program = parser.parse_program();
//         parser.check_parser_errors();
//
//         let expected: Vec<Stmt> = vec![
//             Stmt::Expression(Expr::Infix(
//                 Box::new(Expr::Integer(5)),
//                 Token::Plus,
//                 Box::new(Expr::Integer(5)),
//             )),
//             Stmt::Expression(Expr::Infix(
//                 Box::new(Expr::Integer(5)),
//                 Token::Minus,
//                 Box::new(Expr::Integer(5)),
//             )),
//             Stmt::Expression(Expr::Infix(
//                 Box::new(Expr::Integer(5)),
//                 Token::Asterisk,
//                 Box::new(Expr::Integer(5)),
//             )),
//             Stmt::Expression(Expr::Infix(
//                 Box::new(Expr::Integer(5)),
//                 Token::Slash,
//                 Box::new(Expr::Integer(5)),
//             )),
//             Stmt::Expression(Expr::Infix(
//                 Box::new(Expr::Integer(5)),
//                 Token::Gt,
//                 Box::new(Expr::Integer(5)),
//             )),
//             Stmt::Expression(Expr::Infix(
//                 Box::new(Expr::Integer(5)),
//                 Token::Lt,
//                 Box::new(Expr::Integer(5)),
//             )),
//             Stmt::Expression(Expr::Infix(
//                 Box::new(Expr::Integer(5)),
//                 Token::Eq,
//                 Box::new(Expr::Integer(5)),
//             )),
//             Stmt::Expression(Expr::Infix(
//                 Box::new(Expr::Integer(5)),
//                 Token::NotEq,
//                 Box::new(Expr::Integer(5)),
//             )),
//             Stmt::Expression(Expr::Infix(
//                 Box::new(Expr::Boolean(true)),
//                 Token::Eq,
//                 Box::new(Expr::Boolean(true)),
//             )),
//             Stmt::Expression(Expr::Infix(
//                 Box::new(Expr::Boolean(true)),
//                 Token::NotEq,
//                 Box::new(Expr::Boolean(false)),
//             )),
//         ];
//
//         assert_eq!(program.statements, expected);
//     }
//
//     #[test]
//     fn test_operator_precedence_parsing() {
//         let tests: Vec<(&str, &str)> = vec![
//             ("-a * b", "((-a) * b)"),
//             ("!-a", "(!(-a))"),
//             ("a + b + c", "((a + b) + c)"),
//             ("a + b - c", "((a + b) - c)"),
//             ("a * b * c", "((a * b) * c)"),
//             ("a * b / c", "((a * b) / c)"),
//             ("a + b / c", "(a + (b / c))"),
//             ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
//             ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
//             ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
//             ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
//             (
//                 "3 + 4 * 5 == 3 * 1 + 4 * 5",
//                 "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
//             ),
//             ("true", "true"),
//             ("false", "false"),
//             ("3 > 5 == false", "((3 > 5) == false)"),
//             ("3 < 5 == true", "((3 < 5) == true)"),
//             ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
//             ("(5 + 5) * 2", "((5 + 5) * 2)"),
//             ("2 / (5 + 5)", "(2 / (5 + 5))"),
//             ("-(5 + 5)", "(-(5 + 5))"),
//             ("!(true == true)", "(!(true == true))"),
//             ("(1 + 1)", "(1 + 1)"),
//             ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
//             (
//                 "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
//                 "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
//             ),
//             (
//                 "add(a + b + c * d / f + g)",
//                 "add((((a + b) + ((c * d) / f)) + g))",
//             ),
//             (
//                 "a * [1, 2, 3, 4][b * c] * d",
//                 "((a * ([1, 2, 3, 4][(b * c)])) * d)",
//             ),
//             (
//                 "add(a * b[2], b[1], 2 * [1, 2][1])",
//                 "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
//             ),
//         ];
//
//         for (input, expected) in tests {
//             let lexer = Lexer::new(input.to_string());
//             let mut parser = Parser::new(lexer);
//             let program = parser.parse_program();
//             parser.check_parser_errors();
//             assert_eq!(program.to_string(), expected.to_string());
//         }
//     }
//
//     #[test]
//     fn test_if_expression() {
//         let input = "if (x < y) { x }";
//
//         let lexer = Lexer::new(input.to_string());
//         let mut parser = Parser::new(lexer);
//         let program = parser.parse_program();
//         parser.check_parser_errors();
//
//         let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::If(
//             Box::new(Expr::Infix(
//                 Box::new(Expr::Identifier("x".to_string())),
//                 Token::Lt,
//                 Box::new(Expr::Identifier("y".to_string())),
//             )),
//             BlockStmt {
//                 statements: vec![Stmt::Expression(Expr::Identifier("x".to_string()))],
//             },
//             None,
//         ))];
//
//         assert_eq!(program.statements, expected);
//     }
//
//     #[test]
//     fn test_if_else_expression() {
//         let input = "if (x < y) { x } else { y }";
//
//         let lexer = Lexer::new(input.to_string());
//         let mut parser = Parser::new(lexer);
//         let program = parser.parse_program();
//         parser.check_parser_errors();
//
//         let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::If(
//             Box::new(Expr::Infix(
//                 Box::new(Expr::Identifier("x".to_string())),
//                 Token::Lt,
//                 Box::new(Expr::Identifier("y".to_string())),
//             )),
//             BlockStmt {
//                 statements: vec![Stmt::Expression(Expr::Identifier("x".to_string()))],
//             },
//             Some(BlockStmt {
//                 statements: vec![Stmt::Expression(Expr::Identifier("y".to_string()))],
//             }),
//         ))];
//
//         assert_eq!(program.statements, expected);
//     }
//
//     #[test]
//     fn test_function_literal_parsing() {
//         let input = "fn(x, y) { x + y; }";
//
//         let lexer = Lexer::new(input.to_string());
//         let mut parser = Parser::new(lexer);
//         let program = parser.parse_program();
//         parser.check_parser_errors();
//
//         let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::Function(
//             vec![
//                 Expr::Identifier("x".to_string()),
//                 Expr::Identifier("y".to_string()),
//             ],
//             BlockStmt {
//                 statements: vec![Stmt::Expression(Expr::Infix(
//                     Box::new(Expr::Identifier("x".to_string())),
//                     Token::Plus,
//                     Box::new(Expr::Identifier("y".to_string())),
//                 ))],
//             },
//         ))];
//
//         assert_eq!(program.statements, expected);
//     }
//
//     #[test]
//     fn test_function_parameter_parsing() {
//         let input = "fn() {};
//                           fn(x) {};
//                           fn(x, y, z) {};";
//
//         let lexer = Lexer::new(input.to_string());
//         let mut parser = Parser::new(lexer);
//         let program = parser.parse_program();
//         parser.check_parser_errors();
//
//         let expected: Vec<Stmt> = vec![
//             Stmt::Expression(Expr::Function(vec![], BlockStmt { statements: vec![] })),
//             Stmt::Expression(Expr::Function(
//                 vec![Expr::Identifier("x".to_string())],
//                 BlockStmt { statements: vec![] },
//             )),
//             Stmt::Expression(Expr::Function(
//                 vec![
//                     Expr::Identifier("x".to_string()),
//                     Expr::Identifier("y".to_string()),
//                     Expr::Identifier("z".to_string()),
//                 ],
//                 BlockStmt { statements: vec![] },
//             )),
//         ];
//
//         assert_eq!(program.statements, expected);
//     }
//
//     #[test]
//     fn test_call_expression_parsing() {
//         let input = "add(1, 2 * 3, 4 + 5);";
//
//         let lexer = Lexer::new(input.to_string());
//         let mut parser = Parser::new(lexer);
//         let program = parser.parse_program();
//         parser.check_parser_errors();
//
//         let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::Call(
//             Box::new(Expr::Identifier("add".to_string())),
//             vec![
//                 Expr::Integer(1),
//                 Expr::Infix(
//                     Box::new(Expr::Integer(2)),
//                     Token::Asterisk,
//                     Box::new(Expr::Integer(3)),
//                 ),
//                 Expr::Infix(
//                     Box::new(Expr::Integer(4)),
//                     Token::Plus,
//                     Box::new(Expr::Integer(5)),
//                 ),
//             ],
//         ))];
//
//         assert_eq!(program.statements, expected);
//     }
// }
