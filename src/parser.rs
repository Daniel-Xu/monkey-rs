use crate::ast::{BlockStmt, Expr, Program, Stmt};
use crate::lexer::Lexer;
use crate::token::Token::{self, *};
use Precedence::*;

#[derive(Debug)]
pub enum ParserError {
    ExpectedIdent(Token),
    ExpectedAssign(Token),
    ExpectedLParen(Token),
    ExpectedRParen(Token),
    ExpectedLBrace(Token),
    ExpectedRBracket(Token),
    ExpectedColon(Token),
    ExpectedComma(Token),
    ExpectedToken { expected: Token, got: Token },
    ExpectedRBrace(Token),
    ExpectedPrefixToken(Token),
    UnknownError,
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,      // ==, !=
    LessGreater, // >, <
    Sum,         // +, -
    Product,     // *, /
    Prefix,      // -x, !true
    Call,        // myFunction(x)
    Index,       // myArray[2]
}

impl Precedence {
    fn from_token(token: &Token) -> Self {
        match token {
            Plus | Minus => Sum,
            Asterisk | Slash => Product,
            Lt | Gt => LessGreater,
            Eq | NotEq => Equals,
            LParen => Call,
            LBracket => Index,
            _ => Lowest,
        }
    }
}

type Result<T> = std::result::Result<T, ParserError>;

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
                Ok(stmt) => program.statements.push(stmt),
                Err(error) => self.errors.push(error),
            }

            self.next_token();
        }

        program
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expr_statement(),
        }
    }

    fn cur_token_is(&self, input: Token) -> bool {
        self.cur_token == input
    }

    fn peek_token_is(&self, input: Token) -> bool {
        self.peek_token == input
    }

    /*
     * After this method, the cur_token will be the token passed in
     */
    fn move_to_peek<F>(&mut self, input: Token, error_constructor: F) -> Result<()>
    where
        F: Fn(Token) -> ParserError,
    {
        let peek = self.peek_token.clone();
        if peek == input {
            self.next_token();
            Ok(())
        } else {
            Err(error_constructor(peek))
        }
    }

    fn parse_prefix_expr(&mut self) -> Result<Expr> {
        let cur_token = self.cur_token.clone();
        self.next_token();
        let right = self.parse_expr(Prefix)?;
        Ok(Expr::Prefix(cur_token, Box::new(right)))
    }

    fn parse_prefix(&mut self) -> Result<Expr> {
        // why can't self move here
        match &self.cur_token {
            // why we need clone here
            Token::Ident(id) => Ok(Expr::Identifier(id.clone())),
            Token::True => Ok(Expr::Boolean(true)),
            Token::False => Ok(Expr::Boolean(false)),
            Token::Int(u) => Ok(Expr::Integer(*u)),
            Token::Bang | Token::Minus => self.parse_prefix_expr(),
            Token::LParen => self.parse_group_expr(),
            Token::If => self.parse_if(),
            Token::Function => self.parse_funcion_literal(),
            t => Err(ParserError::ExpectedPrefixToken(t.clone())),
        }
    }

    fn parse_infix(&mut self, left: Expr) -> Result<Expr> {
        // why can't self move here
        let cur_token = self.cur_token.clone();
        let cur_precedence = Precedence::from_token(&self.cur_token);

        // we move to the starting of next expression
        self.next_token();
        let right = self.parse_expr(cur_precedence)?;

        Ok(Expr::Infix(Box::new(left), cur_token, Box::new(right)))
    }

    fn maybe_skip_semicolon(&mut self) {
        if self.peek_token_is(Semicolon) {
            self.next_token();
        }
    }

    // This is the entry of all expressions
    // we have prefix
    // infix
    // the basic algorithm is simple
    // we start to parse with a lowest precedence
    // if we found more content, we need to keep parsing infix
    // if the infix precedence is smaller than current precedence, we need to stop
    // and let the upper stack to drive the process again
    // inside the parse_infix, we need to parse the expression again.
    fn parse_expr(&mut self, precedence: Precedence) -> Result<Expr> {
        // find prefix
        let mut prefix = self.parse_prefix()?;

        // 5 + 3 * 2
        // ^ we are here
        while !self.peek_token_is(Token::Semicolon)
            && precedence < Precedence::from_token(&self.peek_token)
        {
            // we jump to the operator +, and start to parse the infix expression
            self.next_token();
            prefix = self.parse_infix(prefix)?;
        }

        Ok(prefix)
    }

    fn parse_expr_statement(&mut self) -> Result<Stmt> {
        // expr
        let expr = self.parse_expr(Lowest)?;

        self.maybe_skip_semicolon();

        // return
        Ok(Stmt::Expression(expr))
    }

    fn parse_return_statement(&mut self) -> Result<Stmt> {
        // handle expr
        self.next_token();

        let expr = self.parse_expr(Lowest)?;

        self.maybe_skip_semicolon();

        Ok(Stmt::Return(expr))
    }

    // let x = 5
    // ^
    fn parse_let_statement(&mut self) -> Result<Stmt> {
        let id: String;
        // handle identifier
        if let Token::Ident(ident) = self.peek_token.clone() {
            id = ident;
            self.next_token();
        } else {
            return Err(ParserError::ExpectedIdent(self.peek_token.clone()));
        }

        // handle =
        self.move_to_peek(Token::Assign, ParserError::ExpectedAssign)?;
        self.next_token();

        let expr = self.parse_expr(Lowest)?;
        self.maybe_skip_semicolon();

        Ok(Stmt::Let(id, expr))
    }

    fn check_parser_errors(&self) {
        if self.errors.is_empty() {
            return;
        }

        eprintln!("Parser has {} errors:", self.errors.len());

        for error in &self.errors {
            eprintln!("the value is: {:?}", error);
        }
    }
    fn parse_group_expr(&mut self) -> Result<Expr> {
        self.next_token();

        /*
         *  when we are at `)`, the lowest < lowest can't be valid, so the parse_expr will stop and return
         *  the current AST. we need to check the next token is `(`
         */
        let current = self.parse_expr(Lowest)?;
        self.move_to_peek(Token::RParen, ParserError::ExpectedRParen)?;
        Ok(current)
    }

    fn parse_block(&mut self) -> Result<BlockStmt> {
        // skip {
        self.next_token();

        let mut block = BlockStmt::new();
        while !self.cur_token_is(RBrace) && !self.cur_token_is(Token::Eof) {
            let stmt = self.parse_stmt()?;
            block.statements.push(stmt);
            self.next_token();
        }

        Ok(block)
    }

    fn parse_if(&mut self) -> Result<Expr> {
        // skip if
        self.move_to_peek(Token::LParen, ParserError::ExpectedLParen)?;

        // parse (xxx), cur_token will be )
        let condition = self.parse_expr(Lowest)?;
        self.move_to_peek(Token::LBrace, ParserError::ExpectedLBrace)?;

        let if_block = self.parse_block()?;

        let else_block = if self.peek_token_is(Token::Else) {
            self.next_token(); // move to else
            self.move_to_peek(Token::LBrace, ParserError::ExpectedLBrace)?;

            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(Expr::If(Box::new(condition), if_block, else_block))
    }

    fn parse_funcion_literal(&mut self) -> Result<Expr> {
        // fn(x, y) { x + y; }
        self.move_to_peek(Token::LParen, ParserError::ExpectedLParen)?;
        let args = self.parse_args()?;

        self.move_to_peek(Token::LBrace, ParserError::ExpectedLBrace)?;

        let body = self.parse_block()?;

        Ok(Expr::Function(args, body))
    }

    fn parse_args(&mut self) -> Result<Vec<Expr>> {
        let mut args = vec![];

        // ()
        //  ^
        // (x, y)
        //  ^
        self.next_token();
        if self.cur_token_is(Token::RParen) {
            return Ok(args);
        };

        let arg = match &self.cur_token {
            // why do we need clone here
            Token::Ident(s) => Expr::Identifier(s.clone()),
            t => return Err(ParserError::ExpectedIdent(t.clone())), // TODO clone?
        };

        args.push(arg);

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();

            let arg = match &self.cur_token {
                // why do we need clone here
                Token::Ident(s) => Expr::Identifier(s.clone()),
                t => return Err(ParserError::ExpectedIdent(t.clone())), // TODO clone?
            };
            args.push(arg);
        }

        self.move_to_peek(Token::RParen, ParserError::ExpectedRParen)?;

        Ok(args)
    }
}

#[cfg(test)]
mod test_precidence {
    use super::*;

    #[test]
    fn test_ord() {
        assert!(Precedence::Lowest < Precedence::Equals);
        assert!(Precedence::Equals > Precedence::Lowest);
    }
}

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

    #[test]
    fn test_return_statements() {
        let input = r"return 5;
                           return 2 + 3;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected = vec![
            Stmt::Return(Expr::Integer(5)),
            Stmt::Return(Expr::Infix(
                Box::new(Expr::Integer(2)),
                Token::Plus,
                Box::new(Expr::Integer(3)),
            )),
        ];

        assert_eq!(program.statements, expected);
    }
}

#[cfg(test)]
mod test_parser_expressions {
    use std::vec;

    use super::*;

    #[test]
    fn test_identifier() {
        let input = r"x";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected = vec![Stmt::Expression(Expr::Identifier("x".to_string()))];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_integer() {
        let input = r"5";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::Integer(5))];

        assert_eq!(program.statements, expected);
    }

    // #[test]
    // fn test_string() {
    //     let input = "\"hello world\"; \"hello world 2\"";
    //
    //     let lexer = Lexer::new(input.to_string());
    //     let mut parser = Parser::new(lexer);
    //     let program = parser.parse_program();
    //     parser.check_parser_errors();
    //
    //     let expected: Vec<Stmt> = vec![
    //         Stmt::Expression(Expr::Str("hello world".to_string())),
    //         Stmt::Expression(Expr::Str("hello world 2".to_string())),
    //     ];
    //
    //     assert_eq!(program.statements, expected);
    // }

    #[test]
    fn test_boolean() {
        let input = r"true;
                           false;
                           let foobar = true;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected: Vec<Stmt> = vec![
            Stmt::Expression(Expr::Boolean(true)),
            Stmt::Expression(Expr::Boolean(false)),
            Stmt::Let("foobar".to_string(), Expr::Boolean(true)),
        ];

        assert_eq!(program.statements, expected);
    }

    // #[test]
    // fn test_array() {
    //     let input = "[];
    //                       [1, 2];
    //                       [3, 4+5];
    //                       [6, 7, \"hello\"];";
    //
    //     let program = Program::new(input);
    //
    //     let expected: Vec<Stmt> = vec![
    //         Stmt::Expression(Expr::Array(vec![])),
    //         Stmt::Expression(Expr::Array(vec![Expr::Integer(1), Expr::Integer(2)])),
    //         Stmt::Expression(Expr::Array(vec![
    //             Expr::Integer(3),
    //             Expr::Infix(
    //                 Box::new(Expr::Integer(4)),
    //                 Token::Plus,
    //                 Box::new(Expr::Integer(5)),
    //             ),
    //         ])),
    //         Stmt::Expression(Expr::Array(vec![
    //             Expr::Integer(6),
    //             Expr::Integer(7),
    //             Expr::Str("hello".to_string()),
    //         ])),
    //     ];
    //
    //     assert_eq!(program.statements, expected);
    // }

    // #[test]
    // fn test_index() {
    //     let input = "myArray[2]";
    //
    //     let program = Program::new(input);
    //
    //     let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::Index(
    //         Box::new(Expr::Identifier("myArray".to_string())),
    //         Box::new(Expr::Integer(2)),
    //     ))];
    //
    //     assert_eq!(program.statements, expected);
    // }

    // #[test]
    // fn test_map_empty() {
    //     let input = "{}";
    //     let program = Program::new(input);
    //
    //     let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::Hash(vec![]))];
    //
    //     assert_eq!(program.statements, expected);
    // }

    // #[test]
    // fn test_map_strings() {
    //     let input = "{\"one\": 1, \"two\": 2, \"three\": 3};
    //                       {1: \"one\", 23: \"two\" + \"three\"}";
    //
    //     let program = Program::new(input);
    //
    //     let expected: Vec<Stmt> = vec![
    //         Stmt::Expression(Expr::Hash(vec![
    //             (Expr::Str("one".to_string()), Expr::Integer(1)),
    //             (Expr::Str("two".to_string()), Expr::Integer(2)),
    //             (Expr::Str("three".to_string()), Expr::Integer(3)),
    //         ])),
    //         Stmt::Expression(Expr::Hash(vec![
    //             (Expr::Integer(1), Expr::Str("one".to_string())),
    //             (
    //                 Expr::Integer(23),
    //                 Expr::Infix(
    //                     Box::new(Expr::Str("two".to_string())),
    //                     Token::Plus,
    //                     Box::new(Expr::Str("three".to_string())),
    //                 ),
    //             ),
    //         ])),
    //     ];
    //     assert_eq!(program.statements, expected);
    // }

    #[test]
    fn test_prefix_expression() {
        let input = "!5;
                          -15;
                          !true;
                          !false;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected: Vec<Stmt> = vec![
            Stmt::Expression(Expr::Prefix(Token::Bang, Box::new(Expr::Integer(5)))),
            Stmt::Expression(Expr::Prefix(Token::Minus, Box::new(Expr::Integer(15)))),
            Stmt::Expression(Expr::Prefix(Token::Bang, Box::new(Expr::Boolean(true)))),
            Stmt::Expression(Expr::Prefix(Token::Bang, Box::new(Expr::Boolean(false)))),
        ];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_infix_expressions() {
        let input = "5 + 5;
                          5 - 5;
                          (5 - 5);
                          5 * 5;
                          5 / 5;
                          5 > 5;
                          5 < 5;
                          5 == 5;
                          5 != 5;
                          true == true;
                          true != false;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected: Vec<Stmt> = vec![
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Plus,
                Box::new(Expr::Integer(5)),
            )),
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Minus,
                Box::new(Expr::Integer(5)),
            )),
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Minus,
                Box::new(Expr::Integer(5)),
            )),
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Asterisk,
                Box::new(Expr::Integer(5)),
            )),
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Slash,
                Box::new(Expr::Integer(5)),
            )),
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Gt,
                Box::new(Expr::Integer(5)),
            )),
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Lt,
                Box::new(Expr::Integer(5)),
            )),
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Eq,
                Box::new(Expr::Integer(5)),
            )),
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::NotEq,
                Box::new(Expr::Integer(5)),
            )),
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Boolean(true)),
                Token::Eq,
                Box::new(Expr::Boolean(true)),
            )),
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Boolean(true)),
                Token::NotEq,
                Box::new(Expr::Boolean(false)),
            )),
        ];

        assert_eq!(program.statements, expected);
    }

    // #[test]
    // fn test_operator_precedence_parsing() {
    //     let tests: Vec<(&str, &str)> = vec![
    //         ("-a * b", "((-a) * b)"),
    //         ("!-a", "(!(-a))"),
    //         ("a + b + c", "((a + b) + c)"),
    //         ("a + b - c", "((a + b) - c)"),
    //         ("a * b * c", "((a * b) * c)"),
    //         ("a * b / c", "((a * b) / c)"),
    //         ("a + b / c", "(a + (b / c))"),
    //         ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
    //         ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
    //         ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
    //         ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
    //         (
    //             "3 + 4 * 5 == 3 * 1 + 4 * 5",
    //             "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
    //         ),
    //         ("true", "true"),
    //         ("false", "false"),
    //         ("3 > 5 == false", "((3 > 5) == false)"),
    //         ("3 < 5 == true", "((3 < 5) == true)"),
    //         ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
    //         ("(5 + 5) * 2", "((5 + 5) * 2)"),
    //         ("2 / (5 + 5)", "(2 / (5 + 5))"),
    //         ("-(5 + 5)", "(-(5 + 5))"),
    //         ("!(true == true)", "(!(true == true))"),
    //         ("(1 + 1)", "(1 + 1)"),
    //         ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
    //         (
    //             "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
    //             "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
    //         ),
    //         (
    //             "add(a + b + c * d / f + g)",
    //             "add((((a + b) + ((c * d) / f)) + g))",
    //         ),
    //         (
    //             "a * [1, 2, 3, 4][b * c] * d",
    //             "((a * ([1, 2, 3, 4][(b * c)])) * d)",
    //         ),
    //         (
    //             "add(a * b[2], b[1], 2 * [1, 2][1])",
    //             "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
    //         ),
    //     ];
    //
    //     for (input, expected) in tests {
    //         let lexer = Lexer::new(input.to_string());
    //         let mut parser = Parser::new(lexer);
    //         let program = parser.parse_program();
    //         parser.check_parser_errors();
    //         assert_eq!(program.to_string(), expected.to_string());
    //     }
    // }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::If(
            Box::new(Expr::Infix(
                Box::new(Expr::Identifier("x".to_string())),
                Token::Lt,
                Box::new(Expr::Identifier("y".to_string())),
            )),
            BlockStmt {
                statements: vec![Stmt::Expression(Expr::Identifier("x".to_string()))],
            },
            None,
        ))];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::If(
            Box::new(Expr::Infix(
                Box::new(Expr::Identifier("x".to_string())),
                Token::Lt,
                Box::new(Expr::Identifier("y".to_string())),
            )),
            BlockStmt {
                statements: vec![Stmt::Expression(Expr::Identifier("x".to_string()))],
            },
            Some(BlockStmt {
                statements: vec![Stmt::Expression(Expr::Identifier("y".to_string()))],
            }),
        ))];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::Function(
            vec![
                Expr::Identifier("x".to_string()),
                Expr::Identifier("y".to_string()),
            ],
            BlockStmt {
                statements: vec![Stmt::Expression(Expr::Infix(
                    Box::new(Expr::Identifier("x".to_string())),
                    Token::Plus,
                    Box::new(Expr::Identifier("y".to_string())),
                ))],
            },
        ))];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_function_parameter_parsing() {
        let input = "fn() {};
                          fn(x) {};
                          fn(x, y, z) {};";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected: Vec<Stmt> = vec![
            Stmt::Expression(Expr::Function(vec![], BlockStmt { statements: vec![] })),
            Stmt::Expression(Expr::Function(
                vec![Expr::Identifier("x".to_string())],
                BlockStmt { statements: vec![] },
            )),
            Stmt::Expression(Expr::Function(
                vec![
                    Expr::Identifier("x".to_string()),
                    Expr::Identifier("y".to_string()),
                    Expr::Identifier("z".to_string()),
                ],
                BlockStmt { statements: vec![] },
            )),
        ];

        assert_eq!(program.statements, expected);
    }

    // #[test]
    // fn test_call_expression_parsing() {
    //     let input = "add(1, 2 * 3, 4 + 5);";
    //
    //     let lexer = Lexer::new(input.to_string());
    //     let mut parser = Parser::new(lexer);
    //     let program = parser.parse_program();
    //     parser.check_parser_errors();
    //
    //     let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::Call(
    //         Box::new(Expr::Identifier("add".to_string())),
    //         vec![
    //             Expr::Integer(1),
    //             Expr::Infix(
    //                 Box::new(Expr::Integer(2)),
    //                 Token::Asterisk,
    //                 Box::new(Expr::Integer(3)),
    //             ),
    //             Expr::Infix(
    //                 Box::new(Expr::Integer(4)),
    //                 Token::Plus,
    //                 Box::new(Expr::Integer(5)),
    //             ),
    //         ],
    //     ))];
    //
    //     assert_eq!(program.statements, expected);
    // }
}
