use crate::token::Token;

pub struct Lexer {
    input: Vec<char>,
    cur_pos: usize,
    next_pos: usize,
    cur_char: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Lexer {
            input: input.chars().collect(), //relearn
            cur_pos: 0,
            next_pos: 0,
            cur_char: '\0',
        };

        l.read_char();
        l
    }

    fn skip_whitespace(&mut self) {
        while self.cur_char.is_whitespace() {
            self.read_char()
        }
    }

    fn read_char(&mut self) {
        self.cur_char = match self.input.get(self.next_pos) {
            Some(&c) => c,
            None => '\0',
        };

        self.cur_pos = self.next_pos;
        self.next_pos += 1;
    }

    fn peek_char(&self) -> char {
        return match self.input.get(self.next_pos) {
            Some(&c) => c,
            None => '\0',
        };
    }

    fn is_letter(&self) -> bool {
        return self.cur_char.is_alphabetic() || self.cur_char == '_';
    }
    fn is_digit(&self) -> bool {
        return self.cur_char.is_digit(10);
    }

    fn read_identifier(&mut self) -> String {
        // let abc123 = 123
        let start = self.cur_pos;
        while self.is_letter() {
            self.read_char();
        }
        self.input[start..self.cur_pos].iter().collect()
    }

    fn read_string(&mut self) -> String {
        // skip the quote
        let start = self.cur_pos + 1;

        loop {
            self.read_char();
            if self.cur_char == '\'' || self.cur_char == '"' || self.cur_char == '\0' {
                break;
            }
        }
        self.input[start..self.cur_pos].iter().collect()
    }

    fn read_number(&mut self) -> u32 {
        // let abc123 = 123
        let start = self.cur_pos;
        while self.is_digit() {
            self.read_char();
        }
        self.input[start..self.cur_pos]
            .iter()
            .collect::<String>()
            .parse()
            .unwrap()
    }

    fn lookup_identifier(&self, s: String) -> Token {
        return match s.as_str() {
            "fn" => Token::Function,
            "let" => Token::Let,
            "if" => Token::If,
            "return" => Token::Return,
            "true" => Token::True,
            "false" => Token::False,
            "else" => Token::Else,
            _ => Token::Ident(s),
        };
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let rval = match self.cur_char {
            '=' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    Token::Eq
                }
                _ => Token::Assign,
            },
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '<' => Token::Lt,
            '>' => Token::Gt,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            ':' => Token::Colon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            '-' => Token::Minus,
            '+' => Token::Plus,
            '!' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    Token::NotEq
                }

                _ => Token::Bang,
            },
            '\0' => Token::Eof,
            c if c.is_alphabetic() => {
                let identifier = self.read_identifier();
                return self.lookup_identifier(identifier);
            }
            c if c.is_digit(10) => return Token::Int(self.read_number()),

            c if c == '\'' || c == '\"' => Token::Str(self.read_string()),

            _ => {
                println!("{:?}", self.cur_char);
                Token::Illegal
            }
        };

        self.read_char();

        rval
    }
}

// zip => Lexer implements Iterator
impl Iterator for Lexer {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next_token())
    }
}

#[cfg(test)]
mod test_lexer {
    use super::*;

    fn input_produces_tokens(input: &str, tokens: Vec<Token>) {
        let lexer = Lexer::new(input.to_string());

        for (tok, expected) in lexer.zip(tokens.iter()) {
            // println!("{:?}, {:?}", tok, expected);
            assert_eq!(*expected, tok);
        }
    }

    #[test]
    fn test_next_token_0() {
        let input = r"let five = 5;
                           let ten = 10;

                           let add = fn(x, y) {
                               x + y;
                           };";

        let tokens = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Eof,
        ];

        input_produces_tokens(input, tokens);
    }

    #[test]
    fn test_next_token_1() {
        let input = r"let result = add(five, ten);
                           !-/*5;
                           5 < 10 > 5;";

        let tokens = vec![
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Gt,
            Token::Int(5),
            Token::Semicolon,
            Token::Eof,
        ];

        input_produces_tokens(input, tokens);
    }

    #[test]
    fn test_next_token_2() {
        let input = r"if (5 < 10) {
                               return true;
                           } else {
                               return false;
                           }";

        let tokens = vec![
            Token::If,
            Token::LParen,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Eof,
        ];

        input_produces_tokens(input, tokens);
    }

    #[test]
    fn test_next_token_3() {
        let input = r"10 == 10;
                           10 != 9;";

        let tokens = vec![
            Token::Int(10),
            Token::Eq,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::NotEq,
            Token::Int(9),
            Token::Semicolon,
            Token::Eof,
            Token::Eof,
        ];

        input_produces_tokens(input, tokens);
    }

    #[test]
    fn test_next_token_strs() {
        let input = "\"foobar\";
                          \"foo bar\";
                          \"\";
                          \"123\";";

        let tokens = vec![
            Token::Str("foobar".to_string()),
            Token::Semicolon,
            Token::Str("foo bar".to_string()),
            Token::Semicolon,
            Token::Str("".to_string()),
            Token::Semicolon,
            Token::Str("123".to_string()),
            Token::Semicolon,
            Token::Eof,
        ];

        input_produces_tokens(input, tokens);
    }

    #[test]
    fn test_next_token_arrays() {
        let input = "[1, 2];
                          [3, \"hi\"];";

        let tokens = vec![
            Token::LBracket,
            Token::Int(1),
            Token::Comma,
            Token::Int(2),
            Token::RBracket,
            Token::Semicolon,
            Token::LBracket,
            Token::Int(3),
            Token::Comma,
            Token::Str("hi".to_string()),
            Token::RBracket,
            Token::Semicolon,
            Token::Eof,
        ];

        input_produces_tokens(input, tokens);
    }

    #[test]
    fn test_next_token_hash() {
        let input = "{\"foo\": \"bar\"}";

        let tokens = vec![
            Token::LBrace,
            Token::Str("foo".to_string()),
            Token::Colon,
            Token::Str("bar".to_string()),
            Token::RBrace,
            Token::Eof,
        ];

        input_produces_tokens(input, tokens);
    }
}
