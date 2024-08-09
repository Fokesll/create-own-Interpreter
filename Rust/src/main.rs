#![allow(dead_code)]
#![allow(unused_variables)]

use core::f64;
use core::fmt;
use core::panic;
use std::cmp;
use std::cmp::Ordering;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::ops;
use std::process::ExitCode;
use std::usize;

const KEYWORDS: &[&str] = &[
    "and", "class", "else", "false", "for", "fun", "if", "nil", "or", "print", "return", "super",
    "this", "true", "var", "while",
];

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[allow(non_camel_case_types)]
enum TokenType {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    STAR,
    DOT,
    COMMA,
    PLUS,
    MINUS,
    SEMICOLON,
    EQUAL,
    EQUAL_EQUAL,
    BANG_EQUAL,
    BANG,
    LESS,
    LESS_EQUAL,
    GREATER,
    GREATER_EQUAL,
    SLASH,
    STRING,
    NUMBER,
    IDENTIFIER,
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
}

fn str_to_type(s: &str) -> TokenType {
    match s {
        "and" => TokenType::AND,
        "class" => TokenType::CLASS,
        "else" => TokenType::ELSE,
        "false" => TokenType::FALSE,
        "for" => TokenType::FOR,
        "fun" => TokenType::FUN,
        "if" => TokenType::IF,
        "nil" => TokenType::NIL,
        "or" => TokenType::OR,
        "print" => TokenType::PRINT,
        "return" => TokenType::RETURN,
        "super" => TokenType::SUPER,
        "this" => TokenType::THIS,
        "true" => TokenType::TRUE,
        "var" => TokenType::VAR,
        "while" => TokenType::WHILE,
        _ => TokenType::NIL,
    }
}

#[derive(Clone, Debug)]
struct Token {
    token_type: TokenType,
    literal: String,
    content: String,
    line_no: usize,
}

impl Token {
    pub fn new(token_type: TokenType, line: usize) -> Self {
        let literal_ = match token_type {
            TokenType::LEFT_PAREN => String::from("("),
            TokenType::RIGHT_PAREN => String::from(")"),
            TokenType::LEFT_BRACE => String::from("{"),
            TokenType::RIGHT_BRACE => String::from("}"),
            TokenType::STAR => String::from("*"),
            TokenType::DOT => String::from("."),
            TokenType::COMMA => String::from(","),
            TokenType::PLUS => String::from("+"),
            TokenType::MINUS => String::from("-"),
            TokenType::SEMICOLON => String::from(";"),
            TokenType::EQUAL_EQUAL => String::from("=="),
            TokenType::EQUAL => String::from("="),
            TokenType::BANG_EQUAL => String::from("!="),
            TokenType::BANG => String::from("!"),
            TokenType::LESS => String::from("<"),
            TokenType::LESS_EQUAL => String::from("<="),
            TokenType::GREATER => String::from(">"),
            TokenType::GREATER_EQUAL => String::from(">="),
            TokenType::SLASH => String::from("/"),
            TokenType::AND => String::from("and"),
            TokenType::CLASS => String::from("class"),
            TokenType::ELSE => String::from("else"),
            TokenType::FALSE => String::from("false"),
            TokenType::FOR => String::from("for"),
            TokenType::FUN => String::from("fun"),
            TokenType::IF => String::from("if"),
            TokenType::NIL => String::from("nil"),
            TokenType::OR => String::from("or"),
            TokenType::PRINT => String::from("print"),
            TokenType::RETURN => String::from("return"),
            TokenType::SUPER => String::from("super"),
            TokenType::THIS => String::from("this"),
            TokenType::TRUE => String::from("true"),
            TokenType::VAR => String::from("var"),
            TokenType::WHILE => String::from("while"),
            _ => String::from(""),
        };
        Self {
            token_type: token_type,
            literal: literal_,
            line_no: line,
            content: "null".to_string(),
        }
    }

    pub fn new_with_content(token_type: TokenType, line: usize, contents: String) -> Self {
        Self {
            token_type: token_type,
            literal: "".to_string(),
            line_no: line,
            content: contents,
        }
    }
    pub fn new_with_literal(token_type: TokenType, line: usize, l: String) -> Self {
        if token_type == TokenType::NUMBER {
            let mut content_ = l.clone();
            if content_.chars().last().expect("blah").is_numeric() && !content_.contains(".") {
                content_.push('.');
                content_.push('0');
            }
            if content_.ends_with(".00") {
                content_.pop();
            }
            match content_.find(".") {
                Some(p) => {
                    if content_.ends_with("0") && p < content_.len() - 2 {
                        content_.pop();
                    }
                }
                None => {}
            }
            return Self {
                token_type: token_type,
                literal: l,
                line_no: line,
                content: content_,
            };
        }
        if token_type == TokenType::STRING {
            return Self {
                token_type: token_type,
                literal: format!("\"{}\"", l),
                line_no: line,
                content: l,
            };
        }
        Self {
            token_type: token_type,
            literal: l,
            line_no: line,
            content: "null".to_string(),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {} {}", self.token_type, self.literal, self.content)
    }
}

struct Tokenizer {
    chars: Vec<char>,
    cur_char_pos: usize,
}

impl Tokenizer {
    pub fn new(src: String) -> Self {
        Self {
            chars: src.chars().collect(),
            cur_char_pos: 0,
        }
    }

    pub fn advance(&mut self) -> char {
        let now_char = self.chars.get(self.cur_char_pos);
        self.cur_char_pos += 1;

        match now_char {
            Some(x) => x.to_owned(),
            None => '\0',
        }
    }

    pub fn peek_curr_char(&self) -> char {
        let now_char = self.chars.get(self.cur_char_pos);
        match now_char {
            Some(x) => x.to_owned(),
            None => '\0',
        }
    }

    pub fn peek_next_char(&self) -> char {
        let nc = self.chars.get(self.cur_char_pos + 1);
        match nc {
            Some(x) => x.to_owned(),
            None => '\0',
        }
    }
    fn put_back(&mut self) {
        self.cur_char_pos -= 1
    }
}

fn tokenize(src: String) -> (Vec<Token>, u8) {
    let mut line_number: usize = 1;
    //let errors: Vec<(usize, char)> = Vec::new();
    let mut tokens: Vec<Token> = Vec::new();
    let mut err_code: u8 = 0;
    let mut tokenizer = Tokenizer::new(src);
    loop {
        let c = tokenizer.advance();
        if c.is_alphanumeric() || c == '_' {
            if c.is_numeric() {
                // NUMBER
                let mut num_string = String::from("");
                num_string.push(c);
                loop {
                    let ch = tokenizer.advance();
                    let nc = tokenizer.peek_next_char();
                    if ch.is_numeric()
                        || (ch == '.' && !num_string.contains(".") && nc.is_numeric())
                    {
                        num_string.push(ch);
                    } else {
                        tokenizer.put_back();
                        break;
                    }
                }

                tokens.push(Token::new_with_literal(TokenType::NUMBER, 0, num_string))
            } else {
                // IDENTIFIER
                let mut ident = String::from("");
                ident.push(c);
                loop {
                    let c = tokenizer.advance();
                    if c.is_alphanumeric() || c == '_' {
                        ident.push(c);
                    } else {
                        tokenizer.put_back();
                        break;
                    }
                }
                if KEYWORDS.contains(&ident.as_str()) {
                    tokens.push(Token::new_with_literal(
                        str_to_type(ident.as_str()),
                        0,
                        ident,
                    ))
                } else {
                    tokens.push(Token::new_with_literal(TokenType::IDENTIFIER, 0, ident));
                }
            }
        } else {
            match c {
                '(' => tokens.push(Token::new(TokenType::LEFT_PAREN, 0)),
                ')' => tokens.push(Token::new(TokenType::RIGHT_PAREN, 0)),
                '{' => tokens.push(Token::new(TokenType::LEFT_BRACE, 0)),
                '}' => tokens.push(Token::new(TokenType::RIGHT_BRACE, 0)),
                '*' => tokens.push(Token::new(TokenType::STAR, 0)),
                '.' => tokens.push(Token::new(TokenType::DOT, 0)),
                ',' => tokens.push(Token::new(TokenType::COMMA, 0)),
                '+' => tokens.push(Token::new(TokenType::PLUS, 0)),
                '-' => tokens.push(Token::new(TokenType::MINUS, 0)),
                ';' => tokens.push(Token::new(TokenType::SEMICOLON, 0)),
                '=' => {
                    if tokenizer.peek_curr_char() == '=' {
                        tokenizer.advance();
                        tokens.push(Token::new(TokenType::EQUAL_EQUAL, 0));
                    } else {
                        tokens.push(Token::new(TokenType::EQUAL, 0))
                    }
                }
                '!' => {
                    if tokenizer.peek_curr_char() == '=' {
                        tokenizer.advance();
                        tokens.push(Token::new(TokenType::BANG_EQUAL, 0));
                    } else {
                        tokens.push(Token::new(TokenType::BANG, 0))
                    }
                }
                '<' => {
                    if tokenizer.peek_curr_char() == '=' {
                        tokenizer.advance();
                        tokens.push(Token::new(TokenType::LESS_EQUAL, 0));
                    } else {
                        tokens.push(Token::new(TokenType::LESS, 0))
                    }
                }
                '>' => {
                    if tokenizer.peek_curr_char() == '=' {
                        tokenizer.advance();
                        tokens.push(Token::new(TokenType::GREATER_EQUAL, 0));
                    } else {
                        tokens.push(Token::new(TokenType::GREATER, 0))
                    }
                }
                '/' => {
                    if tokenizer.peek_curr_char() == '/' {
                        // Comment
                        loop {
                            let got_char = tokenizer.advance();
                            if got_char == '\n' {
                                line_number += 1;
                                break;
                            } else if got_char == '\0' {
                                break;
                            }
                        }
                    } else {
                        tokens.push(Token::new(TokenType::SLASH, 0))
                    }
                }
                '"' => {
                    let mut string_content = String::from("");
                    let mut should_push_token = true;
                    loop {
                        let ch = tokenizer.advance();
                        if ch == '\0' {
                            writeln!(
                                io::stderr(),
                                "[line {}] Error: Unterminated string.",
                                line_number,
                            )
                            .unwrap();
                            should_push_token = false;
                            err_code = 65;
                            break;
                        }
                        if ch == '"' {
                            break;
                        }
                        string_content.push(ch);
                    }
                    if should_push_token {
                        tokens.push(Token::new_with_literal(
                            TokenType::STRING,
                            0,
                            string_content,
                        ))
                    }
                }
                '\n' => line_number += 1,
                '\0' => break,
                '\t' | ' ' => {}
                _ => {
                    //errors.push((line_number, c));
                    writeln!(
                        io::stderr(),
                        "[line {}] Error: Unexpected character: {}",
                        line_number,
                        c
                    )
                    .unwrap();
                    err_code = 65;
                }
            }
        }
    }
    (tokens, err_code)
}

struct Parser {
    tokens: Vec<Token>,
    cur_tok_index: usize,
    errors: Vec<String>,
    err_code: u8,
}

fn expr_as_str(e: Box<Expr>) -> String {
    match *e {
        Expr::Binary(l, o, r) => {
            format!("({} {} {})", o.literal, expr_as_str(l), expr_as_str(r))
        }
        Expr::Literal(l) => {
            format!("{}", l.replace("\"", ""))
        }
        Expr::Unary(op, r) => {
            format!("({} {})", op.literal, expr_as_str(r))
        }
        Expr::Grouping(g) => {
            format!("(group {})", expr_as_str(g))
        }
    }
}

enum LiteralType {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
}

impl ops::Mul<LiteralType> for LiteralType {
    type Output = LiteralType;

    fn mul(self, _rhs: LiteralType) -> LiteralType {
        match self {
            LiteralType::Number(n) => match _rhs {
                LiteralType::Number(_rhs_n) => return LiteralType::Number(n * _rhs_n),
                _ => panic!("Trying to multiply with not a number"),
            },
            _ => panic!("Trying to multiply with not a number"),
        }
    }
}

impl ops::Div<LiteralType> for LiteralType {
    type Output = LiteralType;

    fn div(self, _rhs: LiteralType) -> LiteralType {
        match self {
            LiteralType::Number(n) => match _rhs {
                LiteralType::Number(_rhs_n) => return LiteralType::Number(n / _rhs_n),
                _ => panic!("Trying to divide with not a number"),
            },
            _ => panic!("Trying to divide with not a number"),
        }
    }
}

impl ops::Add<LiteralType> for LiteralType {
    type Output = LiteralType;

    fn add(self, _rhs: LiteralType) -> LiteralType {
        match (self, _rhs) {
            (LiteralType::Number(l), LiteralType::Number(r)) => return LiteralType::Number(l + r),
            (LiteralType::String(l), LiteralType::String(r)) => {
                return LiteralType::String(format!(
                    "{}{}",
                    l.replace("\"", ""),
                    r.replace("\"", "")
                ))
            }
            (LiteralType::String(l), LiteralType::Number(r)) => {
                return LiteralType::String(format!("{}{}", l.replace("\"", ""), r))
            }
            (_, _) => panic!("Fuck"),
        }
    }
}

impl ops::Sub<LiteralType> for LiteralType {
    type Output = LiteralType;

    fn sub(self, _rhs: LiteralType) -> LiteralType {
        match self {
            LiteralType::Number(n) => match _rhs {
                LiteralType::Number(_rhs_n) => return LiteralType::Number(n - _rhs_n),
                _ => panic!("Trying to subtract with not a number"),
            },
            _ => panic!("Trying to subtract with not a number"),
        }
    }
}

impl PartialOrd for LiteralType {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        match (self, other) {
            (LiteralType::Number(l), LiteralType::Number(r)) => {
                if l < r {
                    Some(Ordering::Less)
                } else if l > r {
                    Some(Ordering::Greater)
                } else {
                    Some(Ordering::Equal)
                }
            }
            (_, _) => None,
        }
    }

    fn gt(&self, other: &Self) -> bool {
        match (self, other) {
            (LiteralType::Number(l), LiteralType::Number(r)) => return l > r,
            (_, _) => return false,
        }
    }

    fn ge(&self, other: &Self) -> bool {
        match (self, other) {
            (LiteralType::Number(l), LiteralType::Number(r)) => return l >= r,
            (_, _) => return false,
        }
    }

    fn lt(&self, other: &Self) -> bool {
        match (self, other) {
            (LiteralType::Number(l), LiteralType::Number(r)) => return l < r,
            (_, _) => return false,
        }
    }

    fn le(&self, other: &Self) -> bool {
        match (self, other) {
            (LiteralType::Number(l), LiteralType::Number(r)) => return l <= r,
            (_, _) => return false,
        }
    }
}

impl PartialEq for LiteralType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LiteralType::String(l), LiteralType::String(r)) => l == r,
            (LiteralType::Number(l), LiteralType::Number(r)) => l == r,
            (LiteralType::Bool(l), LiteralType::Bool(r)) => l == r,
            (_, _) => false,
        }
    }
}

impl fmt::Display for LiteralType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralType::Number(n) => write!(f, "{}", n),
            LiteralType::String(s) => write!(f, "{}", s.replace("\"", "")),
            LiteralType::Bool(b) => write!(f, "{}", b),
            LiteralType::Nil => write!(f, "nil"),
        }
    }
}

enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Literal(String),
    Unary(Token, Box<Expr>),
    Grouping(Box<Expr>),
}

impl Parser {
    fn new(token: Vec<Token>) -> Self {
        Self {
            tokens: token,
            cur_tok_index: 0,
            err_code: 0,
            errors: Vec::new(),
        }
    }

    fn previous(&self) -> Token {
        self.tokens
            .get(self.cur_tok_index - 1)
            .expect("Index for token out of range")
            .clone()
    }

    fn advance(&mut self) -> Token {
        if !self.at_end() {
            self.cur_tok_index += 1;
        }
        self.previous()
    }

    fn peek(&self) -> Token {
        self.tokens
            .get(self.cur_tok_index)
            .expect("Peek: Index of token out of range")
            .clone()
    }

    fn at_end(&self) -> bool {
        self.cur_tok_index >= self.tokens.len()
    }

    fn check(&self, token: TokenType) -> bool {
        if self.at_end() {
            return false;
        }

        self.peek().token_type == token
    }

    fn match_(&mut self, tokens: Vec<TokenType>) -> bool {
        for t in tokens {
            if self.check(t) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn consume(&mut self, token: TokenType) -> Result<TokenType, u8> {
        let consumed = self.advance();

        let token_type = consumed.token_type;
        if token == token_type {
            Ok(token)
        } else {
            self.errors.push(String::from("Unmatched Parentheses"));
            Err(65)
        }
    }

    fn primary(&mut self) -> Result<Box<Expr>, u8> {
        if self.match_(vec![TokenType::FALSE]) {
            return Ok(Box::new(Expr::Literal("false".to_string())));
        }
        if self.match_(vec![TokenType::TRUE]) {
            return Ok(Box::new(Expr::Literal("true".to_string())));
        }
        if self.match_(vec![TokenType::NIL]) {
            return Ok(Box::new(Expr::Literal("nil".to_string())));
        }
        if self.match_(vec![TokenType::NUMBER]) {
            return Ok(Box::new(Expr::Literal(self.previous().content)));
        }
        if self.match_(vec![TokenType::STRING]) {
            return Ok(Box::new(Expr::Literal(format!(
                "\"{}\"",
                self.previous().content
            ))));
        }

        if self.match_(vec![TokenType::LEFT_PAREN]) {
            let expr = match self.expression() {
                Ok(e) => e,
                Err(c) => return Err(c),
            };
            match self.consume(TokenType::RIGHT_PAREN) {
                Err(c) => return Err(c),
                Ok(_) => return Ok(Box::new(Expr::Grouping(expr))),
            }
        } else {
            self.errors.push(String::from("Unmatched Parentheses"));
            Err(65)
        }
    }

    fn unary(&mut self) -> Result<Box<Expr>, u8> {
        if self.match_(vec![TokenType::BANG, TokenType::MINUS]) {
            let operator = self.previous();
            let right = match self.unary() {
                Ok(u) => u,
                Err(c) => return Err(c),
            };
            return Ok(Box::new(Expr::Unary(operator, right)));
        }
        self.primary()
    }

    fn factor(&mut self) -> Result<Box<Expr>, u8> {
        let mut expr = match self.unary() {
            Ok(u) => u,
            Err(c) => return Err(c),
        };
        loop {
            if !self.match_(vec![TokenType::SLASH, TokenType::STAR]) {
                break;
            }
            let operator = self.previous();
            let right = match self.unary() {
                Ok(u) => u,
                Err(c) => return Err(c),
            };
            expr = Box::new(Expr::Binary(expr, operator, right));
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Box<Expr>, u8> {
        let mut expr = match self.factor() {
            Ok(f) => f,
            Err(c) => return Err(c),
        };
        loop {
            if !self.match_(vec![TokenType::MINUS, TokenType::PLUS]) {
                break;
            }
            let operator = self.previous();
            let right = match self.factor() {
                Ok(f) => f,
                Err(c) => return Err(c),
            };
            expr = Box::new(Expr::Binary(expr, operator, right));
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Box<Expr>, u8> {
        let mut expr = match self.term() {
            Ok(t) => t,
            Err(c) => return Err(c),
        };
        loop {
            if !self.match_(vec![
                TokenType::GREATER,
                TokenType::GREATER_EQUAL,
                TokenType::LESS,
                TokenType::LESS_EQUAL,
            ]) {
                break;
            }
            let operator = self.previous();
            let right = match self.term() {
                Ok(t) => t,
                Err(c) => return Err(c),
            };
            expr = Box::new(Expr::Binary(expr, operator, right));
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Box<Expr>, u8> {
        let mut expr = match self.comparison() {
            Ok(c) => c,
            Err(c) => return Err(c),
        };
        loop {
            if !self.match_(vec![TokenType::BANG_EQUAL, TokenType::EQUAL_EQUAL]) {
                break;
            }
            let operator = self.previous();
            let right = match self.comparison() {
                Ok(c) => c,
                Err(c) => return Err(c),
            };
            expr = Box::new(Expr::Binary(expr, operator, right));
        }
        Ok(expr)
    }

    fn expression(&mut self) -> Result<Box<Expr>, u8> {
        self.equality()
    }

    pub fn parse(&mut self) -> Result<Box<Expr>, u8> {
        let expr = self.expression();
        expr
    }
}

struct Evaluator {
    errors: Vec<String>,
   // ast: Box<Expr>,
}

impl Evaluator {
    fn new() -> Self {
        Self {
            // ast: ast_,
            errors: Vec::new(),
        }
    }

    fn evaluate(&mut self, ast: Box<Expr>) -> Result<LiteralType, u8> {
        let res = self.eval_expr(&ast);
        return res;
    }

    fn eval_literal(&mut self, e: &Box<Expr>) -> LiteralType {
        match &(**e) {
            Expr::Literal(v) => {
                if v == "nil" {
                    return LiteralType::Nil;
                }

                let n = v.parse::<f64>();
                if n.is_ok() {
                    return LiteralType::Number(n.unwrap());
                }

                let b = v.parse::<bool>();
                if b.is_ok() {
                    return LiteralType::Bool(b.unwrap());
                } else {
                    return LiteralType::String((*v).clone().replace("\"", ""));
                }
            }
            _ => return LiteralType::Nil,
        }
    }

    fn eval_grouping(&mut self, e: &Box<Expr>) -> Result<LiteralType, u8> {
        match &(**e) {
            Expr::Grouping(g) => return self.eval_expr(g),
            _ => return Err(65),
        }
    }

    fn eval_unary(&mut self, e: &Box<Expr>) -> Result<LiteralType, u8> {
        match &(**e) {
            Expr::Unary(op, e) => {
                let evaled = self.eval_expr(e);
                if evaled.is_err() {
                    return evaled;
                }
                let res = match op.token_type {
                    TokenType::MINUS => match evaled.unwrap() {
                        LiteralType::Number(n) => {
                            let res = n * -1.0;
                            let l = LiteralType::Number(res);
                            Ok(l)
                        }
                        _ => {
                            self.errors.push(String::from("Operand must be a number."));
                            Err(70)
                        }
                    },
                    TokenType::BANG => match evaled.unwrap() {
                        LiteralType::Number(_) => Ok(LiteralType::Bool(false)),
                        LiteralType::Bool(b) => Ok(LiteralType::Bool(!b)),
                        LiteralType::Nil => Ok(LiteralType::Bool(!false)),
                        _ => Err(65),
                    },
                    _ => Err(65),
                };
                return res;
            }
            _ => return Err(65),
        }
    }

    fn eval_binary(&mut self, e: &Box<Expr>) -> Result<LiteralType, u8> {
        match &(**e) {
            Expr::Binary(left, op, right) => {
                let left_res = match self.eval_expr(left) {
                    Ok(r) => r,
                    Err(c) => return Err(c),
                };
                let right_res = match self.eval_expr(right) {
                    Ok(r) => r,
                    Err(c) => return Err(c),
                };
                match op.token_type {
                    TokenType::STAR => return Ok(left_res * right_res),
                    TokenType::SLASH => return Ok(left_res / right_res),
                    TokenType::PLUS => return Ok(left_res + right_res),
                    TokenType::MINUS => return Ok(left_res - right_res),
                    TokenType::GREATER => return Ok(LiteralType::Bool(left_res > right_res)),
                    TokenType::GREATER_EQUAL => {
                        return Ok(LiteralType::Bool(left_res >= right_res))
                    }
                    TokenType::LESS => return Ok(LiteralType::Bool(left_res < right_res)),
                    TokenType::LESS_EQUAL => return Ok(LiteralType::Bool(left_res <= right_res)),
                    TokenType::EQUAL_EQUAL => return Ok(LiteralType::Bool(left_res == right_res)),
                    TokenType::BANG_EQUAL => return Ok(LiteralType::Bool(left_res != right_res)),
                    _ => todo!(),
                }
            }
            _ => return Err(65),
        }
    }

    fn print_literal(&self, l: &LiteralType) -> String {
        match l {
            LiteralType::String(v) => format!("{}", v),
            LiteralType::Bool(v) => format!("{}", v),
            LiteralType::Number(v) => format!("{}", v),
            LiteralType::Nil => format!("Lox: nil"),
        }
    }

    fn eval_expr(&mut self, e: &Box<Expr>) -> Result<LiteralType, u8> {
        let _ = match &(**e) {
            Expr::Literal(_) => {
                let val = self.eval_literal(e);
                //println!("{}", self.print_literal(&val));
                return Ok(val);
            }
            Expr::Grouping(_) => {
                let val = self.eval_grouping(e);
                return val;
            }
            Expr::Unary(_, _) => {
                let val = self.eval_unary(e);
                return val;
            }
            Expr::Binary(_, _, _) => {
                let val = self.eval_binary(e);
                return val;
            }
        };
    }
}

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return ExitCode::SUCCESS;
    }
    let command = &args[1];
    let filename = &args[2];
    let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
        String::new()
    });
    match command.as_str() {
        "tokenize" => {
            let token_err_code = tokenize(file_contents);
            for t in &token_err_code.0 {
                println!("{}", format!("{}", t))
            }
            println!("EOF  null");
            ExitCode::from(token_err_code.1)
        }
        "evaluate" => {
            let token_err_code = tokenize(file_contents);
            let tokens = token_err_code.0;
            let mut parser = Parser::new(tokens);
            let ast = parser.parse();
            let mut evaluator = Evaluator::new();
            let res = evaluator.evaluate(ast.expect("sad"));
            match res {
                Ok(r) => {
                    println!("{}", r);
                    ExitCode::SUCCESS
                }
                Err(c) => {
                    for e in evaluator.errors {
                        writeln!(io::stderr(), "{}", e).unwrap();
                    }
                    ExitCode::from(c)
                }
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            ExitCode::SUCCESS
        }
    }
}
