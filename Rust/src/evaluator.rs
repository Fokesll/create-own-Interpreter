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