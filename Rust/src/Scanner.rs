use std::process::exit;

static mut EXIT_CODE:i32 = 0;

pub fn tokenize(input: &str) {
    let mut line_number: i32 = 1;
    let mut chars = input.chars().peekable();
    while let Some(char) = chars.next() {
        match char {
            '(' => println!("LEFT_PAREN ( null"),
            ')' => println!("RIGHT_PAREN ) null"),
            '{' => println!("LEFT_BRACE {{ null"),
            '}' => println!("RIGHT_BRACE }} null"),
            '*' => println!("STAR * null"),
            '.' => println!("DOT . null"),
            ',' => println!("COMMA , null"),
            ';' => println!("SEMICOLON ; null"),
            '+' => println!("PLUS + null"),
            '-' => println!("MINUS - null"),
            '!' => {
                if chars.peek() == Some(&'=') {
                    chars.next();
                    println!("BANG_EQUAL != null");
                } else {
                    println!("BANG ! null")
                }
            }
            '=' => {
                if chars.peek() == Some(&'=') {
                    chars.next();
                    println!("EQUAL_EQUAL == null");
                } else {
                    println!("EQUAL = null")
                }
            }
            '<' => {
                if chars.peek() == Some(&'=') {
                    chars.next();
                    println!("LESS_EQUAL <= null");
                } else {
                    println!("LESS < null")
                }
            }
            '>' => {
                if chars.peek() == Some(&'=') {
                    chars.next();
                    println!("GREATER_EQUAL >= null");
                } else {
                    println!("GREATER > null")
                }
            }
            '"' => {
                let mut str = String::new();
                while let Some(c) = chars.next() {
                    if c == '"' {
                        println!("STRING \"{}\" {}", str, str);
                        str.clear();
                        break;
                    } else if c == '\n' || chars.peek() == None {
                        eprintln!("[line {}] Error: Unterminated string.", line_number);
                        unsafe { EXIT_CODE = 65 };
                        break;
                    } else {
                        str.push(c);
                    }
                }
            }
            d if d.is_digit(10) => {
                let mut str = String::from(d);

                let mut comma_detected = false;
                while let Some(t) = chars.peek() {
                    if t.is_digit(10) {
                        str.push(*t);
                        chars.next();
                    } else if *t == '.' && !comma_detected {
                        comma_detected = true;
                        str.push(*t);
                        chars.next();
                    } else {
                        break;
                    }
                }

                if str.ends_with('.') {
                    str.push('0');
                    check_number_out_put(&str);
                    println!("DOT . null");
                } else {
                    check_number_out_put(&str);
                }
            }
            s if s.is_alphabetic() || s == '_' => {
                let mut str: String = String::from(s);
                while let Some(t) = chars.peek() {
                    if t.is_alphabetic() || t.is_digit(10) || *t == '_' {
                        str.push(*t);
                        chars.next();
                    } else {
                        break;
                    }
                }
                let res = check_identifier(&str);

                if res.len() > 0 {
                    println!("{} {} null", res, str);
                } else {
                    println!("IDENTIFIER {} null", str);
                }
            }
            '/' => {
                if chars.peek() == Some(&'/') {
                    while let Some(c) = chars.next() {
                        if c == '\n' {
                            break;
                        }
                    }
                    line_number += 1;
                } else {
                    println!("SLASH / null")
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => line_number += 1,
            _ => {
                eprintln!(
                    "[line {}] Error: Unexpected character: {}",
                    line_number, char
                );
                unsafe { EXIT_CODE = 65 };
            }
        }
    }
    println!("EOF  null");

    if unsafe { EXIT_CODE } != 0 {
        exit(unsafe { EXIT_CODE });
    }
}

fn check_number_out_put(input: &str) {
    if input.ends_with(".0") {
        println!("NUMBER {} {}", input.replace(".0", ""), input)
    } else if input.ends_with(".00") {
        println!("NUMBER {} {}", input, input.replace(".00", ".0"))
    } else if !input.contains(".") {
        println!("NUMBER {} {}.0", input, input)
    } else {
        println!("NUMBER {} {}", input, input)
    }
}

fn check_identifier(input: &str) -> String {
    let mut result = String::new();
    match input {
        "or" => result = "OR".to_string(),
        "and" => result = "AND".to_string(),
        "class" => result = "NOT".to_string(),
        "else" => result = "ELSE".to_string(),
        "if" => result = "IF".to_string(),
        "true" => result = "TRUE".to_string(),
        "false" => result = "FALSE".to_string(),
        "while" => result = "WHILE".to_string(),
        "for" => result = "FOR".to_string(),
        "fun" => result = "FUN".to_string(),
        "print" => result = "PRINT".to_string(),
        "return" => result = "RETURN".to_string(),
        "this" => result = "THIS".to_string(),
        "var" => result = "VAR".to_string(),
        "super" => result = "SUPER".to_string(),
        "nil" => result = "NIL".to_string(),
        _ => (),
    }
    return result;
}
