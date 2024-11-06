use core::panic;
use std::collections::HashMap;
use std::io::Read;
use std::{env, process::exit};
use std::fs::File;
use anyhow::{bail, Result};

#[derive(Debug, PartialEq)]
enum Token<'a> {
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Colon,
    Comma,
    Str(&'a str),
    Number(Numeric),
    True,
    False,
    Null,
    Eof,
}

#[derive(Debug, PartialEq)]
enum Numeric {
    Float(f64),
    Integer(i64),
}

struct Lexer<'a> {
    json: &'a str,
}

impl<'a> Lexer<'a> {
    fn new(json: &'a str) -> Self {
        Self { 
            json,
        }
    }

    fn lex(&mut self) -> Result<Vec<Token>> {
        use Token::*;
        let mut tokens: Vec<Token> = vec![];
        loop {
            let mut chars = self.json.chars();
            match chars.next() {
                Some(c) => {
                    match c {
                        '{' => {
                            tokens.push(LeftBrace);
                            self.json = chars.as_str();
                        }
                        '}' => {
                            tokens.push(RightBrace);
                            self.json = chars.as_str();
                        }
                        '[' => {
                            tokens.push(LeftBracket);
                            self.json = chars.as_str();
                        }
                        ']' => {
                            tokens.push(RightBracket);
                            self.json = chars.as_str();
                        }
                        ':' => {
                            tokens.push(Colon);
                            self.json = chars.as_str();
                        }
                        ',' => {
                            tokens.push(Comma);
                            self.json = chars.as_str();
                        }
                        '"' => {
                            let s = self.lex_string()?;
                            tokens.push(Str(s))
                        }
                        '0'..='9' | '-' => {
                            let num = self.lex_number()?;
                            tokens.push(Number(num));
                        }
                        't' if self.json.starts_with("true") => {
                            tokens.push(True);
                            self.json = &self.json["true".len()..];
                        }
                        'f' if self.json.starts_with("false") => {
                            tokens.push(False);
                            self.json = &self.json["false".len()..];
                        }
                        'n' if self.json.starts_with("null") => {
                            tokens.push(Null);
                            self.json = &self.json["null".len()..];
                        }
                        ' ' | '\t' | '\r' | '\n' => {
                            self.json = chars.as_str();
                        }
                        c => bail!("Lexing failure on character {c}"),
                    }
                }
                None => {
                    tokens.push(Eof);
                    break;
                }
            }
        }
        Ok(tokens)
    }

    fn lex_string(&mut self) -> Result<&'a str> {
        let mut charindices = self.json.char_indices();
        let _ = charindices.next(); // skip first '"'
        let mut escape = false;
        let n;
        loop {
            match charindices.next() {
                None => bail!("Unexpected end of input in string literal"),
                Some((i, '"')) if !escape => {
                    n = i;
                    break;
                }
                Some((_, '\\')) => {
                    escape = true;
                }
                Some(_) => {
                    escape = false;
                }
            }
        }
        let result = &self.json[1..n];
        self.json = charindices.as_str();
        Ok(result)
    }

    fn lex_number(&mut self) -> Result<Numeric> {
        let mut charindices = self.json.char_indices().peekable();
        let mut n = 0;
        let mut floating_point = false;
        if matches!(charindices.peek(), Some((_, '-'))) {
            charindices.next();
        }
        if matches!(charindices.peek(), Some((_, '.')) | Some((_, 'e')) | Some((_, 'E'))) {
            bail!("Invalid numeric literal");
        }
        if matches!(charindices.peek(), Some((_, '0'))) {
            charindices.next().unwrap(); // safe from if condition
            if let Some((_, c)) = charindices.peek() {
                match c {
                    'e' | 'E' | '.' | ' ' | '\t' | '\r' | '\n' => {}
                    _ => bail!("Invalid numeric literal"),
                }
            }
        } 
        // read integer
        loop {
            match charindices.peek() {
                None => {
                    n = self.json.as_bytes().len();
                    break;
                }
                Some((i, c)) => {
                    match c {
                        '0'..='9' => {
                            charindices.next();
                        }
                        '.' | 'E' | 'e'  => {
                            break;
                        }
                        ' ' | '\t' | '\r' | '\n' => {
                            n = *i;
                            break;
                        }
                        _ => bail!("Invalid numeric literal"),
                    }
                }
            }
        }
        // read fraction
        if matches!(charindices.peek(), Some((_, '.'))) {
            floating_point = true;
            charindices.next();
            loop {
                match charindices.peek() {
                    None => {
                        n = self.json.as_bytes().len();
                        break;
                    }
                    Some((i, c)) => match c {
                        '0'..='9' => {
                            charindices.next();
                        }
                        'e' | 'E' => break,
                        ' ' | '\t' | '\r' | '\n' => {
                            n = *i;
                            break;
                        }
                        _ => bail!("Invalid numeric literal"),
                    }
                }
            }
        }
        // read exponent
        if matches!(charindices.peek(), Some((_, 'e')) | Some((_, 'E'))) {
            floating_point = true;
            charindices.next();
            if matches!(charindices.peek(), Some((_, '+')) | Some((_, '-'))) {
                charindices.next();
            }
            loop {
                match charindices.peek() {
                    None => {
                        n = self.json.as_bytes().len();
                        break;
                    }
                    Some((i, c)) => match c {
                        '0'..='9' => {
                            charindices.next();
                        }
                        ' ' | '\t' | '\r' | '\n' => {
                            n = *i;
                            break;
                        }
                        _ => bail!("Invalid numeric literal"),
                    }
                }
            }
        }
        let result = if floating_point {
            let res = self.json[0..n].parse::<f64>()?;
            Ok(Numeric::Float(res))
        } else {
            let res = self.json[0..n].parse::<i64>()?;
            Ok(Numeric::Integer(res))
        };
        self.json = &self.json[n..];
        result
    }
} 

enum JsonTree {
    Object(HashMap<String, JsonTree>),
    Array(Vec<JsonTree>),
}

struct Parser<'a> {
    index: usize,
    tokens: Vec<Token<'a>>,
}

impl<'a> Parser<'a> {
    fn new(tokens: Vec<Token<'a>>) -> Self {
        Self {
            index: 0,
            tokens
        }
    }

    fn consume(&mut self, target: Token) -> Result<(), String> {
        let token = match self.tokens.get(self.index) {
            None => panic!("Unexpected end of input"),
            Some(t) => t,
        };
        if *token != target {
            return Err(format!("Invalid JSON; expected token {:?} but found {:?}", target, token));
        }
        self.index += 1;
        Ok(())
    } 

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.index + 1)
    }

    fn parse(&mut self) -> Result<JsonTree, String> {
        self.value()
    }

    fn value(&mut self) -> Result<JsonTree, String> {
        todo!()
    }

    fn object(&mut self) -> Result<JsonTree, String> {
        self.consume(Token::LeftBrace);
        self.members();
        self.consume(Token::RightBrace);
        todo!()
    }

    fn members(&mut self) -> Result<JsonTree, String> {
        let elements: HashMap<String, JsonTree> = HashMap::new();
        loop {
            //let name = self.string();
            //self.consume(Token::Colon)?;
            //elements.insert(name, member);
            //if matches!(self.peek(), Some(Token::Comma)) {
            //    self.consume(Token::Comma);
            //} else {
            //    break;
            //}
        }
    }
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        print_usage();
        exit(-1);
    }
    for arg in &args[1..] {
        let mut file = match File::open(arg) {
            Ok(f) => f,
            Err(e) => { 
                println!("Encountered error opening file {arg}: {e}");
                exit(10);
            }
        };
        let mut contents = String::new();
        match file.read_to_string(&mut contents) {
            Ok(_) => {}
            Err(e) => {
                println!("Encountered error reading file {arg}: {e}");
                exit(11);
            }
        }

        let mut lexer = Lexer::new(&contents);
        let tokens = lexer.lex()?;
        let mut parser = Parser::new(tokens);
        match parser.parse() {
            Ok(_) => {}
            Err(e) => {
                println!("Error parsing {arg}: {e}");
                exit(1);
            }
        }
    }
    Ok(())
}

fn print_usage() {
    println!("Incorrect usage do better");
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_lexing() {
        let text = r#""test \"\b\n\r\" string""#;
        let mut lexer = Lexer::new(text);
        let result_vec = lexer.lex().unwrap();
        let result = &result_vec[0];
        assert_eq!(*result, Token::Str(r#"test \"\b\n\r\" string"#));
    }

    #[test]
    fn test_integer_lexing() {
        let text = "1234567890 -999 0";
        let mut lexer = Lexer::new(text);
        let result_vec = lexer.lex().unwrap();
        assert_eq!(result_vec[0], Token::Number(Numeric::Integer(1234567890)));
        assert_eq!(result_vec[1], Token::Number(Numeric::Integer(-999)));
        assert_eq!(result_vec[2], Token::Number(Numeric::Integer(0)));
    }

    #[test]
    fn test_fraction_lexing() {
        let text = "0.0 0.999 -100.0";
        let mut lexer = Lexer::new(text);
        let result_vec = lexer.lex().unwrap();
        assert_eq!(result_vec[0], Token::Number(Numeric::Float(0.0)));
        assert_eq!(result_vec[1], Token::Number(Numeric::Float(0.999)));
        assert_eq!(result_vec[2], Token::Number(Numeric::Float(-100.0)));
    }

    #[test]
    fn test_exponent_lexing() {
        let text = "1e0 -1.0e1 100.001e3";
        let mut lexer = Lexer::new(text);
        let result_vec = lexer.lex().unwrap();
        assert_eq!(result_vec[0], Token::Number(Numeric::Float(1.0)));
        assert_eq!(result_vec[1], Token::Number(Numeric::Float(-10.0)));
        assert_eq!(result_vec[2], Token::Number(Numeric::Float(100001.0)));
    }

    #[test]
    fn test_bad_integer() {
        let text = "010000";
        let mut lexer = Lexer::new(text);
        let result = lexer.lex();
        assert!(result.is_err());
    }

    #[test]
    fn test_bad_fractional() {
        let text = ".001";
        let mut lexer = Lexer::new(text);
        let result = lexer.lex();
        assert!(result.is_err());
    }

    #[test]
    fn test_bad_exponent() {
        let text = "E001";
        let mut lexer = Lexer::new(text);
        let result = lexer.lex();
        assert!(result.is_err());
    }

    #[test]
    fn test_keywords() {
        let text = "true false null";
        let mut lexer = Lexer::new(text);
        let result_vec = lexer.lex().unwrap();
        assert_eq!(result_vec[0], Token::True);
        assert_eq!(result_vec[1], Token::False);
        assert_eq!(result_vec[2], Token::Null);
    }

    //#[test]
    //fn test_empty_object() {
    //    let contents = "{}".to_string();
    //    let mut lexer = Lexer::new(&contents);
    //    let tokens = lexer.lex().unwrap();
    //    let mut parser = Parser::new(tokens);
    //    assert!(parser.parse().is_ok());
    //}
    //
    //#[test]
    //fn test_invalid_object() {
    //    let contents = "{".to_string();
    //    let mut lexer = Lexer::new(&contents);
    //    let tokens = lexer.lex().unwrap();
    //    let mut parser = Parser::new(tokens);
    //    assert!(parser.parse().is_err());
    //}
}
