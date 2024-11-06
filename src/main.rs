use std::collections::HashMap;
use std::io::Read;
use std::iter::Peekable;
use std::slice::Iter;
use std::{env, process::exit};
use std::fs::File;
use anyhow::{bail, Context, Result};

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
}

#[derive(Debug, PartialEq, Copy, Clone)]
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

#[derive(Debug)]
enum JsonTree<'a> {
    Object(HashMap<&'a str, JsonTree<'a>>),
    Array(Vec<JsonTree<'a>>),
    Atom(Token<'a>),
}

struct Parser {
}

impl<'a> Parser {
    fn new() -> Self {
        Self {
        }
    }

    fn consume(&self, iter: &mut Peekable<Iter<Token>>, target: Token) -> Result<()> {
        let token = match iter.next() {
            None => panic!("Unexpected end of input"),
            Some(t) => t,
        };
        if *token != target {
            bail!(format!("Invalid JSON; expected token {:?} but found {:?}", target, token));
        }
        Ok(())
    } 

    fn parse(&'a self, iter: &mut Peekable<Iter<Token<'a>>>) -> Result<Option<JsonTree>> {
        self.value(iter)
    }

    fn value(&'a self, iter: &mut Peekable<Iter<Token<'a>>>) -> Result<Option<JsonTree>> {
        match iter.peek() {
            Some(Token::LeftBrace) => {
                match self.object(iter) {
                    Ok(object) => Ok(Some(object)),
                    Err(e) => Err(e.context("In parsing of object")),
                }
            }
            Some(Token::LeftBracket) => Ok(self.array(iter).ok()),
            Some(Token::Str(s)) => {
                iter.next();
                Ok(Some(JsonTree::Atom(Token::Str(s))))
            }
            Some(Token::Number(n)) => {
                iter.next();
                Ok(Some(JsonTree::Atom(Token::Number(*n))))
            }
            Some(Token::True) => {
                iter.next();
                Ok(Some(JsonTree::Atom(Token::True)))
            }
            Some(Token::False) => {
                iter.next();
                Ok(Some(JsonTree::Atom(Token::False)))
            }
            Some(Token::Null) => {
                iter.next();
                Ok(Some(JsonTree::Atom(Token::Null)))
            }
            None => Ok(None),
            Some(t) => bail!("Unexpected token {t:?}"),
        }
    }

    fn object(&'a self, iter: &mut Peekable<Iter<Token<'a>>>) -> Result<JsonTree> {
        iter.next(); // consume left brace
        let members = self.members(iter);
        let token = iter.next();
        if !matches!(token, Some(Token::RightBrace)) {
            bail!("Expected }}, got {token:?}");
        }
        members
    }

    fn members(&'a self, iter: &mut Peekable<Iter<Token<'a>>>) -> Result<JsonTree> {
        let mut elements: HashMap<&str, JsonTree> = HashMap::new();
        if matches!(iter.peek(), Some(Token::RightBrace)) {
            return Ok(JsonTree::Object(elements));
        }
        loop {
            let name = match iter.next() {
                Some(Token::Str(s)) => s,
                t => bail!("Invalid token {t:?}"),
            };
            self.consume(iter, Token::Colon).context("':' expected in object member definition")?;
            let member = match self.value(iter) {
                Ok(Some(value)) => value,
                Ok(None) => bail!("Unexpected end of input"),
                Err(e) => return Err(e.context("In object member definition")),
            };
            elements.insert(name, member);
            if matches!(iter.peek(), Some(Token::Comma)) {
                let _ = self.consume(iter, Token::Comma);
            } else {
                break;
            }
        }
        Ok(JsonTree::Object(elements))
    }

    fn array(&self, iter: &mut Peekable<Iter<Token>>) -> Result<JsonTree> {
        let mut elements = Vec::new();
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
        let parser = Parser::new();
        match parser.parse(&mut tokens.iter().peekable()) {
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
    fn test_bad_integer_lexing() {
        let text = "010000";
        let mut lexer = Lexer::new(text);
        let result = lexer.lex();
        assert!(result.is_err());
    }

    #[test]
    fn test_bad_fractional_lexing() {
        let text = ".001";
        let mut lexer = Lexer::new(text);
        let result = lexer.lex();
        assert!(result.is_err());
    }

    #[test]
    fn test_bad_exponent_lexing() {
        let text = "E001";
        let mut lexer = Lexer::new(text);
        let result = lexer.lex();
        assert!(result.is_err());
    }

    #[test]
    fn test_keywords_lexing() {
        let text = "true false null";
        let mut lexer = Lexer::new(text);
        let result_vec = lexer.lex().unwrap();
        assert_eq!(result_vec[0], Token::True);
        assert_eq!(result_vec[1], Token::False);
        assert_eq!(result_vec[2], Token::Null);
    }

    #[test]
    fn test_empty_object_parsing() {
        let contents = "{}";
        let mut lexer = Lexer::new(contents);
        let tokens = lexer.lex().unwrap();
        let parser = Parser::new();
        assert!(parser.parse(&mut tokens.iter().peekable()).is_ok());
    }

    #[test]
    fn test_invalid_object_parsing() {
        let contents = "{";
        let mut lexer = Lexer::new(contents);
        let tokens = lexer.lex().unwrap();
        let parser = Parser::new();
        assert!(parser.parse(&mut tokens.iter().peekable()).is_err());
    }

    #[test]
    fn test_basic_object_parsing1() {
        let contents = r#"{"hello": "world"}"#;
        let mut lexer = Lexer::new(contents);
        let tokens = lexer.lex().unwrap();
        let parser = Parser::new();
        let jsontree = parser.parse(&mut tokens.iter().peekable()).unwrap().unwrap();
        assert!(matches!(jsontree, JsonTree::Object(_)));
        match jsontree {
            JsonTree::Object(map) => 
                assert!(matches!(map.get("hello").unwrap(), JsonTree::Atom(Token::Str("world")))),
            _ => panic!(),
        }
    }

    #[test]
    fn test_basic_object_parsing2() {
        let contents = r#"{"hello": "world", "goodbye": true}"#;
        let mut lexer = Lexer::new(contents);
        let tokens = lexer.lex().unwrap();
        let parser = Parser::new();
        let jsontree = parser.parse(&mut tokens.iter().peekable()).unwrap().unwrap();
        assert!(matches!(jsontree, JsonTree::Object(_)));
        match jsontree {
            JsonTree::Object(map) => {
                assert!(matches!(map.get("hello").unwrap(), JsonTree::Atom(Token::Str("world"))));
                assert!(matches!(map.get("goodbye").unwrap(), JsonTree::Atom(Token::True)));
            }
            _ => panic!(),
        }
    }
}
