use std::collections::HashMap;
use std::iter::Peekable;
use std::slice::Iter;
use anyhow::{bail, Result};
use crate::lexer::Token;

#[derive(Debug)]
pub enum JsonTree<'a> {
    Object(HashMap<&'a str, JsonTree<'a>>),
    Array(Vec<JsonTree<'a>>),
    Atom(Token<'a>),
}

pub struct Parser { }

impl<'a> Parser {
    pub fn new() -> Self {
        Self { }
    }

    pub fn parse(&'a self, vec: &'a [Token]) -> Result<Option<JsonTree>> {
        self.value(&mut vec.iter().peekable())
    }

    fn value(&'a self, iter: &mut Peekable<Iter<Token<'a>>>) -> Result<Option<JsonTree>> {
        match iter.peek() {
            Some(Token::LeftBrace) => {
                match self.object(iter) {
                    Ok(object) => Ok(Some(object)),
                    Err(e) => Err(e.context("In parsing of object")),
                }
            }
            Some(Token::LeftBracket) => {
                match self.array(iter) {
                    Ok(array) => Ok(Some(array)),
                    Err(e) => Err(e.context("In parsing of array"))
                }
            }
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
        let mut elements: HashMap<&str, JsonTree> = HashMap::new();
        if matches!(iter.peek(), Some(Token::RightBrace)) {
            return Ok(JsonTree::Object(elements));
        }
        loop {
            let name = match iter.next() {
                Some(Token::Str(s)) => s,
                t => bail!("Invalid token {t:?}"),
            };
            match iter.next() {
                Some(Token::Colon) => {}
                Some(t) => bail!("Expected ':' but found {:?}", t),
                None => bail!("Unexpected end of input"),
            }
            let member = match self.value(iter) {
                Ok(Some(value)) => value,
                Ok(None) => bail!("Unexpected end of input"),
                Err(e) => return Err(e.context("In object member definition")),
            };
            elements.insert(name, member);
            if matches!(iter.peek(), Some(Token::Comma)) {
                iter.next();
            } else {
                break;
            }
        }
        let token = iter.next();
        if !matches!(token, Some(Token::RightBrace)) {
            bail!("Expected }}, got {token:?}");
        }
        Ok(JsonTree::Object(elements))
    }

    fn array(&'a self, iter: &mut Peekable<Iter<Token<'a>>>) -> Result<JsonTree> {
        iter.next();
        let mut elements = Vec::new();
        if matches!(iter.peek(), Some(Token::RightBracket)) {
            iter.next();
            return Ok(JsonTree::Array(elements));
        }
        loop {
            let element = match self.value(iter) {
                Ok(Some(value)) => value,
                Ok(None) => bail!("Unexpected end of input"),
                Err(e) => return Err(e.context("In array definition")),
            };
            elements.push(element);
            if matches!(iter.peek(), Some(Token::Comma)) {
                iter.next();
            } else {
                break;
            }
        }
        let token = iter.next();
        if !matches!(token, Some(Token::RightBracket)) {
            bail!("Expected ] but got {token:?}");
        }
        Ok(JsonTree::Array(elements))
    }
}

#[cfg(test)]
mod tests {
    use core::panic;

    use super::*;
    use crate::{lexer::Numeric, Lexer};

    #[test]
    fn test_parse_empty_object() {
        let contents = "{}";
        let mut lexer = Lexer::new(contents);
        let tokens = lexer.lex().unwrap();
        let parser = Parser::new();
        assert!(parser.parse(&tokens).is_ok());
    }

    #[test]
    fn test_parse_invalid_object() {
        let contents = r#"{"foo": }"#;
        let mut lexer = Lexer::new(contents);
        let tokens = lexer.lex().unwrap();
        let parser = Parser::new();
        assert!(parser.parse(&tokens).is_err());
    }

    #[test]
    fn test_parse_basic_object1() {
        let contents = r#"{"hello": "world"}"#;
        let mut lexer = Lexer::new(contents);
        let tokens = lexer.lex().unwrap();
        let parser = Parser::new();
        let jsontree = parser.parse(&tokens).unwrap().unwrap();
        assert!(matches!(jsontree, JsonTree::Object(_)));
        match jsontree {
            JsonTree::Object(map) => 
            assert!(matches!(map.get("hello").unwrap(), JsonTree::Atom(Token::Str("world")))),
            _ => panic!(),
        }
    }

    #[test]
    fn test_parse_basic_object2() {
        let contents = r#"{"hello": "world", "goodbye": true}"#;
        let mut lexer = Lexer::new(contents);
        let tokens = lexer.lex().unwrap();
        let parser = Parser::new();
        let jsontree = parser.parse(&tokens).unwrap().unwrap();
        assert!(matches!(jsontree, JsonTree::Object(_)));
        match jsontree {
            JsonTree::Object(map) => {
                assert!(matches!(map.get("hello").unwrap(), JsonTree::Atom(Token::Str("world"))));
                assert!(matches!(map.get("goodbye").unwrap(), JsonTree::Atom(Token::True)));
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_parse_array() {
        let contents = r#"[1, 2.0, "foo", false, null]"#;
        let mut lexer = Lexer::new(contents);
        let tokens = lexer.lex().unwrap();
        let parser = Parser::new();
        let jsontree = parser.parse(&tokens).unwrap().unwrap();
        match jsontree {
            JsonTree::Array(vec) => {
                assert!(matches!(vec[0], JsonTree::Atom(Token::Number(Numeric::Integer(1)))));
                assert!(matches!(vec[1], JsonTree::Atom(Token::Number(Numeric::Float(2.0)))));
                assert!(matches!(vec[2], JsonTree::Atom(Token::Str("foo"))));
                assert!(matches!(vec[3], JsonTree::Atom(Token::False)));
                assert!(matches!(vec[4], JsonTree::Atom(Token::Null)));
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_parse_nested_array() {
        let contents = r#"[[[]]]"#;
        let mut lexer = Lexer::new(contents);
        let tokens = lexer.lex().unwrap();
        let parser = Parser::new();
        let jsontree = parser.parse(&tokens).unwrap().unwrap();
        match jsontree {
            JsonTree::Array(level2) => {
                match &level2[0] {
                    JsonTree::Array(level3) => {
                        assert!(matches!(level3[0], JsonTree::Array(_)))
                    }
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_parse_complex_object() {
        let contents = r#"{"hello": 99, "bar": [1, 2, 3, 4]}"#;
        let mut lexer = Lexer::new(contents);
        let tokens = lexer.lex().unwrap();
        let parser = Parser::new();
        let jsontree = parser.parse(&tokens).unwrap().unwrap();
        match jsontree {
            JsonTree::Object(map) => {
                assert!(matches!(map.get("hello").unwrap(), JsonTree::Atom(Token::Number(Numeric::Integer(99)))));
                assert!(matches!(map.get("bar").unwrap(), JsonTree::Array(_)));
            }
            _ => panic!(),
        }
    }
}
