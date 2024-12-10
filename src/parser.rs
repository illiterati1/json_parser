use std::collections::HashMap;
use std::iter::Peekable;
use anyhow::{bail, Result};
use crate::lexer::{Lexer, Numeric, Token};

#[derive(Debug, PartialEq)]
pub enum JsonTree<'a> {
    Object(HashMap<&'a str, JsonTree<'a>>),
    Array(Vec<JsonTree<'a>>),
    Str(&'a str),
    Num(Numeric),
    True,
    False,
    Null,
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self { 
            lexer: lexer.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Option<JsonTree<'a>>> {
        self.value()
    }

    fn value(&mut self) -> Result<Option<JsonTree<'a>>> {
        match self.lexer.peek() {
            Some(Token::LeftBrace) => {
                match self.object() {
                    Ok(object) => Ok(Some(object)),
                    Err(e) => Err(e.context("In parsing of object")),
                }
            }
            Some(Token::LeftBracket) => {
                match self.array() {
                    Ok(array) => Ok(Some(array)),
                    Err(e) => Err(e.context("In parsing of array"))
                }
            }
            Some(Token::Str(s)) => {
                let result = Ok(Some(JsonTree::Str(s)));
                self.lexer.next();
                result
            }
            Some(Token::Number(n)) => {
                let result = Ok(Some(JsonTree::Num(*n)));
                self.lexer.next();
                result
            }
            Some(Token::True) => {
                self.lexer.next();
                Ok(Some(JsonTree::True))
            }
            Some(Token::False) => {
                self.lexer.next();
                Ok(Some(JsonTree::False))
            }
            Some(Token::Null) => {
                self.lexer.next();
                Ok(Some(JsonTree::Null))
            }
            None => Ok(None),
            Some(t) => bail!("Unexpected token {t:?}"),
        }
    }

    fn object(&mut self) -> Result<JsonTree<'a>> {
        self.lexer.next(); // consume left brace
        let mut elements: HashMap<&str, JsonTree> = HashMap::new();
        if self.lexer.peek() == Some(&Token::RightBrace) {
            return Ok(JsonTree::Object(elements));
        }
        loop {
            let name = match self.lexer.next() {
                Some(Token::Str(s)) => s,
                t => bail!("Invalid token {t:?}"),
            };
            match self.lexer.next() {
                Some(Token::Colon) => {}
                Some(t) => bail!("Expected ':' but found {:?}", t),
                None => bail!("Unexpected end of input"),
            }
            let member = match self.value() {
                Ok(Some(value)) => value,
                Ok(None) => bail!("Unexpected end of input"),
                Err(e) => return Err(e.context("In object member definition")),
            };
            elements.insert(name, member);
            if self.lexer.peek() == Some(&Token::Comma) {
                self.lexer.next();
            } else {
                break;
            }
        }
        let token = self.lexer.next();
        if !matches!(token, Some(Token::RightBrace)) {
            bail!("Expected }}, got {token:?}");
        }
        Ok(JsonTree::Object(elements))
    }

    fn array(&mut self) -> Result<JsonTree<'a>> {
        self.lexer.next();
        let mut elements = Vec::new();
        if self.lexer.peek() == Some(&Token::RightBracket) {
            self.lexer.next();
            return Ok(JsonTree::Array(elements));
        }
        loop {
            let element = match self.value() {
                Ok(Some(value)) => value,
                Ok(None) => bail!("Unexpected end of input"),
                Err(e) => return Err(e.context("In array definition")),
            };
            elements.push(element);
            if self.lexer.peek() == Some(&Token::Comma) {
                self.lexer.next();
            } else {
                break;
            }
        }
        let token = self.lexer.next();
        if !matches!(token, Some(Token::RightBracket)) {
            bail!("Expected ] but got {token:?}");
        }
        Ok(JsonTree::Array(elements))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer::Numeric, Lexer};

    #[test]
    fn test_parse_empty_object() {
        let contents = "{}";
        let lexer = Lexer::new(contents);
        let mut parser = Parser::new(lexer);
        assert!(parser.parse().is_ok());
    }

    #[test]
    fn test_parse_invalid_object() {
        let contents = r#"{"foo": }"#;
        let lexer = Lexer::new(contents);
        let mut parser = Parser::new(lexer);
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_parse_basic_object1() {
        let contents = r#"{"hello": "world"}"#;
        let lexer = Lexer::new(contents);
        let mut parser = Parser::new(lexer);
        let jsontree = parser.parse().unwrap().unwrap();
        assert!(matches!(jsontree, JsonTree::Object(_)));
        match jsontree {
            JsonTree::Object(map) => 
            assert_eq!(*map.get("hello").unwrap(), JsonTree::Str("world")),
            _ => panic!(),
        }
    }

    #[test]
    fn test_parse_basic_object2() {
        let contents = r#"{"hello": "world", "goodbye": true}"#;
        let lexer = Lexer::new(contents);
        let mut parser = Parser::new(lexer);
        let jsontree = parser.parse().unwrap().unwrap();
        assert!(matches!(jsontree, JsonTree::Object(_)));
        match jsontree {
            JsonTree::Object(map) => {
                assert!(matches!(map.get("hello").unwrap(), JsonTree::Str("world")));
                assert!(matches!(map.get("goodbye").unwrap(), JsonTree::True));
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_parse_array() {
        let contents = r#"[1, 2.0, "foo", false, null]"#;
        let lexer = Lexer::new(contents);
        let mut parser = Parser::new(lexer);
        let jsontree = parser.parse().unwrap().unwrap();
        match jsontree {
            JsonTree::Array(vec) => {
                assert!(matches!(vec[0], JsonTree::Num(Numeric::Integer(1))));
                assert!(matches!(vec[1], JsonTree::Num(Numeric::Float(2.0))));
                assert!(matches!(vec[2], JsonTree::Str("foo")));
                assert!(matches!(vec[3], JsonTree::False));
                assert!(matches!(vec[4], JsonTree::Null));
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_parse_nested_array() {
        let contents = r#"[[[]]]"#;
        let lexer = Lexer::new(contents);
        let mut parser = Parser::new(lexer);
        let jsontree = parser.parse().unwrap().unwrap();
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
        let lexer = Lexer::new(contents);
        let mut parser = Parser::new(lexer);
        let jsontree = parser.parse().unwrap().unwrap();
        match jsontree {
            JsonTree::Object(map) => {
                assert!(matches!(map.get("hello").unwrap(), JsonTree::Num(Numeric::Integer(99))));
                assert!(matches!(map.get("bar").unwrap(), JsonTree::Array(_)));
            }
            _ => panic!(),
        }
    }
}
