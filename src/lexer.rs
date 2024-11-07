use core::fmt;

use anyhow::{bail, Result};

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
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

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LeftBrace => write!(f, "{{"),
            Self::RightBrace => write!(f, "}}"),
            Self::LeftBracket => write!(f, "["),
            Self::RightBracket => write!(f, "]"),
            Self::Colon => write!(f, ":"),
            Self::Comma => write!(f, ","),
            Self::Str(s) => write!(f, "\"{s}\""),
            Self::Number(Numeric::Integer(n)) => write!(f, "{n}"),
            Self::Number(Numeric::Float(n)) => write!(f, "{n}"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Numeric {
    Float(f64),
    Integer(i64),
}

pub struct Lexer<'a> {
    json: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(json: &'a str) -> Self {
        Self { 
            json,
        }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>> {
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
                    'e' | 'E' | '.' | ' ' | '\t' | '\r' | '\n' | ',' | ']' | '}' => {}
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
                        ' ' | '\t' | '\r' | '\n' | ',' | ']' | '}' => {
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
                        ' ' | '\t' | '\r' | '\n' | ',' | ']' | '}' => {
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
                        ' ' | '\t' | '\r' | '\n' | ',' | ']' | '}' => {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexing_string() {
        let text = r#""test \"\b\n\r\" string""#;
        let mut lexer = Lexer::new(text);
        let result_vec = lexer.lex().unwrap();
        let result = &result_vec[0];
        assert_eq!(*result, Token::Str(r#"test \"\b\n\r\" string"#));
    }

    #[test]
    fn test_lexing_integer() {
        let text = "1234567890 -999 0";
        let mut lexer = Lexer::new(text);
        let result_vec = lexer.lex().unwrap();
        assert_eq!(result_vec[0], Token::Number(Numeric::Integer(1234567890)));
        assert_eq!(result_vec[1], Token::Number(Numeric::Integer(-999)));
        assert_eq!(result_vec[2], Token::Number(Numeric::Integer(0)));
    }

    #[test]
    fn test_lexing_fraction() {
        let text = "0.0 0.999 -100.0";
        let mut lexer = Lexer::new(text);
        let result_vec = lexer.lex().unwrap();
        assert_eq!(result_vec[0], Token::Number(Numeric::Float(0.0)));
        assert_eq!(result_vec[1], Token::Number(Numeric::Float(0.999)));
        assert_eq!(result_vec[2], Token::Number(Numeric::Float(-100.0)));
    }

    #[test]
    fn test_lexing_exponent() {
        let text = "1e0 -1.0e1 100.001e3";
        let mut lexer = Lexer::new(text);
        let result_vec = lexer.lex().unwrap();
        assert_eq!(result_vec[0], Token::Number(Numeric::Float(1.0)));
        assert_eq!(result_vec[1], Token::Number(Numeric::Float(-10.0)));
        assert_eq!(result_vec[2], Token::Number(Numeric::Float(100001.0)));
    }

    #[test]
    fn test_lexing_bad_integer() {
        let text = "010000";
        let mut lexer = Lexer::new(text);
        let result = lexer.lex();
        assert!(result.is_err());
    }

    #[test]
    fn test_lexing_bad_fractional() {
        let text = ".001";
        let mut lexer = Lexer::new(text);
        let result = lexer.lex();
        assert!(result.is_err());
    }

    #[test]
    fn test_lexing_bad_exponent() {
        let text = "E001";
        let mut lexer = Lexer::new(text);
        let result = lexer.lex();
        assert!(result.is_err());
    }

    #[test]
    fn test_lexing_keywords() {
        let text = "true false null";
        let mut lexer = Lexer::new(text);
        let result_vec = lexer.lex().unwrap();
        assert_eq!(result_vec[0], Token::True);
        assert_eq!(result_vec[1], Token::False);
        assert_eq!(result_vec[2], Token::Null);
    }

    #[test]
    fn test_lexing_array() {
        let text = "[1, true, 4]";
        let mut lexer = Lexer::new(text);
        let result_vec = lexer.lex().unwrap();
        assert_eq!(result_vec[0], Token::LeftBracket);
        assert_eq!(result_vec[1], Token::Number(Numeric::Integer(1)));
        assert_eq!(result_vec[2], Token::Comma);
        assert_eq!(result_vec[3], Token::True);
        assert_eq!(result_vec[4], Token::Comma);
        assert_eq!(result_vec[5], Token::Number(Numeric::Integer(4)));
        assert_eq!(result_vec[6], Token::RightBracket);
    }
}
