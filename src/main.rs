use std::collections::HashMap;
use std::io::{self, stdin, stdout, BufRead};
use std::str::FromStr;

use logos::{self, Logos};
use serde::Serialize;
use serde_json::to_writer;

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
enum Token {
    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*")]
    Symbol,
    #[token("{")]
    OpenBrace,
    #[token("=")]
    Equal,
    #[token(",")]
    Comma,
    #[token("}")]
    CloseBrace,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[regex(r#""[^"]*""#)]
    StrLit,
    #[regex(r"\d+")]
    NumLit,

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
enum HaskVal {
    Sym(String),
    Num(u64),
    App(Vec<HaskVal>),
    Str(String),
    Rec(HashMap<String, HaskVal>),
}

//const SPACES: &str = "                                                                                                                                                                                           ";

struct Lexer<'a> {
    inner: logos::Lexer<'a, Token>,
    current: Option<Token>,
    current_str: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a <Token as Logos>::Source) -> Self {
        let mut inner = logos::Lexer::new(source);
        let current = inner.next();
        let current_str = inner.slice();
        Self {
            inner, current, current_str
        }
    }
    pub fn next(&mut self) -> Option<Token> {
        self.current_str = self.inner.slice();
        let n = std::mem::replace(&mut self.current, self.inner.next());
        n
    }
    pub fn peek(&self) -> Option<Token> {
        self.current.clone()
    }
    pub fn str(&self) -> &'a str {
        self.current_str
    }
}

fn parse<'a>(stream: &mut Lexer<'a>, depth: usize) -> Option<HaskVal> {
    let mut app = Vec::new();
    loop {
        let n = stream.peek();
        //println!("{}{:?}: {}", &SPACES[..depth], n, stream.str());
        app.push(match n {
            Some(Token::Symbol) => {
                stream.next();
                let r = HaskVal::Sym(stream.str().to_owned());
                r
            }
            Some(Token::NumLit) => {
                stream.next();
                let r = HaskVal::Num(u64::from_str(stream.str()).unwrap());
                r
            }
            Some(Token::StrLit) => {
                stream.next();
                let r = HaskVal::Str(stream.str().to_owned());
                r
            }
            Some(Token::OpenBrace) => {
                stream.next();
                let mut map = HashMap::new();
                loop {
                    let nxt = stream.next()?;
                    //println!("{}brace {:?}: {}", &SPACES[..depth], nxt, stream.str());
                    match nxt {
                        Token::CloseBrace => break Some(HaskVal::Rec(map)),
                        Token::Symbol => {
                            let k = stream.str().to_owned();
                            assert_eq!(stream.next(), Some(Token::Equal));
                            let v = parse(stream, depth + 1)?;
                            map.insert(k, v);
                        }
                        Token::Comma => {}
                        _ => break None,
                    }
                }?
            }
            Some(Token::CloseParen) => break,
            Some(Token::Comma) => break,
            Some(Token::CloseBrace) => break,
            Some(Token::OpenParen) => {
                stream.next()?;
                let n = parse(stream, depth + 1)?;
                assert_eq!(stream.next(), Some(Token::CloseParen));
                n
            }
            None => break,
            _ => return None,
        })
    }
    //println!("{}app {:?}", &SPACES[..depth], app);
    match app.len() {
        0 => None,
        1 => app.pop(),
        _ => Some(HaskVal::App(app)),
    }
}

fn main() -> io::Result<()> {
    for line in stdin().lock().lines() {
        let line = line?;
        let mut lex = Lexer::new(line.as_str());
        if let Some(val) = parse(&mut lex, 0) {
            to_writer(stdout().lock(), &val)?;
            println!("");
            //println!("{:#?}", val);
        }
    }
    Ok(())
}
