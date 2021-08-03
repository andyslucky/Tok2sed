use clap::{AppSettings, Clap};
use libtok2me::tokenizer::Tokenizer;
use libtok2me::{regex_wrapper::RegexWrapper, token_def::Token};
use libtok2me::{token_config::TokenizerConfig, token_def::TokenDef};
use rand::random;
use std::{
    any::type_name,
    fmt::{format, Debug},
    fs::File,
    io::{BufWriter, Write},
    ptr::null_mut,
    thread::current,
};

use petgraph::{data::Build, graph::Graph};
use std::io::{stdin, Read};
use std::{collections::HashMap, error::Error as IError};
use std::{result::Result, str::FromStr};

#[macro_use]
extern crate derive_new;

use itertools::Itertools;
use regex::Regex;
use std::error::Error;
use std::fmt::Formatter;
use std::ptr::null;
macro_rules! version {
    () => {
        1.0
    };
}

macro_rules! version_str {
    () => {
        concat!("", version!())
    };
}

#[derive(Clap, Debug, Clone)]
#[clap(
version = version_str ! (),
author = "Andrew Strickland <andrewpstrickland@gmail.com>",
about = "Tokenized stream editor"
)]
#[clap(setting = AppSettings::ColoredHelp)]
struct Opts {
    #[clap(short, long, about = "YAML file providing token definitions")]
    token_file: FileWrapper,

    #[clap(
    short,
    long,
    about = "Input file to tokenize. If not provided STDIN will be used"
    )]
    input_file: Option<FileWrapper>,

    #[clap(
    short = 's',
    long,
    about = "Path to export state machine diagram to (graphviz file)."
    )]
    export_state_mach: Option<FileWrapper>,

    #[clap(short = 'n', long, about = "No comments in output")]
    disable_comments: bool,

    expression: String,

    replacement: String,
}

impl Opts {
    pub fn get_input(&self) -> Result<Box<dyn Read>, std::io::Error> {
        return self.input_file.clone().map_or(Ok(Box::new(stdin())), |f| {
            Ok(Box::new(f.open(true, false, false)?))
        });
    }

    pub fn get_token_file(&self) -> Result<File, std::io::Error> {
        return self.token_file.open(true, false, false);
    }

    pub fn get_expression(&self) -> Result<Box<dyn Read>, std::io::Error> {
        let cl = self.expression.clone();
        let b = cl.into_bytes();
        let cursor = std::io::Cursor::new(b);
        return Ok(Box::new(cursor));
    }
}

#[derive(Debug, Clone)]
struct FileWrapper {
    path: String,
}

impl FileWrapper {
    pub fn open(&self, read: bool, write: bool, create: bool) -> Result<File, std::io::Error> {
        return std::fs::OpenOptions::new()
            .read(read)
            .write(write)
            .create(create)
            .open(self.path.clone());
    }
}

impl FromStr for FileWrapper {
    type Err = std::io::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(FileWrapper {
            path: s.to_string(),
        })
    }
}

#[derive(Debug)]
enum TokenOrGroup {
    ExprGroup(Pattern),
    ExprToken(String),
}

#[derive(new)]
struct Pattern {
    #[new(value = "random()")]
    id: i32,

    #[new(value = "null_mut()")]
    parent_group: *mut Pattern,

    group_number: u32,

    // #[new(default)]
    // negated: bool,

    #[new(default)]
    exprs: Vec<TokenOrGroup>,
}

impl Debug for Pattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "p:{:?}, id:{}, g:{}, e: {:?}", self.parent_group, self.id, self.group_number, self.exprs)
    }
}

impl Pattern {
    fn _expr_tokenizer(expr: Box<dyn Read>) -> Tokenizer {
        let mut config = TokenizerConfig::new();
        config.with_tokens(vec![
            TokenDef {
                token_type: "LPAREN".to_string(),
                exprs: vec![RegexWrapper::from("\\(".to_string())],
            },
            TokenDef {
                token_type: "RPAREN".to_string(),
                exprs: vec![RegexWrapper::from("\\)")],
            },
            TokenDef {
                token_type: "IDENT".to_string(),
                exprs: vec![RegexWrapper::from("^[a-zA-Z0-9]+")],
            },
            TokenDef {
                token_type: "LT".to_string(),
                exprs: vec![RegexWrapper::from("<")],
            },
            TokenDef {
                token_type: "GT".to_string(),
                exprs: vec![RegexWrapper::from(">")],
            },
        ]);
        return Tokenizer::new(config, expr);
    }

    fn _group_numbers(&self) -> Vec<u32> {
        let mut groups: Vec<u32> = vec![self.group_number];
        if self.parent_group != null_mut() {
            let mut super_group = unsafe { (*(self.parent_group))._group_numbers() };
            groups.append(&mut super_group);
        }
        return groups;
    }

    fn _next_match<F: Fn(Vec<Token>)>(&self, tok: &mut Tokenizer, unmatched_handler: &F) -> Option<Match> {
        // println!("Entering {:?}",self);
        let mut match1: Option<Match> = None;
        let groups = self._group_numbers();
        let mut unmatched_toks: Vec<Token> = vec![];
        for exp in self.exprs.iter() {
            match exp {
                TokenOrGroup::ExprGroup(group) => {
                    if let Some(sub_match) = group._next_match(tok, unmatched_handler) {
                        if let Some(ref mut current_match) = match1 {
                            current_match.fold_left(sub_match);
                        } else {
                            match1 = Some(sub_match);
                        }
                    }
                }
                TokenOrGroup::ExprToken(tok_type) => {
                    if let Some(next_token) = tok.get_token().unwrap() {
                        if let Some(ref next_token_type) = next_token.token_type {
                            if next_token_type == tok_type {
                                if match1.is_none() {
                                    match1 = Some(Match::new());
                                }

                                if let Some(ref mut current_match) = match1 {
                                    current_match.tokens_by_groups.push((next_token, groups.clone()));
                                }

                                continue;
                            }
                        }
                        unmatched_toks.push(next_token);
                    }
                }
            }
        }
        unmatched_handler(unmatched_toks);
        return match1;
    }
}

impl From<Box<dyn Read>> for Pattern {
    fn from(expr: Box<dyn Read>) -> Self {
        let mut tok = Pattern::_expr_tokenizer(expr);
        // use std::ptr::null;
        let mut group_number: u32 = 0;
        let mut init_group = Pattern::new(group_number);
        let mut current_group: &mut Pattern = &mut init_group;
        let mut token_buff: Vec<Token> = vec![];
        while let Some(token) = tok.get_token().unwrap() {
            token_buff.push(token);
            if let Some(act) = match_buff(&token_buff) {
                match act {
                    ProductionType::TOKEN => {
                        let token_type = token_buff[1].token_value.clone();
                        let t = TokenOrGroup::ExprToken(token_type);
                        current_group.exprs.push(t);
                    }
                    ProductionType::CLOSE_GROUP => {
                        if group_number == 0 || current_group.parent_group == null_mut() {
                            panic!("Cannot close non existent group!");
                        }
                        // group_number -= 1;
                        unsafe {
                            current_group = &mut *(current_group.parent_group);
                        }
                    }
                    ProductionType::OPEN_GROUP => {
                        // increment group number
                        group_number += 1;
                        let mut new_group = Pattern::new(group_number);
                        // set parent on new group
                        new_group.parent_group = &mut *current_group;
                        let mut new_group_enum = TokenOrGroup::ExprGroup(new_group);
                        // add group to current group
                        current_group.exprs.push(new_group_enum);
                        let mut new_group = match current_group.exprs.last_mut().unwrap() {
                            TokenOrGroup::ExprGroup(group) => group,
                            TokenOrGroup::ExprToken(t) => panic!("This is pretty much impossible"),
                        };
                        // set current group to new group;
                        current_group = new_group;
                    }
                }
                token_buff.clear();
            }
        }
        return init_group;
    }
}

#[derive(Debug, new)]
struct Match {
    #[new(default)]
    tokens_by_groups: Vec<(Token, Vec<u32>)>,
}

impl Match {
    fn fold_left(&mut self, mut other: Match) {
        self.tokens_by_groups.append(&mut other.tokens_by_groups);
    }

    pub fn group(&self, group_num: u32) -> Option<Vec<&Token>> {
        let tokens: Vec<&Token> = self.tokens_by_groups.iter().filter(|t| t.1.contains(&group_num)).map(|t| &t.0).collect();
        if tokens.len() > 0 {
            return Some(tokens);
        }
        return None;
    }
}

enum ProductionType {
    OPEN_GROUP,
    CLOSE_GROUP,
    TOKEN,
}

#[inline(always)]
fn vec_eq(v1: &Vec<Token>, v2: &Vec<&str>) -> bool {
    let v1_size = v1.len();
    let v2_size = v2.len();
    if v1_size != v2_size {
        return false;
    }
    return v1
        .iter()
        .map(|t| t.token_type.as_ref().unwrap())
        .zip(v2)
        .filter(|&(a, b)| a.eq(&b.to_string()))
        .count()
        == v1_size;
}

fn match_buff(toks: &Vec<Token>) -> Option<ProductionType> {
    if vec_eq(toks, &vec!["LT", "IDENT", "GT"]) {
        // println!("Found Token!");
        return Some(ProductionType::TOKEN);
    }

    if vec_eq(toks, &vec!["LPAREN"]) {
        return Some(ProductionType::OPEN_GROUP);
    }

    if vec_eq(toks, &vec!["RPAREN"]) {
        return Some(ProductionType::CLOSE_GROUP);
    }

    return None;
}

trait AsString {
    fn string(&self) -> String;
}

impl<T: AsRef<Token>> AsString for Vec<T> {
    fn string(&self) -> String {
        return self.iter().map(|t| t.as_ref().token_value.clone()).join("");
    }
}

fn main() -> Result<(), Box<dyn IError>> {
    let opts = Opts::parse();
    let expr_group = Pattern::from(opts.get_expression()?);

    let mut input_tok = Tokenizer::new(TokenizerConfig::from_file(opts.get_token_file()?)?, opts.get_input()?);
    let val = opts.replacement.as_str();
    if let Some(_mat) = expr_group._next_match(&mut input_tok, &|t| print!("{}", t.string())) {
        let re = Regex::new(r"\\(\d)").unwrap();
        // println!("{:?}",re);
        let mut new_val = String::from(val);
        for cap in re.captures_iter(val) {
            // print!("match {:?}",cap);
            let group_num = &cap[1].parse::<u32>().unwrap();
            if let Some(group) = _mat.group(group_num.clone()) {
                new_val = new_val.replace(&cap[0], group.string().as_str());
            }
        }
        print!("{}", new_val);
    }
    Ok(())
}
