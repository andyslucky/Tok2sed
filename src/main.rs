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

// Assume we want to add space after commas not in a string literal
// <COMMA>(<!WS>) -> <COMMA><WS>$1
// tokens will be inside <>
// groups will be inside ()
// tokens and groups can have names (not including spaces) and will be formatted <name:> or (name:).
// named tokens will be referred to in the substitution clause by <:name:> or (:name:)
// <![TOKEN_NAME]> matches any token not in [TOKEN_NAME]
// <COMMA>((<!WS>)<LBRACE>) -> <COMMA> $1

// #[derive(PartialEq, Eq, Hash, new)]
// struct Transition<'a> {
//     from_state: &'a State,
//     symbols: Vec<String>,
//     #[new(value = "false")]
//     negated: bool,
//     group_number: u32,
//     to_state: &'a State,
// }
//
// impl Debug for Transition<'_> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(
//             f,
//             "({} - {:?} -> {})[group: {}]",
//             self.from_state.name, self.symbols, self.to_state.name, self.group_number
//         )
//     }
// }
//
// impl<'a> Transition<'a> {
//     fn matches<T: AsRef<str>>(&self, token_name: T) -> bool {
//         let tok_name_str: &str = token_name.as_ref();
//         if self.negated {
//             return !self.symbols.iter().any(|s| s.as_str() == tok_name_str);
//         }
//         // we will use . as the epsilon transition
//         return self.symbols.iter().any(|s| s.as_str() == tok_name_str);
//     }
// }

// #[derive(Debug, PartialEq, Eq, Hash)]
// struct State {
//     name: String,
//     accepting: bool,
// }
//
// impl State {
//     fn new<T: AsRef<str>>(name: T) -> Self {
//         return State {
//             name: name.as_ref().to_string(),
//             accepting: false,
//         };
//     }
// }

// #[derive(Debug)]
// struct Nfa<'a> {
//     states: Vec<State>,
//     transitions: Vec<Transition<'a>>,
// }
//
// impl<'a> Nfa<'a> {
//     pub fn has_transition(&self, from_state: &State, on_symbols: &String) -> Option<&Transition<'a>> {
//         self.transitions
//             .iter()
//             .find(|t| t.from_state == from_state && t.symbols.contains(on_symbols))
//     }
// }

// struct Pattern<'a> {
//     nfa: Nfa<'a>,
// }
//
// impl<'a> Pattern<'a> {
//     pub fn as_graph(&self) -> Graph<String, String> {
//         let mut graph: Graph<String, String> = Graph::new();
//
//         // fn add_node<'a>(
//         //     g: &mut Graph<String, String>,
//         //     st: &State,
//         // ) -> petgraph::graph::NodeIndex<u32> {
//         //     let node_name = match st.accepting {
//         //         true => format!("{} (accepting)", st.name),
//         //         false => format!("{}", st.name),
//         //     };
//         //     let node = g.add_node(node_name);
//         //     // TODO : Update this
//         //     let uniq_transitions: Vec<&Transition> = st.transitions.iter().unique().collect();
//         //     for transition in uniq_transitions.iter() {
//         //         let new_state = transition.state;
//         //         let child_node = add_node(g, &new_state);
//         //         for token_type in transition.symbols.iter() {
//         //             g.add_edge(node, child_node, token_type.clone());
//         //         }
//         //     }
//         //     return node;
//
//         //     // TODO : end update this
//         // }
//
//         // add_node(&mut graph, &self.nfa.init_state);
//         return graph;
//     }
//
//     pub fn next_match(&self, tokenizer: &mut Tokenizer) -> Option<Match> {
//         return self.handle_next_match(tokenizer, |_| {});
//     }
//
//     pub fn handle_next_match<F: Fn(&Vec<Token>)>(
//         &self,
//         tokenizer: &mut Tokenizer,
//         unmatched_tokens_handler: F,
//     ) -> Option<Match> {
//         let mut tokens = Vec::<(Token,u32)>::new();
//
//         // init state
//         let mut current_state: &State = &self.nfa.states[0];
//         while let Some(token) = tokenizer.get_token().unwrap() {
//             let token_type = token.clone().token_type.unwrap();
//             // println!("Found token!{:?}",token);
//             if let Some(trans) = self
//                 .nfa
//                 .has_transition(current_state, &token_type.clone())
//             {
//                 current_state = trans.to_state;
//                 tokens.push((token.clone(),trans.group_number));
//                 if current_state.accepting {
//                     return Some(Match { tokens });
//                 }
//             } else {
//                 // no transition from current state on token
//                 // handle unmatched tokens
//                 unmatched_tokens_handler(&(tokens.iter().map(|t| t.0.clone()).collect()));
//                 tokens.clear();
//
//                 // reset current state to initial state
//                 current_state = &self.nfa.states[0];
//
//                 //check if the most recent token has a transition from the init state
//                 if let Some(trans) = self
//                     .nfa
//                     .has_transition(current_state, &token_type.clone())
//                 {
//                     // update state and push the token
//                     current_state = trans.to_state;
//                     tokens.push((token.clone(),trans.group_number));
//                 } else {
//                     // no transitions handle it as an unmatched token
//                     unmatched_tokens_handler(&vec![token.clone()]);
//                 }
//             }
//         }
//
//         unmatched_tokens_handler(&(tokens.iter().map(|t| t.0.clone()).collect()));
//         return None;
//     }
//
//     pub fn replace_next(&self, tokenizer: &mut Tokenizer, val: &str) -> bool {
//         if let Some(_mat) = self.handle_next_match(tokenizer, |tokens| {
//             for tok in tokens {
//                 print!("{}", tok.token_value);
//             }
//         }) {
//
//             let re = Regex::new(r"\\(\d)").unwrap();
//             // println!("{:?}",re);
//             let mut new_val = String::from(val);
//             for cap in re.captures_iter(val) {
//                 // print!("match {:?}",cap);
//                 let group_num = &cap[1].parse::<u32>().unwrap();
//                 let group = _mat.group(group_num.clone()).string();
//                 new_val = new_val.replace(&cap[0],group.as_str());
//             }
//             print!("{}", new_val);
//             return true;
//         }
//         return false;
//     }
// }


#[derive(Debug)]
enum TokenOrGroup {
    ExprGroup(Pattern),
    ExprToken(String),
}


#[derive(new)]
struct Pattern {
    #[new(value="random()")]
    id : i32,

    #[new(value = "null_mut()")]
    parent_group: *mut Pattern,

    group_number: u32,

    #[new(default)]
    negated : bool,

    #[new(default)]
    exprs: Vec<TokenOrGroup>,
}

impl Debug for Pattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "p:{:?}, id:{}, g:{}, e: {:?}",self.parent_group, self.id, self.group_number, self.exprs)
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
        let mut groups : Vec<u32> = vec![self.group_number];
        if self.parent_group != null_mut() {
            let mut super_group = unsafe{(*(self.parent_group))._group_numbers()};
            groups.append(&mut super_group);
        }
        return groups;
    }

    fn _next_match<F: Fn(Vec<Token>)>(&self, tok : &mut Tokenizer, unmatched_handler :&F) -> Option<Match>{
        // println!("Entering {:?}",self);
        let mut match1 : Option<Match> = None;
        let groups = self._group_numbers();
        let mut unmatched_toks : Vec<Token> = vec![];
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
                },
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
    fn from(expr : Box<dyn Read>) -> Self {
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

#[derive(Debug,new)]
struct Match {
    #[new(default)]
    tokens_by_groups : Vec<(Token,Vec<u32>)>

}


impl Match {
    fn fold_left(&mut self, mut other : Match) {
        self.tokens_by_groups.append(&mut other.tokens_by_groups);
    }

    pub fn group(&self, group_num : u32) -> Option<Vec<&Token>> {
        let tokens : Vec<&Token> = self.tokens_by_groups.iter().filter(|t| t.1.contains(&group_num)).map(|t| &t.0).collect();
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





// fn flatten_group(group: &Pattern) -> Vec<(String, u32)> {
//     let mut tokens = Vec::<(String, u32)>::new();
//     for e in group.exprs.iter() {
//         match e {
//             TokenOrGroup::ExprGroup(g) => {
//                 tokens.append(&mut flatten_group(g));
//             }
//             TokenOrGroup::ExprToken(token) => tokens.push((token.to_string(), group.group_number)),
//         }
//     }
//     return tokens;
// }

//
// fn handle_group(group: Pattern) -> Nfa<'static> {
//     let mut nfa = Nfa {
//         states: vec![State::new(format!("s{}", 0))],
//         transitions: vec![],
//     };
//     let trans_data: Vec<(String, u32)> = flatten_group(&group);
//
//     for i in 0..trans_data.len() {
//         nfa.states.push(State::new(format!("s{}", i + 1)));
//     }
//     match nfa.states.last_mut() {
//         Some(l) => l.accepting = true,
//         None => {}
//     }
//
//     let states : *const Vec<State> = &nfa.states;
//     for i in 0..trans_data.len() {
//         let token_type = trans_data[i].0.clone();
//         let group_num = trans_data[i].1.clone();
//         unsafe {
//             nfa.transitions.push(Transition::new(&(*states)[i],vec![token_type],group_num,&(*states)[i+1] ))
//         }
//     }
//
//     return nfa;
// }

// fn to_nfa(expr_group: ExpressionGroup) -> Nfa<'static> {
//    return handle_group(expr_group);
// }




// #[derive(Debug)]
// struct Match {
//     tokens: Vec<(Token,u32)>,
// }
//
// impl Match {
//     pub fn group(&self, number : u32) -> Vec<Token>{
//         return self.tokens.iter().filter(|t| t.1 >= number).map(|t| t.0.clone()).collect();
//     }
// }

trait AsString {
    fn string(&self) -> String;
}

impl<T : AsRef<Token>> AsString for Vec<T> {
    fn string(&self) -> String {
        return self.iter().map(|t| t.as_ref().token_value.clone()).join("");
    }
}

fn main() -> Result<(), Box<dyn IError>> {
    let opts = Opts::parse();
    let expr_group = Pattern::from(opts.get_expression()?);
    // println!("Parsed expression {:?}", expr_group);
    // let nfa = handle_group(expr_group);
    // let pattern = Pattern { nfa };

    // println!("{:?}", pattern);

    let mut input_tok = Tokenizer::new(TokenizerConfig::from_file(opts.get_token_file()?)?, opts.get_input()?);
    // loop {
    //     if !pattern.replace_next(&mut input_tok, opts.replacement.as_str()) {
    //         break;
    //     }
    // }
    let val = opts.replacement.as_str();
    if let Some(_mat) = expr_group._next_match(&mut input_tok,&|t|print!("{}",t.string())) {
        let re = Regex::new(r"\\(\d)").unwrap();
        // println!("{:?}",re);
        let mut new_val = String::from(val);
        for cap in re.captures_iter(val) {
            // print!("match {:?}",cap);
            let group_num = &cap[1].parse::<u32>().unwrap();
            if let Some(group) = _mat.group(group_num.clone()) {
                new_val = new_val.replace(&cap[0],group.string().as_str());
            }
        }
        print!("{}", new_val);
    }

    // println!("{}",expr_group._next_match(&mut input_tok,&|t|print!("{}",t.string())).unwrap().group(2).unwrap().string());

    // if let Some(state_file) = opts.export_state_mach {
    //     let graph = pattern.as_graph();
    //     let _ = state_file
    //         .open(false, true, true)?
    //         .write_all(petgraph::dot::Dot::new(&graph).to_string().as_bytes())?;
    // }
    Ok(())
}
