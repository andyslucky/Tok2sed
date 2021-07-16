use std::{fs::File, io::{BufWriter, Write}};
use clap::{AppSettings, Clap};
use libtok2me::token_config::TokenizerConfig;
use libtok2me::token_def::Token;
use libtok2me::tokenizer::Tokenizer;

use std::io::{stdin, Read};
use std::{collections::HashMap, error::Error as IError};
use std::{result::Result, str::FromStr};
use petgraph::{data::Build, graph::Graph};
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
    version = version_str!(),
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

    #[clap(short = 's', long, about = "Path to export state machine diagram to (graphviz file).")]
    export_state_mach : Option<FileWrapper>,

    #[clap(short = 'n', long, about = "No comments in output")]
    disable_comments: bool,

    #[clap(short = 'F', long, about = "File containing expression sequence")]
    expression_file: Option<FileWrapper>,

    #[clap(about = "Expression sequence")]
    expression: Option<String>,
}

impl Opts {
    pub fn get_input(&self) -> Result<Box<dyn Read>, std::io::Error> {
        return self
            .input_file
            .clone()
            .map_or(Ok(Box::new(stdin())), |f| Ok(Box::new(f.open(true, false, false)?)));
    }

    pub fn get_token_file(&self) -> Result<File, std::io::Error> {
        return self.token_file.open(true, false, false);
    }
}

#[derive(Debug,Clone)]
struct FileWrapper{
    path : String
}

impl FileWrapper {
    pub fn open(&self, read : bool, write : bool, create : bool) -> Result<File,std::io::Error> {
        return std::fs::OpenOptions::new().read(read).write(write).create(create).open(self.path.clone());
    }
}

impl FromStr for FileWrapper {
    type Err = std::io::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(FileWrapper{path: s.to_string()})
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

#[derive(Debug)]
struct State {
    transitions: HashMap<String, State>,
    name: String,
    accepting: bool,
}

impl State {
    fn new(name: &str) -> Self {
        return State {
            transitions: HashMap::new(),
            name: String::from(name),
            accepting: false,
        };
    }

    pub fn with_accepting(&mut self, accepting: bool) -> &mut Self {
        self.accepting = accepting;
        return self;
    }

    pub fn add_transition(&mut self, token_type: &str, state: State) -> &mut Self {
        self.transitions.insert(String::from(token_type), state);
        return self;
    }

    pub fn transition_on(&self, token_type: &String) -> Option<&'_ State> {
        return self.transitions.get(token_type);
    }
}

struct TokenIterator(Tokenizer);
impl Iterator for TokenIterator {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        return self.0.get_token().unwrap();
    }
}

struct Pattern {
    init_state: State,
}

impl Pattern {
    pub fn compile() -> Pattern {
        
        let mut init = State::new("s0");
        let mut s1 = State::new("s1");
        let mut accepting_state = State::new("accept");
        accepting_state.with_accepting(true);
        s1.add_transition("WS", accepting_state);
        init.add_transition("COMMA", s1);
        return Pattern { init_state: init };
    }

    pub fn as_graph(&self) -> Graph<String,String> {
        let mut graph : Graph<String,String> = Graph::new();
        
        fn add_node(g : &mut Graph<String,String>, st : &State) -> petgraph::graph::NodeIndex<u32>{
            let node = g.add_node(st.name.clone());
            for (token_type,new_state) in st.transitions.iter() {
                let child_node = add_node(g, &new_state);
                g.add_edge(node, child_node, token_type.clone());
            }
            return node;
        }

        add_node(&mut graph, &self.init_state);
        return graph;
    }

    pub fn next_match(&self, tokenizer: &mut Tokenizer) -> Option<Match> {
        return self.handle_next_match(tokenizer, |_| {});
    }

    pub fn handle_next_match<F: Fn(&Vec<Token>)>(
        &self,
        tokenizer: &mut Tokenizer,
        unmatched_tokens_handler: F,
    ) -> Option<Match> {
        let mut tokens = Vec::<Token>::new();
        let mut current_state: &State = &self.init_state;
        while let Some(token) = tokenizer.get_token().unwrap() {
            if let Some(new_state) =
                current_state.transition_on(&token.token_type.as_ref().unwrap())
            {
                current_state = new_state;
                tokens.push(token.clone());
                if current_state.accepting {
                    return Some(Match { tokens });
                }
            } else {
                // no transition from current state on token
                // handle unmatched tokens
                unmatched_tokens_handler(&tokens);
                tokens.clear();

                // reset current state to initial state
                current_state = &self.init_state;

                //check if the most recent token has a transition from the init state
                if let Some(new_state) =
                    current_state.transition_on(&token.token_type.as_ref().unwrap())
                {
                    // update state and push the token
                    current_state = new_state;
                    tokens.push(token.clone());
                } else {
                    // no transitions handle it as an unmatched token
                    unmatched_tokens_handler(&vec![token.clone()]);
                }
            }
        }
        return None;
    }

    pub fn replace_next(&self, tokenizer: &mut Tokenizer, val: &str) -> bool {
        if let Some(_mat) = self.handle_next_match(tokenizer, |tokens| {
            for tok in tokens {
                print!("{}", tok.token_value);
            }
        }) {
            print!("{}", val);
            return true;
        }
        return false;
    }
}


struct MatchGroup {
    group_number : u32,
    tokens : Vec<Token>,
    sub_groups : Vec<MatchGroup>
}


#[derive(Debug)]
struct Match {
    tokens: Vec<Token>,
}

impl ToString for Match {
    fn to_string(&self) -> String {
        let mut val = String::new();
        for tok in self.tokens.iter() {
            val.push_str(tok.token_value.as_str());
        }
        return val;
    }
}

fn main() -> Result<(), Box<dyn IError>> {
    let opts = Opts::parse();
    let conf: TokenizerConfig = TokenizerConfig::from_file(opts.get_token_file()?)?;
    let mut tokenizer = Tokenizer::new(conf, opts.get_input()?);
    let pattern = Pattern::compile();
    loop {
        if !pattern.replace_next(&mut tokenizer, "Hey this is cool") {
            break;
        }

    }
    
    if let Some(state_file) = opts.export_state_mach {
        let graph = pattern.as_graph();
        let _ = state_file.open(false, true, true)?.write_all(petgraph::dot::Dot::new(&graph).to_string().as_bytes())?;
    }
    Ok(())
}
