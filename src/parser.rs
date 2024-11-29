use std::cmp::PartialEq;
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::fmt;

#[derive(PartialEq, Debug)]
pub enum Token {
    LParen,
    RParen,
    Not,
    And,
    Or,
    Var(char)
}

impl Token {
    fn precedence(&self) -> usize {
        match self {
            Token::LParen => 0,
            Token::RParen => 0,
            Token::Not => 0,
            Token::And => 2,
            Token::Or => 1,
            Token::Var(_) => 0,
        }
    }
}

pub struct Lexer {
}

impl Lexer {
    pub fn lex(content: &str) -> VecDeque<Token> {
        content.chars()
            .flat_map(|c| {
                match c {
                    ' ' | '\n' => None,
                    '!' => Some(Token::Not),
                    '&' => Some(Token::And),
                    '|' => Some(Token::Or),
                    '(' => Some(Token::LParen),
                    ')' => Some(Token::RParen),
                    c => Some(Token::Var(c.to_ascii_uppercase()))
                }
            })
            .collect()
    }
}

pub struct Parser;


impl Parser {

    // only allow operations of precendence
    fn parse_full(tokens: &mut VecDeque<Token>, min_precedence: usize) -> Result<Expr, ()> {
        let mut curr = Self::parse_unary(tokens)?;
        while let Some(token) = tokens.front() {
            match token {
                Token::LParen => return Err(()),
                Token::RParen => {
                    tokens.pop_front();
                    break;
                }
                Token::Not => return Err(()),
                Token::And => {
                    let precedence = token.precedence();
                    if precedence < min_precedence  {
                        break;
                    }

                    tokens.pop_front();
                    let rhs = Self::parse_full(tokens, precedence)?;
                    curr = Expr::And(Box::new(curr), Box::new(rhs));
                }
                Token::Or => {
                    let precedence = token.precedence();
                    if precedence < min_precedence  {
                        break;
                    }

                    tokens.pop_front();
                    let rhs = Self::parse_full(tokens, precedence)?;
                    curr = Expr::Or(Box::new(curr), Box::new(rhs));
                }
                Token::Var(_) => return Err(())
            }
        }

        Ok(curr)
    }

    fn parse_unary(tokens: &mut VecDeque<Token>) -> Result<Expr, ()> {
        if tokens.is_empty() {
            return Err(());
        }

        match tokens.pop_front().unwrap() {
            Token::LParen => Self::parse_full(tokens, 0),
            Token::RParen => Err(()),
            Token::Not => Ok(Expr::Not(Box::new(Self::parse_unary(tokens)?))),
            Token::And => Err(()),
            Token::Or => Err(()),
            Token::Var(c) => Ok(Expr::Var(c))
        }
    }

    pub fn parse(mut tokens: VecDeque<Token>) -> Result<Expr, ()> {
        // verify matching parenthesis
        let mut open = 0;
        for token in &tokens {
            match token {
                Token::LParen => open += 1,
                Token::RParen => {
                    open -= 1;
                    if open < 0 {
                        return Err(())
                    }
                }
                _ => {}
            }
        }

        Self::parse_full(&mut tokens, 0)
    }
}

pub enum Expr {
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
    Var(char)
}

impl Expr {
    pub fn variables(&self, vars: &mut BTreeSet<char>) {
        match self {
            Expr::And(u, v) => {
                u.variables(vars);
                v.variables(vars);
            }
            Expr::Or(u, v) => {
                u.variables(vars);
                v.variables(vars);
            }
            Expr::Not(u) => {
                u.variables(vars);
            }
            Expr::Var(v) => {
                vars.insert(*v);
            }
        }
    }

    pub fn eval(&self, assignment: &BTreeMap<char, bool>) -> bool {
        match self {
            Expr::And(u, v) => {
                u.eval(assignment) && v.eval(assignment)
            }
            Expr::Or(u, v) => {
                u.eval(assignment) || v.eval(assignment)
            }
            Expr::Not(u) => {
                !u.eval(assignment)
            }
            Expr::Var(c) => {
                assignment[c]
            }
        }
    }

    // 2 ^ {|variable|}
    pub fn eval_all(&self, variables: &BTreeSet<char>) -> Vec<bool> {
        let num_var = variables.len();

        (0 .. (1usize << num_var))
            .into_iter()
            .map(|subset| {
                let mut assignment = BTreeMap::new();
                for (j, char) in variables.iter().enumerate() {
                    assignment.insert(*char, (subset & (1usize << j)) != 0);
                }
                self.eval(&assignment)
            })
            .collect()
    }
}

// we will only do this in product of sums 
// so the format is assumed for that
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::And(u, v) => {
                write!(f, "{} & {}", u, v)
            },
            Expr::Or(u, v) => {
                write!(f, "({}) | ({})", u, v)
            },
            Expr::Not(u) => {
                write!(f, "!{}", u)
            }
            Expr::Var(c) => {
                write!(f, "{}", c)
            }
        }
    }
}

pub struct BooleanSimplifier;

impl BooleanSimplifier {
    pub fn simplify(expr: Expr) -> Expr {
        // find variables and evaluations
        // find prime implicants
        // try all possible prime implicants to see which subset is best
        expr
    }
}

