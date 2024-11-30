use std::cmp::PartialEq;
use std::collections::{BTreeMap, BTreeSet, HashSet, VecDeque};
use std::fmt;

#[derive(PartialEq, Debug, Clone)]
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
        let start = tokens.clone();

        let mut curr = Self::parse_unary(tokens)?;
        while let Some(token) = tokens.front() {
            match token {
                Token::LParen => return Err(()),
                Token::RParen => {
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
            Token::LParen => {
                let ret = Self::parse_full(tokens, 0);
                assert_eq!(tokens.pop_front(), Some(Token::RParen));
                ret
            },
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

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::And(u, v) => {
                write!(f, "({:?} & {:?})", u, v)
            },
            Expr::Or(u, v) => {
                write!(f, "({:?} | {:?})", u, v)
            },
            Expr::Not(u) => {
                write!(f, "!{:?}", u)
            }
            Expr::Var(c) => {
                write!(f, "{}", c)
            }
        }
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
                write!(f, "{} | {}", u, v)
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

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
struct Implicant {
    fixed_true: usize,
    fixed_false: usize,
}

impl Implicant {
    pub fn toggle(&self, index: usize) -> Implicant {
        Implicant {
            fixed_true: self.fixed_true ^ (1usize << index),
            fixed_false: self.fixed_false ^ (1usize << index),
        }
    }

    pub fn relax(&self, index: usize) -> Implicant {
        Implicant {
            fixed_true: self.fixed_true & !(1usize << index),
            fixed_false: self.fixed_false & !(1usize << index),
        }
    }

    pub fn contains(&self, min_term: usize) -> bool {
        min_term & self.fixed_true == self.fixed_true &&
            !min_term & self.fixed_false == self.fixed_false
    }
}

impl BooleanSimplifier {
    fn prime_implicants(num_var: usize, evals: &Vec<bool>) -> Vec<Implicant> {
        assert_eq!(evals.len(), (1usize << num_var));
        // all minterms

        let mut implicants = HashSet::new();
        let mask = (1usize << num_var) - 1;
        for (i, minterm) in evals.iter().enumerate() {
            if *minterm {
                implicants.insert(Implicant {
                    fixed_true: i,
                    fixed_false: !i & mask
                });
            }
        }


        let mut prime_implicants = Vec::new();

        while !implicants.is_empty() {
            let mut next_implicants = HashSet::new();

            for implicant in &implicants {
                let mut merged = false;
                for j in 0..num_var {
                    let toggled = implicant.toggle(j);
                    if implicants.contains(&toggled) {
                        next_implicants.insert(implicant.relax(j));
                        merged = true;
                    }
                }

                if !merged {
                    prime_implicants.push(*implicant);
                }
            }

            implicants = next_implicants;
        }

        prime_implicants
    }

    pub fn simplify(expr: Expr) -> Expr {
        // find variables and evaluations
        let mut variables = BTreeSet::new();
        expr.variables(&mut variables);
        let evaluations = expr.eval_all(&variables);

        // find prime implicants
        let primes = Self::prime_implicants(variables.len(), &evaluations);

        // try all possible prime implicants to see which subset is best
        // brute force
        let p = primes.len();
        let mut best = (1usize << p) - 1;

        for i in 0..(1usize << p) {
            if i.count_ones() > best.count_ones() {
                continue
            }

            let mut all = true;
            for j in 0..(1usize << variables.len()) {
                let mut found = false;
                for k in 0..p {
                    if (i & (1 << k)) != 0 && primes[k].contains(j) {
                        found = true;
                        break
                    }
                }

                if !found {
                    all = false;
                    break;
                }
            }

            if all {
                best = i;
            }
        }

        // create an product expression from
        // the ith prime implicant
        let product = |i: usize| {
            let prime = primes[i];

            let mut old = if prime.fixed_true == 0 && prime.fixed_false == 0 {
                // tautology
                // again should've included literals...
                Some(Expr::Or(
                    Box::new(Expr::Var('-')),
                    Box::new(Expr::Not(Box::new(Expr::Var('-'))))
                ))
            }
            else {
                None
            };

            for (vind, name) in variables.iter().enumerate() {
                let curr = if prime.fixed_true & (1usize << vind) != 0 {
                    Some(Expr::Var(*name))
                }
                else if prime.fixed_false & (1usize << vind) != 0 {
                    Some(Expr::Not(Box::new(Expr::Var(*name))))
                }
                else {
                    None
                };

                if let Some(curr) = curr {
                    old = if let Some(old) = old {
                        Some(Expr::And(Box::new(old), Box::new(curr)))
                    } else {
                        Some(curr)
                    }
                }
            }

            old.unwrap()
        };

        let mut expression = None;
        for i in 0..p {
            if best & (1usize << i) != 0 {
                let curr = product(i);
                expression = if let Some(old) = expression {
                    Some(Expr::Or(Box::new(old), Box::new(curr)))
                } else {
                    Some(curr)
                }
            }
        }
        expression.unwrap_or(
            // some contradiction
            // probably should've included literals 0 and 1 in hindsight
            Expr::And(
                Box::new(Expr::Var('-')),
                Box::new(Expr::Not(Box::new(Expr::Var('-')))),
            )
        )
    }
}

