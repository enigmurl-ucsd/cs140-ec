use std::io::stdin;
use crate::parser::{BooleanSimplifier, Lexer, Parser};

mod parser;

fn main() {
    println!("Enter boolean expression to convert to POS.");
    println!("Example: A & ((!C | C) & B) | G -> A & B | G");
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();

    let tokens = Lexer::lex(&input);
    let expr = Parser::parse(tokens).unwrap();
    let simplified = BooleanSimplifier::simplify(expr);

    println!("SOP form: {}", simplified);
}
