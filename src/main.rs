#![deny(missing_debug_implementations, future_incompatible)]
#![warn(missing_copy_implementations)]

mod expression;
mod lexer;
mod parser;
mod pos;
mod token;

fn main() {
    env_logger::init();

    let inputs = &[
        "42",
        "nil",
        "break",
        r#""hello, world""#,
        "-42",
        "-nil",
        "-break",
        r#"-"hello, world""#,
        "(42)",
        "(nil)",
        "(break)",
        "(nil; break; 42; -37; -(1; 2))"
    ];

    for input in inputs {
        let mut parser = parser::Parser::new(input);

        println!("{}\n  {:#?}", input, parser.parse())
    }
}
