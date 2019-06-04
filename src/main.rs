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
        "(nil; break; 42; -37; -(1; 2))",
        r#"for x := 1 to 2 do "oops""#,
        r#"while "lol" do "fart""#,
        "foo()",
        "bar(1)",
        "baz(1, 2)",
        "quux(f(), g(), h())",
        "foo.bar[baz]",
        "xyz[42] of 33",
        "xyz[42].foo",
    ];

    for input in inputs {
        let mut parser = parser::Parser::new(input);

        println!("{}\n  {:#?}", input, parser.parse_program())
    }
}
