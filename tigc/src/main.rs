#![deny(missing_debug_implementations, future_incompatible)]
#![warn(missing_copy_implementations)]

use tiger_syntax::Parser;

fn main() {
    let inputs = &[
        "x := nil"
    ];

    for input in inputs {
        let mut parser = Parser::new(input);

        println!("{}\n  {:#?}", input, parser.parse_program())
    }
}
