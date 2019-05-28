#![deny(missing_debug_implementations)]
#![warn(missing_copy_implementations)]

mod lexer;
mod pos;
mod token;

fn main() {
    env_logger::init();

    let input = r#"
        let x: string = "hello, world";
        let y: string = "this string has \"quotes\"";
        let z = "listicles:\n\t1. they suck";
        let z2 = "vim? is that you?\^J\^I-> yes";

        let w = "\65-1 steak sauce";

        // "\666"
        "\^J"
        "\n"
        "\t"
    "#;

    let lexer = lexer::Lexer::new(input);

    for tok in lexer {
        if let Ok(tok) = &tok {
            let start = tok.span.start.offset;
            let end = tok.span.end.offset;

            let original = &input[start..end];
            print!("{:?} ", original);
        }

        println!("{:?}", tok);
    }
}
