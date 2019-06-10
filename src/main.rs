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
        "let in 42 end",
        "let in end",
        "let in 42; 43; 45 end",
        "let var x := 42 in x end",
        "let var y: integer := 42 in y end",
        r#"
            let
                var prefix: string := "hello"
                var suffix := "world"
            in
                concat(
                    concat(prefix, ", "),
                    suffix
                )
            end
        "#,
        r#"
        let
            function get_message(): string = "hello, world"
        in
            print(get_message())
        end
        "#,
        r#"
            let
                function concat3(x: string, y: string, z: string): string =
                    concat(x, concat(y, z))

                var prefix := "hello"
                var suffix := "world"
            in
                print(concat3(prefix, ", ", world))
            end
        "#,
        r#"
            let
                type any = { any: string }
                type any_array = array of any
                type other_any_array = any_array
            in
                any_array[3] of any { any: "any" }
            end
        "#,
        "let type dupes = { x: x, x: y } in end",
        "let function dupes(x: x, x: y) = ()"
    ];

    for input in inputs {
        let mut parser = parser::Parser::new(input);

        println!("{}\n  {:#?}", input, parser.parse_program())
    }
}
