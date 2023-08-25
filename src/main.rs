mod parser;
mod repl;
mod syntax;
mod typing;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub grammar);

#[test]
fn test() {
    let _input = r#" 
          "#;
    let input2 = r#"
        let f = function (x, y) -> { [x, y] } {
            f (10, 20)
        }
    "#;
    let result = grammar::ExprParser::new().parse(input2);
    println!("{:?}", result)
}

fn main() {}
