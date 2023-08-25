use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub grammar);

#[test]
fn test() {
    let _input = r#" 
          "#;
    let _input2 = r#"
        function add (x : Natural, y : Natural) -> Natural {
            (function (b) -> { (b + y) })
        }
    "#;
    let input2 = r#"
        function add (x : Natural, y : Natural) -> Natural {
           function () -> { (Person { name: x }) }
        }

        struct Person { }
   "#;
    let _input3 = "(Person { })";
    let result = grammar::ProgramParser::new().parse(input2);
    println!("{:?}", result)
}