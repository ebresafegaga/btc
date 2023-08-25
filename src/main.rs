use parser::grammar;

mod parser;
mod repl;
mod syntax;
mod typing;

fn main() {
    let source = "";
    let ast = grammar::ProgramParser::new().parse(source);
    
}
