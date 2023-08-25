pub type Name = String;

#[derive(Debug)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Lt,
    Eq,
}

// A type
#[derive(Clone, Debug)]
pub enum Type {
    // Unit (commonly known as "void" in C-based PLs)
    Unit,
    // A natural number
    Natural,
    // String
    String,
    // Bool
    Bool,
    // List[X]
    List(Box<Type>),
    // The type for functions
    // e.g (Int, Int) -> Bool
    Arrow(Vec<Type>, Box<Type>),
    // e.g Student { name: String }
    Struct(Name, Vec<(Name, Type)>),
    // A place holder for error messages (and should only be used internally)
    Unknown,
}

// An expression
#[derive(Debug)]
pub enum Expr {
    // e.g ()
    Unit,
    // e.g x, y, foo
    Variable(Name),
    // e.g 0, 2, 3
    Natural(u32),
    // e.g "foo", "bar"
    String(String),
    // e.g true, false
    Bool(bool),
    // e.g (true : Bool)
    Annotation(Box<Expr>, Type),
    // e.g let name = "foo"; length (name)
    Let(Name, Box<Expr>, Box<Expr>),
    // e.g [1, 3, 3]
    List(Vec<Expr>),
    // if true { ... } else { ... }
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    // e.g function (x, y) { x + y }
    Lambda(Vec<Name>, Box<Expr>),
    // Also called "function call": e.g add (10, 12)
    Application(Box<Expr>, Vec<Expr>),
    // e.g 1 + 2, x < 10
    Primitive(Operator, Box<Expr>, Box<Expr>),
    // e.g Student { name: "foo"}
    Struct(Name, Vec<(Name, Expr)>),
    // Also meant to be used internally. Can be useful for
    // creating "dummy" placeholders while developing / debugging.
    Unknown,
}

// Top level definitions
#[derive(Debug)]
pub enum Def {
    // Top level struct declaration
    Struct(Name, Vec<(Name, Type)>),
    // Top level function
    Fun(Name, Expr),
}
