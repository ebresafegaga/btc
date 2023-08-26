use std::fmt;

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
    // This is a variable that references a concerete type.
    // This is not a unification type variable or a universally quantified
    // type variable or an existentially quantified type variable.
    // It is used to name a previously defiend type
    // e.g (String, Natural) -> Student
    // where `Student` is a struct
    Named(Name),
    // A place holder for error messages (and should only be used internally)
    Unknown,
}

// A simple pretty printer for types
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "()"),
            Type::Natural => write!(f, "Natural"),
            Type::String => write!(f, "String"),
            Type::Bool => write!(f, "Bool"),
            Type::Named(name) => write!(f, "{}", name),
            Type::Unknown => write!(f, "?"),

            Type::List(ty) => {
                write!(f, "List[")?;
                ty.fmt(f)?;
                write!(f, "]")
            }

            Type::Arrow(domains, codomain) => {
                write!(f, "(")?;
                for domain in domains {
                    domain.fmt(f)?;
                    // this prints an extra , after the last type
                    write!(f, ", ")?;
                }
                write!(f, ")")?;
                write!(f, " -> ")?;
                codomain.fmt(f)
            }

            Type::Struct(name, fields) => {
                write!(f, "{}", name)?;
                write!(f, "{{")?;

                for (name, ty) in fields {
                    write!(f, "{}", name)?;
                    write!(f, " ")?;
                    ty.fmt(f)?;
                }
                
                write!(f, "}}")
            }
        }
    }
}

#[test]
fn test() {
    let t = Type::List(Box::new(Type::String));
    println!("{}", t)
}

// A source location
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Loc {
    // A location in the source program
    Known { start: usize, end: usize },
    // An internally generated location used for desugaring
    // syntax. The the location it contains is the closest
    // loc. We can keep also keep track of information that
    // tells us if the position comes afer or before loc.
    Generated { start: usize, end: usize },
}

impl Loc {
    pub fn new(start: usize, end: usize) -> Loc {
        Loc::Known { start, end }
    }

    pub fn generate(&self) -> Loc {
        match *self {
            Loc::Known { start, end } => Loc::Generated { start, end },
            Loc::Generated { start, end } => Loc::Generated { start, end },
        }
    }
}

// An (non recursive) expression
#[derive(Debug)]
pub enum ExprOne<T> {
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
    Annotation(Box<T>, Type),
    // e.g let name = "foo"; length (name)
    Let(Name, Box<T>, Box<T>),
    // e.g [1, 3, 3]
    List(Vec<T>),
    // if true { ... } else { ... }
    If(Box<T>, Box<T>, Box<T>),
    // e.g function (x, y) { x + y }
    Lambda(Vec<Name>, Box<T>),
    // Also called "function call": e.g add (10, 12)
    Application(Box<T>, Vec<T>),
    // e.g 1 + 2, x < 10
    Primitive(Operator, Box<T>, Box<T>),
    // e.g Student { name: "foo"}
    Struct(Name, Vec<(Name, T)>),
    // e.g person.name
    StructIndex(Box<T>, Name),
    // Also meant to be used internally. Can be useful for
    // creating "dummy" placeholders while developing / debugging.
    Unknown,
}

// A full expression decorated with a location
#[derive(Debug)]
pub struct Expr(pub Loc, pub ExprOne<Expr>);

// Top level definitions
#[derive(Debug)]
pub enum Def {
    // Top level struct declaration
    Struct(Name, Vec<(Name, Type)>),
    // Top level function
    Fun(Name, Expr),
}
