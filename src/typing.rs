use std::{rc::Rc, cell::RefCell};

/// The Type Checker.
use crate::syntax;

// An element in the type checking context
#[derive(Debug)]
pub enum Binding {
    // A term variable binding
    VarExpr(syntax::Type),
    // A struct binding (definition)
    TyStruct(Vec<(syntax::Name, syntax::Type)>),
}

// #[derive(Default)]
// pub struct TypingConext {
//     current: Rc<Vec<(syntax::Name, Binding)>>,
//     previous: RefCell<Option<Rc<TypingConext>>>,
// }

// The typing context contains information about variable bindings
// and type declarations
// TODO; Use a nested environment!!
pub type TypingContext = Vec<(syntax::Name, Binding)>;



// This describes the subtyping relation between two types
#[derive(Debug, PartialEq, Eq)]
pub enum SubtypingRelation {
    Subsumes,
    Norelation,
}

// A type (checking) error
#[derive(Debug)]
pub enum Error {
    // Expected type A, but got B
    TypeMismatch(syntax::Type, syntax::Type),
    // We expected the type to have a particular shape
    // e.g sometimes we know we want a "function" type,
    // but we might not know the exact domain and codomain type.
    Expected(syntax::Type),
    // When calling a function, you provided an incorrect
    // number of arguments
    FunctionArgMismatch,
    // When constructing a struct, you provided an incorrent
    // number of arguments.
    StructArgMismatch,
    // A lambda expression has an incorrect number of arguments
    // based on the type it got from a type annotation.
    LambdaArgMismatch,
    // Cannot infer a type
    NeedsMoreTypeAnnonation,
    // You used an undefined variable.
    UnboundVariable,
    // You used an undefined type name.
    UnboundType,
    // We know the type of the struct and the field you used
    // is not present in the type.
    UnboundFieldName,
    // Error is too complex for the type checker to give a
    // good error message.
    Unknown,
}

// "Context" functions

// Put a (name, type) pair in the type checking context
pub fn assume_var_exp(ctx: &mut TypingContext, name: &syntax::Name, ty: &syntax::Type) {
    let binding = Binding::VarExpr(ty.clone());
    // Because we want lexical bindings
    ctx.insert(0, (name.clone(), binding));
}

// Put a (name, struct) definition in the type checking context
pub fn assume_ty_struct(
    ctx: &mut TypingContext,
    name: &syntax::Name,
    ty: &Vec<(syntax::Name, syntax::Type)>,
) {
    let binding = Binding::TyStruct(ty.clone());
    ctx.insert(0, (name.clone(), binding));
}

// Search the context for a specific struct decl
pub fn lookup_ty_struct<'a>(
    ctx: &'a TypingContext,
    name: &'a syntax::Name,
) -> Result<&'a Vec<(syntax::Name, syntax::Type)>, Error> {
    ctx.iter()
        .filter_map(|(x, binding)| match binding {
            Binding::TyStruct(body) => Some((x, body)),
            _ => None,
        })
        .find_map(|(x, ty)| if x == name { Some(ty) } else { None })
        .ok_or(Error::UnboundType)
}

// Search the context for a variable binding
pub fn lookup_var_expr<'a>(
    ctx: &'a TypingContext,
    name: &'a syntax::Name,
) -> Result<&'a syntax::Type, Error> {
    ctx.iter()
        .filter_map(|(x, binding)| match binding {
            Binding::VarExpr(ty) => Some((x, ty)),
            _ => None,
        })
        .find_map(|(x, ty)| if x == name { Some(ty) } else { None })
        .ok_or(Error::UnboundVariable)
}

// Given a context Ctx, a type A, and a type B, is A <: B in Ctx?
// or is A a subtype of B?
pub fn subsumes(ctx: &TypingContext, a: &syntax::Type, b: &syntax::Type) -> SubtypingRelation {
    use syntax::*;
    match (a, b) {
        // A base type is a subtype of itself
        (Type::Unit, Type::Unit) => SubtypingRelation::Subsumes,
        (Type::Natural, Type::Natural) => SubtypingRelation::Subsumes,
        (Type::Bool, Type::Bool) => SubtypingRelation::Subsumes,
        (Type::String, Type::String) => SubtypingRelation::Subsumes,

        // Covariance.
        (Type::List(t1), Type::List(t2)) => subsumes(ctx, t1, t2),

        // Arrow types are *contravariant* in their arguments and *covariant* in their return types:
        // "Be very lenient in what you accept and be very conservative in what you give out."
        (Type::Arrow(a1, a2), Type::Arrow(b1, b2)) => {
            // first ensure we have the same number of arguments
            if a1.len() != b1.len() {
                return SubtypingRelation::Norelation;
            }
            for (codom, dom) in b1.iter().zip(a1) {
                match subsumes(ctx, codom, dom) {
                    SubtypingRelation::Norelation => return SubtypingRelation::Norelation,
                    SubtypingRelation::Subsumes => continue,
                }
            }
            subsumes(ctx, a2, b2)
        }

        // For A <: B to hold for object types A and B, then A must, at least,
        // have *all* the fields present in B, and foreach field (bi: Bi) present
        // in B, A must have a corresponding (ai: Ai) where Ai <: Bi
        (Type::Struct(_, body1), Type::Struct(_, body2)) => {
            for (name, t1) in body2 {
                let field = body1
                    .iter()
                    .find_map(|(x, t2)| if x == name { Some(t2) } else { None });
                match field {
                    None => return SubtypingRelation::Norelation,
                    Some(t2) => match subsumes(ctx, t1, t2) {
                        SubtypingRelation::Subsumes => continue,
                        SubtypingRelation::Norelation => return SubtypingRelation::Norelation,
                    },
                }
            }
            SubtypingRelation::Subsumes
        }

        _ => SubtypingRelation::Norelation,
    }
}

// Inference mode
// e => T
// Infer the type of an expression
pub fn infer(ctx: &mut TypingContext, expr: &syntax::Expr) -> Result<syntax::Type, Error> {
    use syntax::*;
    match expr {
        Expr::Unit => Ok(Type::Unit),
        Expr::Natural(..) => Ok(Type::Natural),
        Expr::String(..) => Ok(Type::String),
        Expr::Variable(name) => {
            let ty = lookup_var_expr(ctx, &name)?;
            Ok(ty.clone())
        }
        Expr::Bool(..) => Ok(Type::Bool),
        Expr::Annotation(_, ty) => Ok(ty.clone()),

        Expr::Let(name, expr, body) => {
            let ty = infer(ctx, expr)?;
            assume_var_exp(ctx, name, &ty);
            infer(ctx, &body)
        }

        Expr::Application(f, args) => {
            let fty = infer(ctx, f)?;
            match fty {
                Type::Arrow(argsty, resultty) => {
                    // Check the arguments
                    if argsty.len() != args.len() {
                        return Err(Error::FunctionArgMismatch);
                    }

                    args.iter()
                        .zip(argsty)
                        .map(|(e, ty)| check(ctx, e, &ty))
                        .collect::<Result<(), _>>()?;

                    Ok(*resultty.clone())
                }
                // We can only perform "call" on a function type
                _ => Err(Error::Expected(Type::Arrow(
                    vec![Type::Unknown],
                    Box::new(Type::Unknown),
                ))),
            }
        }

        Expr::Primitive(op, left, right) => match op {
            Operator::Add | Operator::Sub | Operator::Mul | Operator::Div => {
                check(ctx, left, &Type::Natural)?;
                check(ctx, right, &Type::Natural)?;
                Ok(Type::Natural)
            }
            Operator::Lt | Operator::Gt => {
                check(ctx, left, &Type::Natural)?;
                check(ctx, right, &Type::Natural)?;
                Ok(Type::Bool)
            }
            Operator::Eq => {
                let leftty = infer(ctx, left)?;
                let rightty = infer(ctx, right)?;
                match subsumes(ctx, &leftty, &rightty) {
                    SubtypingRelation::Subsumes => Ok(Type::Bool),
                    SubtypingRelation::Norelation => Err(Error::TypeMismatch(leftty, rightty)),
                }
            }
        },

        Expr::Struct(name, body) => {
            let objectty = lookup_ty_struct(ctx, name)?.clone();
            if objectty.len() != body.len() {
                return Err(Error::StructArgMismatch);
            }

            // O(n^2)
            body.iter()
                .map(|(name, expr)| {
                    let entry = objectty
                        .iter()
                        .find_map(|(x, ty)| if x == name { Some(ty) } else { None });
                    match entry {
                        None => return Err(Error::UnboundFieldName),
                        Some(ty) => check(ctx, expr, ty),
                    }
                })
                .collect::<Result<(), _>>()?;

            Ok(Type::Struct(name.clone(), objectty))
        }

        Expr::List(..) | Expr::Lambda(..) | Expr::If(..) => Err(Error::NeedsMoreTypeAnnonation),

        // Only used internally
        Expr::Unknown => Ok(Type::Unknown),
    }
}

// Checking mode
// e <= T
// Check that an expression has a specific type
pub fn check(ctx: &mut TypingContext, expr: &syntax::Expr, ty: &syntax::Type) -> Result<(), Error> {
    use syntax::*;
    match expr {
        Expr::Lambda(args, body) => match ty {
            Type::Arrow(domain, codomain) => {
                if args.len() != domain.len() {
                    return Err(Error::LambdaArgMismatch);
                }
                for (name, ty) in args.iter().zip(domain) {
                    assume_var_exp(ctx, name, ty)
                }
                check(ctx, body, &codomain)
            }
            _ => Err(Error::Expected(Type::Arrow(
                vec![Type::Unknown],
                Box::new(Type::Unknown),
            ))),
        },

        Expr::List(exprs) => {
            for expr in exprs {
                check(ctx, expr, ty)?;
            }
            Ok(())
        }

        Expr::If(predicate, t, f) => {
            check(ctx, predicate, &Type::Bool)?;
            check(ctx, t, ty)?;
            check(ctx, f, ty)?;
            Ok(())
        }

        expr => {
            let inferty = infer(ctx, expr)?;
            // is the inferred type a subtype of the type we expect?
            match subsumes(ctx, &inferty, ty) {
                SubtypingRelation::Subsumes => Ok(()),
                SubtypingRelation::Norelation => Err(Error::TypeMismatch(inferty, ty.clone())),
            }
        }
    }
}

// fn typecheck_type

fn stdlib_typing_context() -> TypingContext {
    Vec::new()
}

fn process_toplevel(ctx: &mut TypingContext, def: &syntax::Def) -> Result<(), Error> {
    use syntax::*;
    match def {
        Def::Struct(name, ty) => {
            assume_ty_struct(ctx, &name, &ty);
            Ok(())
        }
        Def::Fun(name, expr) => {
            // We expect expr to be annotated with a type
            let ty = infer(ctx, &expr)?;
            assume_var_exp(ctx, &name, &ty);
            Ok(())
        }
    }
}

fn typecheck_program(program: &Vec<syntax::Def>) -> Result<(), Error> {
    let mut ctx = TypingContext::default();
    for def in program {
        process_toplevel(&mut ctx, def)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::*;

    #[test]
    fn test_subtyping() {
        let top = Type::Struct(String::from("Top"), vec![]);
        let person = Type::Struct(
            String::from("Person"),
            vec![(String::from("name"), Type::String)],
        );
        let ctx = Vec::new();
        let result = subsumes(&ctx, &person, &top);
        assert_eq!(
            result,
            SubtypingRelation::Subsumes,
            "Any struct is a subtype of the empty struct"
        )
    }
}
