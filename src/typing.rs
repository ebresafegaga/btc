use crate::syntax;

pub enum Binding {
    // A term variable binding
    TermVariable(syntax::Type),
    // An object binding
    TypeVariable(Vec<(syntax::Name, syntax::Type)>),
}

pub type TypingCtx = Vec<(syntax::Name, Binding)>;

pub enum SubtypingRelation {
    Subsumes,
    Norelation,
}

pub enum Error {
    TypeMismatch(syntax::Type, syntax::Type),
    Expected(syntax::Type),
    FunctionArgMismatch,
    ObjectArgMismatch,
    LambdaArgMismatch,
    NeedsMoreTypeAnnonation,
    UnboundVariable,
    UnboundType,
    UnboundFieldName,
    Unknown,
}

// "Context" functions

pub fn assume_term_variable(ctx: &mut TypingCtx, name: &syntax::Name, ty: &syntax::Type) {
    let binding = Binding::TermVariable(ty.clone());
    ctx.insert(0, (name.clone(), binding));
}

pub fn lookup_type_variable<'a>(
    ctx: &'a TypingCtx,
    name: &'a syntax::Name,
) -> Result<&'a Vec<(syntax::Name, syntax::Type)>, Error> {
    ctx.iter()
        .filter_map(|(x, binding)| match binding {
            Binding::TypeVariable(body) => Some((x, body)),
            _ => None,
        })
        .find_map(|(x, ty)| if x == name { Some(ty) } else { None })
        .ok_or(Error::UnboundType)
}

pub fn lookup_term_variable<'a>(
    ctx: &'a TypingCtx,
    name: &'a syntax::Name,
) -> Result<&'a syntax::Type, Error> {
    ctx.iter()
        .filter_map(|(x, binding)| match binding {
            Binding::TermVariable(ty) => Some((x, ty)),
            _ => None,
        })
        .find_map(|(x, ty)| if x == name { Some(ty) } else { None })
        .ok_or(Error::UnboundVariable)
}

// Given a context Ctx, a type A, and a type B, is A <: B in Ctx?
pub fn subsumes(ctx: &TypingCtx, a: &syntax::Type, b: &syntax::Type) -> SubtypingRelation {
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
        // "Be as lenient in what you accept and be as conservative in what you give out."
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
        (Type::Object(_, body1), Type::Object(_, body2)) => {
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
pub fn infer(ctx: &mut TypingCtx, expr: &syntax::Expr) -> Result<syntax::Type, Error> {
    use syntax::*;
    match expr {
        Expr::Unit => Ok(Type::Unit),
        Expr::Natural(..) => Ok(Type::Natural),
        Expr::String(..) => Ok(Type::String),
        Expr::Variable(name) => {
            let ty = lookup_term_variable(ctx, &name)?;
            Ok(ty.clone())
        }
        Expr::Bool(..) => Ok(Type::Bool),
        Expr::Annotation(_, ty) => Ok(ty.clone()),

        Expr::Let(name, expr, body) => {
            let ty = infer(ctx, expr)?;
            assume_term_variable(ctx, name, &ty);
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

        Expr::Object(name, body) => {
            let objectty = lookup_type_variable(ctx, name)?.clone();
            if objectty.len() != body.len() {
                return Err(Error::ObjectArgMismatch);
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

            Ok(Type::Object(name.clone(), objectty))
        }

        Expr::List(..) | Expr::Lambda(..) | Expr::If(..) => Err(Error::NeedsMoreTypeAnnonation),
    }
}

// Checking mode
// e <= T
pub fn check(ctx: &mut TypingCtx, expr: &syntax::Expr, ty: &syntax::Type) -> Result<(), Error> {
    use syntax::*;
    match expr {
        Expr::Lambda(args, body) => match ty {
            Type::Arrow(domain, codomain) => {
                if args.len() != domain.len() {
                    return Err(Error::LambdaArgMismatch);
                }
                for (name, ty) in args.iter().zip(domain) {
                    assume_term_variable(ctx, name, ty)
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
