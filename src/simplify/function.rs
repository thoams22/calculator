use crate::ast::function::PredefinedFunction;

use super::Expression;

#[derive(PartialEq, Debug, Clone)]
pub enum FunctionType {
    Predefined(PredefinedFunction, Vec<Expression>),
    UserDefined(String, Vec<Expression>),
}

impl FunctionType {
    pub fn args(&self) -> &Vec<Expression> {
        match self {
            FunctionType::Predefined(_, args) => args,
            FunctionType::UserDefined(_, args) => args,
        }
    }

    pub fn name(&self) -> String {
        match self {
            FunctionType::Predefined(kind, _) => kind.to_string(),
            FunctionType::UserDefined(name, _) => name.to_string(),
        }
    }

    pub fn args_count(&self) -> usize {
        match self {
            FunctionType::Predefined(kind, _) => kind.args_count(),
            FunctionType::UserDefined(_, args) => args.len(),
        }
    }

    pub fn is_logarithm(&self) -> bool {
        match self {
            FunctionType::Predefined(kind, _) => {
                matches!(kind, PredefinedFunction::Ln | PredefinedFunction::Log)
            }
            FunctionType::UserDefined(_, _) => false,
        }
    }

    pub fn is_root(&self) -> bool {
        match self {
            FunctionType::Predefined(kind, _) => {
                matches!(kind, PredefinedFunction::Sqrt | PredefinedFunction::Root)
            }
            FunctionType::UserDefined(_, _) => false,
        }
    }

    pub fn is_trigonometric(&self) -> bool {
        match self {
            FunctionType::Predefined(kind, _) => matches!(
                kind,
                PredefinedFunction::Sin
                    | PredefinedFunction::Cos
                    | PredefinedFunction::Tan
                    | PredefinedFunction::Asin
                    | PredefinedFunction::Acos
                    | PredefinedFunction::Atan
            ),
            FunctionType::UserDefined(_, _) => false,
        }
    }
}

impl FunctionType {
    pub fn equal(&self, other: &FunctionType) -> bool {
        match (self, other) {
            (
                FunctionType::Predefined(type_1, args_1),
                FunctionType::Predefined(type_2, args_2),
            ) => {
                if type_1 == type_2 && args_1.len() == args_2.len() {
                    for (arg_1, arg_2) in args_1.iter().zip(args_2.iter()) {
                        if !arg_1.equal(arg_2) {
                            return false;
                        }
                    }
                    return true;
                }
                false
            }
            (
                FunctionType::UserDefined(name_1, args_1),
                FunctionType::UserDefined(name_2, args_2),
            ) => {
                if name_1 == name_2 && args_1.len() == args_2.len() {
                    for (arg_1, arg_2) in args_1.iter().zip(args_2.iter()) {
                        if !arg_1.equal(arg_2) {
                            return false;
                        }
                    }
                    return true;
                }
                false
            }
            (_, _) => false,
        }
    }
}

impl FunctionType {
    pub fn simplify(self) -> Expression {
        for expr in self.args().clone().iter_mut() {
            *expr = expr.clone().simplify();
        }

        match self {
            FunctionType::Predefined(kind, args) => match kind {
                PredefinedFunction::Ln => match &args[0] {
                    Expression::Number(1) => Expression::Number(0),
                    Expression::Constant(crate::ast::ConstantKind::E) => Expression::Number(1),
                    Expression::Exponentiation(expr) => {
                        if expr
                            .get_base()
                            .equal(&Expression::Constant(crate::ast::ConstantKind::E))
                        {
                            expr.get_exponent()
                        } else {
                            Expression::multiplication(
                                expr.get_exponent(),
                                Expression::Function(Box::new(FunctionType::Predefined(
                                    kind,
                                    vec![expr.get_base()],
                                )))
                                .simplify(),
                            )
                        }
                    }
                    _ => Expression::function(FunctionType::Predefined(kind, args)),
                },
                PredefinedFunction::Log => match &args[1] {
                    Expression::Number(1) => Expression::Number(0),
                    expr if expr.equal(&args[0]) => Expression::Number(1),
                    Expression::Exponentiation(expr) => Expression::multiplication(
                        expr.get_exponent(),
                        Expression::Function(Box::new(FunctionType::Predefined(
                            kind,
                            vec![expr.get_base()],
                        )))
                        .simplify(),
                    ),
                    _ => Expression::function(FunctionType::Predefined(kind, args)),
                },
                PredefinedFunction::Sqrt => match &args[0] {
                    Expression::Number(0) => Expression::Number(0),
                    Expression::Number(1) => Expression::Number(1),
                    Expression::Exponentiation(expr) => {
                        if let Expression::Number(num) = expr.get_exponent() {
                            if num % 2 == 0 {
                                Expression::function(FunctionType::Predefined(
                                    PredefinedFunction::Abs,
                                    vec![expr.get_base()],
                                ))
                            } else {
                                Expression::function(FunctionType::Predefined(kind, args))
                            }
                        } else {
                            Expression::function(FunctionType::Predefined(kind, args))
                        }
                    }
                    _ => Expression::function(FunctionType::Predefined(kind, args)),
                },
                PredefinedFunction::Root => match &args[1] {
                    Expression::Number(0) => Expression::Number(1),
                    Expression::Number(1) => args[0].clone(),
                    Expression::Exponentiation(expr) => {
                        if let Expression::Number(num) = expr.get_exponent() {
                            if let Expression::Number(root_num) = args[1] {
                                if num % root_num == 0 {
                                    if num % 2 == 0 {
                                        Expression::function(FunctionType::Predefined(
                                            PredefinedFunction::Abs,
                                            vec![expr.get_base()],
                                        ))
                                    } else {
                                        Expression::function(FunctionType::Predefined(kind, args))
                                    }
                                } else {
                                    Expression::function(FunctionType::Predefined(kind, args))
                                }
                            } else {
                                Expression::function(FunctionType::Predefined(kind, args))
                            }
                        } else {
                            Expression::function(FunctionType::Predefined(kind, args))
                        }
                    }
                    _ => Expression::function(FunctionType::Predefined(kind, args)),
                },
                PredefinedFunction::Sin => match args[0] {
                    Expression::Number(0) => Expression::Number(0),
                    Expression::Number(90) => Expression::Number(1),
                    Expression::Number(180) => Expression::Number(0),
                    Expression::Number(270) => Expression::Number(-1),
                    Expression::Number(360) => Expression::Number(0),
                    _ => Expression::function(FunctionType::Predefined(kind, args)),
                },
                PredefinedFunction::Cos => match args[0] {
                    Expression::Number(0) => Expression::Number(1),
                    Expression::Number(90) => Expression::Number(0),
                    Expression::Number(180) => Expression::Number(-1),
                    Expression::Number(270) => Expression::Number(0),
                    Expression::Number(360) => Expression::Number(1),
                    _ => Expression::function(FunctionType::Predefined(kind, args)),
                },
                PredefinedFunction::Tan => match args[0] {
                    Expression::Number(0) => Expression::Number(0),
                    Expression::Number(180) => Expression::Number(0),
                    Expression::Number(360) => Expression::Number(0),
                    _ => Expression::function(FunctionType::Predefined(kind, args)),
                },
                PredefinedFunction::Asin => match args[0] {
                    Expression::Number(0) => Expression::Number(0),
                    Expression::Number(1) => Expression::Number(90),
                    Expression::Number(-1) => Expression::Number(-90),
                    _ => Expression::function(FunctionType::Predefined(kind, args)),
                },
                PredefinedFunction::Acos => match args[0] {
                    Expression::Number(0) => Expression::Number(90),
                    Expression::Number(1) => Expression::Number(0),
                    Expression::Number(-1) => Expression::Number(180),
                    _ => Expression::function(FunctionType::Predefined(kind, args)),
                },
                PredefinedFunction::Atan => match args[0] {
                    Expression::Number(0) => Expression::Number(0),
                    _ => Expression::function(FunctionType::Predefined(kind, args)),
                },
                PredefinedFunction::Abs => match args[0] {
                    Expression::Number(num) => Expression::Number(num.abs()),
                    _ => Expression::function(FunctionType::Predefined(kind, args)),
                },
            },
            FunctionType::UserDefined(_, _) => Expression::Function(Box::new(self)),
        }
    }
}
