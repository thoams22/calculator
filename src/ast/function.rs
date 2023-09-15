use std::fmt::{Display, Formatter};

use super::{math::is_perfect_power, ConstantKind, Expression};

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

    pub fn args_mut(&mut self) -> &mut Vec<Expression> {
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
    pub fn simplify(mut self) -> Expression {
        for expr in self.args_mut().iter_mut() {
            *expr = expr.clone().simplify();
        }

        match self {
            FunctionType::Predefined(kind, args) => match kind {
                PredefinedFunction::Ln => match &args[0] {
                    Expression::Number(1) => Expression::Number(0),
                    Expression::Constant(ConstantKind::E) => Expression::Number(1),
                    Expression::Exponentiation(expr) => {
                        if expr
                            .get_base()
                            .equal(&Expression::Constant(ConstantKind::E))
                        {
                            expr.get_exponent()
                        } else {
                            Expression::multiplication(
                                expr.get_exponent(),
                                Expression::Function(Box::new(FunctionType::Predefined(
                                    kind,
                                    vec![expr.get_base()],
                                ))),
                            )
                            .simplify()
                        }
                    }
                    _ => Expression::function(FunctionType::Predefined(kind, args)),
                },
                PredefinedFunction::Log => match &args[1] {
                    Expression::Number(1) => Expression::Number(0),
                    expr if expr.equal(&args[0]) => Expression::Number(1),
                    Expression::Number(num) => {
                        if let Expression::Number(num2) = &args[0] {
                            if let Some((base, exponenent)) = is_perfect_power(num) {
                                println!("{} {}", base, exponenent);
                                if base == *num2 {
                                    return Expression::Number(exponenent as i64);
                                }
                            }
                        }
                        Expression::function(FunctionType::Predefined(kind, args))
                    }
                    Expression::Exponentiation(expr) => {
                        if expr.get_base().equal(&args[0]) {
                            expr.get_exponent()
                        } else {
                            Expression::multiplication(
                                expr.get_exponent(),
                                Expression::Function(Box::new(FunctionType::Predefined(
                                    kind,
                                    vec![args[0].clone(), expr.get_base()],
                                ))),
                            )
                            .simplify()
                        }
                    }
                    _ => Expression::function(FunctionType::Predefined(kind, args)),
                },
                PredefinedFunction::Sqrt => match &args[0] {
                    Expression::Number(0) => Expression::Number(0),
                    Expression::Number(1) => Expression::Number(1),
                    Expression::Number(num) => {
                        if let Some((base, exponenent)) = is_perfect_power(num) {
                            if exponenent == 2_u32 {
                                return Expression::Number(base);
                            }
                        }
                        Expression::function(FunctionType::Predefined(kind, args))
                    }
                    Expression::Exponentiation(expr) => {
                        if let Expression::Number(num) = expr.get_exponent() {
                            if num % 2 == 0 {
                                return Expression::function(FunctionType::Predefined(
                                    PredefinedFunction::Abs,
                                    vec![expr.get_base()],
                                ));
                            }
                        }
                        Expression::function(FunctionType::Predefined(kind, args))
                    }
                    _ => Expression::function(FunctionType::Predefined(kind, args)),
                },
                PredefinedFunction::Root => match &args[1] {
                    Expression::Number(0) => Expression::Number(1),
                    Expression::Number(1) => args[0].clone(),
                    Expression::Number(num) => {
                        if let Expression::Number(num2) = &args[0] {
                            if let Some((base, exponenent)) = is_perfect_power(num) {
                                if exponenent == *num2 as u32 {
                                    return Expression::Number(base);
                                }
                            }
                        }
                        Expression::function(FunctionType::Predefined(kind, args))
                    }
                    Expression::Exponentiation(expr) => {
                        if let Expression::Number(num) = expr.get_exponent() {
                            if let Expression::Number(root_num) = args[1] {
                                if num % root_num == 0 {
                                        return Expression::function(FunctionType::Predefined(
                                            PredefinedFunction::Abs,
                                            vec![expr.get_base()],
                                        ));
                                }
                            }
                        }
                        Expression::function(FunctionType::Predefined(kind, args))
                    }
                    _ => Expression::function(FunctionType::Predefined(kind, args)),
                },
                // PredefinedFunction::Sin => match args[0] {
                //     Expression::Number(0) => Expression::Number(0),
                //     Expression::Number(90) => Expression::Number(1),
                //     Expression::Number(180) => Expression::Number(0),
                //     Expression::Number(270) => Expression::Number(-1),
                //     Expression::Number(360) => Expression::Number(0),
                //     _ => Expression::function(FunctionType::Predefined(kind, args)),
                // },
                // PredefinedFunction::Cos => match args[0] {
                //     Expression::Number(0) => Expression::Number(1),
                //     Expression::Number(90) => Expression::Number(0),
                //     Expression::Number(180) => Expression::Number(-1),
                //     Expression::Number(270) => Expression::Number(0),
                //     Expression::Number(360) => Expression::Number(1),
                //     _ => Expression::function(FunctionType::Predefined(kind, args)),
                // },
                // PredefinedFunction::Tan => match args[0] {
                //     Expression::Number(0) => Expression::Number(0),
                //     Expression::Number(180) => Expression::Number(0),
                //     Expression::Number(360) => Expression::Number(0),
                //     _ => Expression::function(FunctionType::Predefined(kind, args)),
                // },
                // PredefinedFunction::Asin => match args[0] {
                //     Expression::Number(0) => Expression::Number(0),
                //     Expression::Number(1) => Expression::Number(90),
                //     Expression::Number(-1) => Expression::Number(-90),
                //     _ => Expression::function(FunctionType::Predefined(kind, args)),
                // },
                // PredefinedFunction::Acos => match args[0] {
                //     Expression::Number(0) => Expression::Number(90),
                //     Expression::Number(1) => Expression::Number(0),
                //     Expression::Number(-1) => Expression::Number(180),
                //     _ => Expression::function(FunctionType::Predefined(kind, args)),
                // },
                // PredefinedFunction::Atan => match args[0] {
                //     Expression::Number(0) => Expression::Number(0),
                //     _ => Expression::function(FunctionType::Predefined(kind, args)),
                // },
                PredefinedFunction::Abs => match args[0] {
                    Expression::Number(num) => Expression::Number(num.abs()),
                    _ => Expression::function(FunctionType::Predefined(kind, args)),
                },
                _ => Expression::function(FunctionType::Predefined(kind, args)),
            },
            FunctionType::UserDefined(_, _) => Expression::Function(Box::new(self)),
        }
    }
}

impl Display for FunctionType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionType::Predefined(fun, args) => {
                write!(f, "{fun}(")?;
                for arg in args {
                    write!(f, "{arg}, ")?;
                }
                write!(f, ")")
            },
            FunctionType::UserDefined(fun, args) => {
                write!(f, "{fun}(")?;
                for arg in args {
                    write!(f, "{arg}, ")?;
                }
                write!(f, ")")
            },
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum PredefinedFunction {
    // 1 args
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Ln,
    Sqrt,
    Abs,
    // 2 args
    Log,
    Root,
}

impl PredefinedFunction {
    pub fn args_count(&self) -> usize {
        match self {
            PredefinedFunction::Sin
            | PredefinedFunction::Cos
            | PredefinedFunction::Tan
            | PredefinedFunction::Asin
            | PredefinedFunction::Acos
            | PredefinedFunction::Atan
            | PredefinedFunction::Ln
            | PredefinedFunction::Sqrt
            | PredefinedFunction::Abs => 1,
            PredefinedFunction::Log | PredefinedFunction::Root => 2,
        }
    }

    pub fn all() -> Vec<&'static str> {
        vec![
            "asin", "acos", "atan", "sqrt", "root", "sin", "cos", "tan", "abs", "log", "ln",
        ]
    }

    pub fn get_function(name: &str) -> Option<Self> {
        match name {
            "sin" => Some(PredefinedFunction::Sin),
            "cos" => Some(PredefinedFunction::Cos),
            "tan" => Some(PredefinedFunction::Tan),
            "asin" => Some(PredefinedFunction::Asin),
            "acos" => Some(PredefinedFunction::Acos),
            "atan" => Some(PredefinedFunction::Atan),
            "ln" => Some(PredefinedFunction::Ln),
            "sqrt" => Some(PredefinedFunction::Sqrt),
            "abs" => Some(PredefinedFunction::Abs),
            "log" => Some(PredefinedFunction::Log),
            "root" => Some(PredefinedFunction::Root),
            _ => None,
        }
    }
}

impl Display for PredefinedFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PredefinedFunction::Sin => write!(f, "sin"),
            PredefinedFunction::Cos => write!(f, "cos"),
            PredefinedFunction::Tan => write!(f, "tan"),
            PredefinedFunction::Asin => write!(f, "asin"),
            PredefinedFunction::Acos => write!(f, "acos"),
            PredefinedFunction::Atan => write!(f, "atan"),
            PredefinedFunction::Ln => write!(f, "ln"),
            PredefinedFunction::Sqrt => write!(f, "sqrt"),
            PredefinedFunction::Abs => write!(f, "abs"),
            PredefinedFunction::Log => write!(f, "log"),
            PredefinedFunction::Root => write!(f, "root"),
        }
    }
}
