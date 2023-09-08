use std::fmt::{Formatter, Display};

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
}

impl Display for FunctionType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionType::Predefined(kind, args) => {
                write!(f, "{}(", kind)?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg)?;
                    if i != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            FunctionType::UserDefined(name, args) => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg)?;
                    if i != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
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

#[derive(PartialEq, Debug, Clone)]
pub struct Function {
    pub function_type: FunctionType,
}

// Constructor
impl Function {
    pub fn new(function_type: FunctionType) -> Self {
        Self { function_type }
    }

    pub fn new_predefined_function(kind: PredefinedFunction, args: Vec<Expression>) -> Self {
        Self {
            function_type: FunctionType::Predefined(kind, args),
        }
    }

    pub fn new_user_defined_function(name: String, args: Vec<Expression>) -> Self {
        Self {
            function_type: FunctionType::UserDefined(name, args),
        }
    }
}

impl Function {
    pub fn simplify(self) -> Expression {
        todo!()
    }
}