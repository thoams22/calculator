use std::{fmt::{Display, Formatter}, collections::HashMap};

use super::{ConstantKind, Expression, Expr, varibale::Variable, State};
use crate::utils::is_perfect_power;

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
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

impl Expr for FunctionType {
    fn equal(&self, other: &FunctionType) -> bool {
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

    fn simplify(mut self) -> Expression {
        for expr in self.args_mut().iter_mut() {
            *expr = expr.clone().simplify();
        }

        match self {
            FunctionType::Predefined(kind, args) => match kind {
                PredefinedFunction::Ln => match &args[0] {
                    Expression::Number(num) if num.sub_expr == 1 => Expression::number(0),
                    Expression::Constant(ConstantKind::E) => Expression::number(1),
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
                    Expression::Number(num) if num.sub_expr == 1 => Expression::number(0),
                    expr if expr.equal(&args[0]) => Expression::number(1),
                    Expression::Number(num) => {
                        if let Expression::Number(num2) = &args[0] {
                            if let Some((base, exponenent)) = is_perfect_power(&num.sub_expr) {
                                println!("{} {}", base, exponenent);
                                if base == num2.sub_expr {
                                    return Expression::number(exponenent as i64);
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
                    Expression::Number(num) if num.sub_expr == 0 => Expression::number(0),
                    Expression::Number(num) if num.sub_expr == 1 => Expression::number(1),
                    Expression::Number(num) => {
                        if let Some((base, exponenent)) = is_perfect_power(&num.sub_expr) {
                            if exponenent == 2_u32 {
                                return Expression::number(base);
                            }
                        }
                        Expression::function(FunctionType::Predefined(kind, args))
                    }
                    Expression::Exponentiation(expr) => {
                        if let Expression::Number(num) = expr.get_exponent() {
                            if num.sub_expr % 2 == 0 {
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
                    Expression::Number(num) if num.sub_expr == 0 => Expression::number(1),
                    Expression::Number(num) if num.sub_expr == 1=> args[0].clone(),
                    Expression::Number(num) => {
                        if let Expression::Number(num2) = &args[0] {
                            if let Some((base, exponenent)) = is_perfect_power(&num.sub_expr) {
                                if exponenent == num2.sub_expr as u32 {
                                    return Expression::number(base);
                                }
                            }
                        }
                        Expression::function(FunctionType::Predefined(kind, args))
                    }
                    Expression::Exponentiation(expr) => {
                        if let Expression::Number(num) = expr.get_exponent() {
                            if let Expression::Number(root_num) = &args[1] {
                                if num.sub_expr % root_num.sub_expr == 0 {
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
                PredefinedFunction::Abs => match &args[0] {
                    Expression::Number(num) => Expression::number(num.sub_expr.abs()),
                    _ => Expression::function(FunctionType::Predefined(kind, args)),
                },
                _ => Expression::function(FunctionType::Predefined(kind, args)),
            },
            FunctionType::UserDefined(_, _) => Expression::Function(Box::new(self)),
        }
    }

    fn contain_vars(&self) -> Option<std::collections::HashMap<super::varibale::Variable, usize>> {
        match self {
            FunctionType::Predefined(_, args) | FunctionType::UserDefined(_, args) => {
                let mut map: HashMap<Variable, usize> = HashMap::new();
                for expr in args.iter() {
                    if let Some(mut sub_map) = expr.contain_vars() {
                        for (key, value) in sub_map.iter_mut() {
                            if let Some(occurence) = map.get_mut(key) {
                                *occurence += *value;
                            } else {
                                map.insert(key.clone(), *value);
                            }
                        }
                    }
                }
                if map.is_empty() {
                    None
                } else {
                    Some(map)
                }
            }
        }
    }

    fn contain_var(&self, variable: &super::varibale::Variable) -> bool {
        match self {
            FunctionType::Predefined(_, args) | FunctionType::UserDefined(_, args) => {
                args.iter().any(|expr| expr.contain_var(variable))
            }
        }
    }

    fn get_order(&self) -> i64 {
        3
    }

    fn print_tree(&self, span: Option<&str>) {
        let current_span = span.unwrap_or("");
        let new_span = current_span.to_string() + "   ";
        println!("{}Function : {}", current_span, self.name());
        for arg in self.args() {
            print!("{}", current_span);
            arg.print_tree(Some(&new_span));
        }
    }

    fn calc_pos(
        &self,
        position: &mut Vec<(String, (i8, i8))>,
        prev_state: super::State,
        memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>,
    ) {
        let (mut pos_y, mut pos_x) = match prev_state {
            State::Over(pos_y, pos_x) => (
                pos_y + self.get_height(memoized) - self.get_above_height(memoized),
                pos_x,
            ),
            State::Under(pos_y, pos_x) => {
                (pos_y - self.get_above_height(memoized) + 1, pos_x)
            }
            State::Same(pos_y, pos_x) => (pos_y, pos_x),
        };

        // Modify the position to be at the middle of the function
        position.push((self.name(), (pos_y, pos_x)));
        pos_x += self.name().len() as i8;

        self.make_parenthesis(&mut pos_y, &mut pos_x, position, false, memoized);

        for (i, arg) in self.args().iter().enumerate() {
            if i != 0 {
                position.push((", ".to_string(), (pos_y, pos_x)));
                pos_x += 2;
            }
            arg.calc_pos(position, State::Same(pos_y, pos_x), memoized);
            pos_x += arg.get_length(memoized);
        }
        self.make_parenthesis(&mut pos_y, &mut pos_x, position, true, memoized);
    }

    fn get_length(&self, memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        let mut length = self.name().len() as i8;
        for arg in self.args() {
            length += arg.get_length(memoized) + 2;
        }
        length
    }

    fn get_height(&self, memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        let mut max_height = 0;
        for arg in self.args() {
            max_height = max_height.max(arg.get_height(memoized));
        }
        max_height
    }

    fn get_above_height(&self, memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        let mut max_height = 0;
        for expr in self.args() {
            max_height = max_height.max(expr.get_above_height(memoized));
        }
        max_height
    }
}

impl Display for FunctionType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionType::Predefined(fun, _) => {
                write!(f, "{fun}(")?;
                for (i, arg) in self.args().iter().enumerate() {
                    if i == 0 {
                        write!(f, "{arg}")?;
                    } else {
                        write!(f, ", {arg}")?;
                    }
                }
                write!(f, ")")
            }
            FunctionType::UserDefined(fun, _) => {
                write!(f, "{fun}(")?;
                for (i, arg) in self.args().iter().enumerate() {
                    if i == 0 {
                        write!(f, "{arg}")?;
                    } else {
                        write!(f, ", {arg}")?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
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
