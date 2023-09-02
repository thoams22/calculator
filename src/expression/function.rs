use crate::tokenizer::CalcError;

use super::{math::is_perfect_power, Expression};

#[derive(PartialEq, PartialOrd, Debug, Clone, Copy)]
pub enum Functions {
    Ln,
    Log2,
    Log10,
    Log,
    Sqrt,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
}

#[derive(PartialEq, PartialOrd, Debug, Clone, Copy)]
pub enum FunctionsType {
    Logarithm,
    Root,
    Trigonometric,
}

impl Functions {
    pub fn argument(&self) -> u8 {
        match self {
            Functions::Ln
            | Functions::Log2
            | Functions::Log10
            | Functions::Sqrt
            | Functions::Sin
            | Functions::Cos
            | Functions::Tan
            | Functions::Asin
            | Functions::Acos
            | Functions::Atan => 1,
            Functions::Log => 2,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Function {
    sub_expr: Vec<Expression>,
    function: Functions,
    function_type: FunctionsType,
}

// constructor
impl Function {
    pub fn sqrt(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Sqrt,
            function_type: FunctionsType::Root,
        }
    }

    pub fn ln(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Ln,
            function_type: FunctionsType::Logarithm,
        }
    }

    pub fn log2(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Log2,
            function_type: FunctionsType::Logarithm,
        }
    }

    pub fn log10(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Log10,
            function_type: FunctionsType::Logarithm,
        }
    }

    pub fn sin(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Sin,
            function_type: FunctionsType::Trigonometric,
        }
    }

    pub fn cos(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Cos,
            function_type: FunctionsType::Trigonometric,
        }
    }
    pub fn tan(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Tan,
            function_type: FunctionsType::Trigonometric,
        }
    }
    pub fn atan(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Atan,
            function_type: FunctionsType::Trigonometric,
        }
    }
    pub fn acos(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Acos,
            function_type: FunctionsType::Trigonometric,
        }
    }
    pub fn asin(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Asin,
            function_type: FunctionsType::Trigonometric,
        }
    }
    pub fn log(base: Expression, expr: Expression) -> Self {
        Self {
            sub_expr: vec![base, expr],
            function: Functions::Log,
            function_type: FunctionsType::Logarithm,
        }
    }
}

impl Function {
    pub fn len(&self) -> usize {
        self.function.argument().into()
    }

    pub fn get(&self, index: usize) -> Option<&Expression> {
        self.sub_expr.get(index)
    }

    pub fn get_function(&self) -> Functions {
        self.function
    }

    pub fn get_type(&self) -> FunctionsType {
        self.function_type
    }

    /// log(a, b) => Some(a)
    ///
    /// sqrt(a) => Some(2.0)
    ///
    /// other => None
    pub fn get_base(&self) -> Option<Expression> {
        match self.function_type {
            FunctionsType::Logarithm => match self.function {
                Functions::Ln => Some(Expression::e()),
                Functions::Log10 => Some(Expression::Number(10.0)),
                Functions::Log2 => Some(Expression::Number(2.0)),
                Functions::Log => Some(self.sub_expr[0].clone()),
                _ => None,
            },
            FunctionsType::Root => match self.function {
                Functions::Sqrt => Some(Expression::Number(2.0)),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn get_expression(&self) -> Expression {
        match self.function {
            Functions::Log => self.sub_expr[1].clone(),
            _ => self.sub_expr[0].clone(),
        }
    }

    pub fn swap_remove(&mut self, index: usize) -> bool {
        if index > self.function.argument().into() {
            return false;
        }
        self.sub_expr.swap_remove(index);
        true
    }

    pub fn remove(&mut self, index: usize) -> bool {
        if index > self.function.argument().into() {
            return false;
        }
        self.sub_expr.remove(index);
        true
    }

    pub fn insert(&mut self, index: usize, other: Expression) -> bool {
        if index > self.function.argument().into() {
            return false;
        }
        self.sub_expr.insert(index, other);
        true
    }

    pub fn equal(&self, other: &Expression) -> bool {
        if let Expression::Function(func) = other {
            if func.get_function() == self.get_function() && self.len() == other.len() {
                let len = self.len();
                let mut index: Vec<bool> = vec![false; len * 2];
                'i: for i in 0..len {
                    for j in 0..len {
                        if self.sub_expr[i].equal(&other.get(j).unwrap())
                            && !(index[i] || index[j + len])
                        {
                            index[i] = true;
                            index[j + len] = true;
                            continue 'i;
                        }
                    }
                    return false;
                }
                return index.iter().all(|&x| x);
            }
        }
        false
    }

    pub fn simplify(mut self) -> Expression {
        for expression in self.sub_expr.iter_mut() {
            *expression = expression.clone().simplify();
        }
        match self.function_type {
            FunctionsType::Logarithm => self.simplify_logarithm(),
            _ => Expression::Function(Box::new(self)),
        }
    }

    fn simplify_logarithm(self) -> Expression {
        match self.function {
            Functions::Log | Functions::Ln | Functions::Log10 | Functions::Log2 => {
                if self.get_base().unwrap().equal(&self.get_expression()) {
                    return Expression::Number(1.0);
                } else if self.get_expression() == Expression::Number(1.0) {
                    return Expression::Number(0.0);
                } else if let Expression::Number(num) = &self.get_expression() {
                    if let Some((root, exponent)) = is_perfect_power(num) {
                        if Expression::Number(root) == self.get_base().unwrap() {
                            return Expression::Number(exponent as f64);
                        } else {
                            return Expression::multiplication(
                                Expression::Number(exponent as f64),
                                Expression::Function(Box::new(Function {
                                    sub_expr: vec![Expression::Number(root)],
                                    function: self.function,
                                    function_type: self.function_type,
                                })),
                            );
                        }
                    }
                } else if let Expression::Exponentiation(expr) = &self.get_expression() {
                    if expr.get_base() == self.get_base().unwrap() {
                        return expr.get_exponent();
                    } else if let Expression::Number(num) = expr.get_base() {
                        if let Some((root, exponent)) = is_perfect_power(&num) {
                            if Expression::Number(root) == self.get_base().unwrap() {
                                return Expression::multiplication(
                                    expr.get_exponent(),
                                    Expression::Number(exponent as f64),
                                )
                                .simplify();
                            } else {
                                return Expression::multiplication_from_vec(vec![
                                    Expression::Number(exponent as f64),
                                    expr.get_exponent(),
                                    Expression::Function(Box::new(Function {
                                        sub_expr: vec![Expression::Number(root)],
                                        function: self.function,
                                        function_type: self.function_type,
                                    })),
                                ])
                                .simplify();
                            }
                        }
                    }
                }
            }
            _ => {}
        }
        Expression::Function(Box::new(self))
    }

    pub fn evaluate(&self) -> Result<f64, CalcError> {
        let mut argument: Vec<f64> = Vec::new();
        let mut result: f64 = 0.0;
        for ele in &self.sub_expr {
            match ele.evaluate() {
                Ok(number) => {
                    match self.function {
                        Functions::Ln => result = number.ln(),
                        Functions::Log2 => result = number.log2(),
                        Functions::Log10 => result = number.log10(),
                        Functions::Log => {
                            if argument.is_empty() {
                                argument.push(number);
                            } else {
                                result = number.log(argument[0]);
                            }
                        }
                        Functions::Sqrt => result = number.sqrt(),
                        Functions::Sin => result = number.sin(),
                        Functions::Cos => result = number.cos(),
                        Functions::Tan => result = number.tan(),
                        Functions::Asin => result = number.asin(),
                        Functions::Acos => result = number.acos(),
                        Functions::Atan => result = number.atan(),
                    };
                }
                Err(err) => return Err(err),
            }
        }
        Ok(result)
    }

    pub fn to_string(&self) -> Result<String, CalcError> {
        let mut result: String = String::new();
        let mut argument: Vec<String> = Vec::new();

        match self.function {
            Functions::Ln => result += "ln(",
            Functions::Log2 => result += "log2(",
            Functions::Log10 => result += "log10(",
            Functions::Log => result += "log(",
            Functions::Sqrt => result += "sqrt(",
            Functions::Sin => result += "sin(",
            Functions::Cos => result += "cos(",
            Functions::Tan => result += "tan(",
            Functions::Asin => result += "asin(",
            Functions::Acos => result += "acos(",
            Functions::Atan => result += "atan(",
        };

        for ele in &self.sub_expr {
            match ele.to_string() {
                Ok(number) => {
                    argument.push(number);
                }
                Err(err) => return Err(err),
            }
        }
        result += &argument.join(", ");

        result += ")";
        Ok(result)
    }
}

pub fn is_function(token: &str) -> Option<Functions> {
    match token {
        "ln" => Some(Functions::Ln),
        "log2" => Some(Functions::Log2),
        "log10" => Some(Functions::Log10),
        "log" => Some(Functions::Log),
        "sqrt" => Some(Functions::Sqrt),
        "sin" => Some(Functions::Sin),
        "cos" => Some(Functions::Cos),
        "tan" => Some(Functions::Tan),
        "asin" => Some(Functions::Asin),
        "acos" => Some(Functions::Acos),
        "atan" => Some(Functions::Atan),
        _ => None,
    }
}
