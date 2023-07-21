use crate::tokenizer::CalcError;

use super::Expression;

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
}

// constructor
impl Function {
    pub fn sqrt(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Sqrt,
        }
    }

    pub fn ln(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Ln,
        }
    }

    pub fn log2(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Log2,
        }
    }

    pub fn log10(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Log10,
        }
    }

    pub fn sin(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Sin,
        }
    }

    pub fn cos(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Cos,
        }
    }
    pub fn tan(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Tan,
        }
    }
    pub fn atan(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Atan,
        }
    }
    pub fn acos(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Acos,
        }
    }
    pub fn asin(expr: Expression) -> Self {
        Self {
            sub_expr: vec![expr],
            function: Functions::Asin,
        }
    }
    pub fn log(base: Expression, expr: Expression) -> Self {
        Self {
            sub_expr: vec![base, expr],
            function: Functions::Log,
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

    pub fn swap_remove(&mut self, index: usize) {
        self.sub_expr.swap_remove(index);
    }

    pub fn remove(&mut self, index: usize) {
        self.sub_expr.remove(index);
    }

    pub fn add(&mut self, other: Expression) {
        self.sub_expr.push(other)
    }

    pub fn insert(&mut self, index: usize, other: Expression) {
        self.sub_expr.insert(index, other)
    }

    pub fn equal(&self, other: &Expression) -> bool {
        if let Expression::Addition(_) = other {
            let mut count = 0;
            'i: for i in 0..self.len() {
                for j in 0..other.len() {
                    if self.sub_expr[i].equal(&other.get(j).unwrap()) {
                        count += 1;
                        continue 'i;
                    }
                }
                return false;
            }

            return count == self.len();
        }
        false
    }
    pub fn simplify(self) -> Expression {
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
                                result = argument[0].log(number);
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
        let mut result: String = String::from("(");
        let mut argument: Vec<String> = Vec::new();
        
        for ele in &self.sub_expr {
            match ele.to_string() {
                Ok(number) => {
                    match self.function {
                        Functions::Ln => result += &format!("ln({number})"),
                        Functions::Log2 => result += &format!("log2({number})"),
                        Functions::Log10 => result += &format!("log10({number})"),
                        Functions::Log => {
                            if argument.is_empty() {
                                argument.push(number);
                            } else {
                                result += &format!("log({}{number})", argument[0]);
                            }
                        }
                        Functions::Sqrt => result += &format!("sqrt({number})"),
                        Functions::Sin => result += &format!("sin({number})"),
                        Functions::Cos => result += &format!("cos({number})"),
                        Functions::Tan => result += &format!("tan({number})"),
                        Functions::Asin => result += &format!("asin({number})"),
                        Functions::Acos => result += &format!("acos({number})"),
                        Functions::Atan => result += &format!("atan({number})"),
                    };
                }
                Err(err) => return Err(err),
            }
        }

        result += ")";
        Ok(result)
    } 

}
