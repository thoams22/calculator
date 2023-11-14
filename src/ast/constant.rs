use std::fmt::{Display, Formatter};

use super::{Expr, Expression};

#[derive(PartialEq, Debug, Clone, Copy, Hash, Eq)]
pub enum ConstantKind {
    E,
    Pi,
    Tau,
}

impl ConstantKind {
    pub fn as_text(&self) -> &str {
        match self {
            ConstantKind::E => "e",
            ConstantKind::Pi => "pi",
            ConstantKind::Tau => "tau",
        }
    }

    pub fn as_f64(&self) -> f64 {
        match self {
            ConstantKind::E => std::f64::consts::E,
            ConstantKind::Pi => std::f64::consts::PI,
            ConstantKind::Tau => std::f64::consts::TAU,
        }
    }

    pub fn get_all() -> Vec<ConstantKind> {
        vec![ConstantKind::E, ConstantKind::Pi, ConstantKind::Tau]
    }
}
impl Expr for ConstantKind {
    fn equal(&self, other: &ConstantKind) -> bool {
        self == other
    }

    fn contain_vars(&self) -> Option<std::collections::HashMap<super::varibale::Variable, usize>> {
        None
    }

    fn contain_var(&self, _variable: &super::varibale::Variable) -> bool {
        false
    }

    fn simplify(self) -> Expression {
       Expression::Constant(self)
    }

    fn get_order(&self) -> i64 {
        1
    }

    fn print_tree(&self, span: Option<&str>) {
        println!("{}{self}", span.unwrap_or(""));
        }

    fn calc_pos(
        &self,
        position: &mut Vec<(String, (i8, i8))>,
        prev_state: super::State,
        _memoized: &mut std::collections::HashMap<super::Expression, (i8, i8, i8)>,
    ) {
        position.push((self.as_text().to_string(), prev_state.get_pos()))
    }

    fn get_length(&self, _memoized: &mut std::collections::HashMap<super::Expression, (i8, i8, i8)>) -> i8 {
        self.as_text().len() as i8    
    }

    fn get_height(&self,_memoized: &mut std::collections::HashMap<super::Expression, (i8, i8, i8)>) -> i8 {
        1
    }

    fn get_above_height(&self, _memoized: &mut std::collections::HashMap<super::Expression, (i8, i8, i8)>) -> i8 {
        1
    }
}

impl Display for ConstantKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstantKind::E => write!(f, "e"),
            ConstantKind::Pi => write!(f, "pi"),
            ConstantKind::Tau => write!(f, "tau"),
        }
    }
}