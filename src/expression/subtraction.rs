use super::Expression;

#[derive(PartialEq, Debug, Clone)]
pub struct Subtraction {
    sub_expr: Vec<Expression>,
}

impl Subtraction {
    pub fn new(first: Expression, second: Expression) -> Self {
        let sub_expr: Vec<Expression> = vec![first, second];
        Self { sub_expr }
    }
    pub fn size(&self) -> usize {
        self.sub_expr.len()
    }

    pub fn from_vec(sub_expr: Vec<Expression>) -> Self {
        Self { sub_expr }
    }

    pub fn get(&self, index: usize) -> Option<&Expression> {
        self.sub_expr.get(index)
    }

    pub fn remove(&mut self, index: usize) {
        self.sub_expr.remove(index);
    }

    pub fn equal(&self, other: &Expression) -> bool {
        if let Expression::Subtraction(_) = other {
            let mut count = 0;
            'i: for i in 0..self.size() {
                for j in 0..other.size() {
                    if self.sub_expr[i].equal(&other.get(j).unwrap()) {
                        count += 1;
                        continue 'i;
                    }
                }
                return false;
            }

            if count == self.size() {
                return true;
            } else {
                return false;
            }
        }
        false
    }

    pub fn swap_remove(&mut self, index: usize){
        self.sub_expr.swap_remove(index);
    }
    pub fn simplify(self) -> Expression {
        let simplified_first = self.sub_expr.get(0).unwrap().clone().simplify();
        let simplified_second = self.sub_expr.get(1).unwrap().clone().simplify();
        match (&simplified_first, &simplified_second) {
            (Expression::Number(num1), Expression::Number(num2)) => Expression::Number(num1 - num2),
            (Expression::Variable(var1), Expression::Variable(var2)) => {
                if var1 == var2 {
                    Expression::Number(0.0)
                } else {
                    Expression::Subtraction(Box::new(Subtraction::new(
                        simplified_first,
                        simplified_second,
                    )))
                }
            }
            _ => Expression::Subtraction(Box::new(Subtraction::new(
                simplified_first,
                simplified_second,
            ))),
        }
    }
}
