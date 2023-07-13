pub enum Expression {
    Number(Number),
    Multiplication(Multiplication),
}

trait Expr {
    fn simplify(&self) -> Expression;
}

#[derive(PartialEq, PartialOrd, Debug, Clone, Copy)]
struct Number {
    value: f64,
}

impl Number {
    fn new(value: f64) -> Self {
        Self { value }
    }
}

impl Expr for Number {
    fn simplify(&self) -> Expression {
        Expression::Number(*self)
    }
}

struct Multiplication {
    first: Box<Expression>,
    second: Box<Expression>,
}

impl Multiplication {
    fn new(first: Expression, second: Expression) -> Self {
        Self {
            first: Box::new(first),
            second: Box::new(second),
        }
    }
}

// impl Expr for Multiplication {
//     fn simplify(&self) -> Expression {
//         let first = *self.first;
//         let second = *self.second;
//         match first {
//             Expression::Number(first_num) => {
//                 match second {
//                     Expression::Number(second_num) => return Expression::Number(Number::new(first_num.value*second_num.value)),
//                     Expression::Multiplication(second_mult) => {
//                         let result = second_mult.simplify();
//                         match result {
//                             Expression::Number(second_num) => return Expression::Number(Number::new(first_num.value*second_num.value)),
//                             Expression::Multiplication(_) => return result,
//                         }
//                     },
//                 }
//             },
//             Expression::Multiplication(first_mult) => todo!(),
//         }
//     }
// }
