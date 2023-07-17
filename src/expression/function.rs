// use super::Expression;

// #[derive(PartialEq, Debug, Clone)]
// pub struct Function {
//     first: Expression,
//     second: Expression,
// }

// impl Function {
//     pub fn new(first: Expression, second: Expression) -> Self {
//         Self {
//             first,
//             second,
//         }
//     }

//     pub fn simplify(self) -> Expression {
//         let simplified_first = self.first.simplify();
//         let simplified_second = self.second.simplify();
//         match (&simplified_first, &simplified_second) {
//             (Expression::Number(num1), Expression::Number(num2)) => Expression::Number(num1.powf(*num2)),
//             _ => Expression::Function(Box::new(Function::new(simplified_first, simplified_second))),
//         }
//     }
// }