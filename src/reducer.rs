use crate::tokenizer::Instruction;

pub fn reduction(tokens: Vec<Instruction>) -> Vec<Vec<Instruction>> {
    // let mut reduced: Vec<Instruction> = Vec::new();
    let mut group: Vec<Vec<Instruction>> = Vec::new();
    let mut buffer: Vec<Instruction> = Vec::new();

    for token in tokens {
        match token {
            Instruction::Addition | Instruction::Substraction => {
                let len = buffer.len();
                if len >= 2 {
                    group.push(buffer.drain(len - 2..).collect());
                } else if len == 1 {
                    if let Some(last_group) = group.last_mut() {
                        last_group.extend(buffer.drain(..));
                    }
                }
                group.push(vec![token])
            }
            Instruction::Division | Instruction::Multiplication => {
                let len = buffer.len();
                if len >= 2 {
                    buffer.push(token);
                    group.push(buffer.drain(len - 2..).collect());
                } else if len == 1 {
                    if let Some(last_group) = group.last_mut() {
                        buffer.push(token);
                        last_group.extend(buffer.drain(..));
                    }
                } else {
                    group.push(vec![token])
                }
            }
            Instruction::Number(_) | Instruction::Variable(_) => buffer.push(token),
            _ => {}
        }
    }

    // group.iter().for_each(
    //     |grp| reduced.extend(grp.iter())
    // );

    group
    // reduced
}

pub fn reduce_add(tokens: Vec<Vec<Instruction>>) -> Vec<Instruction> {

    // [[Variable('a'), Variable('b')], [Addition]]
    match tokens.len() {
        1 => panic!("Not enough argument in reduce_add"),
        2 => {
            match tokens.get(0).unwrap_or(&Vec::new()).len() {
                0 => panic!("First argument empty"),
                1 => panic!("Not enough argument in first argument"),
                2 => {
                    let first_arg = tokens.get(0).unwrap(); 
                    match (first_arg.get(0), first_arg.get(0)) {
                        (None, None) => todo!(),
                        (None, Some(_)) => todo!(),
                        (Some(_), None) => todo!(),
                        (Some(Instruction::Variable(x)), Some(Instruction::Variable(y))) => {
                            if x == y {
                                return vec![Instruction::Number(2.0),Instruction::Variable(*x), Instruction::Multiplication];
                            } else {
                                return vec![Instruction::Variable(*x),Instruction::Variable(*y), Instruction::Addition];
                            }
                        },
                        (Some(_), Some(_))=> todo!(), 
                    }
                }
            }
        }
        // 3 => {}
        _ => panic!("Too much argument in reduce_add")
    }
    
}

// pub fn reduce_mult(tokens: Vec<Vec<Instruction>>) -> Vec<Vec<Instruction>> {

// }



#[cfg(test)]
mod tests_reduction {
    use super::reduction;
    use crate::tokenizer::{Function, Instruction};

    #[test]
    fn diff_exp() {
        let mut result = reduction(vec![
            Instruction::Variable('a'),
            Instruction::Variable('b'),
            Instruction::Substraction,
            Instruction::Variable('a'),
            Instruction::Variable('b'),
            Instruction::Addition,
            Instruction::Multiplication,
        ]);
        assert_eq!(
            result,
            vec![vec![
                Instruction::Variable('a'),
                Instruction::Number(2.0),
                Instruction::Exponentiation,
                Instruction::Variable('b'),
                Instruction::Number(2.0),
                Instruction::Exponentiation,
                Instruction::Substraction,
            ]],
            "diff_exp 1"
        );

        result = reduction(vec![
            Instruction::Variable('a'),
            Instruction::Variable('a'),
            Instruction::Multiplication,
            Instruction::Variable('b'),
            Instruction::Substraction,
            Instruction::Variable('a'),
            Instruction::Variable('a'),
            Instruction::Multiplication,
            Instruction::Variable('b'),
            Instruction::Addition,
            Instruction::Multiplication,
        ]);
        assert_eq!(
            result,
            vec![vec![
                Instruction::Variable('a'),
                Instruction::Number(4.0),
                Instruction::Exponentiation,
                Instruction::Variable('b'),
                Instruction::Number(2.0),
                Instruction::Exponentiation,
                Instruction::Substraction,
            ]],
            "diff_exp 2"
        );

        result = reduction(vec![
            Instruction::Number(16.0),
            Instruction::Number(2.0),
            Instruction::Multiplication,
            Instruction::Number(3.0),
            Instruction::Multiplication,
            Instruction::Number(3.0),
            Instruction::Number(2.0),
            Instruction::Exponentiation,
            Instruction::Multiplication,
            Instruction::Number(5.0),
            Instruction::Number(4.0),
            Instruction::Multiplication,
            Instruction::Number(4.0),
            Instruction::Multiplication,
            Instruction::Addition,
            Instruction::Number(5.0),
            Instruction::Addition,
        ]);
        assert_eq!(
            result,
            vec![vec![
                Instruction::Number(949.0),
            ]],
            "diff_exp 3"
        );

        result = reduction(vec![
            Instruction::Number(16.0),
            Instruction::Number(4.0),
            Instruction::Function(Function::Sqrt),
            Instruction::Multiplication,
            Instruction::Number(8.0),
            Instruction::Function(Function::Log2),
            Instruction::Multiplication,
            Instruction::Number(8.0),
            Instruction::Function(Function::Log2),
            Instruction::Number(4.0),
            Instruction::Function(Function::Sqrt),
            Instruction::Exponentiation,
            Instruction::Multiplication,
            Instruction::Number(5.0),
            Instruction::Number(4.0),
            Instruction::Multiplication,
            Instruction::Number(4.0),
            Instruction::Multiplication,
            Instruction::Addition,
            Instruction::Number(5.0),
            Instruction::Addition,
        ]);
        assert_eq!(
            result,
            vec![vec![
                Instruction::Number(16.0),
                Instruction::Number(4.0),
                Instruction::Function(Function::Sqrt),
                Instruction::Multiplication,
                Instruction::Number(8.0),
                Instruction::Function(Function::Log2),
                Instruction::Multiplication,
                Instruction::Number(8.0),
                Instruction::Function(Function::Log2),
                Instruction::Number(4.0),
                Instruction::Function(Function::Sqrt),
                Instruction::Exponentiation,
                Instruction::Multiplication,
                Instruction::Number(85.0),
                Instruction::Addition,
            ]],
            "diff_exp 4"
        );
    }
}
