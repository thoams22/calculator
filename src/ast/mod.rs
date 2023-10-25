pub mod addition;
pub mod equality;
pub mod exponentiation;
pub mod fraction;
pub mod function;
pub mod multiplication;
pub mod negation;
pub mod varibale;

use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use self::addition::Addition;
use self::equality::Equality;
use self::exponentiation::Exponentiation;
use self::fraction::Fraction;
use self::function::FunctionType;
use self::multiplication::Multiplication;
use self::negation::Negation;
use self::varibale::Variable;

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Simplify(Expression),
    Solve(Expression),
    SolveFor(Expression, Variable),
    Replace(Expression, Equality),
    Error,
}

impl Statement {
    pub fn print_console(&self) {
        let mut position: Vec<(String, (i8, i8))> = Vec::new();

        // memoize (length, height, above_height)
        let mut memoize: HashMap<Expression, (i8, i8, i8)> = HashMap::new();

        match self {
            Statement::Simplify(expr) => expr.calc_pos(&mut position, State::Same(0, 0), &mut memoize),
            Statement::Solve(expr) => expr.calc_pos(&mut position, State::Same(0, 0), &mut memoize),
            Statement::SolveFor(expr, var) => {
                let mut length = expr.get_length(&mut memoize);
                expr.calc_pos(&mut position, State::Same(0, 0), &mut memoize);
                Expression::Variable(Variable::new(",".to_string())).calc_pos(&mut position, State::Same(0, length), &mut memoize);
                length += 2;
                Expression::Variable(var.clone()).calc_pos(&mut position, State::Same(0, length), &mut memoize);
            }
            Statement::Replace(expr, eq) => {
                let mut length = expr.get_length(&mut memoize);
                expr.calc_pos(&mut position, State::Same(0, 0), &mut memoize);
                Expression::Variable(Variable::new(",".to_string())).calc_pos(&mut position, State::Same(0, length), &mut memoize);
                length += 2;
                Expression::Equality(Box::new(eq.clone()))
                    .calc_pos(&mut position, State::Same(0, length), &mut memoize);
            }
            Statement::Error => panic!("there should'nt be error in the ast"),
        }

        let (min_x, max_x, min_y, max_y) = position.iter().fold(
            (0, 0, 0, 0),
            |(min_x, max_x, min_y, max_y), &(_, (y, x))| {
                (min_x.min(x), max_x.max(x), min_y.min(y), max_y.max(y))
            },
        );

        let mut grid = HashMap::new();

        // Populate the grid with characters
        for (character, (y, x)) in position {
            grid.insert((y, x), character);
        }

        // Print the grid
        for y in (min_y..=max_y).rev() {
            let mut skip = 0;
            for x in min_x..=max_x {
                if skip > 0 {
                    skip -= 1;
                    continue;
                }
                if let Some(character) = grid.get(&(y, x)) {
                    print!("{}", character);
                    skip = character.len() as i8 - 1;
                } else {
                    print!(" ");
                }
            }
            println!();
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Simplify(expr) => write!(f, "{}", expr),
            Statement::Solve(expr) => write!(f, "{}", expr),
            Statement::SolveFor(expr, var) => write!(f, "{}, {}", expr, var),
            Statement::Replace(expr, eq) => write!(f, "{}, {}", expr, eq),
            Statement::Error => write!(f, "Error"),
        }
    }
}

pub trait Expr {
    fn equal(&self, other: &Expression) -> bool;
    fn contain_vars(&self) -> Option<HashMap<Variable, usize>>;
    fn contain_var(&self, variable: &Variable) -> bool;
    
    fn simplify(self) -> Expression;
    fn get_order(&self) -> u8;
    
    fn print_console(&self);
    fn calc_pos(&self, position: &mut Vec<(String, (i8, i8))>, prev_state: State, memoized: &mut HashMap<Expression, (i8, i8, i8)>);
    fn get_length(&self, memoized: &mut HashMap<Expression, (i8, i8, i8)>) -> i8;
    fn get_height(&self, memoized: &mut HashMap<Expression, (i8, i8, i8)>) -> i8;
    fn get_above_height(&self, memoized: &mut HashMap<Expression, (i8, i8, i8)>) -> i8;
}

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub enum Expression {
    Number(i64),
    Variable(Variable),
    Constant(ConstantKind),
    Addition(Box<Addition>),
    Multiplication(Box<Multiplication>),
    Exponentiation(Box<Exponentiation>),
    Fraction(Box<Fraction>),
    Equality(Box<Equality>),
    Negation(Box<Negation>),
    Function(Box<FunctionType>),
    Error,
}

// Helper constructor
impl Expression {
    pub fn addition(left: Expression, right: Expression) -> Expression {
        Expression::Addition(Box::new(Addition::new(left, right)))
    }

    pub fn addition_from_vec(sub_expr: Vec<Expression>) -> Expression {
        Expression::Addition(Box::new(Addition::from_vec(sub_expr)))
    }

    pub fn multiplication(left: Expression, right: Expression) -> Expression {
        Expression::Multiplication(Box::new(Multiplication::new(left, right)))
    }

    pub fn multiplication_from_vec(sub_expr: Vec<Expression>) -> Expression {
        Expression::Multiplication(Box::new(Multiplication::from_vec(sub_expr)))
    }

    pub fn fraction(numerator: Expression, denominator: Expression) -> Expression {
        Expression::Fraction(Box::new(Fraction::new(numerator, denominator)))
    }

    pub fn negation(operand: Expression) -> Expression {
        Expression::Negation(Box::new(Negation::new(operand)))
    }

    pub fn function(function: FunctionType) -> Expression {
        Expression::Function(Box::new(function))
    }

    pub fn exponentiation(base: Expression, exponent: Expression) -> Expression {
        Expression::Exponentiation(Box::new(Exponentiation::new(base, exponent)))
    }

    pub fn equality(left: Expression, right: Expression) -> Expression {
        Expression::Equality(Box::new(Equality::new(left, right)))
    }

    pub fn variable(var: String) -> Expression {
        Expression::Variable(Variable::new(var))
    }
}

// use generic and impl trait to reduce the following ?
impl Expression {
    pub fn equal(&self, other: &Expression) -> bool {
        match (self, other) {
            (Expression::Number(left), Expression::Number(right)) => left == right,
            (Expression::Variable(left), Expression::Variable(right)) => left.equal(right),
            (Expression::Constant(left), Expression::Constant(right)) => left.equal(right),
            (Expression::Addition(left), Expression::Addition(right)) => left.equal(right),
            (Expression::Multiplication(left), Expression::Multiplication(right)) => {
                left.equal(right)
            }
            (Expression::Exponentiation(left), Expression::Exponentiation(right)) => {
                left.equal(right)
            }
            (Expression::Fraction(left), Expression::Fraction(right)) => left.equal(right),
            (Expression::Equality(left), Expression::Equality(right)) => left.equal(right),
            (Expression::Negation(left), Expression::Negation(right)) => left.equal(right),
            (Expression::Function(left), Expression::Function(right)) => left.equal(right),
            _ => false,
        }
    }

    pub fn simplify(self) -> Expression {
        match self {
            Expression::Addition(add) => add.simplify(),
            Expression::Multiplication(mult) => mult.simplify(),
            Expression::Exponentiation(expo) => expo.simplify(),
            Expression::Fraction(frac) => frac.simplify(),
            Expression::Equality(eq) => eq.simplify(),
            Expression::Negation(neg) => neg.simplify(),
            Expression::Function(func) => func.simplify(),
            Expression::Error => panic!("There should be no error in the expression tree"),
            _ => self,
        }
    }

    pub fn get_order(&self) -> i64 {
        match self {
            Expression::Number(_) => 0,
            Expression::Variable(_) => 1,
            Expression::Constant(_) => 1,
            Expression::Addition(add) => add
                .sub_expr
                .iter()
                .map(|expr| expr.get_order())
                .max()
                .unwrap_or(0),
            Expression::Multiplication(mult) => mult
                .sub_expr
                .iter()
                .map(|expr| expr.get_order())
                .max()
                .unwrap_or(0),
            Expression::Exponentiation(expo) => {
                if let Expression::Number(num) = expo.get_exponent() {
                    num
                } else {
                    2
                }
            }
            Expression::Fraction(frac) => frac
                .get_numerator()
                .get_order()
                .max(frac.get_denominator().get_order()),
            Expression::Equality(eq) => eq
                .sub_expr
                .iter()
                .map(|expr| expr.get_order())
                .max()
                .unwrap_or(0),
            Expression::Negation(neg) => neg.sub_expr.get_order(),
            Expression::Function(_) => 3,
            Expression::Error => panic!("There should be no error in the expression tree"),
        }
    }

    pub fn contain_vars(&self) -> Option<HashMap<Variable, usize>> {
        match self {
            Expression::Number(_) => None,
            Expression::Variable(var) => {
                let mut map = HashMap::new();
                map.insert(var.clone(), 1);
                Some(map)
            }
            Expression::Constant(_) => None,
            Expression::Addition(add) => {
                let mut map: HashMap<Variable, usize> = HashMap::new();
                for expr in add.sub_expr.iter() {
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
            Expression::Multiplication(mult) => {
                let mut map: HashMap<Variable, usize> = HashMap::new();
                for expr in mult.sub_expr.iter() {
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
            Expression::Exponentiation(expo) => {
                let mut map: HashMap<Variable, usize> = HashMap::new();
                for expr in expo.sub_expr.iter() {
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
            Expression::Fraction(frac) => {
                let mut map: HashMap<Variable, usize> = HashMap::new();
                for expr in frac.sub_expr.iter() {
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
            Expression::Equality(eq) => {
                let mut map: HashMap<Variable, usize> = HashMap::new();
                for expr in eq.sub_expr.iter() {
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
            Expression::Negation(neg) => {
                let mut map: HashMap<Variable, usize> = HashMap::new();
                if let Some(mut sub_map) = neg.sub_expr.contain_vars() {
                    for (key, value) in sub_map.iter_mut() {
                        if let Some(occurence) = map.get_mut(key) {
                            *occurence += *value;
                        } else {
                            map.insert(key.clone(), *value);
                        }
                    }
                }

                if map.is_empty() {
                    None
                } else {
                    Some(map)
                }
            }
            Expression::Function(func) => match *func.clone() {
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
            },
            Expression::Error => panic!("There should be no error in the expression tree"),
        }
    }

    pub fn contain_var(&self, variable: &Variable) -> bool {
        match self {
            Expression::Number(_) => false,
            Expression::Variable(var) => var == variable,
            Expression::Constant(_) => false,
            Expression::Addition(add) => add.sub_expr.iter().any(|expr| expr.contain_var(variable)),
            Expression::Multiplication(mult) => {
                mult.sub_expr.iter().any(|expr| expr.contain_var(variable))
            }
            Expression::Exponentiation(expo) => {
                expo.sub_expr.iter().any(|expr| expr.contain_var(variable))
            }
            Expression::Fraction(frac) => {
                frac.sub_expr.iter().any(|expr| expr.contain_var(variable))
            }
            Expression::Equality(eq) => eq.sub_expr.iter().any(|expr| expr.contain_var(variable)),
            Expression::Negation(neg) => neg.sub_expr.contain_var(variable),
            Expression::Function(func) => match *func.clone() {
                FunctionType::Predefined(_, args) | FunctionType::UserDefined(_, args) => {
                    args.iter().any(|expr| expr.contain_var(variable))
                }
            },
            Expression::Error => panic!("There should be no error in the expression tree"),
        }
    }
}

// Print functions
impl Expression {
    pub fn print_tree(&self, span: Option<&str>) {
        let current_span = span.unwrap_or("");
        let new_span = current_span.to_string() + "   ";
        match self {
            Expression::Number(num) => println!("{}{num}", current_span),
            Expression::Addition(add) => {
                println!("{}Addition :", current_span);
                for expr in add.sub_expr.iter() {
                    print!("{}", current_span);
                    expr.print_tree(Some(&new_span));
                }
            }
            Expression::Multiplication(mult) => {
                println!("{}Multiplication :", current_span);
                for expr in mult.sub_expr.iter() {
                    print!("{}", current_span);
                    expr.print_tree(Some(&new_span));
                }
            }
            Expression::Exponentiation(expo) => {
                println!("{}Exponentiation :", current_span);
                print!("{}", current_span);
                expo.sub_expr[0].print_tree(Some(&new_span));
                print!("{}", current_span);
                expo.sub_expr[1].print_tree(Some(&new_span));
            }
            Expression::Fraction(frac) => {
                println!("{}Fraction :", current_span);
                print!("{}", current_span);
                frac.sub_expr[0].print_tree(Some(&new_span));
                print!("{}", current_span);
                frac.sub_expr[1].print_tree(Some(&new_span));
            }
            Expression::Equality(eq) => {
                println!("{}Equality :", current_span);
                print!("{}", current_span);
                eq.sub_expr[0].print_tree(Some(&new_span));
                print!("{}", current_span);
                eq.sub_expr[1].print_tree(Some(&new_span));
            }
            Expression::Negation(neg) => {
                println!("{}Negation :", current_span);
                print!("{}", current_span);
                neg.sub_expr.print_tree(Some(&new_span));
            }
            Expression::Error => println!("{}Error", current_span),
            Expression::Variable(var) => println!("{}{var}", current_span),
            Expression::Constant(cons) => println!("{}{cons}", current_span),
            Expression::Function(func) => {
                println!("{}Function : {}", current_span, func.name());
                for arg in func.args() {
                    print!("{}", current_span);
                    arg.print_tree(Some(&new_span));
                }
            }
        }
    }

    pub fn print_console(&self) {
        let mut position: Vec<(String, (i8, i8))> = Vec::new();

        // memoize (length, height, above_height)
        let mut memoize: HashMap<Expression, (i8, i8, i8)> = HashMap::new();

        self.calc_pos(&mut position, State::Same(0, 0), &mut memoize);

        let (min_x, max_x, min_y, max_y) = position.iter().fold(
            (0, 0, 0, 0),
            |(min_x, max_x, min_y, max_y), &(_, (y, x))| {
                (min_x.min(x), max_x.max(x), min_y.min(y), max_y.max(y))
            },
        );

        let mut grid = HashMap::new();

        // Populate the grid with characters
        for (character, (y, x)) in position {
            grid.insert((y, x), character);
        }

        // Print the grid
        for y in (min_y..=max_y).rev() {
            let mut skip = 0;
            for x in min_x..=max_x {
                if skip > 0 {
                    skip -= 1;
                    continue;
                }
                if let Some(character) = grid.get(&(y, x)) {
                    print!("{}", character);
                    skip = character.len() as i8 - 1;
                } else {
                    print!(" ");
                }
            }
            println!();
        }
    }

    fn calc_pos(&self, position: &mut Vec<(String, (i8, i8))>, prev_state: State, memoized: &mut HashMap<Expression, (i8, i8, i8)>) {
        match self {
            Expression::Number(num) => position.push((num.to_string(), prev_state.get_pos())),
            Expression::Variable(var) => position.push((var.sub_expr.clone(), prev_state.get_pos())),
            Expression::Constant(cons) => {
                position.push((cons.as_text().to_string(), prev_state.get_pos()))
            }
            Expression::Addition(add) => {
                let mut max_height = 1;
                let mut max_height_index = 0;
                for (i, expr) in add.sub_expr.iter().enumerate() {
                    let height = expr.get_height(memoized);
                    if height > max_height {
                        max_height = height;
                        max_height_index = i;
                    }
                }
                let len = position.len();
                let mut position_clone = position.clone();

                add.sub_expr[max_height_index].calc_pos(&mut position_clone, prev_state, memoized);

                let pos_y = position_clone[len].1 .0;
                let mut pos_x = prev_state.get_pos_x();

                for (i, expr) in add.sub_expr.iter().enumerate() {
                    if i > 0 {
                        position.push((" + ".to_string(), (pos_y, pos_x)));
                        pos_x += 3;
                    }

                    expr.calc_pos(position, State::Same(pos_y, pos_x), memoized);
                    pos_x += expr.get_length(memoized);
                }
            }
            Expression::Multiplication(mult) => {
                let mut max_height = 1;
                let mut max_height_index = 0;
                for (i, expr) in mult.sub_expr.iter().enumerate() {
                    let height = expr.get_height(memoized);
                    if height > max_height {
                        max_height = height;
                        max_height_index = i;
                    }
                }

                let len = position.len();
                let mut position_clone = position.clone();

                mult.sub_expr[max_height_index].calc_pos(&mut position_clone, prev_state, memoized);

                let pos_y = position_clone[len].1 .0;
                let mut pos_x = prev_state.get_pos_x();

                for (i, expr) in mult.sub_expr.iter().enumerate() {
                    if i > 0 {
                        position.push((" * ".to_string(), (pos_y, pos_x)));
                        pos_x += 3;
                    }

                    expr.calc_pos(position, State::Same(pos_y, pos_x), memoized);
                    pos_x += expr.get_length(memoized);
                }
            }
            Expression::Exponentiation(expo) => match prev_state {
                State::Over(mut pos_y, mut pos_x) | State::Same(mut pos_y, mut pos_x) => {
                    expo.get_base()
                        .calc_pos(position, State::Same(pos_y, pos_x), memoized);
                    pos_y += expo.get_base().get_above_height(memoized);
                    pos_x += expo.get_base().get_length(memoized);
                    expo.get_exponent()
                        .calc_pos(position, State::Over(pos_y, pos_x), memoized);
                }
                State::Under(mut pos_y, mut pos_x) => {
                    pos_y -=
                        expo.get_exponent().get_height(memoized) - 1 + expo.get_base().get_above_height(memoized);
                    expo.get_base()
                        .calc_pos(position, State::Same(pos_y, pos_x), memoized);
                    pos_x += expo.get_base().get_length(memoized);
                    expo.get_exponent().calc_pos(
                        position,
                        State::Over(pos_y + expo.get_base().get_above_height(memoized), pos_x)
                        , memoized
                    );
                }
            },
            Expression::Fraction(frac) => {
                let (pos_y, mut pos_x) = match prev_state {
                    State::Over(pos_y, pos_x) => {
                        (pos_y + frac.get_denominator().get_height(memoized), pos_x)
                    }
                    State::Under(pos_y, pos_x) => {
                        (pos_y - frac.get_numerator().get_height(memoized), pos_x)
                    }
                    State::Same(pos_y, pos_x) => (pos_y, pos_x),
                };

                let num_length = frac.get_numerator().get_length(memoized);
                let denom_length = frac.get_denominator().get_length(memoized);

                if denom_length < num_length {
                    for i in 0..num_length {
                        position.push(("-".to_string(), (pos_y, pos_x + i)));
                    }
                    frac.get_numerator()
                        .calc_pos(position, State::Over(pos_y + 1, pos_x), memoized);
                    pos_x += (num_length - denom_length) / 2;
                    frac.get_denominator()
                        .calc_pos(position, State::Under(pos_y - 1, pos_x), memoized);
                } else {
                    for i in 0..denom_length {
                        position.push(("-".to_string(), (pos_y, pos_x + i)));
                    }
                    frac.get_denominator()
                        .calc_pos(position, State::Under(pos_y - 1, pos_x), memoized);
                    pos_x += (denom_length - num_length) / 2;
                    frac.get_numerator()
                        .calc_pos(position, State::Over(pos_y + 1, pos_x), memoized);
                }
            }
            Expression::Equality(eq) => {
                let (pos_y, mut pos_x) = prev_state.get_pos();
                eq.get_left_side().calc_pos(position, prev_state, memoized);
                pos_x += eq.get_left_side().get_length(memoized);
                position.push((" = ".to_string(), (pos_y, pos_x)));
                pos_x += 3;
                eq.get_right_side()
                    .calc_pos(position, State::Same(pos_y, pos_x), memoized);
            }
            Expression::Negation(neg) => {
                let (pos_y, mut pos_x) = prev_state.get_pos();
                position.push(("-".to_string(), (pos_y, pos_x)));
                pos_x += 1;
                neg.sub_expr.calc_pos(position, State::Same(pos_y, pos_x), memoized);
            }
            Expression::Function(func) => {
                let (mut pos_y, mut pos_x) = match prev_state {
                    State::Over(pos_y, pos_x) => {
                        (pos_y + self.get_height(memoized) - self.get_above_height(memoized), pos_x)
                    }
                    State::Under(pos_y, pos_x) => (pos_y - self.get_above_height(memoized) + 1, pos_x),
                    State::Same(pos_y, pos_x) => (pos_y, pos_x),
                };

                // Modify the position to be at the middle of the function
                position.push((func.name(), (pos_y, pos_x)));
                pos_x += func.name().len() as i8;

                self.make_parenthesis(&mut pos_y, &mut pos_x, position, false, memoized);

                for (i, arg) in func.args().iter().enumerate() {
                    if i != 0 {
                        position.push((", ".to_string(), (pos_y, pos_x)));
                        pos_x += 2;
                    }
                    arg.calc_pos(position, State::Same(pos_y, pos_x), memoized);
                    pos_x += arg.get_length(memoized);
                }
                self.make_parenthesis(&mut pos_y, &mut pos_x, position, true, memoized);
            }
            Expression::Error => {
                println!("Error");
            }
        }
    }

    fn get_length(&self, memoized: &mut HashMap<Expression, (i8, i8, i8)>) -> i8 {
        if let Some((l, _h, _ah)) = memoized.get(self) {
            if *l != 0 {
                return *l;
            }
        }
        let length = match self {
            Expression::Number(num) => num.to_string().len() as i8,
            Expression::Variable(var) => var.to_string().len() as i8,
            Expression::Constant(cons) => cons.as_text().len() as i8,
            Expression::Addition(add) => {
                let mut length = 0;
                for expr in add.sub_expr.iter() {
                    length += expr.get_length(memoized) + 3;
                }
                length -= 3;
                length
            }
            Expression::Multiplication(mult) => {
                let mut length = 0;
                for expr in mult.sub_expr.iter() {
                    length += expr.get_length(memoized) + 3;
                }
                length -= 3;
                length
            }
            Expression::Exponentiation(expo) => {
                expo.get_base().get_length(memoized) + expo.get_exponent().get_length(memoized)
            }
            Expression::Fraction(frac) => frac
                .get_numerator()
                .get_length(memoized)
                .max(frac.get_denominator().get_length(memoized)),
            Expression::Equality(eq) => {
                eq.get_left_side().get_length(memoized) + eq.get_right_side().get_length(memoized)
            }
            Expression::Negation(neg) => neg.sub_expr.get_length(memoized) + 1,
            Expression::Function(func) => {
                let mut length = func.name().len() as i8 + 2;
                for arg in func.args() {
                    length += arg.get_length(memoized) + 2;
                }
                length - 2
            }
            Expression::Error => {
                panic!("There should be no error in the expression tree");
            }
        };

        if let Some((l, _h, _ah)) = memoized.get_mut(self) {
            *l = length;
        } else {
            memoized.insert(self.clone(), (length, 0, 0));
        }
        length
    }
    
    fn get_height(&self, memoized: &mut HashMap<Expression, (i8, i8, i8)>) -> i8 {
        if let Some((_l, h, _ah)) = memoized.get(self) {
            if *h != 0 {
                return *h;
            }
        }
        let height = match self {
            Expression::Number(_) | Expression::Variable(_) | Expression::Constant(_) => 1,
            Expression::Addition(add) => {
                let mut max_height = 0;
                for expr in &add.sub_expr {
                    max_height = max_height.max(expr.get_height(memoized));
                }
                max_height
            }
            Expression::Multiplication(mult) => {
                let mut max_height = 0;
                for expr in &mult.sub_expr {
                    max_height = max_height.max(expr.get_height(memoized));
                }
                max_height
            }
            Expression::Exponentiation(expo) => {
                expo.get_base().get_height(memoized) + expo.get_exponent().get_height(memoized)
            }
            Expression::Fraction(frac) => {
                frac.get_numerator().get_height(memoized) + frac.get_denominator().get_height(memoized) + 1
            }
            Expression::Equality(eq) => eq
                .get_left_side()
                .get_height(memoized)
                .max(eq.get_right_side().get_height(memoized)),
            Expression::Negation(neg) => neg.sub_expr.get_height(memoized),
            Expression::Function(func) => {
                let mut max_height = 0;
                for arg in func.args() {
                    max_height = max_height.max(arg.get_height(memoized));
                }
                max_height
            }
            Expression::Error => {
                panic!("There should be no error in the expression tree");
            }
        };

        if let Some((_l, h, _ah)) = memoized.get_mut(self) {
            *h = height;
        } else {
            memoized.insert(self.clone(), (0, height, 0));
        }

        height
    }

    /// Return the height of the expression above the current zero.
    ///
    /// Used for parenthesis to know how much to go up
    fn get_above_height(&self, memoized: &mut HashMap<Expression, (i8, i8, i8)>) -> i8 {
        if let Some((_l, _h, ah)) = memoized.get(self) {
            if *ah != 0 {
                return *ah;
            }
        }
        let above_height = match self {
            Expression::Number(_) | Expression::Variable(_) | Expression::Constant(_) => 1,
            Expression::Addition(add) => {
                let mut max_height = 0;
                for expr in &add.sub_expr {
                    max_height = max_height.max(expr.get_above_height(memoized));
                }
                max_height
            }
            Expression::Multiplication(mult) => {
                let mut max_height = 0;
                for expr in &mult.sub_expr {
                    max_height = max_height.max(expr.get_above_height(memoized));
                }
                max_height
            }
            Expression::Exponentiation(expo) => {
                expo.get_base().get_above_height(memoized) + expo.get_exponent().get_height(memoized)
            }
            Expression::Fraction(frac) => frac.get_numerator().get_height(memoized) + 1,
            Expression::Equality(eq) => eq
                .get_left_side()
                .get_above_height(memoized)
                .max(eq.get_right_side().get_above_height(memoized)),
            Expression::Negation(neg) => neg.sub_expr.get_above_height(memoized),
            Expression::Function(func) => {
                let mut max_height = 0;
                for expr in func.args() {
                    max_height = max_height.max(expr.get_above_height(memoized));
                }
                max_height
            }
            Expression::Error => panic!("There should be no error in the expression tree"),
        };

        if let Some((_l, _h, ah)) = memoized.get_mut(self) {
            *ah = above_height;
        } else {
            memoized.insert(self.clone(), (0, 0, above_height));
        }

        above_height
    }

    /// Allways make the right parenthesis except if left is true
    fn make_parenthesis(
        &self,
        pos_y: &mut i8,
        pos_x: &mut i8,
        position: &mut Vec<(String, (i8, i8))>,
        left: bool,
        memoized: &mut HashMap<Expression, (i8, i8, i8)>
    ) {
        let height = self.get_height(memoized);
        if height == 1 {
            if left {
                position.push((")".to_string(), (*pos_y, *pos_x)));
            } else {
                position.push(("(".to_string(), (*pos_y, *pos_x)));
            }
            *pos_x += 1;
        } else {
            // better to make a single if left ?
            let mut stage = *pos_y + self.get_above_height(memoized) - 1;
            if left {
                position.push(("\\".to_string(), (stage, *pos_x)));
            } else {
                position.push(("/".to_string(), (stage, *pos_x)));
            }
            stage -= 1;

            for _ in 0..(height - 2) {
                position.push(("|".to_string(), (stage, *pos_x)));
                stage -= 1;
            }

            if left {
                position.push(("/".to_string(), (stage, *pos_x)));
            } else {
                position.push(("\\".to_string(), (stage, *pos_x)));
            }
            *pos_x += 1;
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum State {
    // y, x
    Over(i8, i8),
    Under(i8, i8),
    Same(i8, i8),
}

impl State {
    pub fn get_height(&self) -> i8 {
        match self {
            State::Over(num, _) => *num,
            State::Under(num, _) => *num,
            State::Same(num, _) => *num,
        }
    }

    pub fn get_pos_x(&self) -> i8 {
        match self {
            State::Over(_, num) => *num,
            State::Under(_, num) => *num,
            State::Same(_, num) => *num,
        }
    }

    pub fn get_pos(&self) -> (i8, i8) {
        match self {
            State::Over(num1, num2) => (*num1, *num2),
            State::Under(num1, num2) => (*num1, *num2),
            State::Same(num1, num2) => (*num1, *num2),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Number(num) => write!(f, "{num}"),
            Expression::Variable(var) => write!(f, "{var}"),
            Expression::Constant(cons) => write!(f, "{cons}"),
            Expression::Addition(add) => {
                let mut first = true;
                for expr in add.sub_expr.iter() {
                    if first {
                        first = false;
                    } else {
                        write!(f, " + ")?;
                    }
                    write!(f, "{}", expr)?;
                }
                Ok(())
            }
            Expression::Multiplication(mult) => {
                let mut first = true;
                for expr in mult.sub_expr.iter() {
                    if first {
                        first = false;
                    } else {
                        write!(f, " * ")?;
                    }
                    write!(f, "{}", expr)?;
                }
                Ok(())
            }
            Expression::Exponentiation(expo) => {
                write!(f, "({})^({})", expo.get_base(), expo.get_exponent())
            }
            Expression::Fraction(frac) => write!(
                f,
                "({}) / ({})",
                frac.get_numerator(),
                frac.get_denominator()
            ),
            Expression::Equality(eq) => {
                write!(f, "{}", eq)
            }
            Expression::Negation(neg) => write!(f, "-({})", neg.sub_expr),
            Expression::Function(func) => write!(f, "{}", func),
            Expression::Error => write!(f, "Error"),
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy, Hash, Eq)]
pub enum ConstantKind {
    E,
    Pi,
}

impl ConstantKind {
    pub fn as_text(&self) -> &str {
        match self {
            ConstantKind::E => "e",
            ConstantKind::Pi => "pi",
        }
    }

    pub fn equal(&self, other: &ConstantKind) -> bool {
        self == other
    }
}

impl Display for ConstantKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstantKind::E => write!(f, "e"),
            ConstantKind::Pi => write!(f, "pi"),
        }
    }
}

#[cfg(test)]
mod test_simplify {

    use crate::ast::{
        function::{FunctionType, PredefinedFunction},
        ConstantKind, Expression, varibale::Variable,
    };

    #[test]
    fn test_simplify_addition() {
        // 1+2+3
        assert_eq!(
            Expression::addition(
                Expression::addition(Expression::Number(1), Expression::Number(2)),
                Expression::Number(3),
            )
            .simplify(),
            Expression::Number(6)
        );

        // 1 + x +3 +Y
        assert!(Expression::addition_from_vec(vec![
            Expression::Number(1),
            Expression::variable("x".to_string()),
            Expression::Number(3),
            Expression::Variable(Variable::new("Y".to_string())),
        ])
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::Number(4),
            Expression::variable("x".to_string()),
            Expression::variable("Y".to_string()),
        ])));

        // 2 +x +4 +X +x
        assert!(Expression::addition_from_vec(vec![
            Expression::Number(2),
            Expression::variable("x".to_string()),
            Expression::Number(4),
            Expression::Variable(Variable::new("X".to_string())),
            Expression::variable("x".to_string()),
        ])
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::Number(6),
            Expression::variable("X".to_string()),
            Expression::multiplication(Expression::Number(2), Expression::variable("x".to_string()),),
        ])));

        // 2x + 2y + x
        assert!(Expression::addition_from_vec(vec![
            Expression::multiplication(Expression::Number(2), Expression::variable("x".to_string()),),
            Expression::multiplication(Expression::Number(2), Expression::variable("y".to_string()),),
            Expression::variable("x".to_string()),
        ])
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::multiplication(Expression::Number(3), Expression::variable("x".to_string()),),
            Expression::multiplication(Expression::Number(2), Expression::variable("y".to_string()),),
        ])));

        // x^2 + y^2 + 2xy + x^2
        assert!(Expression::addition_from_vec(vec![
            Expression::exponentiation(Expression::variable("x".to_string()), Expression::Number(2)),
            Expression::exponentiation(Expression::variable("y".to_string()), Expression::Number(2)),
            Expression::multiplication(
                Expression::multiplication(Expression::Number(2), Expression::variable("x".to_string())),
                Expression::variable("y".to_string()),
            ),
            Expression::exponentiation(Expression::variable("x".to_string()), Expression::Number(2)),
        ])
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::multiplication(
                Expression::Number(2),
                Expression::exponentiation(Expression::variable("x".to_string()), Expression::Number(2))
            ),
            Expression::multiplication_from_vec(vec![
                Expression::Number(2),
                Expression::variable("x".to_string()),
                Expression::variable("y".to_string()),
            ]),
            Expression::exponentiation(Expression::variable("y".to_string()), Expression::Number(2)),
        ])));

        // a/b + b/a
        assert!(Expression::addition(
            Expression::fraction(Expression::variable("a".to_string()), Expression::variable("b".to_string())),
            Expression::fraction(Expression::variable("b".to_string()), Expression::variable("a".to_string())),
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::addition(
                Expression::exponentiation(Expression::variable("a".to_string()), Expression::Number(2)),
                Expression::exponentiation(Expression::variable("b".to_string()), Expression::Number(2)),
            ),
            Expression::multiplication(Expression::variable("a".to_string()), Expression::variable("b".to_string())),
        )));

        // e + e
        assert!(Expression::addition(
            Expression::Constant(ConstantKind::E),
            Expression::Constant(ConstantKind::E)
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::Number(2),
            Expression::Constant(ConstantKind::E)
        )));

        // sin(x) + sin(y)
        assert!(Expression::addition(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::variable("x".to_string())]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::variable("y".to_string())]
            )),
        )
        .simplify()
        .equal(&Expression::addition(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::variable("x".to_string())]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::variable("y".to_string())]
            )),
        )));

        // sin(x) + sin(x)
        assert!(Expression::addition(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::variable("x".to_string())]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::variable("x".to_string())]
            )),
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::Number(2),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::variable("x".to_string())]
            )),
        )));

        // 2sin(x) + sin(x)
        assert!(Expression::addition(
            Expression::multiplication(
                Expression::Number(2),
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sin,
                    vec![Expression::variable("x".to_string())]
                ))
            ),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::variable("x".to_string())]
            )),
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::Number(3),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::variable("x".to_string())]
            )),
        )));

        // ln(x) + ln(y)
        assert!(Expression::addition(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::variable("x".to_string())]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::variable("y".to_string())]
            )),
        )
        .simplify()
        .equal(&Expression::function(FunctionType::Predefined(
            PredefinedFunction::Ln,
            vec![Expression::multiplication(
                Expression::variable("x".to_string()),
                Expression::variable("y".to_string())
            )]
        ))));

        // 2ln(x) + ln(x+1)
        assert!(Expression::addition(
            Expression::multiplication(
                Expression::Number(2),
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Ln,
                    vec![Expression::variable("x".to_string())]
                ))
            ),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::addition(
                    Expression::variable("x".to_string()),
                    Expression::Number(1)
                )]
            )),
        )
        .simplify()
        .equal(&Expression::function(FunctionType::Predefined(
            PredefinedFunction::Ln,
            vec![Expression::addition(
                Expression::exponentiation(Expression::variable("x".to_string()), Expression::Number(3)),
                Expression::exponentiation(Expression::variable("x".to_string()), Expression::Number(2)),
            )]
        ))));

        // ln(x) + ln(x)
        assert!(Expression::addition(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::variable("x".to_string())]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::variable("x".to_string())]
            )),
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::Number(2),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::variable("x".to_string()),]
            ))
        )));

        // 2ln(x) + ln(x)
        assert!(Expression::addition(
            Expression::multiplication(
                Expression::Number(2),
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Ln,
                    vec![Expression::variable("x".to_string())]
                ))
            ),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::variable("x".to_string())]
            )),
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::Number(3),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::variable("x".to_string()),]
            ))
        )));

        // a(f) + a(f)
        assert!(Expression::addition(
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::variable("f".to_string())]
            )),
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::variable("f".to_string())]
            )),
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::Number(2),
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::variable("f".to_string())]
            )),
        )));

        // a(f) + a(g)
        assert!(Expression::addition(
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::variable("f".to_string())]
            )),
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::variable("g".to_string())]
            )),
        )
        .simplify()
        .equal(&Expression::addition(
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::variable("f".to_string())]
            )),
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::variable("g".to_string())]
            )),
        )));
    }

    #[test]
    fn test_simplify_multiplication() {
        // 1*2*3
        assert_eq!(
            Expression::multiplication(
                Expression::multiplication(Expression::Number(1), Expression::Number(2)),
                Expression::Number(3),
            )
            .simplify(),
            Expression::Number(6)
        );

        // 1 * x *3 *Y
        assert!(Expression::multiplication_from_vec(vec![
            Expression::Number(1),
            Expression::variable("x".to_string()),
            Expression::Number(3),
            Expression::variable("Y".to_string()),
        ])
        .simplify()
        .equal(&Expression::multiplication_from_vec(vec![
            Expression::Number(3),
            Expression::variable("x".to_string()),
            Expression::variable("Y".to_string()),
        ])));

        // 2 *x *4 *X *x
        assert!(Expression::multiplication_from_vec(vec![
            Expression::Number(2),
            Expression::variable("x".to_string()),
            Expression::Number(4),
            Expression::variable("X".to_string()),
            Expression::variable("x".to_string()),
        ])
        .simplify()
        .equal(&Expression::multiplication_from_vec(vec![
            Expression::Number(8),
            Expression::variable("X".to_string()),
            Expression::exponentiation(Expression::variable("x".to_string()), Expression::Number(2)),
        ])));

        // 2x * 2y * x
        assert!(Expression::multiplication_from_vec(vec![
            Expression::multiplication(Expression::Number(2), Expression::variable("x".to_string()),),
            Expression::multiplication(Expression::Number(2), Expression::variable("y".to_string()),),
            Expression::variable("x".to_string()),
        ])
        .simplify()
        .equal(&Expression::multiplication_from_vec(vec![
            Expression::Number(4),
            Expression::variable("y".to_string()),
            Expression::exponentiation(Expression::variable("x".to_string()), Expression::Number(2)),
        ])));

        // x^2 * y^2 * 2xy * x^2
        assert!(Expression::multiplication_from_vec(vec![
            Expression::exponentiation(Expression::variable("x".to_string()), Expression::Number(2)),
            Expression::exponentiation(Expression::variable("y".to_string()), Expression::Number(2)),
            Expression::multiplication(
                Expression::multiplication(Expression::Number(2), Expression::variable("x".to_string())),
                Expression::variable("y".to_string()),
            ),
            Expression::exponentiation(Expression::variable("x".to_string()), Expression::Number(2)),
        ])
        .simplify()
        .equal(&Expression::multiplication_from_vec(vec![
            Expression::exponentiation(Expression::variable("x".to_string()), Expression::Number(5)),
            Expression::Number(2),
            Expression::exponentiation(Expression::variable("y".to_string()), Expression::Number(3)),
        ])));

        // a/b * b/a
        assert!(Expression::multiplication(
            Expression::fraction(Expression::variable("a".to_string()), Expression::variable("b".to_string())),
            Expression::fraction(Expression::variable("b".to_string()), Expression::variable("a".to_string())),
        )
        .simplify()
        .equal(&Expression::Number(1)));

        // a/b * c/d
        assert!(Expression::multiplication(
            Expression::fraction(Expression::variable("a".to_string()), Expression::variable("b".to_string())),
            Expression::fraction(Expression::variable("c".to_string()), Expression::variable("d".to_string())),
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::multiplication(Expression::variable("a".to_string()), Expression::variable("c".to_string())),
            Expression::multiplication(Expression::variable("b".to_string()), Expression::variable("d".to_string())),
        )));

        // e * e
        assert!(Expression::multiplication(
            Expression::Constant(ConstantKind::E),
            Expression::Constant(ConstantKind::E)
        )
        .simplify()
        .equal(&Expression::exponentiation(
            Expression::Constant(ConstantKind::E),
            Expression::Number(2)
        )));

        // sin(x) * sin(y)
        assert!(Expression::multiplication(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::variable("x".to_string())]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::variable("y".to_string())]
            )),
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::variable("x".to_string())]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::variable("y".to_string())]
            )),
        )));

        // sin(x) * sin(x)
        assert!(Expression::multiplication(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::variable("x".to_string())]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::variable("x".to_string())]
            )),
        )
        .simplify()
        .equal(&Expression::exponentiation(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::variable("x".to_string())]
            )),
            Expression::Number(2)
        )));

        // 2sin(x) * sin(x)
        assert!(Expression::multiplication(
            Expression::multiplication(
                Expression::Number(2),
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sin,
                    vec![Expression::variable("x".to_string())]
                ))
            ),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::variable("x".to_string())]
            )),
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::Number(2),
            Expression::exponentiation(
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sin,
                    vec![Expression::variable("x".to_string())]
                )),
                Expression::Number(2)
            )
        )));

        // ln(x) * ln(y)
        assert!(Expression::multiplication(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::variable("x".to_string())]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::variable("y".to_string())]
            )),
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::variable("x".to_string())]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::variable("y".to_string())]
            )),
        )));

        // ln(x) * ln(x)
        assert!(Expression::multiplication(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::variable("x".to_string())]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::variable("x".to_string())]
            )),
        )
        .simplify()
        .equal(&Expression::exponentiation(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::variable("x".to_string())]
            )),
            Expression::Number(2)
        )));

        // a(f) * a(f)
        assert!(Expression::multiplication(
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::variable("f".to_string())]
            )),
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::variable("f".to_string())]
            )),
        )
        .simplify()
        .equal(&Expression::exponentiation(
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::variable("f".to_string())]
            )),
            Expression::Number(2)
        )));

        // a(f) * a(g)
        assert!(Expression::multiplication(
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::variable("f".to_string())]
            )),
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::variable("g".to_string())]
            )),
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::variable("f".to_string())]
            )),
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::variable("g".to_string())]
            )),
        )));
    }

    #[test]
    fn test_simplify_exponentiation() {
        // 1^2
        assert_eq!(
            Expression::exponentiation(Expression::Number(1), Expression::Number(2)).simplify(),
            Expression::Number(1)
        );

        // 2^2
        assert_eq!(
            Expression::exponentiation(Expression::Number(2), Expression::Number(2)).simplify(),
            Expression::Number(4)
        );

        // 2^2^3
        assert_eq!(
            Expression::exponentiation(
                Expression::Number(2),
                Expression::exponentiation(Expression::Number(2), Expression::Number(3))
            )
            .simplify(),
            Expression::Number(256)
        );

        // (2^2)^2
        assert_eq!(
            Expression::exponentiation(
                Expression::exponentiation(Expression::Number(2), Expression::Number(2)),
                Expression::Number(2)
            )
            .simplify(),
            Expression::Number(16)
        );

        // x^x
        assert!(
            Expression::exponentiation(Expression::variable("x".to_string()), Expression::variable("x".to_string()))
                .simplify()
                .equal(&Expression::exponentiation(
                    Expression::variable("x".to_string()),
                    Expression::variable("x".to_string())
                ))
        );

        // x^log(x, y)
        assert!(Expression::exponentiation(
            Expression::variable("x".to_string()),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Log,
                vec![Expression::variable("x".to_string()), Expression::variable("y".to_string())]
            ))
        )
        .simplify()
        .equal(&Expression::variable("y".to_string()),));

        // x^log(y, x)
        assert!(Expression::exponentiation(
            Expression::variable("x".to_string()),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Log,
                vec![Expression::variable("y".to_string()), Expression::variable("x".to_string())]
            ))
        )
        .simplify()
        .equal(&Expression::exponentiation(
            Expression::variable("x".to_string()),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Log,
                vec![Expression::variable("y".to_string()), Expression::variable("x".to_string())]
            ))
        )));

        // e^ln(x)
        assert!(Expression::exponentiation(
            Expression::Constant(ConstantKind::E),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::variable("x".to_string())]
            ))
        )
        .simplify()
        .equal(&Expression::variable("x".to_string())));

        // (1/2)^2
        assert!(Expression::exponentiation(
            Expression::fraction(Expression::Number(1), Expression::Number(2)),
            Expression::Number(2)
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::Number(1),
            Expression::Number(4)
        )));

        // (ab)^2
        assert!(Expression::exponentiation(
            Expression::multiplication(Expression::variable("a".to_string()), Expression::variable("b".to_string()),),
            Expression::Number(2)
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::exponentiation(Expression::variable("a".to_string()), Expression::Number(2),),
            Expression::exponentiation(Expression::variable("b".to_string()), Expression::Number(2),),
        )));

        // (a+b)^2
        assert!(Expression::exponentiation(
            Expression::addition(Expression::variable("a".to_string()), Expression::variable("b".to_string()),),
            Expression::Number(2)
        )
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::exponentiation(Expression::variable("a".to_string()), Expression::Number(2),),
            Expression::multiplication_from_vec(vec![
                Expression::Number(2),
                Expression::variable("a".to_string()),
                Expression::variable("b".to_string()),
            ]),
            Expression::exponentiation(Expression::variable("b".to_string()), Expression::Number(2),),
        ])));

        // (a-b)^2
        assert!(Expression::exponentiation(
            Expression::addition(
                Expression::variable("a".to_string()),
                Expression::negation(Expression::variable("b".to_string()),),
            ),
            Expression::Number(2)
        )
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::exponentiation(Expression::variable("a".to_string()), Expression::Number(2),),
            Expression::multiplication_from_vec(vec![
                Expression::Number(2),
                Expression::variable("a".to_string()),
                Expression::negation(Expression::variable("b".to_string())),
            ]),
            Expression::exponentiation(
                Expression::negation(Expression::variable("b".to_string())),
                Expression::Number(2),
            ),
        ])));

        // (a+b)^3
        assert!(Expression::exponentiation(
            Expression::addition(Expression::variable("a".to_string()), Expression::variable("b".to_string()),),
            Expression::Number(3)
        )
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::exponentiation(Expression::variable("a".to_string()), Expression::Number(3),),
            Expression::multiplication_from_vec(vec![
                Expression::Number(3),
                Expression::exponentiation(Expression::variable("a".to_string()), Expression::Number(2),),
                Expression::variable("b".to_string()),
            ]),
            Expression::multiplication_from_vec(vec![
                Expression::Number(3),
                Expression::exponentiation(Expression::variable("b".to_string()), Expression::Number(2),),
                Expression::variable("a".to_string()),
            ]),
            Expression::exponentiation(Expression::variable("b".to_string()), Expression::Number(3),),
        ])));

        //(a+b+c)^2
        assert!(Expression::exponentiation(
            Expression::addition_from_vec(vec![
                Expression::variable("a".to_string()),
                Expression::variable("b".to_string()),
                Expression::variable("c".to_string()),
            ]),
            Expression::Number(2)
        )
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::exponentiation(Expression::variable("a".to_string()), Expression::Number(2),),
            Expression::multiplication_from_vec(vec![
                Expression::Number(2),
                Expression::variable("a".to_string()),
                Expression::variable("b".to_string()),
            ]),
            Expression::multiplication_from_vec(vec![
                Expression::Number(2),
                Expression::variable("a".to_string()),
                Expression::variable("c".to_string()),
            ]),
            Expression::exponentiation(Expression::variable("b".to_string()), Expression::Number(2),),
            Expression::multiplication_from_vec(vec![
                Expression::Number(2),
                Expression::variable("b".to_string()),
                Expression::variable("c".to_string()),
            ]),
            Expression::exponentiation(Expression::variable("c".to_string()), Expression::Number(2),),
        ])));
    }

    #[test]
    fn test_simplify_fraction() {
        // 1/2
        assert!(
            Expression::fraction(Expression::Number(1), Expression::Number(2))
                .simplify()
                .equal(&Expression::fraction(
                    Expression::Number(1),
                    Expression::Number(2)
                ))
        );

        // 2/2
        assert!(
            Expression::fraction(Expression::Number(2), Expression::Number(2))
                .simplify()
                .equal(&Expression::Number(1))
        );

        // 2/4
        assert!(
            Expression::fraction(Expression::Number(2), Expression::Number(4))
                .simplify()
                .equal(&Expression::fraction(
                    Expression::Number(1),
                    Expression::Number(2)
                ))
        );

        // 2/4/2
        assert!(Expression::fraction(
            Expression::fraction(Expression::Number(2), Expression::Number(4)),
            Expression::Number(2)
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::Number(1),
            Expression::Number(4)
        )));

        // -a/b
        assert!(Expression::fraction(
            Expression::negation(Expression::variable("a".to_string())),
            Expression::variable("b".to_string())
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::negation(Expression::variable("a".to_string())),
            Expression::variable("b".to_string())
        )));

        // a/-b
        assert!(Expression::fraction(
            Expression::variable("a".to_string()),
            Expression::negation(Expression::variable("b".to_string()))
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::negation(Expression::variable("a".to_string())),
            Expression::variable("b".to_string())
        )));

        // -a/-b
        assert!(Expression::fraction(
            Expression::negation(Expression::variable("a".to_string())),
            Expression::negation(Expression::variable("b".to_string()))
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::variable("a".to_string()),
            Expression::variable("b".to_string())
        )));

        //a/a^n
        assert!(Expression::fraction(
            Expression::variable("a".to_string()),
            Expression::exponentiation(Expression::variable("a".to_string()), Expression::variable("n".to_string()))
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::Number(1),
            Expression::exponentiation(
                Expression::variable("a".to_string()),
                Expression::addition(Expression::variable("n".to_string()), Expression::Number(-1))
            )
        )));

        // a^2/a
        assert!(Expression::fraction(
            Expression::exponentiation(Expression::variable("a".to_string()), Expression::Number(2)),
            Expression::variable("a".to_string())
        )
        .simplify()
        .equal(&Expression::variable("a".to_string())));

        // a^2/a^3
        assert!(Expression::fraction(
            Expression::exponentiation(Expression::variable("a".to_string()), Expression::Number(2)),
            Expression::exponentiation(Expression::variable("a".to_string()), Expression::Number(3))
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::Number(1),
            Expression::variable("a".to_string())
        )));

        // 2^n/2
        assert!(Expression::fraction(
            Expression::exponentiation(Expression::Number(2), Expression::variable("n".to_string())),
            Expression::Number(2)
        )
        .simplify()
        .equal(&Expression::exponentiation(
            Expression::Number(2),
            Expression::addition(Expression::variable("n".to_string()), Expression::Number(-1))
        )));

        // 2 / 2^n
        assert!(Expression::fraction(
            Expression::Number(2),
            Expression::exponentiation(Expression::Number(2), Expression::variable("n".to_string()))
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::Number(1),
            Expression::exponentiation(
                Expression::Number(2),
                Expression::addition(Expression::variable("n".to_string()), Expression::Number(-1))
            )
        )));

        // 4a / 2
        assert!(Expression::fraction(
            Expression::multiplication(Expression::Number(4), Expression::variable("a".to_string())),
            Expression::Number(2)
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::Number(2),
            Expression::variable("a".to_string())
        )));

        // 4a / 2b
        assert!(Expression::fraction(
            Expression::multiplication(Expression::Number(4), Expression::variable("a".to_string())),
            Expression::multiplication(Expression::Number(2), Expression::variable("b".to_string()))
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::multiplication(Expression::Number(2), Expression::variable("a".to_string())),
            Expression::variable("b".to_string())
        )));

        // (12a + 6)/(24a^2)
        assert!(Expression::fraction(
            Expression::addition(
                Expression::multiplication(Expression::Number(12), Expression::variable("a".to_string())),
                Expression::Number(6)
            ),
            Expression::multiplication(
                Expression::Number(24),
                Expression::exponentiation(Expression::variable("a".to_string()), Expression::Number(2))
            )
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::addition(
                Expression::Number(1),
                Expression::multiplication(Expression::Number(2), Expression::variable("a".to_string()))
            ),
            Expression::multiplication(
                Expression::Number(4),
                Expression::exponentiation(Expression::variable("a".to_string()), Expression::Number(2))
            )
        )));

        // (18x^2y + 3xy + 9xy^2) / (6x^2y)
        assert!(Expression::fraction(
            Expression::addition_from_vec(vec![
                Expression::multiplication(
                    Expression::multiplication(Expression::Number(18), Expression::variable("y".to_string())),
                    Expression::exponentiation(Expression::variable("x".to_string()), Expression::Number(2))
                ),
                Expression::multiplication(
                    Expression::multiplication(Expression::Number(3), Expression::variable("x".to_string())),
                    Expression::variable("y".to_string())
                ),
                Expression::multiplication(
                    Expression::multiplication(Expression::Number(9), Expression::variable("x".to_string())),
                    Expression::exponentiation(Expression::variable("y".to_string()), Expression::Number(2))
                ),
            ]),
            Expression::multiplication(
                Expression::multiplication(Expression::Number(6), Expression::variable("y".to_string())),
                Expression::exponentiation(Expression::variable("x".to_string()), Expression::Number(2))
            )
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::addition_from_vec(vec![
                Expression::multiplication(Expression::Number(3), Expression::variable("y".to_string())),
                Expression::Number(1),
                Expression::multiplication(Expression::Number(6), Expression::variable("x".to_string())),
            ]),
            Expression::multiplication(Expression::Number(2), Expression::variable("x".to_string()))
        )));

        // (a/b)/(c/d)
        assert!(Expression::fraction(
            Expression::fraction(Expression::variable("a".to_string()), Expression::variable("b".to_string()),),
            Expression::fraction(Expression::variable("c".to_string()), Expression::variable("d".to_string()),),
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::multiplication(Expression::variable("a".to_string()), Expression::variable("d".to_string()),),
            Expression::multiplication(Expression::variable("b".to_string()), Expression::variable("c".to_string()),),
        )));

        Expression::fraction(
            Expression::fraction(Expression::variable("a".to_string()), Expression::variable("b".to_string())),
            Expression::variable("c".to_string()),
        )
        .simplify()
        .print_tree(None);

        // (a/b)/c
        assert!(Expression::fraction(
            Expression::fraction(Expression::variable("a".to_string()), Expression::variable("b".to_string()),),
            Expression::variable("c".to_string()),
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::variable("a".to_string()),
            Expression::multiplication(Expression::variable("b".to_string()), Expression::variable("c".to_string()),),
        )));

        // a/(b/c)
        assert!(Expression::fraction(
            Expression::variable("a".to_string()),
            Expression::fraction(Expression::variable("b".to_string()), Expression::variable("c".to_string()),),
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::multiplication(Expression::variable("a".to_string()), Expression::variable("c".to_string()),),
            Expression::variable("b".to_string()),
        )));
    }

    #[test]
    fn test_negation() {
        // --2
        assert!(
            Expression::negation(Expression::negation(Expression::Number(2)))
                .simplify()
                .equal(&Expression::Number(2))
        );

        // -2
        assert!(Expression::negation(Expression::Number(2))
            .simplify()
            .equal(&Expression::Number(-2)));

        // -2 + 3
        assert!(Expression::addition(
            Expression::negation(Expression::Number(2)),
            Expression::Number(3)
        )
        .simplify()
        .equal(&Expression::Number(1)));

        // -2 - 3
        assert!(Expression::addition(
            Expression::negation(Expression::Number(2)),
            Expression::negation(Expression::Number(3))
        )
        .simplify()
        .equal(&Expression::Number(-5)));

        // -2 * 2
        assert!(Expression::multiplication(
            Expression::negation(Expression::Number(2)),
            Expression::Number(2)
        )
        .simplify()
        .equal(&Expression::Number(-4)));

        // -2 * -2
        assert!(Expression::multiplication(
            Expression::negation(Expression::Number(2)),
            Expression::negation(Expression::Number(2))
        )
        .simplify()
        .equal(&Expression::Number(4)));
    }
}
