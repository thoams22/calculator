pub mod addition;
pub mod complex;
pub mod constant;
pub mod derivation;
pub mod equality;
pub mod exponentiation;
pub mod fraction;
pub mod function;
pub mod multiplication;
pub mod negation;
pub mod number;
pub mod varibale;

use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use crate::solver::{solve, SolverError};
use crate::utils::substitute;

use self::addition::Addition;
use self::complex::Complex;
use self::constant::ConstantKind;
use self::derivation::Derivation;
use self::equality::Equality;
use self::exponentiation::Exponentiation;
use self::fraction::Fraction;
use self::function::FunctionType;
use self::multiplication::Multiplication;
use self::negation::Negation;
use self::number::Number;
use self::varibale::Variable;

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Simplify(Expression),
    Solve(Expression),
    SolveFor(Expression, Variable),
    Replace(Expression, Equality),
}

impl Statement {
    pub fn print_console(&self) {
        let mut position: Vec<(String, (i8, i8))> = Vec::new();

        // memoize (length, height, above_height)
        let mut memoize: HashMap<Expression, (i8, i8, i8)> = HashMap::new();

        match self {
            Statement::Simplify(expr) => {
                expr.calc_pos(&mut position, State::Same(0, 0), &mut memoize)
            }
            Statement::Solve(expr) => expr.calc_pos(&mut position, State::Same(0, 0), &mut memoize),
            Statement::SolveFor(expr, var) => {
                let mut length = expr.get_length(&mut memoize);
                expr.calc_pos(&mut position, State::Same(0, 0), &mut memoize);
                Expression::Variable(Variable::new(",".to_string())).calc_pos(
                    &mut position,
                    State::Same(0, length),
                    &mut memoize,
                );
                length += 2;
                Expression::Variable(var.clone()).calc_pos(
                    &mut position,
                    State::Same(0, length),
                    &mut memoize,
                );
            }
            Statement::Replace(expr, eq) => {
                let mut length = expr.get_length(&mut memoize);
                expr.calc_pos(&mut position, State::Same(0, 0), &mut memoize);
                Expression::Variable(Variable::new(",".to_string())).calc_pos(
                    &mut position,
                    State::Same(0, length),
                    &mut memoize,
                );
                length += 2;
                Expression::Equality(Box::new(eq.clone())).calc_pos(
                    &mut position,
                    State::Same(0, length),
                    &mut memoize,
                );
            }
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

    pub fn solve(&self) -> Result<Vec<Expression>, SolverError> {
        match self.clone() {
            Statement::Simplify(expression) => Ok(vec![expression.simplify()]),
            Statement::Solve(expression) => solve(expression, None),
            Statement::SolveFor(expression, variable) => solve(expression, Some(variable)),
            Statement::Replace(expression, equality) => {
                solve(substitute(expression, &equality), None)
            }
        }
    }

    pub fn equal(&mut self, other: &Statement) -> bool {
        match (self, other) {
            (Statement::Simplify(expr_1), Statement::Simplify(expr_2)) => expr_1.equal(expr_2),
            (Statement::Solve(expr_1), Statement::Solve(expr_2)) => expr_1.equal(expr_2),
            (Statement::SolveFor(expr_1, var_1), Statement::SolveFor(expr_2, var_2)) => {
                expr_1.equal(expr_2) && var_1.equal(var_2)
            }
            (Statement::Replace(expr_1, eq_1), Statement::Replace(expr_2, eq_2)) => {
                expr_1.equal(expr_2) && eq_1.equal(eq_2)
            }
            _ => false,
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
        }
    }
}

pub trait Expr {
    fn equal(&self, other: &Self) -> bool;
    fn contain_vars(&self) -> Option<HashMap<Variable, usize>>;
    fn contain_var(&self, variable: &Variable) -> bool;

    fn simplify(self) -> Expression;
    fn get_order(&self) -> i64;

    fn print_tree(&self, span: Option<&str>);
    fn calc_pos(
        &self,
        position: &mut Vec<(String, (i8, i8))>,
        prev_state: State,
        memoized: &mut HashMap<Expression, (i8, i8, i8)>,
    );
    fn get_length(&self, memoized: &mut HashMap<Expression, (i8, i8, i8)>) -> i8;
    fn get_height(&self, memoized: &mut HashMap<Expression, (i8, i8, i8)>) -> i8;
    fn get_above_height(&self, memoized: &mut HashMap<Expression, (i8, i8, i8)>) -> i8;

    /// Allways make the left parenthesis except if right is true
    fn make_parenthesis(
        &self,
        pos_y: &mut i8,
        pos_x: &mut i8,
        position: &mut Vec<(String, (i8, i8))>,
        right: bool,
        memoized: &mut HashMap<Expression, (i8, i8, i8)>,
    ) {
        let height = self.get_height(memoized);
        if height == 1 {
            if right {
                position.push((")".to_string(), (*pos_y, *pos_x)));
            } else {
                position.push(("(".to_string(), (*pos_y, *pos_x)));
            }
            *pos_x += 1;
        } else {
            // better to make a single if right ?
            let mut stage = *pos_y + self.get_above_height(memoized) - 1;
            if right {
                position.push(("\\".to_string(), (stage, *pos_x)));
            } else {
                position.push(("/".to_string(), (stage, *pos_x)));
            }
            stage -= 1;

            for _ in 0..(height - 2) {
                position.push(("|".to_string(), (stage, *pos_x)));
                stage -= 1;
            }

            if right {
                position.push(("/".to_string(), (stage, *pos_x)));
            } else {
                position.push(("\\".to_string(), (stage, *pos_x)));
            }
            *pos_x += 1;
        }
    }
}

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub enum Expression {
    Number(Number),
    Variable(Variable),
    Constant(ConstantKind),
    Complex(Box<Complex>),
    Addition(Box<Addition>),
    Multiplication(Box<Multiplication>),
    Exponentiation(Box<Exponentiation>),
    Fraction(Box<Fraction>),
    Equality(Box<Equality>),
    Negation(Box<Negation>),
    Function(Box<FunctionType>),
    ImaginaryUnit,
    Derivation(Box<Derivation>),
}

// Helper constructor
impl Expression {
    pub fn addition(left: Expression, right: Expression) -> Expression {
        Expression::Addition(Box::new(Addition::new(left, right)))
    }

    pub fn addition_from_vec(sub_expr: Vec<Expression>) -> Expression {
        Expression::Addition(Box::new(Addition::from_vec(sub_expr)))
    }

    pub fn substraction(left: Expression, right: Expression) -> Expression {
        Expression::Addition(Box::new(Addition::new(left, Expression::negation(right))))
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

    pub fn number(num: i64) -> Expression {
        Expression::Number(Number::new(num))
    }

    pub fn complex(real: Expression, imaginary: Expression) -> Expression {
        Expression::Complex(Box::new(Complex::new(real, imaginary)))
    }

    pub fn derivation(
        expression: Expression,
        derivation_variable: Variable,
        derivation_degree: Option<Expression>,
    ) -> Expression {
        Expression::Derivation(Box::new(Derivation::new(
            expression,
            derivation_variable,
            derivation_degree,
        )))
    }
}

impl Expression {
    pub fn equal(&self, other: &Expression) -> bool {
        match (self, other) {
            (Expression::Number(left), Expression::Number(right)) => left.equal(right),
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
            (Expression::Complex(left), Expression::Complex(right)) => left.equal(right),
            (Expression::ImaginaryUnit, Expression::ImaginaryUnit) => true,
            // (Expression::Error, Expression::Error) => true,
            (Expression::Derivation(left), Expression::Derivation(right)) => left.equal(right),
            _ => false,
        }
    }

    pub fn simplify(self) -> Expression {
        match self {
            Expression::Number(num) => num.simplify(),
            Expression::Variable(var) => var.simplify(),
            Expression::Constant(cons) => cons.simplify(),
            Expression::Addition(add) => add.simplify(),
            Expression::Multiplication(mult) => mult.simplify(),
            Expression::Exponentiation(expo) => expo.simplify(),
            Expression::Fraction(frac) => frac.simplify(),
            Expression::Equality(eq) => eq.simplify(),
            Expression::Negation(neg) => neg.simplify(),
            Expression::Function(func) => func.simplify(),
            Expression::Complex(complex) => complex.simplify(),
            Expression::ImaginaryUnit => self,
            Expression::Derivation(derivation) => derivation.simplify(),
        }
    }

    pub fn get_order(&self) -> i64 {
        match self {
            Expression::Number(num) => num.get_order(),
            Expression::Variable(var) => var.get_order(),
            Expression::Constant(cons) => cons.get_order(),
            Expression::Addition(add) => add.get_order(),
            Expression::Multiplication(mult) => mult.get_order(),
            Expression::Exponentiation(expo) => expo.get_order(),
            Expression::Fraction(frac) => frac.get_order(),
            Expression::Equality(eq) => eq.get_order(),
            Expression::Negation(neg) => neg.get_order(),
            Expression::Function(func) => func.get_order(),
            Expression::Complex(complex) => complex.get_order(),
            Expression::ImaginaryUnit => 2,
            Expression::Derivation(derivation) => derivation.get_order(),
        }
    }

    pub fn contain_vars(&self) -> Option<HashMap<Variable, usize>> {
        match self {
            Expression::Number(num) => num.contain_vars(),
            Expression::Variable(var) => var.contain_vars(),
            Expression::Constant(cons) => cons.contain_vars(),
            Expression::Addition(add) => add.contain_vars(),
            Expression::Multiplication(mult) => mult.contain_vars(),
            Expression::Exponentiation(expo) => expo.contain_vars(),
            Expression::Fraction(frac) => frac.contain_vars(),
            Expression::Equality(eq) => eq.contain_vars(),
            Expression::Negation(neg) => neg.contain_vars(),
            Expression::Function(func) => func.contain_vars(),
            Expression::Complex(complex) => complex.contain_vars(),
            Expression::ImaginaryUnit => None,
            Expression::Derivation(derivation) => derivation.contain_vars(),
        }
    }

    pub fn contain_var(&self, variable: &Variable) -> bool {
        match self {
            Expression::Number(num) => num.contain_var(variable),
            Expression::Variable(var) => var.contain_var(variable),
            Expression::Constant(cons) => cons.contain_var(variable),
            Expression::Addition(add) => add.contain_var(variable),
            Expression::Multiplication(mult) => mult.contain_var(variable),
            Expression::Exponentiation(expo) => expo.contain_var(variable),
            Expression::Fraction(frac) => frac.contain_var(variable),
            Expression::Equality(eq) => eq.contain_var(variable),
            Expression::Negation(neg) => neg.contain_var(variable),
            Expression::Function(func) => func.contain_var(variable),
            Expression::Complex(complex) => complex.contain_var(variable),
            Expression::ImaginaryUnit => false,
            Expression::Derivation(derivation) => derivation.contain_var(variable),
        }
    }
}

// Print functions
impl Expression {
    pub fn print_tree(&self, span: Option<&str>) {
        match self {
            Expression::Number(num) => num.print_tree(span),
            Expression::Variable(var) => var.print_tree(span),
            Expression::Constant(cons) => cons.print_tree(span),
            Expression::Addition(add) => add.print_tree(span),
            Expression::Multiplication(mult) => mult.print_tree(span),
            Expression::Exponentiation(expo) => expo.print_tree(span),
            Expression::Fraction(frac) => frac.print_tree(span),
            Expression::Equality(eq) => eq.print_tree(span),
            Expression::Negation(neg) => neg.print_tree(span),
            Expression::Function(func) => func.print_tree(span),
            Expression::Complex(complex) => complex.print_tree(span),
            Expression::ImaginaryUnit => {
                println!("{}Imaginary unit", span.unwrap_or(""))
            }
            Expression::Derivation(derivation) => derivation.print_tree(span),
            // Expression::Error => println!("{}Error", span.unwrap_or("")),
        }
    }

    pub fn print_console(&self) {
        let mut position: Vec<(String, (i8, i8))> = Vec::new();

        // memoize (length, height, above_height)
        let mut memoize: HashMap<Expression, (i8, i8, i8)> = HashMap::new();

        // println!("{:?}", self);

        self.calc_pos(&mut position, State::Same(0, 0), &mut memoize);

        let (min_x, max_x, min_y, max_y) = position.iter().fold(
            (0, 0, 0, 0),
            |(min_x, max_x, min_y, max_y), &(_, (y, x))| {
                (min_x.min(x), max_x.max(x), min_y.min(y), max_y.max(y))
            },
        );

        let mut grid = HashMap::new();

        // println!("{:?}", position);

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

    fn calc_pos(
        &self,
        position: &mut Vec<(String, (i8, i8))>,
        prev_state: State,
        memoized: &mut HashMap<Expression, (i8, i8, i8)>,
    ) {
        match self {
            Expression::Number(num) => num.calc_pos(position, prev_state, memoized),
            Expression::Variable(var) => var.calc_pos(position, prev_state, memoized),
            Expression::Constant(cons) => cons.calc_pos(position, prev_state, memoized),
            Expression::Addition(add) => add.calc_pos(position, prev_state, memoized),
            Expression::Multiplication(mult) => mult.calc_pos(position, prev_state, memoized),
            Expression::Exponentiation(expo) => expo.calc_pos(position, prev_state, memoized),
            Expression::Fraction(frac) => frac.calc_pos(position, prev_state, memoized),
            Expression::Equality(eq) => eq.calc_pos(position, prev_state, memoized),
            Expression::Negation(neg) => neg.calc_pos(position, prev_state, memoized),
            Expression::Function(func) => func.calc_pos(position, prev_state, memoized),
            Expression::Complex(complex) => complex.calc_pos(position, prev_state, memoized),
            Expression::ImaginaryUnit => position.push((String::from("i"), prev_state.get_pos())),
            Expression::Derivation(derivation) => {
                derivation.calc_pos(position, prev_state, memoized)
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
            Expression::Number(num) => num.get_length(memoized),
            Expression::Variable(var) => var.get_length(memoized),
            Expression::Constant(cons) => cons.get_length(memoized),
            Expression::Addition(add) => add.get_length(memoized),
            Expression::Multiplication(mult) => mult.get_length(memoized),
            Expression::Exponentiation(expo) => expo.get_length(memoized),
            Expression::Fraction(frac) => frac.get_length(memoized),
            Expression::Equality(eq) => eq.get_length(memoized),
            Expression::Negation(neg) => neg.get_length(memoized),
            Expression::Function(func) => func.get_length(memoized),
            Expression::Complex(complex) => complex.get_length(memoized),
            Expression::ImaginaryUnit => 1,
            Expression::Derivation(derivation) => derivation.get_length(memoized),
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
            Expression::Number(num) => num.get_height(memoized),
            Expression::Variable(var) => var.get_height(memoized),
            Expression::Constant(cons) => cons.get_height(memoized),
            Expression::Addition(add) => add.get_height(memoized),
            Expression::Multiplication(mult) => mult.get_height(memoized),
            Expression::Exponentiation(expo) => expo.get_height(memoized),
            Expression::Fraction(frac) => frac.get_height(memoized),
            Expression::Equality(eq) => eq.get_height(memoized),
            Expression::Negation(neg) => neg.get_height(memoized),
            Expression::Function(func) => func.get_height(memoized),
            Expression::Complex(complex) => complex.get_height(memoized),
            Expression::ImaginaryUnit => 1,
            Expression::Derivation(derivation) => derivation.get_height(memoized),
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
            Expression::Number(num) => num.get_above_height(memoized),
            Expression::Variable(var) => var.get_above_height(memoized),
            Expression::Constant(cons) => cons.get_above_height(memoized),
            Expression::Addition(add) => add.get_above_height(memoized),
            Expression::Multiplication(mult) => mult.get_above_height(memoized),
            Expression::Exponentiation(expo) => expo.get_above_height(memoized),
            Expression::Fraction(frac) => frac.get_above_height(memoized),
            Expression::Equality(eq) => eq.get_above_height(memoized),
            Expression::Negation(neg) => neg.get_above_height(memoized),
            Expression::Function(func) => func.get_above_height(memoized),
            Expression::Complex(complex) => complex.get_above_height(memoized),
            Expression::ImaginaryUnit => 1,
            Expression::Derivation(derivation) => derivation.get_above_height(memoized),
        };

        if let Some((_l, _h, ah)) = memoized.get_mut(self) {
            *ah = above_height;
        } else {
            memoized.insert(self.clone(), (0, 0, above_height));
        }

        above_height
    }

    pub fn make_parenthesis(
        &self,
        pos_y: &mut i8,
        pos_x: &mut i8,
        position: &mut Vec<(String, (i8, i8))>,
        right: bool,
        memoized: &mut HashMap<Expression, (i8, i8, i8)>,
    ) {
        match self {
            Expression::Number(number) => {
                number.make_parenthesis(pos_y, pos_x, position, right, memoized)
            }
            Expression::Variable(varibale) => {
                varibale.make_parenthesis(pos_y, pos_x, position, right, memoized)
            }
            Expression::Constant(constant) => {
                constant.make_parenthesis(pos_y, pos_x, position, right, memoized)
            }
            Expression::Complex(complex) => {
                complex.make_parenthesis(pos_y, pos_x, position, right, memoized)
            }
            Expression::Addition(addition) => {
                addition.make_parenthesis(pos_y, pos_x, position, right, memoized)
            }
            Expression::Multiplication(multiplication) => {
                multiplication.make_parenthesis(pos_y, pos_x, position, right, memoized)
            }
            Expression::Exponentiation(exponentiation) => {
                exponentiation.make_parenthesis(pos_y, pos_x, position, right, memoized)
            }
            Expression::Fraction(fraction) => {
                fraction.make_parenthesis(pos_y, pos_x, position, right, memoized)
            }
            Expression::Equality(equality) => {
                equality.make_parenthesis(pos_y, pos_x, position, right, memoized)
            }
            Expression::Negation(negation) => {
                negation.make_parenthesis(pos_y, pos_x, position, right, memoized)
            }
            Expression::Function(function) => {
                function.make_parenthesis(pos_y, pos_x, position, right, memoized)
            }
            Expression::ImaginaryUnit => {}
            Expression::Derivation(derivation) => {
                derivation.make_parenthesis(pos_y, pos_x, position, right, memoized)
            }
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
    pub fn get_pos_y(&self) -> i8 {
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
            Expression::Negation(neg) => write!(f, "{}", neg),
            Expression::Function(func) => write!(f, "{}", func),
            Expression::Complex(complex) => write!(f, "{}", complex),
            Expression::ImaginaryUnit => write!(f, "i"),
            Expression::Derivation(derivation) => write!(f, "{}", derivation),
            // Expression::Error => write!(f, "Error"),
        }
    }
}

#[cfg(test)]
mod test_simplify {

    use crate::ast::{
        function::{FunctionType, PredefinedFunction},
        varibale::Variable,
        ConstantKind, Expression,
    };

    #[test]
    fn test_simplify_addition() {
        // 1+2+3
        assert_eq!(
            Expression::addition(
                Expression::addition(Expression::number(1), Expression::number(2)),
                Expression::number(3),
            )
            .simplify(),
            Expression::number(6)
        );

        // 1 + x +3 +Y
        assert!(Expression::addition_from_vec(vec![
            Expression::number(1),
            Expression::variable("x".to_string()),
            Expression::number(3),
            Expression::Variable(Variable::new("Y".to_string())),
        ])
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::number(4),
            Expression::variable("x".to_string()),
            Expression::variable("Y".to_string()),
        ])));

        // 2 +x +4 +X +x
        assert!(Expression::addition_from_vec(vec![
            Expression::number(2),
            Expression::variable("x".to_string()),
            Expression::number(4),
            Expression::Variable(Variable::new("X".to_string())),
            Expression::variable("x".to_string()),
        ])
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::number(6),
            Expression::variable("X".to_string()),
            Expression::multiplication(
                Expression::number(2),
                Expression::variable("x".to_string()),
            ),
        ])));

        // 2x + 2y + x
        assert!(Expression::addition_from_vec(vec![
            Expression::multiplication(
                Expression::number(2),
                Expression::variable("x".to_string()),
            ),
            Expression::multiplication(
                Expression::number(2),
                Expression::variable("y".to_string()),
            ),
            Expression::variable("x".to_string()),
        ])
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::multiplication(
                Expression::number(3),
                Expression::variable("x".to_string()),
            ),
            Expression::multiplication(
                Expression::number(2),
                Expression::variable("y".to_string()),
            ),
        ])));

        // x^2 + y^2 + 2xy + x^2
        assert!(Expression::addition_from_vec(vec![
            Expression::exponentiation(
                Expression::variable("x".to_string()),
                Expression::number(2)
            ),
            Expression::exponentiation(
                Expression::variable("y".to_string()),
                Expression::number(2)
            ),
            Expression::multiplication(
                Expression::multiplication(
                    Expression::number(2),
                    Expression::variable("x".to_string())
                ),
                Expression::variable("y".to_string()),
            ),
            Expression::exponentiation(
                Expression::variable("x".to_string()),
                Expression::number(2)
            ),
        ])
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::multiplication(
                Expression::number(2),
                Expression::exponentiation(
                    Expression::variable("x".to_string()),
                    Expression::number(2)
                )
            ),
            Expression::multiplication_from_vec(vec![
                Expression::number(2),
                Expression::variable("x".to_string()),
                Expression::variable("y".to_string()),
            ]),
            Expression::exponentiation(
                Expression::variable("y".to_string()),
                Expression::number(2)
            ),
        ])));

        // a/b + b/a
        assert!(Expression::addition(
            Expression::fraction(
                Expression::variable("a".to_string()),
                Expression::variable("b".to_string())
            ),
            Expression::fraction(
                Expression::variable("b".to_string()),
                Expression::variable("a".to_string())
            ),
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::addition(
                Expression::exponentiation(
                    Expression::variable("a".to_string()),
                    Expression::number(2)
                ),
                Expression::exponentiation(
                    Expression::variable("b".to_string()),
                    Expression::number(2)
                ),
            ),
            Expression::multiplication(
                Expression::variable("a".to_string()),
                Expression::variable("b".to_string())
            ),
        )));

        // e + e
        assert!(Expression::addition(
            Expression::Constant(ConstantKind::E),
            Expression::Constant(ConstantKind::E)
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::number(2),
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
            Expression::number(2),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::variable("x".to_string())]
            )),
        )));

        // 2sin(x) + sin(x)
        assert!(Expression::addition(
            Expression::multiplication(
                Expression::number(2),
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
            Expression::number(3),
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
                Expression::number(2),
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Ln,
                    vec![Expression::variable("x".to_string())]
                ))
            ),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::addition(
                    Expression::variable("x".to_string()),
                    Expression::number(1)
                )]
            )),
        )
        .simplify()
        .equal(&Expression::function(FunctionType::Predefined(
            PredefinedFunction::Ln,
            vec![Expression::addition(
                Expression::exponentiation(
                    Expression::variable("x".to_string()),
                    Expression::number(3)
                ),
                Expression::exponentiation(
                    Expression::variable("x".to_string()),
                    Expression::number(2)
                ),
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
            Expression::number(2),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::variable("x".to_string()),]
            ))
        )));

        // 2ln(x) + ln(x)
        assert!(Expression::addition(
            Expression::multiplication(
                Expression::number(2),
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
            Expression::number(3),
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
            Expression::number(2),
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
                Expression::multiplication(Expression::number(1), Expression::number(2)),
                Expression::number(3),
            )
            .simplify(),
            Expression::number(6)
        );

        // 1 * x *3 *Y
        assert!(Expression::multiplication_from_vec(vec![
            Expression::number(1),
            Expression::variable("x".to_string()),
            Expression::number(3),
            Expression::variable("Y".to_string()),
        ])
        .simplify()
        .equal(&Expression::multiplication_from_vec(vec![
            Expression::number(3),
            Expression::variable("x".to_string()),
            Expression::variable("Y".to_string()),
        ])));

        // 2 *x *4 *X *x
        assert!(Expression::multiplication_from_vec(vec![
            Expression::number(2),
            Expression::variable("x".to_string()),
            Expression::number(4),
            Expression::variable("X".to_string()),
            Expression::variable("x".to_string()),
        ])
        .simplify()
        .equal(&Expression::multiplication_from_vec(vec![
            Expression::number(8),
            Expression::variable("X".to_string()),
            Expression::exponentiation(
                Expression::variable("x".to_string()),
                Expression::number(2)
            ),
        ])));

        // 2x * 2y * x
        assert!(Expression::multiplication_from_vec(vec![
            Expression::multiplication(
                Expression::number(2),
                Expression::variable("x".to_string()),
            ),
            Expression::multiplication(
                Expression::number(2),
                Expression::variable("y".to_string()),
            ),
            Expression::variable("x".to_string()),
        ])
        .simplify()
        .equal(&Expression::multiplication_from_vec(vec![
            Expression::number(4),
            Expression::variable("y".to_string()),
            Expression::exponentiation(
                Expression::variable("x".to_string()),
                Expression::number(2)
            ),
        ])));

        // x^2 * y^2 * 2xy * x^2
        assert!(Expression::multiplication_from_vec(vec![
            Expression::exponentiation(
                Expression::variable("x".to_string()),
                Expression::number(2)
            ),
            Expression::exponentiation(
                Expression::variable("y".to_string()),
                Expression::number(2)
            ),
            Expression::multiplication(
                Expression::multiplication(
                    Expression::number(2),
                    Expression::variable("x".to_string())
                ),
                Expression::variable("y".to_string()),
            ),
            Expression::exponentiation(
                Expression::variable("x".to_string()),
                Expression::number(2)
            ),
        ])
        .simplify()
        .equal(&Expression::multiplication_from_vec(vec![
            Expression::exponentiation(
                Expression::variable("x".to_string()),
                Expression::number(5)
            ),
            Expression::number(2),
            Expression::exponentiation(
                Expression::variable("y".to_string()),
                Expression::number(3)
            ),
        ])));

        // a/b * b/a
        assert!(Expression::multiplication(
            Expression::fraction(
                Expression::variable("a".to_string()),
                Expression::variable("b".to_string())
            ),
            Expression::fraction(
                Expression::variable("b".to_string()),
                Expression::variable("a".to_string())
            ),
        )
        .simplify()
        .equal(&Expression::number(1)));

        // a/b * c/d
        assert!(Expression::multiplication(
            Expression::fraction(
                Expression::variable("a".to_string()),
                Expression::variable("b".to_string())
            ),
            Expression::fraction(
                Expression::variable("c".to_string()),
                Expression::variable("d".to_string())
            ),
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::multiplication(
                Expression::variable("a".to_string()),
                Expression::variable("c".to_string())
            ),
            Expression::multiplication(
                Expression::variable("b".to_string()),
                Expression::variable("d".to_string())
            ),
        )));

        // e * e
        assert!(Expression::multiplication(
            Expression::Constant(ConstantKind::E),
            Expression::Constant(ConstantKind::E)
        )
        .simplify()
        .equal(&Expression::exponentiation(
            Expression::Constant(ConstantKind::E),
            Expression::number(2)
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
            Expression::number(2)
        )));

        // 2sin(x) * sin(x)
        assert!(Expression::multiplication(
            Expression::multiplication(
                Expression::number(2),
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
            Expression::number(2),
            Expression::exponentiation(
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sin,
                    vec![Expression::variable("x".to_string())]
                )),
                Expression::number(2)
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
            Expression::number(2)
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
            Expression::number(2)
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
            Expression::exponentiation(Expression::number(1), Expression::number(2)).simplify(),
            Expression::number(1)
        );

        // 2^2
        assert_eq!(
            Expression::exponentiation(Expression::number(2), Expression::number(2)).simplify(),
            Expression::number(4)
        );

        // 2^2^3
        assert_eq!(
            Expression::exponentiation(
                Expression::number(2),
                Expression::exponentiation(Expression::number(2), Expression::number(3))
            )
            .simplify(),
            Expression::number(256)
        );

        // (2^2)^2
        assert_eq!(
            Expression::exponentiation(
                Expression::exponentiation(Expression::number(2), Expression::number(2)),
                Expression::number(2)
            )
            .simplify(),
            Expression::number(16)
        );

        // x^x
        assert!(Expression::exponentiation(
            Expression::variable("x".to_string()),
            Expression::variable("x".to_string())
        )
        .simplify()
        .equal(&Expression::exponentiation(
            Expression::variable("x".to_string()),
            Expression::variable("x".to_string())
        )));

        // x^log(x, y)
        assert!(Expression::exponentiation(
            Expression::variable("x".to_string()),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Log,
                vec![
                    Expression::variable("x".to_string()),
                    Expression::variable("y".to_string())
                ]
            ))
        )
        .simplify()
        .equal(&Expression::variable("y".to_string()),));

        // x^log(y, x)
        assert!(Expression::exponentiation(
            Expression::variable("x".to_string()),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Log,
                vec![
                    Expression::variable("y".to_string()),
                    Expression::variable("x".to_string())
                ]
            ))
        )
        .simplify()
        .equal(&Expression::exponentiation(
            Expression::variable("x".to_string()),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Log,
                vec![
                    Expression::variable("y".to_string()),
                    Expression::variable("x".to_string())
                ]
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
            Expression::fraction(Expression::number(1), Expression::number(2)),
            Expression::number(2)
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::number(1),
            Expression::number(4)
        )));

        // (ab)^2
        assert!(Expression::exponentiation(
            Expression::multiplication(
                Expression::variable("a".to_string()),
                Expression::variable("b".to_string()),
            ),
            Expression::number(2)
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::exponentiation(
                Expression::variable("a".to_string()),
                Expression::number(2),
            ),
            Expression::exponentiation(
                Expression::variable("b".to_string()),
                Expression::number(2),
            ),
        )));

        // (a+b)^2
        assert!(Expression::exponentiation(
            Expression::addition(
                Expression::variable("a".to_string()),
                Expression::variable("b".to_string()),
            ),
            Expression::number(2)
        )
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::exponentiation(
                Expression::variable("a".to_string()),
                Expression::number(2),
            ),
            Expression::multiplication_from_vec(vec![
                Expression::number(2),
                Expression::variable("a".to_string()),
                Expression::variable("b".to_string()),
            ]),
            Expression::exponentiation(
                Expression::variable("b".to_string()),
                Expression::number(2),
            ),
        ])));

        // (a-b)^2
        assert!(Expression::exponentiation(
            Expression::addition(
                Expression::variable("a".to_string()),
                Expression::negation(Expression::variable("b".to_string()),),
            ),
            Expression::number(2)
        )
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::exponentiation(
                Expression::variable("a".to_string()),
                Expression::number(2),
            ),
            Expression::multiplication_from_vec(vec![
                Expression::number(2),
                Expression::variable("a".to_string()),
                Expression::negation(Expression::variable("b".to_string())),
            ]),
            Expression::exponentiation(
                Expression::negation(Expression::variable("b".to_string())),
                Expression::number(2),
            ),
        ])));

        // (a+b)^3
        assert!(Expression::exponentiation(
            Expression::addition(
                Expression::variable("a".to_string()),
                Expression::variable("b".to_string()),
            ),
            Expression::number(3)
        )
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::exponentiation(
                Expression::variable("a".to_string()),
                Expression::number(3),
            ),
            Expression::multiplication_from_vec(vec![
                Expression::number(3),
                Expression::exponentiation(
                    Expression::variable("a".to_string()),
                    Expression::number(2),
                ),
                Expression::variable("b".to_string()),
            ]),
            Expression::multiplication_from_vec(vec![
                Expression::number(3),
                Expression::exponentiation(
                    Expression::variable("b".to_string()),
                    Expression::number(2),
                ),
                Expression::variable("a".to_string()),
            ]),
            Expression::exponentiation(
                Expression::variable("b".to_string()),
                Expression::number(3),
            ),
        ])));

        //(a+b+c)^2
        assert!(Expression::exponentiation(
            Expression::addition_from_vec(vec![
                Expression::variable("a".to_string()),
                Expression::variable("b".to_string()),
                Expression::variable("c".to_string()),
            ]),
            Expression::number(2)
        )
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::exponentiation(
                Expression::variable("a".to_string()),
                Expression::number(2),
            ),
            Expression::multiplication_from_vec(vec![
                Expression::number(2),
                Expression::variable("a".to_string()),
                Expression::variable("b".to_string()),
            ]),
            Expression::multiplication_from_vec(vec![
                Expression::number(2),
                Expression::variable("a".to_string()),
                Expression::variable("c".to_string()),
            ]),
            Expression::exponentiation(
                Expression::variable("b".to_string()),
                Expression::number(2),
            ),
            Expression::multiplication_from_vec(vec![
                Expression::number(2),
                Expression::variable("b".to_string()),
                Expression::variable("c".to_string()),
            ]),
            Expression::exponentiation(
                Expression::variable("c".to_string()),
                Expression::number(2),
            ),
        ])));
    }

    #[test]
    fn test_simplify_fraction() {
        // 1/2
        assert!(
            Expression::fraction(Expression::number(1), Expression::number(2))
                .simplify()
                .equal(&Expression::fraction(
                    Expression::number(1),
                    Expression::number(2)
                ))
        );

        // 2/2
        assert!(
            Expression::fraction(Expression::number(2), Expression::number(2))
                .simplify()
                .equal(&Expression::number(1))
        );

        // 2/4
        assert!(
            Expression::fraction(Expression::number(2), Expression::number(4))
                .simplify()
                .equal(&Expression::fraction(
                    Expression::number(1),
                    Expression::number(2)
                ))
        );

        // 2/4/2
        assert!(Expression::fraction(
            Expression::fraction(Expression::number(2), Expression::number(4)),
            Expression::number(2)
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::number(1),
            Expression::number(4)
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
            Expression::exponentiation(
                Expression::variable("a".to_string()),
                Expression::variable("n".to_string())
            )
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::number(1),
            Expression::exponentiation(
                Expression::variable("a".to_string()),
                Expression::addition(
                    Expression::variable("n".to_string()),
                    Expression::number(-1)
                )
            )
        )));

        // a^2/a
        assert!(Expression::fraction(
            Expression::exponentiation(
                Expression::variable("a".to_string()),
                Expression::number(2)
            ),
            Expression::variable("a".to_string())
        )
        .simplify()
        .equal(&Expression::variable("a".to_string())));

        // a^2/a^3
        assert!(Expression::fraction(
            Expression::exponentiation(
                Expression::variable("a".to_string()),
                Expression::number(2)
            ),
            Expression::exponentiation(
                Expression::variable("a".to_string()),
                Expression::number(3)
            )
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::number(1),
            Expression::variable("a".to_string())
        )));

        // 2^n/2
        assert!(Expression::fraction(
            Expression::exponentiation(
                Expression::number(2),
                Expression::variable("n".to_string())
            ),
            Expression::number(2)
        )
        .simplify()
        .equal(&Expression::exponentiation(
            Expression::number(2),
            Expression::addition(
                Expression::variable("n".to_string()),
                Expression::number(-1)
            )
        )));

        // 2 / 2^n
        assert!(Expression::fraction(
            Expression::number(2),
            Expression::exponentiation(
                Expression::number(2),
                Expression::variable("n".to_string())
            )
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::number(1),
            Expression::exponentiation(
                Expression::number(2),
                Expression::addition(
                    Expression::variable("n".to_string()),
                    Expression::number(-1)
                )
            )
        )));

        // 4a / 2
        assert!(Expression::fraction(
            Expression::multiplication(
                Expression::number(4),
                Expression::variable("a".to_string())
            ),
            Expression::number(2)
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::number(2),
            Expression::variable("a".to_string())
        )));

        // 4a / 2b
        assert!(Expression::fraction(
            Expression::multiplication(
                Expression::number(4),
                Expression::variable("a".to_string())
            ),
            Expression::multiplication(
                Expression::number(2),
                Expression::variable("b".to_string())
            )
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::multiplication(
                Expression::number(2),
                Expression::variable("a".to_string())
            ),
            Expression::variable("b".to_string())
        )));

        // (12a + 6)/(24a^2)
        assert!(Expression::fraction(
            Expression::addition(
                Expression::multiplication(
                    Expression::number(12),
                    Expression::variable("a".to_string())
                ),
                Expression::number(6)
            ),
            Expression::multiplication(
                Expression::number(24),
                Expression::exponentiation(
                    Expression::variable("a".to_string()),
                    Expression::number(2)
                )
            )
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::addition(
                Expression::number(1),
                Expression::multiplication(
                    Expression::number(2),
                    Expression::variable("a".to_string())
                )
            ),
            Expression::multiplication(
                Expression::number(4),
                Expression::exponentiation(
                    Expression::variable("a".to_string()),
                    Expression::number(2)
                )
            )
        )));

        // (18x^2y + 3xy + 9xy^2) / (6x^2y)
        assert!(Expression::fraction(
            Expression::addition_from_vec(vec![
                Expression::multiplication(
                    Expression::multiplication(
                        Expression::number(18),
                        Expression::variable("y".to_string())
                    ),
                    Expression::exponentiation(
                        Expression::variable("x".to_string()),
                        Expression::number(2)
                    )
                ),
                Expression::multiplication(
                    Expression::multiplication(
                        Expression::number(3),
                        Expression::variable("x".to_string())
                    ),
                    Expression::variable("y".to_string())
                ),
                Expression::multiplication(
                    Expression::multiplication(
                        Expression::number(9),
                        Expression::variable("x".to_string())
                    ),
                    Expression::exponentiation(
                        Expression::variable("y".to_string()),
                        Expression::number(2)
                    )
                ),
            ]),
            Expression::multiplication(
                Expression::multiplication(
                    Expression::number(6),
                    Expression::variable("y".to_string())
                ),
                Expression::exponentiation(
                    Expression::variable("x".to_string()),
                    Expression::number(2)
                )
            )
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::addition_from_vec(vec![
                Expression::multiplication(
                    Expression::number(3),
                    Expression::variable("y".to_string())
                ),
                Expression::number(1),
                Expression::multiplication(
                    Expression::number(6),
                    Expression::variable("x".to_string())
                ),
            ]),
            Expression::multiplication(
                Expression::number(2),
                Expression::variable("x".to_string())
            )
        )));

        // (a/b)/(c/d)
        assert!(Expression::fraction(
            Expression::fraction(
                Expression::variable("a".to_string()),
                Expression::variable("b".to_string()),
            ),
            Expression::fraction(
                Expression::variable("c".to_string()),
                Expression::variable("d".to_string()),
            ),
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::multiplication(
                Expression::variable("a".to_string()),
                Expression::variable("d".to_string()),
            ),
            Expression::multiplication(
                Expression::variable("b".to_string()),
                Expression::variable("c".to_string()),
            ),
        )));

        Expression::fraction(
            Expression::fraction(
                Expression::variable("a".to_string()),
                Expression::variable("b".to_string()),
            ),
            Expression::variable("c".to_string()),
        )
        .simplify()
        .print_tree(None);

        // (a/b)/c
        assert!(Expression::fraction(
            Expression::fraction(
                Expression::variable("a".to_string()),
                Expression::variable("b".to_string()),
            ),
            Expression::variable("c".to_string()),
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::variable("a".to_string()),
            Expression::multiplication(
                Expression::variable("b".to_string()),
                Expression::variable("c".to_string()),
            ),
        )));

        // a/(b/c)
        assert!(Expression::fraction(
            Expression::variable("a".to_string()),
            Expression::fraction(
                Expression::variable("b".to_string()),
                Expression::variable("c".to_string()),
            ),
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::multiplication(
                Expression::variable("a".to_string()),
                Expression::variable("c".to_string()),
            ),
            Expression::variable("b".to_string()),
        )));
    }

    #[test]
    fn test_negation() {
        // --2
        assert!(
            Expression::negation(Expression::negation(Expression::number(2)))
                .simplify()
                .equal(&Expression::number(2))
        );

        // -2
        assert!(Expression::negation(Expression::number(2))
            .simplify()
            .equal(&Expression::number(-2)));

        // -2 + 3
        assert!(Expression::addition(
            Expression::negation(Expression::number(2)),
            Expression::number(3)
        )
        .simplify()
        .equal(&Expression::number(1)));

        // -2 - 3
        assert!(Expression::addition(
            Expression::negation(Expression::number(2)),
            Expression::negation(Expression::number(3))
        )
        .simplify()
        .equal(&Expression::number(-5)));

        // -2 * 2
        assert!(Expression::multiplication(
            Expression::negation(Expression::number(2)),
            Expression::number(2)
        )
        .simplify()
        .equal(&Expression::number(-4)));

        // -2 * -2
        assert!(Expression::multiplication(
            Expression::negation(Expression::number(2)),
            Expression::negation(Expression::number(2))
        )
        .simplify()
        .equal(&Expression::number(4)));
    }

    #[test]
    fn test_derivation() {
        // d/dx(x)
        assert!(Expression::derivation(
            Expression::variable(String::from("x")),
            Variable::new(String::from("x")),
            Some(Expression::number(1))
        )
        .simplify()
        .equal(&Expression::number(1)));

        // d/dx(x^2)
        assert!(Expression::derivation(
            Expression::exponentiation(
                Expression::variable(String::from("x")),
                Expression::number(2)
            ),
            Variable::new(String::from("x")),
            Some(Expression::number(1))
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::number(2),
            Expression::variable(String::from("x"))
        )));

        // d/dx(x + 8)
        assert!(Expression::derivation(
            Expression::addition(
                Expression::variable(String::from("x")),
                Expression::number(8)
            ),
            Variable::new(String::from("x")),
            Some(Expression::number(1))
        )
        .simplify()
        .equal(&Expression::number(1)));

        // d/dx(x + y)
        assert!(Expression::derivation(
            Expression::addition(
                Expression::variable(String::from("x")),
                Expression::variable(String::from("y"))
            ),
            Variable::new(String::from("x")),
            Some(Expression::number(1))
        )
        .simplify()
        .equal(&Expression::number(1)));

        // d/dx(2 * x)
        assert!(Expression::derivation(
            Expression::multiplication(
                Expression::number(2),
                Expression::variable(String::from("x"))
            ),
            Variable::new(String::from("x")),
            Some(Expression::number(1))
        )
        .simplify()
        .equal(&Expression::number(2)));

        // d/dx(2 * x^2)
        assert!(Expression::derivation(
            Expression::multiplication(
                Expression::number(2),
                Expression::exponentiation(
                    Expression::variable(String::from("x")),
                    Expression::number(2)
                )
            ),
            Variable::new(String::from("x")),
            Some(Expression::number(1))
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::number(4),
            Expression::variable(String::from("x"))
        )));

        // d/dx(x * a^x)
        assert!(Expression::derivation(
            Expression::multiplication(
                Expression::variable(String::from("x")),
                Expression::exponentiation(
                    Expression::variable(String::from("a")),
                    Expression::variable(String::from("x"))
                )
            ),
            Variable::new(String::from("x")),
            Some(Expression::number(1))
        )
        .simplify()
        .equal(&Expression::addition(
            Expression::multiplication(
                Expression::variable(String::from("a")),
                Expression::exponentiation(
                    Expression::variable(String::from("a")),
                    Expression::variable(String::from("x"))
                )
            ),
            Expression::exponentiation(
                Expression::variable(String::from("a")),
                Expression::variable(String::from("x"))
            )
        )));

        // d/dx(x * a^x * e^x)
        assert!(Expression::derivation(
            Expression::multiplication(
                Expression::multiplication(
                    Expression::variable(String::from("x")),
                    Expression::exponentiation(
                        Expression::variable(String::from("a")),
                        Expression::variable(String::from("x"))
                    )
                ),
                Expression::exponentiation(
                    Expression::Constant(ConstantKind::E),
                    Expression::variable(String::from("x"))
                )
            ),
            Variable::new(String::from("x")),
            Some(Expression::number(1))
        )
        .simplify()
        .equal(&Expression::addition(
            Expression::multiplication(
                Expression::variable(String::from("a")),
                Expression::exponentiation(
                    Expression::variable(String::from("a")),
                    Expression::variable(String::from("x"))
                )
            ),
            Expression::multiplication(
                Expression::Constant(ConstantKind::E),
                Expression::exponentiation(
                    Expression::Constant(ConstantKind::E),
                    Expression::variable(String::from("x"))
                )
            )
        )));

        // d/dx(x/ln(x))
        assert!(Expression::derivation(
            Expression::fraction(
                Expression::variable(String::from("x")),
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Ln,
                    vec![Expression::variable(String::from("x"))]
                ))
            ),
            Variable::new(String::from("x")),
            Some(Expression::number(1))
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::addition(
                Expression::number(1),
                Expression::negation(Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Ln,
                    vec![Expression::variable(String::from("x"))]
                )))
            ),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::variable(String::from("x"))]
            ))
        )));

        // d/dx(x/8)
        assert!(Expression::derivation(
            Expression::fraction(
                Expression::variable(String::from("x")),
                Expression::number(8)
            ),
            Variable::new(String::from("x")),
            Some(Expression::number(1))
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::number(1),
            Expression::number(8)
        )));

        // d/dx(8/x)
        assert!(Expression::derivation(
            Expression::fraction(
                Expression::number(8),
                Expression::variable(String::from("x"))
            ),
            Variable::new(String::from("x")),
            Some(Expression::number(1))
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::number(-8),
            Expression::exponentiation(
                Expression::variable(String::from("x")),
                Expression::number(2)
            )
        )));

        // d/dx((x + 2)^2)
        assert!(Expression::derivation(
            Expression::exponentiation(
                Expression::addition(
                    Expression::variable(String::from("x")),
                    Expression::number(2)
                ),
                Expression::number(2)
            ),
            Variable::new(String::from("x")),
            Some(Expression::number(1))
        )
        .simplify()
        .equal(&Expression::addition(
            Expression::multiplication(
                Expression::number(2),
                Expression::variable(String::from("x"))
            ),
            Expression::number(4)
        )));

        // d/dx(1/(x + 2)^2)
        assert!(Expression::derivation(
            Expression::fraction(
                Expression::number(1),
                Expression::exponentiation(
                    Expression::addition(
                        Expression::variable(String::from("x")),
                        Expression::number(2)
                    ),
                    Expression::number(2)
                )
            ),
            Variable::new(String::from("x")),
            Some(Expression::number(1))
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::number(-2),
            Expression::exponentiation(
                Expression::addition(
                    Expression::variable(String::from("x")),
                    Expression::number(2)
                ),
                Expression::number(3)
            )
        )));

        // d/dx((x + 2)^-2)

        // d/dx((x + 2)^(a + 4))
        assert!(Expression::derivation(
            Expression::exponentiation(
                Expression::addition(
                    Expression::variable(String::from("x")),
                    Expression::number(2)
                ),
                Expression::addition(
                    Expression::variable(String::from("a")),
                    Expression::number(4)
                )
            ),
            Variable::new(String::from("x")),
            Some(Expression::number(1))
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::addition(
                Expression::variable(String::from("a")),
                Expression::number(4)
            ),
            Expression::exponentiation(
                Expression::addition(
                    Expression::variable(String::from("x")),
                    Expression::number(2)
                ),
                Expression::addition(
                    Expression::variable(String::from("a")),
                    Expression::number(3)
                )
            )
        )));

        // d/dx(ln(x))
        assert!(Expression::derivation(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::variable(String::from("x"))]
            )),
            Variable::new(String::from("x")),
            Some(Expression::number(1))
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::number(1),
            Expression::variable(String::from("x"))
        )));

        // d/dx(sin(x))

        // d/dx(cos(x^3))
    }
}
