use crate::ast::State;
use crate::utils::is_chain_rule_applicable;
use std::fmt::Display;

use crate::ast::Expr;
use crate::ast::Variable;
use crate::Expression;
use std::collections::HashMap;

use super::constant::ConstantKind;
use super::exponentiation::Exponentiation;
use super::fraction::Fraction;
use super::function::FunctionType;
use super::function::PredefinedFunction;
use super::multiplication::Multiplication;
use super::number::Number;

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub struct Derivation {
    pub sub_expr: Expression,
    pub derivation_variable: Variable,
    pub derivation_degree: Number,
    pub simplified: bool,
}

// Constructor and setter/getter
impl Derivation {
    pub fn new(
        expression: Expression,
        derivation_variable: Variable,
        derivation_degree: Option<Expression>,
    ) -> Self {
        Self {
            sub_expr: expression,
            derivation_variable,
            derivation_degree: if let Some(Expression::Number(num)) = derivation_degree {
                num
            } else {
                Number::new(1)
            },
            simplified: false,
        }
    }

    pub fn get_expression(&self) -> Expression {
        self.sub_expr.clone()
    }

    pub fn get_derivation_variable(&self) -> Variable {
        self.derivation_variable.clone()
    }

    pub fn get_derivation_degree(&self) -> Number {
        self.derivation_degree.clone()
    }

    // pub fn set_derivation_degree(&mut self, degree: Number) {
    //     self.derivation_degree = degree;
    // }

    pub fn set_derivation_variable(&mut self, variable: Variable) {
        self.derivation_variable = variable;
    }

    pub fn set_expression(&mut self, expression: Expression) {
        self.sub_expr = expression;
    }
}

impl Expr for Derivation {
    fn equal(&self, other: &Derivation) -> bool {
        self.get_expression().equal(&other.get_expression())
            && self
                .get_derivation_variable()
                .equal(&other.get_derivation_variable())
            && self
                .get_derivation_degree()
                .equal(&other.get_derivation_degree())
    }

    fn simplify(mut self) -> Expression {
        if self.simplified {
            return Expression::Derivation(Box::new(self));
        }

        if self.get_derivation_degree().sub_expr > 1 {
            let mut expr = self.get_expression();
            for _ in 0..self.get_derivation_degree().sub_expr {
                expr = Expression::derivation(
                    expr,
                    self.derivation_variable.clone(),
                    Some(Expression::number(1)),
                )
            }
            return expr.simplify();
        }

        // TODO implement chain rule
        self.simplified = true;

        match self.get_expression() {
            Expression::Number(_) => Expression::number(0),
            Expression::Variable(var) => {
                if var.equal(&self.get_derivation_variable()) {
                    Expression::number(1)
                } else {
                    Expression::number(0)
                }
            }
            Expression::Constant(_) => Expression::number(0),
            Expression::Complex(_) => todo!(),
            Expression::Addition(add) => {
                let new_add: Vec<Expression> = add
                    .sub_expr
                    .iter()
                    .map(|expr| {
                        Expression::derivation(
                            expr.clone(),
                            self.get_derivation_variable(),
                            Some(Expression::number(1)),
                        )
                    })
                    .collect();
                Expression::addition_from_vec(new_add).simplify()
            }
            Expression::Multiplication(mult) => self.multiplication_derivation(*mult),
            Expression::Exponentiation(expo) => self.exponentiation_derivation(*expo),
            Expression::Fraction(frac) => self.fraction_derivation(*frac),
            Expression::Equality(eq) => Expression::equality(
                Expression::derivation(
                    eq.get_left_side(),
                    self.derivation_variable.clone(),
                    Some(Expression::number(1)),
                ),
                Expression::derivation(
                    eq.get_right_side(),
                    self.derivation_variable,
                    Some(Expression::number(1)),
                ),
            ),
            Expression::Negation(neg) => Expression::negation(Expression::derivation(
                neg.sub_expr,
                self.derivation_variable,
                Some(Expression::number(1)),
            ))
            .simplify(),
            Expression::Function(_) => todo!(),
            Expression::Derivation(expr) => Expression::derivation(
                expr.simplify(),
                self.derivation_variable,
                Some(Expression::number(1)),
            )
            .simplify(),
            _ => Expression::Derivation(Box::new(self)),
        }
    }

    fn contain_vars(&self) -> Option<HashMap<Variable, usize>> {
        let mut map: HashMap<Variable, usize> = HashMap::new();
        if let Some(mut sub_map) = self.sub_expr.contain_vars() {
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

    fn contain_var(&self, variable: &Variable) -> bool {
        self.sub_expr.contain_var(variable)
    }

    fn get_order(&self) -> i64 {
        self.sub_expr.get_order()
    }

    fn print_tree(&self, span: Option<&str>) {
        let current_span = span.unwrap_or("");
        let new_span = current_span.to_string() + "   ";
        println!("{}Derivate :", current_span);
        print!("{}", current_span);
        self.get_expression().print_tree(Some(&new_span));
        print!("{}", current_span);
        self.get_derivation_variable().print_tree(Some(&new_span));
        print!("{}", current_span);
        self.get_derivation_degree().print_tree(Some(&new_span));
    }

    fn calc_pos(
        &self,
        position: &mut Vec<(String, (i8, i8))>,
        prev_state: super::State,
        memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>,
    ) {
        // used to calculate the position of the fraction bar
        let (mut pos_y, mut pos_x) = match prev_state {
            State::Over(pos_y, pos_x) => (
                pos_y
                    + self.get_derivation_variable().get_height(memoized)
                    + if self.get_derivation_degree().sub_expr == 1 {
                        0
                    } else {
                        self.derivation_degree.get_height(memoized)
                    },
                pos_x,
            ),
            State::Under(pos_y, pos_x) => (pos_y - self.get_above_height(memoized), pos_x),
            State::Same(pos_y, pos_x) => (pos_y, pos_x),
        };

        // determines the length of the fraction bar
        let frac_bar_len = 1
            + self.get_derivation_variable().get_length(memoized)
            + if self.get_derivation_degree().sub_expr == 1 {
                0
            } else {
                self.derivation_degree.get_length(memoized)
            };

        for i in 0..frac_bar_len {
            position.push(("-".to_string(), (pos_y, pos_x + i)));
        }

        let d = Expression::variable(String::from("d"));

        if self.derivation_degree.sub_expr > 1 {
            d.calc_pos(position, State::Under(pos_y - 2, pos_x), memoized);
            self.get_derivation_variable().calc_pos(
                position,
                State::Under(pos_y - 2, pos_x + 1),
                memoized,
            );
            self.derivation_degree
                .calc_pos(position, State::Under(pos_y - 1, pos_x + 2), memoized);
        } else {
            d.calc_pos(position, State::Under(pos_y - 1, pos_x), memoized);
            self.get_derivation_variable().calc_pos(
                position,
                State::Under(pos_y - 1, pos_x + 1),
                memoized,
            );
        }

        d.calc_pos(position, State::Over(pos_y + 1, pos_x), memoized);
        if self.derivation_degree.sub_expr > 1 {
            self.derivation_degree
                .calc_pos(position, State::Over(pos_y + 2, pos_x + 1), memoized);
        }

        pos_x += frac_bar_len;

        self.get_expression()
            .make_parenthesis(&mut pos_y, &mut pos_x, position, false, memoized);
        self.get_expression()
            .calc_pos(position, State::Same(pos_y, pos_x), memoized);
        pos_x += self.get_expression().get_length(memoized);
        self.get_expression()
            .make_parenthesis(&mut pos_y, &mut pos_x, position, true, memoized);
    }

    fn get_length(&self, memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        //  x
        // d
        // -----(expr)
        //     x
        // dvar
        // ___________
        // 1    1    1
        //  x.len() if x > 1
        3 + self.derivation_degree.get_length(memoized)
            + self.get_expression().get_length(memoized)
            + if self.get_derivation_degree().sub_expr == 1 {
                0
            } else {
                self.derivation_degree.get_length(memoized)
            }
    }

    fn get_height(&self, memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        //  x           | if x > 1
        // d            | 1 if expr == 1
        // -----(expr)  |
        //     x        | if x > 1
        // dvar         |

        let base_height = match self.get_expression().get_height(memoized) {
            1 => 2 + self.get_derivation_variable().get_height(memoized),
            x if x > 1 => x + self.get_derivation_variable().get_height(memoized),
            _ => panic!("Error in get_height derivation"),
        };

        base_height
            + if self.derivation_degree.sub_expr == 1 {
                0
            } else {
                self.derivation_degree.get_height(memoized) * 2
            }
    }

    fn get_above_height(
        &self,
        memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>,
    ) -> i8 {
        //  x           | 1 if x > 1
        // d            | 1 if expr == 1
        // -----(expr)  |
        //     x
        // dvar
        match self.get_expression().get_height(memoized) {
            1 | 2 => {
                2 + if self.derivation_degree.sub_expr == 1 {
                    0
                } else {
                    1
                }
            }
            x if x > 2 => x,
            _ => panic!("Error in get_above_height derivation"),
        }
    }
}

// Other function
impl Derivation {
    fn multiplication_derivation(self, mult: Multiplication) -> Expression {
        let mut first: Option<Expression> = None;
        let mut constant: Vec<Expression> = vec![];

        mult.sub_expr.iter().for_each(|expr| {
            if expr.contain_var(&self.get_derivation_variable()) {
                if let Some(expr_1) = &first {
                    constant.push(expr_1.clone());

                    first = Some(Expression::addition(
                        Expression::multiplication(
                            Expression::derivation(
                                Expression::multiplication_from_vec(constant.clone()),
                                self.derivation_variable.clone(),
                                Some(Expression::number(1)),
                            ),
                            expr.clone(),
                        ),
                        Expression::multiplication(
                            Expression::multiplication_from_vec(constant.clone()),
                            Expression::derivation(
                                expr.clone(),
                                self.derivation_variable.clone(),
                                Some(Expression::number(1)),
                            ),
                        ),
                    ));
                } else {
                    first = Some(Expression::derivation(
                        expr.clone(),
                        self.derivation_variable.clone(),
                        Some(Expression::number(1)),
                    ));
                }
            } else {
                constant.push(expr.clone());
            }
        });

        if let Some(expr) = first {
            if !constant.is_empty() {
                Expression::multiplication(expr, Expression::multiplication_from_vec(constant))
                    .simplify()
            } else {
                expr.simplify()
            }
        } else {
            Expression::number(0)
        }
    }

    fn exponentiation_derivation(self, expo: Exponentiation) -> Expression {
        match (
            expo.get_base().contain_var(&self.derivation_variable),
            expo.get_exponent().contain_var(&self.derivation_variable),
        ) {
            (true, true) => {
                todo!("Not supported yet")
            }
            (true, false) => {
                let derivate = Expression::multiplication(
                    expo.get_exponent(),
                    Expression::exponentiation(
                        expo.get_base(),
                        Expression::addition(expo.get_exponent(), Expression::number(-1)),
                    ),
                );
                if is_chain_rule_applicable(expo.get_base()) {
                    Expression::multiplication(
                        Expression::derivation(
                            expo.get_base(),
                            self.derivation_variable.clone(),
                            Some(Expression::number(1)),
                        ),
                        derivate.clone(),
                    )
                    .simplify()
                } else {
                    derivate.simplify()
                }
            }
            (false, true) => {
                if let Expression::Constant(ConstantKind::E) = expo.get_base() {
                    if is_chain_rule_applicable(expo.get_exponent()) {
                        Expression::multiplication(
                            Expression::derivation(
                                expo.get_exponent(),
                                self.derivation_variable.clone(),
                                Some(Expression::number(1)),
                            ),
                            self.sub_expr,
                        )
                        .simplify()
                    } else {
                        self.sub_expr
                    }
                } else {
                    let derivate = Expression::multiplication(
                        self.get_expression(),
                        Expression::function(FunctionType::Predefined(
                            PredefinedFunction::Ln,
                            vec![expo.get_base()],
                        )),
                    );
                    if is_chain_rule_applicable(expo.get_exponent()) {
                        Expression::multiplication(
                            derivate,
                            Expression::derivation(
                                expo.get_exponent(),
                                self.derivation_variable,
                                None,
                            ),
                        )
                        .simplify()
                    } else {
                        derivate.simplify()
                    }
                }
            }
            (false, false) => Expression::number(0),
        }
    }

    fn fraction_derivation(self, frac: Fraction) -> Expression {
        match (
            frac.get_numerator().contain_var(&self.derivation_variable),
            frac.get_denominator()
                .contain_var(&self.derivation_variable),
        ) {
            // d/dx(f/g) = (d/dx(f)*g - d/dx(g)*f) / g^2
            (true, true) => Expression::fraction(
                Expression::substraction(
                    Expression::multiplication(
                        Expression::derivation(
                            frac.get_numerator(),
                            self.derivation_variable.clone(),
                            Some(Expression::number(1)),
                        ),
                        frac.get_denominator().clone(),
                    ),
                    Expression::multiplication(
                        frac.get_numerator(),
                        Expression::derivation(
                            frac.get_denominator().clone(),
                            self.derivation_variable.clone(),
                            Some(Expression::number(1)),
                        ),
                    ),
                ),
                Expression::exponentiation(frac.get_denominator(), Expression::number(2)),
            )
            .simplify(),

            // d/dx(f/a) = d/dx(f)/a
            // where a is a constant
            // where f is a function dependant of x
            (true, false) => Expression::multiplication(
                Expression::derivation(
                    frac.get_numerator(),
                    self.derivation_variable,
                    Some(Expression::number(1)),
                ),
                Expression::fraction(Expression::number(1), frac.get_denominator()),
            )
            .simplify(),
            // d/dx(a/f) = -a * d/dx(f)/f^2
            // where a is a constant
            // where f is a function dependant of x
            (false, true) => Expression::negation(Expression::multiplication(
                frac.get_numerator(),
                Expression::fraction(
                    Expression::derivation(
                        frac.get_denominator(),
                        self.derivation_variable.clone(),
                        Some(Expression::number(1)),
                    ),
                    Expression::exponentiation(frac.get_denominator(), Expression::number(2)),
                ),
            ))
            .simplify(),
            (false, false) => Expression::number(0),
        }
    }
}

impl Display for Derivation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let degree_str = if self.get_derivation_degree().sub_expr == 1 {
            String::from("")
        } else {
            format!("^{}", self.get_derivation_degree())
        };
        write!(
            f,
            "d{}/d{}{}({})",
            degree_str,
            self.get_derivation_variable(),
            degree_str,
            self.get_expression()
        )
    }
}
