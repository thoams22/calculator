use crate::ast::{
    addition::Addition, equality::Equality, function, multiplication::Multiplication, Expr,
    Expression,
};

// 25 first primes
const PRIMES_25: [i64; 25] = [
    2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97,
];

// =========
// Structure
// =========
#[derive(PartialEq, Debug, Clone)]
pub struct PrimeFactor {
    pub prime: i64,
    pub exponent: Expression,
}

impl PrimeFactor {
    pub fn new(prime: i64, exponent: Expression) -> Self {
        Self { prime, exponent }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ExpressionExponent {
    pub expression: Expression,
    pub exponent: Expression,
}

impl ExpressionExponent {
    pub fn new(expression: Expression, exponent: Expression) -> Self {
        Self {
            expression,
            exponent,
        }
    }
}

// =========
// Functions
// =========

pub fn factorial(num: i64) -> i64 {
    (1..=num).product()
}

/// Return the multinomial coefficient of the k's exponent permutations associated
fn multinomial_coefficient(n: i64, k: &[i64]) -> i64 {
    let numerator: i64 = factorial(n);
    let denominator: i64 = k.iter().map(|&x| factorial(x)).product();
    numerator / denominator
}

/// Return the developped form of multinomial_expansion of somme to the n_th degree
///
/// (a_0 + a_1 + ... + a_m)^n -> developped form
pub fn multinomial_expansion(n: i64, sum: Addition) -> Expression {
    let mut expantion: Addition = Addition::from_vec(Vec::new());
    let m = sum.sub_expr.len();
    let exponent_permutations: Vec<Vec<i64>> = find_permutations_with_sum(m, n);
    let mut already_calc_coef: Vec<(Vec<i64>, i64)> = Vec::new();

    for exponent_permutation in exponent_permutations {
        let mut mult: Multiplication = Multiplication::from_vec(Vec::new());

        // Because the coefficient are the n_th layer of a Pascal's m-simplex
        // These are repetitive so we store them to not recalculate them
        let mut sorted_exponent_permutations = exponent_permutation.clone();

        // Sort the permutation because in permutation [3, 1, 0, 0] != [1, 3, 0, 0] but the associated coefficient is the same
        sorted_exponent_permutations.sort();
        let mut coeff: i64 = 0;

        // Check if we already calculate the coefficient
        for (term_exponents, coefficient) in &already_calc_coef {
            if *term_exponents == sorted_exponent_permutations {
                coeff = *coefficient;
                break;
            }
        }
        // If not we add it to the list
        if coeff == 0 {
            coeff = multinomial_coefficient(n, &sorted_exponent_permutations);
            already_calc_coef.push((sorted_exponent_permutations, coeff));
        }

        mult.sub_expr.push(Expression::number(coeff));

        // Make a multiplication of all a_m to the power stored in exponent_permutation
        for (j, _) in exponent_permutation.iter().enumerate().take(m) {
            mult.sub_expr.push(Expression::exponentiation(
                sum.sub_expr[j].clone(),
                Expression::number(exponent_permutation[j]),
            ));
        }
        expantion
            .sub_expr
            .push(Expression::Multiplication(Box::new(mult)));
    }
    expantion.simplify()
}

/// Find all permutations of m element that sum to n
fn find_permutations_with_sum(m: usize, n: i64) -> Vec<Vec<i64>> {
    let mut result = Vec::new();
    let mut current_permutation = Vec::new();

    // Helper function to backtrack and find permutations
    fn backtrack(
        m: usize,
        n: i64,
        current_sum: i64,
        current_permutation: &mut Vec<i64>,
        result: &mut Vec<Vec<i64>>,
    ) {
        if current_sum == n && current_permutation.len() == m {
            result.push(current_permutation.clone());
            return;
        }

        if current_sum > n || current_permutation.len() >= m {
            return;
        }

        for i in 0..=n {
            current_permutation.push(i);
            backtrack(m, n, current_sum + i, current_permutation, result);
            current_permutation.pop();
        }
    }

    // Start the backtracking algorithm
    backtrack(m, n, 0, &mut current_permutation, &mut result);

    result
}

/// Return `Some((root, exponent))` if n is a perfect power.
///
/// If `is_perfect_power(n)` `n <= 1` return None
///
/// ### Exemple
/// ```
/// let result = is_perfect_power(8);
/// assert_eq!(Some((2, 3)), result);
/// ```
pub fn is_perfect_power(n: &i64) -> Option<(i64, u32)> {
    if n <= &1 {
        return None;
    }

    let max_exponent = (n).ilog2();
    let mut min_root: Option<(i64, u32)> = None;

    for exponent in 2..=max_exponent {
        let root = (*n as f64).powf(1.0 / exponent as f64).round() as i64;
        if root.pow(exponent) == *n && (min_root.is_none() || root < min_root.unwrap().0) {
            min_root = Some((root, exponent));
        }
    }

    min_root
}

/// Return `(numerator, denominator)` if the number is representable in a i64 fraction.
///
/// ### Exemple
/// ```
/// let fraction = f64_to_fraction(2.5f64);
/// assert_eq!((5, 2), result);
/// ```
fn f64_to_fraction(number: f64) -> (i64, i64) {
    let mut n: u32 = 0;
    let mut numerator: f64 = number;
    while numerator.fract() > 1e-10 {
        numerator *= 10.0;
        n += 1;
    }
    let gcd = gcd(numerator.trunc() as i64, 10_i64.pow(n));
    ((numerator / gcd as f64) as i64, 10_i64.pow(n) / gcd)
}

/// Return the greatest common divisor of first and second.
pub fn gcd(mut first: i64, mut second: i64) -> i64 {
    while second != 0 {
        let r = first % second;
        first = second;
        second = r;
    }
    first
}

/// Return all divisor of n
pub fn find_divisors(n: u64) -> Vec<u64> {
    let mut divisors = Vec::new();
    let mut last_divisor: u64 = u64::MAX;

    for i in 1..=n {
        if n % i == 0 {
            if i >= last_divisor {
                break;
            }
            last_divisor = n / i;
            divisors.insert(divisors.len() / 2, last_divisor);
            if i == last_divisor {
                break;
            }
            divisors.insert(divisors.len() / 2, i);
        }
    }

    divisors
}

/// Returns `Vec<(i64, i64)>` that represents the number as a multiplication of primes
///
/// # Example
/// ```
/// // 8 = 2^3
/// assert_eq!(Vec<(2, 3)>, prime_factor(8));
/// // 468 = 2^2 * 3^2 * 13
/// assert_eq!(Vec<(2, 2), (3, 2), (13, 1)>, prime_factor(468));
///
/// ```
pub fn prime_factor(mut n: i64) -> Vec<(i64, i64)> {
    if n == 1 {
        return vec![(1, 1)];
    }
    let mut factor: Vec<(i64, i64)> = Vec::new();

    for i in PRIMES_25 {
        while n % i == 0 {
            if factor.last().unwrap_or(&(0, 0)).0 != i {
                factor.push((i, 1));
            } else {
                let len = factor.len();
                factor[len - 1].1 += 1;
            }
            n /= i;
        }
    }

    for i in (101..=((n as f64).sqrt().round() as i64)).step_by(2) {
        while n % i == 0 {
            if factor.last().unwrap_or(&(0, 0)).0 != i {
                factor.push((i, 1));
            } else {
                let len = factor.len();
                factor[len - 1].1 += 1;
            }
            n /= i;
        }
    }

    if n > 2 {
        factor.push((n, 1));
    }

    factor
}

// fn polynomials_division() {
//     // x^3 - 27
//     // /
//     // x^2 + 3x + 9

//     // (x^3 - 27) / (x^2 + 3x + 9) = x - 3

//     let numerator = Expression::addition(
//         Expression::exponentiation(
//             Expression::variable("x".to_string()), Expression::number(3)), Expression::number(-27));
//     let denominator = Expression::addition(
//         Expression::addition(
//             Expression::exponentiation(
//                 Expression::variable("x".to_string()), Expression::number(2)),
//             Expression::multiplication(
//                 Expression::variable("x".to_string()), Expression::number(3))),
//         Expression::number(9));

//     let result = Addition::from_vec(Vec::new());

//     for i in 0..50 {

//     }
// }

/// Expression must be simplified
pub fn extract_coefficient_expression_exponent(
    expr: Expression,
) -> (Vec<PrimeFactor>, Vec<ExpressionExponent>) {
    let mut coefficient = 1;
    let mut expressions: Vec<ExpressionExponent> = Vec::new();
    match expr {
        // 12a^2
        Expression::Multiplication(mult) => {
            for i in 0..mult.sub_expr.len() {
                match &mult.sub_expr[i] {
                    // 2
                    Expression::Number(num) => {
                        coefficient *= num.sub_expr;
                    }
                    // a^2
                    Expression::Exponentiation(expo) => {
                        expressions.push(ExpressionExponent::new(
                            expo.get_base(),
                            expo.get_exponent(),
                        ));
                    }
                    _ => {
                        expressions.push(ExpressionExponent::new(
                            mult.sub_expr[i].clone(),
                            Expression::number(1),
                        ));
                    }
                }
            }
            (
                prime_factor(coefficient)
                    .iter()
                    .map(|(prime, expo)| PrimeFactor::new(*prime, Expression::number(*expo)))
                    .collect::<Vec<PrimeFactor>>(),
                expressions,
            )
        }
        Expression::Number(num) => (
            prime_factor(num.sub_expr)
                .iter()
                .map(|(prime, expo)| PrimeFactor::new(*prime, Expression::number(*expo)))
                .collect::<Vec<PrimeFactor>>(),
            expressions,
        ),
        // TODO keep sign of expression
        Expression::Negation(neg) => extract_coefficient_expression_exponent(neg.sub_expr),
        Expression::Exponentiation(expo) => {
            let mut coefs: Vec<PrimeFactor> = vec![PrimeFactor::new(1, Expression::number(1))];
            if let Expression::Number(num) = expo.get_base() {
                coefficient *= num.sub_expr;
                coefs = prime_factor(coefficient)
                    .iter()
                    .map(|(prime, exp)| {
                        PrimeFactor::new(
                            *prime,
                            Expression::multiplication(
                                Expression::number(*exp),
                                expo.get_exponent(),
                            )
                            .simplify(),
                        )
                    })
                    .collect::<Vec<PrimeFactor>>();
            } else {
                expressions.push(ExpressionExponent::new(
                    expo.get_base(),
                    expo.get_exponent(),
                ));
            }
            (coefs, expressions)
        }
        _ => (
            prime_factor(coefficient)
                .iter()
                .map(|(prime, expo)| PrimeFactor::new(*prime, Expression::number(*expo)))
                .collect::<Vec<PrimeFactor>>(),
            vec![ExpressionExponent::new(expr, Expression::number(1))],
        ),
    }
}

/// Substitue the `left` side of the equality for the `right` side in the expression
///
/// ### Exemple
/// ```
/// // x^4 + x^2 + 8 = 0
/// let expression = Expression::equality(
///                    Expression::addition_from_vec(vec![
///                        Expression::exponentiation(
///                            Expression::variable("x".to_string()),
///                            Expression::number(4)
///                        ),
///                        Expression::exponentiation(
///                            Expression::variable("x".to_string()),
///                            Expression::number(2)
///                        ),
///                        Expression::number(8)
///                    ]),
///                    Expression::number(0)
///                );
/// // x^2 = y
/// let equality = Equality::new(
///                  Expression::exponentiation(Expression::variable("x".to_string()), Expression::number(2)),
///                  Expression::variable("y".to_string())
///                );
/// // y^2 + y + 8 = 0
/// let substitued = substitute(expression, equality);
/// ```
pub fn substitute(expression: Expression, equality: &Equality) -> Expression {
    let substitued = equality.get_left_side();
    let substituor = equality.get_right_side();

    if expression.equal(&substitued) {
        return substituor;
    }

    match expression.clone() {
        Expression::Number(_) => expression,
        Expression::ImaginaryUnit => expression,
        Expression::Variable(var) => {
            if let Expression::Variable(var_2) = substitued {
                if var == var_2 {
                    return substituor;
                }
            }
            expression
        }
        Expression::Constant(_) => expression,
        Expression::Addition(add) => {
            let mut new_add: Vec<Expression> = Vec::new();
            add.sub_expr.iter().for_each(|sub| {
                new_add.push(substitute(sub.clone(), equality));
            });
            Expression::addition_from_vec(new_add)
        }
        Expression::Multiplication(mult) => {
            let mut new_mult: Vec<Expression> = Vec::new();
            mult.sub_expr.iter().for_each(|sub| {
                new_mult.push(substitute(sub.clone(), equality));
            });
            Expression::multiplication_from_vec(new_mult)
        }
        Expression::Exponentiation(mut expo) => {
            if expo.get_base() == substitued {
                expo.simplified = false;
                // x^2, x = y^3 => y^9
                if let Expression::Exponentiation(expo_2) = substituor {
                    expo.set_base(expo_2.get_base());
                    expo.set_exponent(Expression::exponentiation(
                        expo_2.get_exponent(),
                        expo.get_exponent(),
                    ))
                } else {
                    expo.set_base(substituor)
                }
            } else if let Expression::Exponentiation(expo_2) = substitued {
                if expo.get_base() == expo_2.get_base() {
                    expo.simplified = false;
                    expo.set_base(substituor);
                    expo.set_exponent(Expression::fraction(
                        expo.get_exponent(),
                        expo_2.get_exponent(),
                    ));
                }
            }
            Expression::Exponentiation(expo).simplify()
        }
        Expression::Fraction(frac) => Expression::fraction(
            substitute(frac.get_numerator(), equality),
            substitute(frac.get_denominator(), equality),
        ),
        Expression::Equality(eq) => Expression::equality(
            substitute(eq.get_left_side(), equality),
            substitute(eq.get_right_side(), equality),
        ),
        Expression::Negation(neg) => Expression::negation(substitute(neg.sub_expr, equality)),
        Expression::Function(func) => match *func {
            function::FunctionType::Predefined(function, args) => {
                return Expression::function(function::FunctionType::Predefined(function, {
                    let mut new_args: Vec<Expression> = Vec::new();
                    args.iter()
                        .for_each(|arg| new_args.push(substitute(arg.clone(), equality)));
                    new_args
                }))
            }
            function::FunctionType::UserDefined(name, args) => {
                return Expression::function(function::FunctionType::UserDefined(name, {
                    let mut new_args: Vec<Expression> = Vec::new();
                    args.iter()
                        .for_each(|arg| new_args.push(substitute(arg.clone(), equality)));
                    new_args
                }));
            }
        },
        Expression::Complex(complex) => {
            let mut new_complex = complex.clone();
            new_complex.set_real(substitute(complex.get_real(), equality));
            new_complex.set_imaginary(substitute(complex.get_imaginary(), equality));
            Expression::Complex(new_complex)
        }
        Expression::Derivation(derivation) => {
            let mut new_derivation = derivation.clone();
            new_derivation.set_expression(substitute(derivation.get_expression(), equality));
            new_derivation.set_derivation_variable(
                if let Expression::Variable(var) = substitute(
                    Expression::Variable(derivation.get_derivation_variable()),
                    equality,
                ) {
                    var
                } else {
                    panic!("Trying to replace derivation variable by a type not <Variable>")
                },
            );
            Expression::Derivation(new_derivation)
        }
    }
}

/// Return the common part in the expression if its an addition `else` return the uniques subcomposant and their power
///
/// ### Exemple
/// ```
/// // 4x^2
/// let expression = Expression::multiplication(
///     Expression::number(4),
///     Expression::exponentiation(Expression::Variable('x'), Expression::number(2)));
///
/// // ((2, 2), (x, 2))
/// let factors = factorise(expression);
/// ```
fn factorise(expr: Expression) -> (Vec<PrimeFactor>, Vec<ExpressionExponent>) {
    match expr {
        Expression::Addition(add) => {
            let components: Vec<(Vec<PrimeFactor>, Vec<ExpressionExponent>)> = add
                .sub_expr
                .into_iter()
                .map(extract_coefficient_expression_exponent)
                .collect();

            let mut commons: (Vec<PrimeFactor>, Vec<ExpressionExponent>) = components[0].clone();

            components.iter().skip(1).for_each(|component| {
                commons.0 = commons
                    .0
                    .iter()
                    .filter_map(|prime_factor| {
                        component
                            .0
                            .iter()
                            .position(|prime_factor_2| {
                                if prime_factor.prime == prime_factor_2.prime {
                                    match (&prime_factor.exponent, &prime_factor_2.exponent) {
                                        (Expression::Number(_), Expression::Number(_)) => true,
                                        _ => prime_factor.exponent.equal(&prime_factor_2.exponent),
                                    }
                                } else {
                                    false
                                }
                            })
                            .map(|pos| {
                                let exponenent =
                                    if let (Expression::Number(num), Expression::Number(num_2)) =
                                        (&prime_factor.exponent, &component.0[pos].exponent)
                                    {
                                        Expression::number(num.sub_expr.min(num_2.sub_expr))
                                    } else {
                                        prime_factor.exponent.clone()
                                    };
                                PrimeFactor::new(prime_factor.prime, exponenent)
                            })
                    })
                    .collect();
                commons.1 = commons
                    .1
                    .iter()
                    .filter_map(|expr| {
                        component
                            .1
                            .iter()
                            .position(|expr_2| {
                                if expr.expression == expr_2.expression {
                                    match (&expr.exponent, &expr.exponent) {
                                        (Expression::Number(_), Expression::Number(_)) => true,
                                        _ => expr.exponent.equal(&expr.exponent),
                                    }
                                } else {
                                    false
                                }
                            })
                            .map(|pos| {
                                let exponenent =
                                    if let (Expression::Number(num), Expression::Number(num_2)) =
                                        (&expr.exponent, &component.1[pos].exponent.clone())
                                    {
                                        Expression::number(num.sub_expr.min(num_2.sub_expr))
                                    } else {
                                        expr.exponent.clone()
                                    };
                                ExpressionExponent::new(expr.expression.clone(), exponenent)
                            })
                    })
                    .collect();
            });
            commons
        }
        _ => {
            let (prime_factor, expressions) = extract_coefficient_expression_exponent(expr);
            (prime_factor, expressions)
        }
    }
}

/// Check if the `expression` is a composed expression.
///
/// It does not take into a count if `expression` contains the derivation variable  
pub fn is_chain_rule_applicable(expression: Expression) -> bool {
    match expression {
        Expression::Addition(_)
        | Expression::Multiplication(_)
        | Expression::Exponentiation(_)
        | Expression::Fraction(_)
        | Expression::Function(_) => true,
        Expression::Negation(neg) => is_chain_rule_applicable(neg.sub_expr),
        Expression::Complex(_) => todo!(),
        Expression::Derivation(_) => todo!(),
        Expression::Equality(_) => todo!(),
        _ => false,
    }
}

#[cfg(test)]
mod test_utils {

    use crate::ast::constant::ConstantKind;
    use crate::utils::function::FunctionType;
    use crate::utils::function::PredefinedFunction;
    use crate::utils::prime_factor;
    use crate::{
        ast::{equality::Equality, Expression},
        utils::substitute,
    };

    #[test]
    fn test_substitute() {
        // x^4 + x^2 + 8 = 0, x^2 = y => y^2 + y + 8 = 0
        assert!(substitute(
            Expression::equality(
                Expression::addition_from_vec(vec![
                    Expression::exponentiation(
                        Expression::variable("x".to_string()),
                        Expression::number(4)
                    ),
                    Expression::exponentiation(
                        Expression::variable("x".to_string()),
                        Expression::number(2)
                    ),
                    Expression::number(8)
                ]),
                Expression::number(0)
            ),
            &Equality::new(
                Expression::exponentiation(
                    Expression::variable("x".to_string()),
                    Expression::number(2)
                ),
                Expression::variable("y".to_string())
            )
        )
        .equal(&Expression::equality(
            Expression::addition_from_vec(vec![
                Expression::exponentiation(
                    Expression::variable("y".to_string()),
                    Expression::number(2)
                ),
                Expression::variable("y".to_string()),
                Expression::number(8)
            ]),
            Expression::number(0)
        )));

        // x^4, x^2 = y^3 => y^6
        assert!(substitute(
            Expression::exponentiation(
                Expression::variable("x".to_string()),
                Expression::number(4)
            ),
            &Equality::new(
                Expression::exponentiation(
                    Expression::variable("x".to_string()),
                    Expression::number(2)
                ),
                Expression::exponentiation(
                    Expression::variable("y".to_string()),
                    Expression::number(3)
                )
            )
        )
        .equal(&Expression::exponentiation(
            Expression::variable("y".to_string()),
            Expression::number(6)
        )));

        // x^2, x = y^3 => y^9
        assert!(substitute(
            Expression::exponentiation(
                Expression::variable("x".to_string()),
                Expression::number(2)
            ),
            &Equality::new(
                Expression::variable("x".to_string()),
                Expression::exponentiation(
                    Expression::variable("y".to_string()),
                    Expression::number(3)
                )
            )
        )
        .equal(&Expression::exponentiation(
            Expression::variable("y".to_string()),
            Expression::number(9)
        )));

        // x^y, x = z^y => z^y^y
        assert!(substitute(
            Expression::exponentiation(
                Expression::variable("x".to_string()),
                Expression::variable("y".to_string())
            ),
            &Equality::new(
                Expression::variable("x".to_string()),
                Expression::exponentiation(
                    Expression::variable("z".to_string()),
                    Expression::variable("y".to_string())
                )
            )
        )
        .equal(&Expression::exponentiation(
            Expression::variable("z".to_string()),
            Expression::exponentiation(
                Expression::variable("y".to_string()),
                Expression::variable("y".to_string())
            )
        )));

        // x^(2y), x^y = z => z^2
        assert!(substitute(
            Expression::exponentiation(
                Expression::variable("x".to_string()),
                Expression::multiplication(
                    Expression::number(2),
                    Expression::variable("y".to_string())
                )
            ),
            &Equality::new(
                Expression::exponentiation(
                    Expression::variable("x".to_string()),
                    Expression::variable("y".to_string())
                ),
                Expression::variable("z".to_string()),
            )
        )
        .equal(&Expression::exponentiation(
            Expression::variable("z".to_string()),
            Expression::number(2)
        )));

        // x^3 + x^(1/2), x^2 = y => y^(3/2) + y^(1/4)
        assert!(substitute(
            Expression::addition(
                Expression::exponentiation(
                    Expression::variable("x".to_string()),
                    Expression::number(3)
                ),
                Expression::exponentiation(
                    Expression::variable("x".to_string()),
                    Expression::fraction(Expression::number(1), Expression::number(2))
                )
            ),
            &Equality::new(
                Expression::exponentiation(
                    Expression::variable("x".to_string()),
                    Expression::number(2)
                ),
                Expression::variable("y".to_string())
            )
        )
        .equal(&Expression::addition(
            Expression::exponentiation(
                Expression::variable("y".to_string()),
                Expression::fraction(Expression::number(3), Expression::number(2))
            ),
            Expression::exponentiation(
                Expression::variable("y".to_string()),
                Expression::fraction(Expression::number(1), Expression::number(4))
            )
        )));

        // e^(2x) + e^x, e^x = y => y^2 + y
        assert!(substitute(
            Expression::addition(
                Expression::exponentiation(
                    Expression::Constant(ConstantKind::E),
                    Expression::multiplication(
                        Expression::number(2),
                        Expression::variable("x".to_string())
                    )
                ),
                Expression::exponentiation(
                    Expression::Constant(ConstantKind::E),
                    Expression::variable("x".to_string())
                )
            ),
            &Equality::new(
                Expression::exponentiation(
                    Expression::Constant(ConstantKind::E),
                    Expression::variable("x".to_string())
                ),
                Expression::variable("y".to_string())
            )
        )
        .equal(&Expression::addition(
            Expression::exponentiation(
                Expression::variable("y".to_string()),
                Expression::number(2)
            ),
            Expression::variable("y".to_string())
        )));

        // ln(x) + ln(x)/2, ln(x) = y => y + y/2
        assert!(substitute(
            Expression::addition(
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Ln,
                    vec![Expression::variable("x".to_string())]
                )),
                Expression::fraction(
                    Expression::function(FunctionType::Predefined(
                        PredefinedFunction::Ln,
                        vec![Expression::variable("x".to_string())]
                    )),
                    Expression::number(2)
                )
            ),
            &Equality::new(
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Ln,
                    vec![Expression::variable("x".to_string())]
                )),
                Expression::variable("y".to_string())
            )
        )
        .equal(&Expression::addition(
            Expression::variable("y".to_string()),
            Expression::fraction(Expression::variable("y".to_string()), Expression::number(2))
        )));
    }

    #[test]
    fn test_prime_factor() {
        // 10
        assert_eq!(vec![(2, 1), (5, 1)], prime_factor(10));

        // 11
        assert_eq!(vec![(11, 1)], prime_factor(11));

        // 169
        assert_eq!(vec![(13, 2)], prime_factor(169));

        // -2
        assert_eq!(vec![(2, 1)], prime_factor(-2));

        // 4089
        assert_eq!(vec![(3, 1), (29, 1), (47, 1)], prime_factor(4089));
    }
}
