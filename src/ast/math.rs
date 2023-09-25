use super::{addition::Addition, multiplication::Multiplication, Expression};

pub fn pascal_triangle(num: i64) {
    let mut coefficient_list: Vec<i64> = Vec::new();
    for n in 1..(num + 2) {
        let mut coeff = 1;
        for k in 1..(n + 1) {
            if n == num + 1 {
                coefficient_list.push(coeff);
            }
            coeff = coeff * (n - k) / k;
        }
    }
    println!("{:?}", coefficient_list);
}

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

        mult.sub_expr.push(Expression::Number(coeff));

        // Make a multiplication of all a_m to the power stored in exponent_permutation
        for (j, _) in exponent_permutation.iter().enumerate().take(m) {
            mult.sub_expr.push(Expression::exponentiation(
                sum.sub_expr[j].clone(),
                Expression::Number(exponent_permutation[j]),
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

pub fn prime_factor(mut n: i64) -> Vec<(i64, i64)> {
    if n == 1 {
        return vec![(1, 1)];
    }
    let mut factor: Vec<(i64, i64)> = Vec::new();
    while n % 2 == 0 {
        if factor.is_empty() {
            factor.push((2, 1));
        } else {
            factor[0].1 += 1;
        }
        n /= 2;
    }

    for i in 3..=((n as f64).sqrt().round() as i64) {
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
//             Expression::Variable('x'), Expression::Number(3)), Expression::Number(-27));
//     let denominator = Expression::addition(
//         Expression::addition(
//             Expression::exponentiation(
//                 Expression::Variable('x'), Expression::Number(2)),
//             Expression::multiplication(
//                 Expression::Variable('x'), Expression::Number(3))),
//         Expression::Number(9));

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
                        coefficient *= num;
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
                            Expression::Number(1),
                        ));
                    }
                }
            }
            (
                prime_factor(coefficient)
                    .iter()
                    .map(|(prime, expo)| PrimeFactor::new(*prime, Expression::Number(*expo)))
                    .collect::<Vec<PrimeFactor>>(),
                expressions,
            )
        }
        Expression::Number(num) => (
            prime_factor(num)
                .iter()
                .map(|(prime, expo)| PrimeFactor::new(*prime, Expression::Number(*expo)))
                .collect::<Vec<PrimeFactor>>(),
            expressions,
        ),
        Expression::Negation(neg) => extract_coefficient_expression_exponent(neg.sub_expr),
        Expression::Exponentiation(expo) => {
            let mut coefs: Vec<PrimeFactor> = vec![PrimeFactor::new(1, Expression::Number(1))];
            if let Expression::Number(num) = expo.get_base() {
                coefficient *= num;
                coefs = prime_factor(coefficient)
                    .iter()
                    .map(|(prime, exp)| {
                        PrimeFactor::new(
                            *prime,
                            Expression::multiplication(
                                Expression::Number(*exp),
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
                .map(|(prime, expo)| PrimeFactor::new(*prime, Expression::Number(*expo)))
                .collect::<Vec<PrimeFactor>>(),
            vec![ExpressionExponent::new(expr, Expression::Number(1))],
        ),
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct PrimeFactor {
    pub prime: i64,
    pub exponent: Expression,
}

impl PrimeFactor {
    pub fn new(prime: i64, exponent: Expression) -> Self {
        Self { prime, exponent }
    }

    pub fn get_all(&self) -> (i64, Expression) {
        (self.prime, self.exponent.clone())
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

    pub fn get_all(&self) -> (Expression, Expression) {
        (self.expression.clone(), self.expression.clone())
    }
}
