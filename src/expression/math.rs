use super::addition::Addition;
use super::Expression;

use super::multiplication::Multiplication;

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
    let m = sum.len();
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

        mult.push(Expression::Number(coeff as f64));

        // Make a multiplication of all a_m to the power stored in exponent_permutation
        for (j, _) in exponent_permutation.iter().enumerate().take(m) {
            mult.push(Expression::exponentiation(
                sum.get(j).unwrap().clone(),
                Expression::Number(exponent_permutation[j] as f64),
            ));
        }
        expantion.push(Expression::Multiplication(Box::new(mult)));
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

/// Return `Some((root, exponent))` if n is a perfect power, else `None`.
/// 
/// If `is_perfect_power(n)` `n <= 1.0` return None 
/// 
/// ### Exemple 
/// ```
/// let result = is_perfect_power(8.0);
/// assert_eq!(Some((2.0, 3)), result);
/// ```
pub fn is_perfect_power(n: &f64) -> Option<(f64, u32)> {
    if n <= &1.0 {
        return None;
    }

    let max_exponent = (n).log2().ceil() as u32;
    let mut min_root: Option<(f64, u32)> = None;

    for exponent in 2..=max_exponent {
        let root = n.powf(1.0 / exponent as f64).round();
        if root.powf(exponent as f64) == *n && (min_root.is_none() || root < min_root.unwrap().0) {
                min_root = Some((root, exponent));
            }
        
    }

    min_root
}

pub fn find_divisors(n: u64) -> Vec<u64> {
    let mut divisors = Vec::new();
    let mut last_divisor: u64 = u64::MAX;
    
    for i in 1..=n {
        if n % i == 0 {
            if i >= last_divisor {
                break;
            }
            last_divisor = n/i;
            divisors.insert(divisors.len()/2,last_divisor);
            if i == last_divisor {
                break;
            }
            divisors.insert(divisors.len()/2,i);
        }
    }

    divisors
}