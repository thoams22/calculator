use super::Expression;
use super::addition::Addition;

use super::multiplication::Multiplication;

pub fn pascal_triangle(num: i64) {
    let mut coefficient_list : Vec<i64>= Vec::new();
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

fn multinomial_coefficient(n: i64, k: &[i64]) -> i64 {
    let numerator: i64 = factorial(n);
    let denominator: i64 = k.iter().map(|&x| factorial(x)).product();
    numerator / denominator
}

/// Return the developped form of multinomial_expansion of somme to the n_th degree
/// 
/// (a_1 + a_2 + ... + a_k)^n -> developped form
pub fn multinomial_expansion(n: i64, add: Addition) -> Expression{
    let mut expantion: Addition = Addition::from_vec(Vec::new());
    let m = add.len();
    let add_term_exponent: Vec<Vec<i64>> = find_permutations_with_sum(m, n);
    let mut already_calc_coef: Vec<(Vec<i64>, i64)> = Vec::new();

    for i in 0..add_term_exponent.len() {
        let mut mult: Multiplication = Multiplication::from_vec(Vec::new()); 
        let mut copy_add_term_exponent = add_term_exponent[i].clone();
        copy_add_term_exponent.sort();
        let mut coeff: i64 = 0; 
        for (term_exponents, coefficient) in &already_calc_coef {
            if *term_exponents == copy_add_term_exponent {
                coeff = *coefficient;
                break;
            }
        };
        if coeff == 0 {
            coeff = multinomial_coefficient(n, &copy_add_term_exponent);
            already_calc_coef.push((copy_add_term_exponent, coeff));
        }
        mult.push(Expression::Number(coeff as f64));
        for j in 0..m {
            mult.push(Expression::exponentiation(add.get(j).unwrap().clone(), Expression::Number(add_term_exponent[i][j] as f64)));
        }
        expantion.push(Expression::Multiplication(Box::new(mult)));
    }
    expantion.simplify()
}

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
            backtrack(
                m,
                n,
                current_sum + i,
                current_permutation,
                result,
            );
            current_permutation.pop();
        }
    }

    // Start the backtracking algorithm
    backtrack(m, n, 0, &mut current_permutation, &mut result);
    
    result
}
    
