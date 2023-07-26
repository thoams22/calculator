use std::{f64::consts::{E, PI}, fmt};

use crate::tokenizer::CalcError;

#[derive(PartialEq, PartialOrd, Debug, Clone, Copy)]
pub enum Constants {
    E,
    Pi,
    Phi,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Constant {
    constant: Constants,
}

// Constructor
impl Constant {
    pub fn e() -> Self{
        Self {
            constant: Constants::E,
        }
    }
    
    pub fn pi() -> Self{
        Self {
            constant: Constants::Pi,
        }
    }
    
    pub fn phi() -> Self{
        Self {
            constant: Constants::Phi,
        }
    }    

    pub fn from_constants(constants: Constants) -> Self {
        Self { constant: constants }
    }
}

impl Constant {
    pub fn evaluate(&self) -> Result<f64, CalcError> {
        match self.constant {
            Constants::E => Ok(E),
            Constants::Pi => Ok(PI),
            Constants::Phi => Ok((1.0 + 5.0_f64.sqrt())/2.0),
        }
    }

    pub fn get_constant(&self) -> Constants {
        self.constant
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self.constant {
            Constants::E => "e",
            Constants::Pi => "pi",
            Constants::Phi => "phi",
        })
    }
}

pub fn is_constant(token: &str) -> Option<Constants> {
    match token {
        "e" => Some(Constants::E),
        "pi" => Some(Constants::Pi),
        "phi" => Some(Constants::Phi),
        _ => None,
    }
}