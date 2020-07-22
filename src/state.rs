use thiserror::Error;

use std::collections::HashMap;

use crate::common::{Token, Value};

pub struct Environment<'a> {
    values: HashMap<&'a Token, Value>,
}

#[derive(Debug, Error)]
pub enum EnvironmentError {
    #[error("Undefined variable {name}.")]
    UndefinedVar { range: (usize, usize), name: String },
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn get(&self, token: &Token) -> Result<&Value, EnvironmentError> {
        self.values
            .get(token)
            .ok_or(EnvironmentError::UndefinedVar {
                range: token.range,
                name: String::new(),
            })
    }

    pub fn define(&mut self, token: &'a Token, value: Value) {
        self.values.insert(token, value);
    }
}
