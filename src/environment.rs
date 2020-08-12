use thiserror::Error;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{common::Value, interpreter::Output};

struct EnvCell {
    cell: RefCell<HashMap<String, Rc<Value>>>,
    pub enclosing: Option<usize>,
}

impl EnvCell {
    pub fn new(enclosing: Option<usize>) -> Self {
        Self {
            cell: RefCell::new(HashMap::new()),
            enclosing,
        }
    }

    pub fn get(&self, key: &str) -> Option<Rc<Value>> {
        self.cell.borrow().get(key).map(|v| v.clone())
    }

    pub fn contains_key(&self, key: &str) -> bool {
        self.cell.borrow().contains_key(key)
    }

    pub fn insert(&self, key: String, value: Value) {
        self.cell.borrow_mut().insert(key, Rc::new(value));
    }

    pub fn insert_ref(&self, key: String, value: Rc<Value>) {
        self.cell.borrow_mut().insert(key, value);
    }
}

#[derive(Error, Debug)]
pub enum EnvError {
    #[error("Undefined variable {name} not found")]
    UndefinedVariable { name: String },
}

pub struct Environment {
    values: RefCell<Vec<EnvCell>>,
}

impl Environment {
    pub fn new() -> (Self, usize) {
        let env = Environment {
            values: RefCell::new(Vec::new()),
        };

        let scope = env.instantiate_new_scope(None);

        (env, scope)
    }

    pub fn len(&self) -> usize {
        self.values.borrow().len()
    }

    pub fn assign(&self, env: usize, name: String, value: Output<Value>) -> Result<(), EnvError> {
        if let Some(map) = self.values.borrow().get(env) {
            if map.contains_key(&name) {
                match value {
                    Output::Val(v) => map.insert(name, v),
                    Output::Ref(v) => map.insert_ref(name, v),
                };
                return Ok(());
            }

            // Search enclosing environments to see if the value exists there.
            if let Some(enc) = map.enclosing {
                return self.assign(enc, name, value);
            }
        }

        Err(EnvError::UndefinedVariable { name })
    }

    pub fn define(&self, env: usize, name: String, value: Output<Value>) {
        if let Some(map) = self.values.borrow().get(env) {
            match value {
                Output::Val(v) => map.insert(name, v),
                Output::Ref(v) => map.insert_ref(name, v),
            }
        }
    }

    pub fn get(&self, env: usize, name: &str) -> Result<Rc<Value>, EnvError> {
        if let Some(map) = self.values.borrow().get(env) {
            if map.contains_key(name) {
                return map.get(name).ok_or(EnvError::UndefinedVariable {
                    name: name.clone().to_owned(),
                });
            }

            if let Some(enc) = map.enclosing {
                return self.get(enc, name);
            }
        }

        Err(EnvError::UndefinedVariable {
            name: name.clone().to_owned(),
        })
    }

    pub fn instantiate_new_scope(&self, enclosing: Option<usize>) -> usize {
        self.values.borrow_mut().push(EnvCell::new(enclosing));
        self.values.borrow_mut().len() - 1
    }

    pub fn drop_scope(&self, scope: usize) {
        self.values.borrow_mut().remove(scope);
    }
}
