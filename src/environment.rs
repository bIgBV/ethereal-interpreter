use thiserror::Error;

use std::{cell::RefCell, collections::HashMap, marker::PhantomData, rc::Rc};

use crate::{common::Value, interpreter::Output};

struct EnvCell<T, U>
where
    T: AsRef<U>,
{
    cell: RefCell<HashMap<String, T>>,
    pub enclosing: Option<usize>,
    _marker: PhantomData<U>,
}

impl<T, U> EnvCell<T, U>
where
    T: AsRef<U>,
{
    pub fn new(enclosing: Option<usize>) -> Self {
        Self {
            cell: RefCell::new(HashMap::new()),
            enclosing,
            _marker: PhantomData,
        }
    }

    pub fn get(&self, key: &str) -> Option<&T> {
        self.cell.borrow().get(key)
    }

    pub fn contains_key(&self, key: &str) -> bool {
        self.cell.borrow().contains_key(key)
    }

    pub fn insert(&self, key: String, value: T) {
        self.cell.borrow_mut().insert(key, value);
    }
}

#[derive(Error, Debug)]
pub enum EnvError {
    #[error("Undefined variable {name} not found")]
    UndefinedVariable { name: String },
}

pub struct Environment {
    values: RefCell<Vec<EnvCell<Output<Value>, Value>>>,
}

impl Environment {
    pub fn new() -> (Self, usize) {
        let env = Environment {
            values: RefCell::new(Vec::new()),
        };

        let scope = env.instantiate_new_scope(None);

        (env, scope)
    }

    pub fn assign(&self, env: usize, name: String, value: Output<Value>) -> Result<(), EnvError> {
        if let Some(map) = self.values.borrow_mut().get(env) {
            if map.contains_key(&name) {
                map.insert(name, value);
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
            map.insert(name, value);
        }
    }

    pub fn get(&self, env: usize, name: &str) -> Result<&Output<Value>, EnvError> {
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
