use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::common::Value;

pub struct Environment {
    values: RefCell<HashMap<String, Rc<Value>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: RefCell::new(HashMap::new()),
        }
    }

    pub fn assign(&self, name: String, value: Value) -> Option<()> {
        if self.values.borrow().contains_key(&name) {
            self.values.borrow_mut().insert(name, Rc::new(value));
            Some(())
        } else {
            None
        }
    }

    pub fn define(&self, name: String, value: Value) {
        self.values.borrow_mut().insert(name, Rc::new(value));
    }

    pub fn get(&self, name: &str) -> Option<Rc<Value>> {
        if self.values.borrow().contains_key(name) {
            self.values.borrow().get(name).map(|v| v.clone())
        } else {
            None
        }
    }
}
