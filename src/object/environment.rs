// How do we use it

use crate::object::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Environment {
    store: HashMap<String, Object>,
}

pub type SharedEnv = Rc<RefCell<Environment>>;

impl Environment {
    pub fn new() -> SharedEnv {
        Rc::new(RefCell::new(Self {
            store: HashMap::new(),
        }))
    }

    pub fn set(&mut self, k: String, v: Object) {
        self.store.insert(k, v);
    }
    pub fn get(&self, k: String) -> Option<Object> {
        match self.store.get(&k) {
            Some(v) => Some(v.clone()),
            None => None,
        }
    }
}
