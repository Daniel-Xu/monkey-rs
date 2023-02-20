// How do we use it

use crate::object::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq)]
pub struct Environment {
    store: HashMap<String, Object>,
    parent: Option<SharedEnv>,
}

pub type SharedEnv = Rc<RefCell<Environment>>;

impl Environment {
    pub fn new() -> SharedEnv {
        Rc::new(RefCell::new(Self {
            store: HashMap::new(),
            parent: None,
        }))
    }

    pub fn new_enclosed(env: SharedEnv) -> SharedEnv {
        Rc::new(RefCell::new(Self {
            store: HashMap::new(),
            parent: Some(env),
        }))
    }

    pub fn set(&mut self, k: String, v: Object) {
        self.store.insert(k, v);
    }
    pub fn get(&self, k: String) -> Option<Object> {
        match self.store.get(&k) {
            Some(v) => Some(v.clone()),
            None => self
                .parent
                .as_ref()
                .and_then(|parent_env: &SharedEnv| parent_env.borrow().get(k)),
        }
    }
}
