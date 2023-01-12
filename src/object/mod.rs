#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    Null,
}

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);
pub const NULL: Object = Object::Null;

impl Object {
    pub fn get_bool(input: bool) -> Object {
        if input {
            return TRUE;
        }
        FALSE
    }
}
