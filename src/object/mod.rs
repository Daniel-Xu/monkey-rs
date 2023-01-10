#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    Null,
}
