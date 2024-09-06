use bytes::Bytes;

use crate::opcode::Const;

use super::{class::Class, function::Fn};

#[derive(Debug, Default)]
pub struct Module {
    pub body: Bytes,
    pub consts: Vec<Const>,
    pub functions: Vec<Fn>,
    pub classes: Vec<Class>,
}
