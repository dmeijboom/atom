use bytes::Buf;

use crate::{
    bytecode::{Bytecode, Serializable},
    gc::{Gc, Handle, Trace},
    lexer::Span,
    runtime::{function::Fn, value::Value},
};

pub struct Frame<'gc> {
    pub offset: usize,
    pub returned: bool,
    pub locals: Vec<Value<'gc>>,
    pub handle: Handle<'gc, Fn>,
}

impl<'gc> Trace for Frame<'gc> {
    fn trace(&self, gc: &mut Gc) {
        gc.mark(&self.handle);
        self.locals.iter().for_each(|v| v.trace(gc));
    }
}

impl<'gc> Frame<'gc> {
    pub fn new(handle: Handle<'gc, Fn>) -> Self {
        Self {
            offset: 0,
            locals: vec![],
            returned: false,
            handle,
        }
    }

    pub fn instance(&self) -> usize {
        self.handle.context.instance
    }

    /// Get the span given the assumption that we're already at the next bytecode
    pub fn span(&self) -> Span {
        let mut tail = &self.handle.body[self.offset - 3..self.offset];
        Span {
            offset: tail.get_uint(3) as u32,
        }
    }
}

impl Iterator for Frame<'_> {
    type Item = Bytecode;

    fn next(&mut self) -> Option<Bytecode> {
        if self.offset < self.handle.body.len() {
            let bc = Bytecode::deserialize(&mut &self.handle.body[self.offset..self.offset + 5]);
            self.offset += 8;
            return Some(bc);
        }

        None
    }
}
