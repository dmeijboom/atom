use bytes::Buf;

use crate::{
    bytecode::{Bytecode, Serializable},
    gc::{Gc, Handle, Trace},
    lexer::Span,
    runtime::{function::Fn, value::Value},
};

pub struct Frame<'gc> {
    pub call_site: usize,
    pub offset: usize,
    pub locals: Vec<Value<'gc>>,
    pub instance: usize,
    pub returned: bool,
    pub global: bool,
    pub handle: Handle<'gc, Fn>,
}

impl<'gc> Trace for Frame<'gc> {
    fn trace(&self, gc: &mut Gc) {
        gc.mark(&self.handle);
        self.locals.iter().for_each(|v| v.trace(gc));
    }
}

impl<'gc> Frame<'gc> {
    pub fn new(call_site: usize, handle: Handle<'gc, Fn>) -> Self {
        Self {
            call_site,
            offset: 0,
            locals: vec![],
            instance: handle.inline.instance,
            returned: false,
            handle,
            global: false,
        }
    }

    pub fn with_global(call_site: usize, handle: Handle<'gc, Fn>, instance: usize) -> Self {
        Self {
            call_site,
            offset: 0,
            locals: vec![],
            instance,
            returned: false,
            handle,
            global: true,
        }
    }

    /// Get the span given the assumption that we're already at the next bytecode
    pub fn span(&self) -> Span {
        self.span_at(self.offset - 3)
    }

    pub fn span_at(&self, offset: usize) -> Span {
        if offset >= self.handle.body.len() {
            return self.span_at(self.handle.body.len() - 3);
        }

        let mut tail = &self.handle.body[if offset == 0 {
            5..8
        } else {
            offset..offset + 3
        }];
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
