use std::mem;

use crate::{
    bytecode::{self, Bytecode, Op},
    compiler::Package,
    gc::{Gc, Handle, Trace},
    runtime::{error::RuntimeError, function::Resumable, Context, Fn, Value},
};

pub struct Frame<'gc> {
    pub offset: usize,
    pub returned: bool,
    pub resumable: bool,
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
            resumable: false,
            handle,
        }
    }

    pub fn as_resumable(&self) -> Option<Handle<'gc, Resumable<'gc>>> {
        if self.resumable {
            return self.locals.last().map(|v| v.as_resumable());
        }

        None
    }

    pub fn from_package(
        gc: &mut Gc<'gc>,
        context: Context,
        package: &mut Package,
    ) -> Result<Self, RuntimeError> {
        let mut func = gc.alloc(Fn::builder().body(mem::take(&mut package.body)).build())?;
        func.context = context;

        // This is not an ordinary function, no need to return a value
        let mut frame = Self::new(func);
        frame.returned = true;

        Ok(frame)
    }

    pub fn context(&self) -> &Context {
        &self.handle.context
    }
}

impl Iterator for Frame<'_> {
    type Item = Bytecode;

    fn next(&mut self) -> Option<Bytecode> {
        if self.offset < self.handle.body.len() {
            let op: Op = self.handle.body[self.offset].into();
            let code = u32::from_be_bytes(
                self.handle.body[self.offset + 1..self.offset + bytecode::SIZE]
                    .try_into()
                    .unwrap(),
            );

            let bc = Bytecode { op, code };
            self.offset += bytecode::SIZE;

            return Some(bc);
        }

        None
    }
}
