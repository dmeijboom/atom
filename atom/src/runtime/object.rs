use crate::{
    collections::IntMap,
    gc::{Handle, Trace},
};

use super::{class::Class, value::Value};

pub struct Object<'gc> {
    pub class: Handle<'gc, Class<'gc>>,
    attrs: IntMap<u32, Value<'gc>>,
}

impl<'gc> Trace for Object<'gc> {
    fn trace(&self, gc: &mut crate::gc::Gc) {
        gc.mark(&self.class);

        for value in self.attrs.values() {
            value.trace(gc);
        }
    }
}

impl<'gc> Object<'gc> {
    pub fn new(class: Handle<'gc, Class<'gc>>) -> Self {
        Self {
            class,
            attrs: IntMap::default(),
        }
    }

    #[inline]
    pub fn get_attr(&self, key: u32) -> Option<Value<'gc>> {
        self.attrs.get(&key).cloned()
    }

    #[inline]
    pub fn set_attr(&mut self, key: u32, value: Value<'gc>) {
        self.attrs.insert(key, value);
    }
}
