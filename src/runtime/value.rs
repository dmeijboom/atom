use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::hash::{Hash, Hasher};
use std::ops::Range;
use std::rc::Rc;

use crate::compiler::IR;
use crate::runtime::atom_ref::AtomRef;

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub body: Vec<IR>,
}

impl Hash for Func {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }

    fn hash_slice<H: Hasher>(data: &[Self], state: &mut H)
    where
        Self: Sized,
    {
        for item in data {
            item.hash(state);
        }
    }
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        self.name.as_str() == other.name.as_str()
    }
}

impl Eq for Func {}

#[derive(Clone, Copy, PartialEq)]
pub enum ValueType {
    Int,
    Float,
    Char,
    Byte,
    Bool,
    Option,
    Range,
    String,
    Type,
    Method,
    Object,
    Array,
    Map,
    Ref,
}

impl ValueType {
    pub fn name(&self) -> &str {
        match self {
            ValueType::Int => "Int",
            ValueType::Float => "Float",
            ValueType::Char => "Char",
            ValueType::Byte => "Byte",
            ValueType::Bool => "Bool",
            ValueType::Option => "Option",
            ValueType::Range => "Range",
            ValueType::String => "String",
            ValueType::Type => "Type",
            ValueType::Method => "Method",
            ValueType::Object => "Object",
            ValueType::Map => "Map",
            ValueType::Array => "Array",
            ValueType::Ref => "Ref",
        }
    }
}

pub type TypeId = usize;

#[derive(Debug, Clone)]
pub struct Method {
    pub id: usize,
    pub value: Value,
    pub class_id: TypeId,
}

impl Eq for Method {}

impl PartialEq for Method {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.class_id == other.class_id
    }
}

impl Hash for Method {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.class_id.hash(state);
    }

    fn hash_slice<H: Hasher>(data: &[Self], state: &mut H)
    where
        Self: Sized,
    {
        for item in data {
            item.hash(state);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct FieldDesc {
    pub mutable: bool,
}

#[derive(Debug, Clone)]
pub enum Data {
    File(Rc<RefCell<fs::File>>),
}

impl Hash for Data {
    fn hash<H: Hasher>(&self, _: &mut H) {}
    fn hash_slice<H: Hasher>(_: &[Self], _: &mut H)
    where
        Self: Sized,
    {
    }
}

impl PartialEq<Self> for Data {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl Eq for Data {}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub struct Object {
    pub class: TypeId,
    pub data: Option<Data>,
    fields: Vec<Value>,
}

impl Object {
    pub fn new(class: TypeId, fields: Vec<Value>) -> Self {
        Self {
            class,
            fields,
            data: None,
        }
    }

    pub fn with_data(mut self, data: Data) -> Self {
        self.data = Some(data);

        self
    }

    pub fn get_field(&self, index: usize) -> Option<&Value> {
        self.fields.get(index)
    }

    pub fn get_field_mut(&mut self, index: usize) -> Option<&mut Value> {
        self.fields.get_mut(index)
    }

    pub fn set_field_value(&mut self, index: usize, value: Value) -> bool {
        if let Some(field) = self.get_field_mut(index) {
            *field = value;

            return true;
        }

        false
    }
}

#[derive(Debug)]
pub enum Value {
    Void,
    Int(i64),
    Float(f64),
    Char(char),
    Byte(u8),
    Bool(bool),
    Ref(AtomRef<Value>),
    Range(Range<i64>),
    Type(TypeId),
    Option(Option<Box<Value>>),
    Method(Box<Method>),
    String(AtomRef<String>),
    Object(AtomRef<Object>),
    Array(AtomRef<Vec<Value>>),
    Map(AtomRef<HashMap<Value, Value>>),
}

macro_rules! eq {
    (deref: $name:ident, $left:expr, $right:expr) => {
        if let Value::$name(left) = $left {
            if let Value::$name(right) = $right {
                return left.as_ref().eq(right.as_ref());
            }
        }
    };

    ($name:ident, $left:expr, $right:expr) => {
        if let Value::$name(left) = $left {
            if let Value::$name(right) = $right {
                return left.eq(right);
            }
        }
    };
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        eq!(Int, self, other);
        eq!(Float, self, other);
        eq!(Char, self, other);
        eq!(Byte, self, other);
        eq!(Bool, self, other);
        eq!(Option, self, other);
        eq!(deref: Ref, self, other);
        eq!(Range, self, other);
        eq!(deref: String, self, other);
        eq!(Type, self, other);
        eq!(Method, self, other);
        eq!(deref: Object, self, other);
        eq!(deref: Array, self, other);
        eq!(deref: Map, self, other);

        false
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Void => panic!("Void can't be hashed"),
            Value::Int(val) => val.hash(state),
            Value::Float(val) => val.to_string().hash(state),
            Value::Char(val) => val.hash(state),
            Value::Byte(val) => val.hash(state),
            Value::Bool(val) => val.hash(state),
            Value::Option(val) => val.hash(state),
            Value::Ref(val) => val.as_ref().hash(state),
            Value::Range(val) => val.hash(state),
            Value::String(val) => val.as_ref().hash(state),
            Value::Type(type_id) => type_id.hash(state),
            Value::Method(method) => method.hash(state),
            Value::Object(object) => object.as_ref().hash(state),
            Value::Array(val) => val.as_ref().hash(state),
            Value::Map(map) => {
                for (key, value) in map.as_ref().iter() {
                    key.hash(state);
                    value.hash(state);
                }
            }
        }
    }

    fn hash_slice<H: Hasher>(data: &[Self], state: &mut H)
    where
        Self: Sized,
    {
        for item in data {
            item.hash(state);
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Value::Void => panic!("Void can't be cloned"),
            Value::Int(val) => Value::Int(*val),
            Value::Float(val) => Value::Float(*val),
            Value::Char(val) => Value::Char(*val),
            Value::Byte(val) => Value::Byte(*val),
            Value::Bool(val) => Value::Bool(*val),
            Value::Option(val) => Value::Option(val.clone()),
            Value::Ref(val) => Value::Ref(AtomRef::clone(val)),
            Value::Range(val) => Value::Range(val.clone()),
            Value::String(val) => Value::String(AtomRef::clone(val)),
            Value::Type(type_id) => Value::Type(*type_id),
            Value::Method(id) => Value::Method(id.clone()),
            Value::Object(val) => Value::Object(AtomRef::clone(val)),
            Value::Array(val) => Value::Array(AtomRef::clone(val)),
            Value::Map(val) => Value::Map(AtomRef::clone(val)),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        *self = source.clone();
    }
}

impl Value {
    pub fn get_type(&self) -> ValueType {
        match self {
            Value::Void => panic!("Void has no type"),
            Value::Int(_) => ValueType::Int,
            Value::Float(_) => ValueType::Float,
            Value::Char(_) => ValueType::Char,
            Value::Byte(_) => ValueType::Byte,
            Value::Bool(_) => ValueType::Bool,
            Value::Option(_) => ValueType::Option,
            Value::Ref(_) => ValueType::Ref,
            Value::Range(_) => ValueType::Range,
            Value::String(_) => ValueType::String,
            Value::Type(_) => ValueType::Type,
            Value::Method(_) => ValueType::Method,
            Value::Object(_) => ValueType::Object,
            Value::Array(_) => ValueType::Array,
            Value::Map(_) => ValueType::Map,
        }
    }
}

impl Eq for Value {}
