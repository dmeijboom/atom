use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::fs;
use std::hash::{Hash, Hasher};
use std::ops::Range;
use std::rc::Rc;

use smallvec::SmallVec;

use crate::compiler::IR;

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

    fn ne(&self, other: &Self) -> bool {
        self.name.as_str() != other.name.as_str()
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
    Range,
    String,
    Class,
    Interface,
    Function,
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
            ValueType::Range => "Range",
            ValueType::String => "String",
            ValueType::Class => "Class",
            ValueType::Interface => "Interface",
            ValueType::Method => "Method",
            ValueType::Function => "Fn",
            ValueType::Object => "Object",
            ValueType::Map => "Map",
            ValueType::Array => "Array",
            ValueType::Ref => "Ref",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct TypeId {
    pub module: String,
    pub name: String,
}

impl TypeId {
    pub fn new(module: impl ToString, name: impl ToString) -> Self {
        TypeId {
            module: module.to_string(),
            name: name.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Method {
    pub name: String,
    pub class: TypeId,
    pub object: Rc<RefCell<Value>>,
}

impl Hash for Method {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.class.hash(state);
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
    fields: SmallVec<[Value; 5]>,
}

impl Object {
    pub fn new(class: TypeId, fields: SmallVec<[Value; 5]>) -> Self {
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

#[derive(Debug, PartialEq)]
pub enum Value {
    Invalid,
    Int(i64),
    Float(f64),
    Char(char),
    Byte(u8),
    Bool(bool),
    Ref(Rc<RefCell<Value>>),
    Range(Range<i64>),
    String(String),
    Class(TypeId),
    Function(TypeId),
    Interface(TypeId),
    Method(Method),
    Object(Box<Object>),
    Array(Vec<Value>),
    Map(HashMap<Value, Value>),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Invalid => write!(f, "!"),
            Value::Int(val) => write!(f, "{}", val),
            Value::Float(val) => write!(f, "{}", val),
            Value::Char(val) => write!(f, "{}", val),
            Value::Byte(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
            Value::Range(val) => write!(f, "{:?}", val),
            Value::String(val) => write!(f, "{}", val),
            Value::Class(id) => {
                write!(f, "{}.{}", id.module, id.name)
            }
            Value::Interface(id) => {
                write!(f, "{}.{}", id.module, id.name)
            }
            Value::Function(id) => {
                write!(f, "{}({}.{})", self.get_type().name(), id.module, id.name)
            }
            Value::Method(method) => {
                write!(
                    f,
                    "{}({}.{}.{})",
                    self.get_type().name(),
                    method.class.module,
                    method.class.name,
                    method.name
                )
            }
            Value::Ref(value_ref) => {
                let value = &value_ref.borrow();

                write!(f, "*{}", value)
            }
            Value::Object(object) => {
                write!(f, "{}.{}", object.class.module, object.class.name,)
            }
            Value::Array(val) => write!(
                f,
                "[{}]",
                val.iter()
                    .map(|value| value.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Value::Map(val) => write!(
                f,
                "{{{}}}",
                val.iter()
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Invalid => panic!("Invalid can't be hashed"),
            Value::Int(val) => val.hash(state),
            Value::Float(val) => val.to_string().hash(state),
            Value::Char(val) => val.hash(state),
            Value::Byte(val) => val.hash(state),
            Value::Bool(val) => val.hash(state),
            Value::Ref(val) => val.borrow().hash(state),
            Value::Range(val) => val.hash(state),
            Value::String(val) => val.hash(state),
            Value::Class(class) => class.hash(state),
            Value::Interface(interface) => interface.hash(state),
            Value::Function(func) => func.hash(state),
            Value::Method(method) => method.hash(state),
            Value::Object(object) => object.hash(state),
            Value::Array(val) => val.hash(state),
            Value::Map(map) => {
                for (key, value) in map.iter() {
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
            Value::Invalid => panic!("Invalid can't be cloned"),
            Value::Int(val) => Value::Int(*val),
            Value::Float(val) => Value::Float(*val),
            Value::Char(val) => Value::Char(*val),
            Value::Byte(val) => Value::Byte(*val),
            Value::Bool(val) => Value::Bool(*val),
            Value::Ref(val) => Value::Ref(Rc::clone(val)),
            Value::Range(val) => Value::Range(val.clone()),
            Value::String(val) => Value::String(val.clone()),
            Value::Class(id) => Value::Class(id.clone()),
            Value::Interface(id) => Value::Interface(id.clone()),
            Value::Function(id) => Value::Function(id.clone()),
            Value::Method(id) => Value::Method(id.clone()),
            Value::Object(object) => Value::Object(object.clone()),
            Value::Array(array) => Value::Array(array.clone()),
            Value::Map(map) => Value::Map(map.clone()),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        *self = source.clone();
    }
}

impl Value {
    pub fn get_type(&self) -> ValueType {
        match self {
            Value::Invalid => panic!("Invalid has no type"),
            Value::Int(_) => ValueType::Int,
            Value::Float(_) => ValueType::Float,
            Value::Char(_) => ValueType::Char,
            Value::Byte(_) => ValueType::Byte,
            Value::Bool(_) => ValueType::Bool,
            Value::Ref(_) => ValueType::Ref,
            Value::Range(_) => ValueType::Range,
            Value::String(_) => ValueType::String,
            Value::Class(_) => ValueType::Class,
            Value::Interface(_) => ValueType::Interface,
            Value::Function(_) => ValueType::Function,
            Value::Method(_) => ValueType::Method,
            Value::Object(_) => ValueType::Object,
            Value::Array(_) => ValueType::Array,
            Value::Map(_) => ValueType::Map,
        }
    }
}

impl Eq for Value {}

pub fn with_auto_deref<T>(value: &Value, handler: impl FnOnce(&Value) -> T) -> T {
    if let Value::Ref(value_ref) = value {
        let value = value_ref.borrow();

        return handler(&value);
    }

    handler(value)
}

pub fn with_auto_deref_mut<T>(value: &mut Value, handler: impl FnOnce(&mut Value) -> T) -> T {
    if let Value::Ref(value_ref) = value {
        let mut value = value_ref.borrow_mut();

        return handler(&mut value);
    }

    handler(value)
}
