use super::result::{CompileError, Result};
use super::types::{self, Type};

macro_rules! no_type {
    ($expr:expr) => {
        if $expr == Type::Unknown {
            return Ok(Type::Unknown);
        }
    };
}

pub struct TypeChecker {}

impl TypeChecker {
    pub fn new() -> Self {
        Self {}
    }

    pub fn condition(&self, cond: Type) -> Result<Type> {
        no_type!(cond);

        if cond != types::BOOL {
            return Err(CompileError::new(format!(
                "invalid type in condition: expected 'Bool', found: {}",
                cond
            )));
        }

        Ok(types::BOOL.clone())
    }

    pub fn unwrap_option(&self, option: Type) -> Result<Type> {
        no_type!(option);

        if let Type::Option(inner_type) = option {
            return Ok(*inner_type);
        }

        Err(CompileError::new(format!("unable to unwrap: {}", option)))
    }

    pub fn index(&self, index_type: Type) -> Result<Type> {
        no_type!(index_type);

        if let Type::Array(inner_type) = index_type {
            return Ok(*inner_type);
        }

        Err(CompileError::new(format!(
            "unable to index type: {}",
            index_type
        )))
    }

    pub fn deref(&self, ref_type: Type) -> Result<Type> {
        no_type!(ref_type);

        if let Type::Ref(inner_type) = ref_type {
            return Ok(*inner_type);
        }

        Err(CompileError::new(format!("unable to deref: {}", ref_type)))
    }
}
