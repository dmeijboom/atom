use crate::runtime::{convert, Result, RuntimeError, Value};
use crate::vm::{Module, VM};

pub fn println(_: &VM, values: Vec<Value>) -> Result<Option<Value>> {
    println!(
        "{}",
        values
            .into_iter()
            .map(|value| format!("{}", value))
            .collect::<Vec<_>>()
            .join(", "),
    );

    Ok(None)
}

/**
 * External methods for the 'std.core.String' type
 */

fn with_string(value: &Value, handler: impl Fn(&String) -> String) -> Result<String> {
    if let Value::Object(object) = value {
        let object = object.borrow();

        if let Value::String(field_value) = &object.fields[0] {
            return Ok(handler(field_value));
        }
    }

    Err(RuntimeError::new(format!(
        "invalid type '{}', expected String",
        value.get_type().name()
    )))
}

pub fn string_to_upper(vm: &VM, _: Vec<Value>) -> Result<Option<Value>> {
    let object = vm.get_local("this").unwrap();
    let value = with_string(&object, |s| s.to_uppercase())?;

    Ok(Some(Value::String(value)))
}

pub fn string_repeat(vm: &VM, values: Vec<Value>) -> Result<Option<Value>> {
    let object = vm.get_local("this").unwrap();
    let count = convert::to_int(values[0].clone())? as usize;
    let value = with_string(&object, |s| s.repeat(count))?;

    Ok(Some(Value::String(value)))
}

pub fn register(module: &mut Module) -> Result<()> {
    module.register_external_fn("println", println);
    module.register_external_method("String", "upper", string_to_upper)?;
    module.register_external_method("String", "repeat", string_repeat)?;

    Ok(())
}
