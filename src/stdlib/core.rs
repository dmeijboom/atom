use crate::runtime::{AtomRef, ExternalFn, Input, Result, RuntimeError, Value};

pub const FUNCTIONS: [(&str, ExternalFn); 3] =
    [("println", println), ("some", some), ("raise", raise)];

pub const METHODS: [(&str, &str, ExternalFn); 23] = [
    ("Option", "isSome", option_is_some),
    ("Option", "isNone", option_is_none),
    ("String", "upper", string_upper),
    ("String", "lower", string_lower),
    ("String", "split", string_split),
    ("String", "startsWith", string_starts_with),
    ("String", "endsWith", string_ends_with),
    ("String", "contains", string_contains),
    ("String", "count", string_count),
    ("String", "find", string_find),
    ("String", "substr", string_substr),
    ("String", "replace", string_replace),
    ("String", "chars", string_chars),
    ("String", "bytes", string_bytes),
    ("String", "repeat", string_repeat),
    ("String", "trim", string_trim),
    ("String", "len", string_len),
    ("Float", "floor", float_floor),
    ("Array", "remove", array_remove),
    ("Array", "push", array_push),
    ("Array", "pop", array_pop),
    ("Array", "len", array_len),
    ("Array", "clear", array_clear),
];

pub fn println(input: Input<'_>) -> Result<Option<Value>> {
    println!(
        "{}",
        input
            .args
            .into_iter()
            .map(|val| format!("{}", val))
            .collect::<Vec<_>>()
            .join(", ")
    );

    Ok(None)
}

pub fn some(input: Input<'_>) -> Result<Option<Value>> {
    Ok(Some(Value::Option(Some(Box::new(input.single())))))
}

pub fn raise(input: Input<'_>) -> Result<Option<Value>> {
    Err(RuntimeError::new(format!("{}", input.args[0])))
}

pub fn option_is_some(mut input: Input<'_>) -> Result<Option<Value>> {
    let value = input.pop_receiver()?;

    Ok(Some(Value::Bool(
        !matches!(value, Value::Option(opt) if opt.is_none()),
    )))
}

pub fn option_is_none(mut input: Input<'_>) -> Result<Option<Value>> {
    let value = input.pop_receiver()?;

    Ok(Some(Value::Bool(
        matches!(value, Value::Option(opt) if opt.is_none()),
    )))
}

pub fn string_upper(mut input: Input<'_>) -> Result<Option<Value>> {
    let s: String = input.pop_receiver()?;
    Ok(Some(Value::String(AtomRef::new(s.as_str().to_uppercase()))))
}

pub fn string_lower(mut input: Input<'_>) -> Result<Option<Value>> {
    let s: String = input.pop_receiver()?;
    Ok(Some(Value::String(AtomRef::new(s.as_str().to_lowercase()))))
}

pub fn string_split(mut input: Input<'_>) -> Result<Option<Value>> {
    let s: String = input.pop_receiver()?;
    let pattern: String = input.pop_first()?;

    if input.args.is_empty() {
        return Ok(Some(Value::Array(AtomRef::new(
            s.split(&pattern)
                .into_iter()
                .map(|item| Value::String(AtomRef::new(item.to_string())))
                .collect(),
        ))));
    }

    let count: usize = input.pop_first()?;

    Ok(Some(Value::Array(AtomRef::new(
        s.splitn(count, &pattern)
            .into_iter()
            .map(|item| Value::String(AtomRef::new(item.to_string())))
            .collect(),
    ))))
}

pub fn string_starts_with(mut input: Input<'_>) -> Result<Option<Value>> {
    let s: String = input.pop_receiver()?;
    let pattern: String = input.pop_first()?;

    Ok(Some(Value::Bool(s.starts_with(&pattern))))
}

pub fn string_ends_with(mut input: Input<'_>) -> Result<Option<Value>> {
    let s: String = input.pop_receiver()?;
    let pattern: String = input.pop_first()?;

    Ok(Some(Value::Bool(s.ends_with(&pattern))))
}

pub fn string_contains(mut input: Input<'_>) -> Result<Option<Value>> {
    let s: String = input.pop_receiver()?;
    let pattern: String = input.pop_first()?;

    Ok(Some(Value::Bool(s.contains(&pattern))))
}

pub fn string_count(mut input: Input<'_>) -> Result<Option<Value>> {
    let s: String = input.pop_receiver()?;
    let pattern: String = input.pop_first()?;

    Ok(Some(Value::Int(s.matches(&pattern).count().into())))
}

pub fn string_find(mut input: Input<'_>) -> Result<Option<Value>> {
    let s: String = input.pop_receiver()?;
    let pattern: String = input.pop_first()?;

    Ok(Some(Value::Option(
        s.find(&pattern)
            .map(|index| Box::new(Value::Int(index.into()))),
    )))
}

pub fn string_substr(mut input: Input<'_>) -> Result<Option<Value>> {
    let s: String = input.pop_receiver()?;
    let index: usize = input.pop_first()?;

    Ok(Some(Value::String(AtomRef::new(s[index..].to_string()))))
}

pub fn string_replace(mut input: Input<'_>) -> Result<Option<Value>> {
    let s: String = input.pop_receiver()?;
    let pattern: String = input.pop_first()?;
    let replacement: String = input.pop_first()?;

    Ok(Some(Value::String(AtomRef::new(
        s.replace(&pattern, &replacement),
    ))))
}

pub fn string_chars(mut input: Input<'_>) -> Result<Option<Value>> {
    let s: String = input.pop_receiver()?;

    Ok(Some(Value::Array(AtomRef::new(
        s.chars().into_iter().map(Value::Char).collect(),
    ))))
}

pub fn string_bytes(mut input: Input<'_>) -> Result<Option<Value>> {
    let s: String = input.pop_receiver()?;

    Ok(Some(Value::Array(AtomRef::new(
        s.bytes().into_iter().map(Value::Byte).collect(),
    ))))
}

pub fn string_repeat(mut input: Input<'_>) -> Result<Option<Value>> {
    let s: String = input.pop_receiver()?;
    let count: usize = input.pop_first()?;

    Ok(Some(Value::String(AtomRef::new(s.repeat(count)))))
}

pub fn string_trim(mut input: Input<'_>) -> Result<Option<Value>> {
    let s: String = input.pop_receiver()?;

    Ok(Some(Value::String(AtomRef::new(s.trim().to_string()))))
}

pub fn string_len(mut input: Input<'_>) -> Result<Option<Value>> {
    let s: String = input.pop_receiver()?;

    Ok(Some(Value::Int(s.len().into())))
}

pub fn float_floor(mut input: Input<'_>) -> Result<Option<Value>> {
    let f: f64 = input.pop_receiver()?;

    Ok(Some(Value::Float(f.floor())))
}

pub fn array_remove(mut input: Input<'_>) -> Result<Option<Value>> {
    let mut a: AtomRef<Vec<Value>> = input.pop_receiver()?;
    let index: usize = input.pop_first()?;

    a.as_mut().remove(index);

    Ok(None)
}

pub fn array_push(mut input: Input<'_>) -> Result<Option<Value>> {
    let mut a: AtomRef<Vec<Value>> = input.pop_receiver()?;
    let item: Value = input.pop_first()?;

    a.as_mut().push(item);

    Ok(None)
}

pub fn array_pop(mut input: Input<'_>) -> Result<Option<Value>> {
    let mut a: AtomRef<Vec<Value>> = input.pop_receiver()?;

    Ok(a.as_mut().pop())
}

pub fn array_clear(mut input: Input<'_>) -> Result<Option<Value>> {
    let mut a: AtomRef<Vec<Value>> = input.pop_receiver()?;

    a.as_mut().clear();

    Ok(None)
}

pub fn array_len(mut input: Input<'_>) -> Result<Option<Value>> {
    let a: Vec<Value> = input.pop_receiver()?;
    Ok(Some(Value::Int(a.len().into())))
}
