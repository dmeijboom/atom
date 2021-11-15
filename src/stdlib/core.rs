use crate::runtime::{AtomRef, ExternalFn, Input, Int, Output, Result, RuntimeError, Value};

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

pub fn println(input: Input<'_>) -> Result<Output> {
    println!(
        "{}",
        input
            .args
            .into_iter()
            .map(|val| format!("{}", val))
            .collect::<Vec<_>>()
            .join(", ")
    );

    Output::void()
}

pub fn some(input: Input<'_>) -> Result<Output> {
    let value: Value = input.single()?;

    Ok(Output::new(Some(Box::new(value))))
}

pub fn raise(input: Input<'_>) -> Result<Output> {
    Err(RuntimeError::new(format!("{}", input.args[0])))
}

pub fn option_is_some(mut input: Input<'_>) -> Result<Output> {
    let value = input.get_receiver()?;

    if let Value::Option(inner) = value {
        return Ok(Output::new(inner.is_some()));
    }

    Err(
        RuntimeError::new(format!("expected 'Option', found: {}", value))
            .with_kind("TypeError".to_string()),
    )
}

pub fn option_is_none(mut input: Input<'_>) -> Result<Output> {
    let value = input.get_receiver()?;

    if let Value::Option(inner) = value {
        return Ok(Output::new(inner.is_none()));
    }

    Err(
        RuntimeError::new(format!("expected 'Option', found: {}", value))
            .with_kind("TypeError".to_string()),
    )
}

pub fn string_upper(mut input: Input<'_>) -> Result<Output> {
    let s: AtomRef<String> = input.take_receiver()?;
    Ok(Output::new(s.as_str().to_uppercase()))
}

pub fn string_lower(mut input: Input<'_>) -> Result<Output> {
    let s: AtomRef<String> = input.take_receiver()?;
    Ok(Output::new(s.as_str().to_lowercase()))
}

pub fn string_split(mut input: Input<'_>) -> Result<Output> {
    let s: AtomRef<String> = input.take_receiver()?;
    let pattern: AtomRef<String> = input.pop_first()?;

    if input.args.is_empty() {
        return Ok(Output::new(
            s.split(pattern.as_str())
                .into_iter()
                .map(|item| Value::String(AtomRef::new(item.to_string())))
                .collect::<Vec<_>>(),
        ));
    }

    let count: usize = input.pop_first()?;

    Ok(Output::new(
        s.splitn(count, pattern.as_str())
            .into_iter()
            .map(|item| Value::String(AtomRef::new(item.to_string())))
            .collect::<Vec<_>>(),
    ))
}

pub fn string_starts_with(mut input: Input<'_>) -> Result<Output> {
    let s: AtomRef<String> = input.take_receiver()?;
    let pattern: AtomRef<String> = input.pop_first()?;

    Ok(Output::new(s.starts_with(pattern.as_str())))
}

pub fn string_ends_with(mut input: Input<'_>) -> Result<Output> {
    let s: AtomRef<String> = input.take_receiver()?;
    let pattern: AtomRef<String> = input.pop_first()?;

    Ok(Output::new(s.ends_with(pattern.as_str())))
}

pub fn string_contains(mut input: Input<'_>) -> Result<Output> {
    let s: AtomRef<String> = input.take_receiver()?;
    let pattern: AtomRef<String> = input.pop_first()?;

    Ok(Output::new(s.contains(pattern.as_str())))
}

pub fn string_count(mut input: Input<'_>) -> Result<Output> {
    let s: AtomRef<String> = input.take_receiver()?;
    let pattern: AtomRef<String> = input.pop_first()?;

    Ok(Output::new(Int::from(s.matches(pattern.as_str()).count())))
}

pub fn string_find(mut input: Input<'_>) -> Result<Output> {
    let s: AtomRef<String> = input.take_receiver()?;
    let pattern: AtomRef<String> = input.pop_first()?;

    Ok(Output::new(
        s.find(pattern.as_str())
            .map(|index| Box::new(Value::Int(index.into()))),
    ))
}

pub fn string_substr(mut input: Input<'_>) -> Result<Output> {
    let s: AtomRef<String> = input.take_receiver()?;
    let index: usize = input.pop_first()?;

    Ok(Output::new(s[index..].to_string()))
}

pub fn string_replace(mut input: Input<'_>) -> Result<Output> {
    let s: AtomRef<String> = input.take_receiver()?;
    let pattern: AtomRef<String> = input.pop_first()?;
    let replacement: AtomRef<String> = input.pop_first()?;

    Ok(Output::new(
        s.as_str().replace(pattern.as_str(), replacement.as_str()),
    ))
}

pub fn string_chars(mut input: Input<'_>) -> Result<Output> {
    let s: AtomRef<String> = input.take_receiver()?;

    Ok(Output::new(
        s.chars().into_iter().map(Value::Char).collect::<Vec<_>>(),
    ))
}

pub fn string_bytes(mut input: Input<'_>) -> Result<Output> {
    let s: AtomRef<String> = input.take_receiver()?;

    Ok(Output::new(
        s.bytes().into_iter().map(Value::Byte).collect::<Vec<_>>(),
    ))
}

pub fn string_repeat(mut input: Input<'_>) -> Result<Output> {
    let s: AtomRef<String> = input.take_receiver()?;
    let count: usize = input.pop_first()?;

    Ok(Output::new(s.repeat(count)))
}

pub fn string_trim(mut input: Input<'_>) -> Result<Output> {
    let s: AtomRef<String> = input.take_receiver()?;

    Ok(Output::new(s.trim().to_string()))
}

pub fn string_len(mut input: Input<'_>) -> Result<Output> {
    let s: AtomRef<String> = input.take_receiver()?;

    Ok(Output::new(Int::from(s.len())))
}

pub fn float_floor(mut input: Input<'_>) -> Result<Output> {
    let f: f64 = input.take_receiver()?;

    Ok(Output::new(f.floor()))
}

pub fn array_remove(mut input: Input<'_>) -> Result<Output> {
    let mut a: AtomRef<Vec<Value>> = input.take_receiver()?;
    let index: usize = input.pop_first()?;

    if a.len() <= index {
        return Ok(Output::new(None));
    }

    let item = a.as_mut().remove(index);

    Ok(Output::new(Some(Box::new(item))))
}

pub fn array_push(mut input: Input<'_>) -> Result<Output> {
    let mut a: AtomRef<Vec<Value>> = input.take_receiver()?;
    let item: Value = input.pop_first()?;

    a.as_mut().push(item);

    Ok(Output::new(None))
}

pub fn array_pop(mut input: Input<'_>) -> Result<Output> {
    let mut a: AtomRef<Vec<Value>> = input.take_receiver()?;

    Ok(Output::new(a.as_mut().pop().map(Box::new)))
}

pub fn array_clear(mut input: Input<'_>) -> Result<Output> {
    let mut a: AtomRef<Vec<Value>> = input.take_receiver()?;

    a.as_mut().clear();

    Ok(Output::new(None))
}

pub fn array_len(mut input: Input<'_>) -> Result<Output> {
    let a: AtomRef<Vec<Value>> = input.take_receiver()?;

    Ok(Output::new(Int::from(a.len())))
}
