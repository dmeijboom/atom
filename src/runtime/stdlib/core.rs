use crate::runtime::error::ErrorKind;
use crate::runtime::{
    AtomArray, AtomRef, AtomRefMut, AtomString, ExternalFn, Fn, Input, Int, Output, Result,
    RuntimeError, Value,
};

pub const FUNCTIONS: [(&str, ExternalFn); 2] = [("println", println), ("rt_raise", runtime_raise)];

pub const METHODS: [(&str, &str, ExternalFn); 24] = [
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
    ("String", "concat", string_concat),
    ("String", "trim", string_trim),
    ("String", "len", string_len),
    ("Float", "floor", float_floor),
    ("Array", "remove", array_remove),
    ("Array", "push", array_push),
    ("Array", "pop", array_pop),
    ("Array", "len", array_len),
    ("Array", "clear", array_clear),
    ("Fn", "name", fn_name),
    ("Int", "size", int_size),
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

pub fn runtime_raise(input: Input<'_>) -> Result<Output> {
    Err(RuntimeError::new(
        ErrorKind::UserError,
        format!("{}", input.args[0]),
    ))
}

pub fn string_upper(mut input: Input<'_>) -> Result<Output> {
    let s: AtomString = input.take_receiver()?;

    Ok(Output::new(AtomString::new(s.to_uppercase())))
}

pub fn string_lower(mut input: Input<'_>) -> Result<Output> {
    let s: AtomString = input.take_receiver()?;

    Ok(Output::new(AtomString::new(s.to_lowercase())))
}

pub fn string_split(mut input: Input<'_>) -> Result<Output> {
    let s: AtomString = input.take_receiver()?;
    let pattern: AtomString = input.pop_first()?;

    if input.args.is_empty() {
        return Ok(Output::new(AtomRefMut::new(AtomRef::new(
            s.split(pattern.as_str())
                .into_iter()
                .map(|item| AtomString::from(item))
                .map(Value::from)
                .collect::<Vec<_>>(),
        ))));
    }

    let count: usize = input.pop_first()?;

    Ok(Output::new(AtomRefMut::new(AtomRef::new(
        s.splitn(count, pattern.as_str())
            .into_iter()
            .map(|item| Value::from(AtomString::from(item)))
            .collect::<Vec<_>>(),
    ))))
}

pub fn string_starts_with(mut input: Input<'_>) -> Result<Output> {
    let s: AtomString = input.take_receiver()?;
    let pattern: AtomString = input.pop_first()?;

    Ok(Output::new(s.starts_with(pattern.as_str())))
}

pub fn string_ends_with(mut input: Input<'_>) -> Result<Output> {
    let s: AtomString = input.take_receiver()?;
    let pattern: AtomString = input.pop_first()?;

    Ok(Output::new(s.ends_with(pattern.as_str())))
}

pub fn string_contains(mut input: Input<'_>) -> Result<Output> {
    let s: AtomString = input.take_receiver()?;
    let pattern: AtomString = input.pop_first()?;

    Ok(Output::new(s.contains(pattern.as_str())))
}

pub fn string_count(mut input: Input<'_>) -> Result<Output> {
    let s: AtomString = input.take_receiver()?;
    let pattern: AtomString = input.pop_first()?;

    Ok(Output::new(Int::from(s.matches(pattern.as_str()).count())))
}

pub fn string_find(mut input: Input<'_>) -> Result<Output> {
    let s: AtomString = input.take_receiver()?;
    let pattern: AtomString = input.pop_first()?;

    Ok(Output::new(
        s.find(pattern.as_str())
            .map(|index| Value::Int(index.into())),
    ))
}

pub fn string_substr(mut input: Input<'_>) -> Result<Output> {
    let s: AtomString = input.take_receiver()?;
    let index: usize = input.pop_first()?;
    let array: AtomArray<u8> = AtomArray::from(&s.as_bytes()[index..]);

    Ok(Output::new(AtomString::from(array)))
}

pub fn string_replace(mut input: Input<'_>) -> Result<Output> {
    let s: AtomString = input.take_receiver()?;
    let pattern: AtomString = input.pop_first()?;
    let replacement: AtomString = input.pop_first()?;

    Ok(Output::new(AtomString::new(
        s.replace(pattern.as_str(), replacement.as_str()),
    )))
}

pub fn string_chars(mut input: Input<'_>) -> Result<Output> {
    let s: AtomString = input.take_receiver()?;

    Ok(Output::new(AtomRefMut::new(AtomRef::new(
        s.chars().into_iter().map(Value::Char).collect::<Vec<_>>(),
    ))))
}

pub fn string_bytes(mut input: Input<'_>) -> Result<Output> {
    let s: AtomString = input.take_receiver()?;

    Ok(Output::new(AtomRefMut::new(AtomRef::new(
        s.bytes().into_iter().map(Value::Byte).collect::<Vec<_>>(),
    ))))
}

pub fn string_repeat(mut input: Input<'_>) -> Result<Output> {
    let s: AtomString = input.take_receiver()?;
    let count: usize = input.pop_first()?;

    Ok(Output::new(AtomString::new(s.repeat(count))))
}

pub fn string_concat(mut input: Input<'_>) -> Result<Output> {
    let s: AtomString = input.take_receiver()?;
    let other: AtomString = input.pop_first()?;

    Ok(Output::new(AtomString::new(
        [s.as_str(), other.as_str()].concat(),
    )))
}

pub fn string_trim(mut input: Input<'_>) -> Result<Output> {
    let s: AtomString = input.take_receiver()?;

    Ok(Output::new(AtomString::from(s.trim())))
}

pub fn string_len(mut input: Input<'_>) -> Result<Output> {
    let s: AtomString = input.take_receiver()?;

    Ok(Output::new(Int::from(s.len())))
}

pub fn float_floor(mut input: Input<'_>) -> Result<Output> {
    let f: f64 = input.take_receiver()?;

    Ok(Output::new(f.floor()))
}

pub fn array_remove(mut input: Input<'_>) -> Result<Output> {
    let mut a: AtomRefMut<Vec<Value>> = input.take_receiver()?;
    let index: usize = input.pop_first()?;

    if a.len() <= index {
        return Ok(Output::new(None));
    }

    let item = a.as_mut().remove(index);

    Ok(Output::new(item))
}

pub fn array_push(mut input: Input<'_>) -> Result<Output> {
    let mut a: AtomRefMut<Vec<Value>> = input.take_receiver()?;
    let item: Value = input.pop_first()?;

    a.as_mut().push(item);

    Ok(Output::new(None))
}

pub fn array_pop(mut input: Input<'_>) -> Result<Output> {
    let mut a: AtomRefMut<Vec<Value>> = input.take_receiver()?;

    Ok(Output::new(a.as_mut().pop()))
}

pub fn array_clear(mut input: Input<'_>) -> Result<Output> {
    let mut a: AtomRefMut<Vec<Value>> = input.take_receiver()?;

    a.as_mut().clear();

    Ok(Output::new(None))
}

pub fn array_len(mut input: Input<'_>) -> Result<Output> {
    let a: AtomRefMut<Vec<Value>> = input.take_receiver()?;

    Ok(Output::new(Int::from(a.len())))
}

pub fn fn_name(mut input: Input<'_>) -> Result<Output> {
    let func: AtomRef<Fn> = input.take_receiver()?;

    Ok(Output::new(AtomString::new(format!(
        "{}.{}",
        func.origin.module_name, func.name
    ))))
}

pub fn int_size(mut input: Input<'_>) -> Result<Output> {
    let i: Int = input.take_receiver()?;

    Ok(Output::new(Int::from(i.size())))
}
