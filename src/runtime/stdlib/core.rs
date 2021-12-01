use crate::runtime::macros::wrap_fn;
use crate::runtime::types::{
    AtomArray, AtomNil, AtomRef, AtomRefMut, AtomString, ExternalFn, Fn, Input, Int, Value,
};
use crate::runtime::{ErrorKind, Result, RuntimeError};

pub const FUNCTIONS: [(&str, ExternalFn); 2] = [
    ("println", wrap_fn!(println)),
    ("rt_raise", wrap_fn!(runtime_raise)),
];

pub const METHODS: [(&str, &str, ExternalFn); 24] = [
    ("String", "upper", wrap_fn!(string_upper)),
    ("String", "lower", wrap_fn!(string_lower)),
    ("String", "split", wrap_fn!(string_split)),
    ("String", "startsWith", wrap_fn!(string_starts_with)),
    ("String", "endsWith", wrap_fn!(string_ends_with)),
    ("String", "contains", wrap_fn!(string_contains)),
    ("String", "count", wrap_fn!(string_count)),
    ("String", "find", wrap_fn!(string_find)),
    ("String", "substr", wrap_fn!(string_substr)),
    ("String", "replace", wrap_fn!(string_replace)),
    ("String", "chars", wrap_fn!(string_chars)),
    ("String", "bytes", wrap_fn!(string_bytes)),
    ("String", "repeat", wrap_fn!(string_repeat)),
    ("String", "concat", wrap_fn!(string_concat)),
    ("String", "trim", wrap_fn!(string_trim)),
    ("String", "len", wrap_fn!(string_len)),
    ("Float", "floor", wrap_fn!(float_floor)),
    ("Array", "remove", wrap_fn!(array_remove)),
    ("Array", "push", wrap_fn!(array_push)),
    ("Array", "pop", wrap_fn!(array_pop)),
    ("Array", "len", wrap_fn!(array_len)),
    ("Array", "clear", wrap_fn!(array_clear)),
    ("Fn", "name", wrap_fn!(fn_name)),
    ("Int", "size", wrap_fn!(int_size)),
];

pub fn println(input: Input<'_>) -> Result<()> {
    println!(
        "{}",
        input
            .args
            .into_iter()
            .map(|val| format!("{}", val))
            .collect::<Vec<_>>()
            .join(", ")
    );

    Ok(())
}

pub fn runtime_raise(input: Input<'_>) -> Result<Value> {
    Err(RuntimeError::new(
        ErrorKind::UserError,
        format!("{}", input.args[0]),
    ))
}

pub fn string_upper(mut input: Input<'_>) -> Result<Value> {
    let s: AtomString = input.take_receiver()?;

    Ok(AtomString::new(s.to_uppercase()).into())
}

pub fn string_lower(mut input: Input<'_>) -> Result<Value> {
    let s: AtomString = input.take_receiver()?;

    Ok(AtomString::new(s.to_lowercase()).into())
}

pub fn string_split(mut input: Input<'_>) -> Result<Value> {
    let s: AtomString = input.take_receiver()?;
    let pattern: AtomString = input.take_arg()?;

    if input.args.is_empty() {
        return Ok(AtomRefMut::new(AtomRef::new(
            s.split(pattern.as_str())
                .into_iter()
                .map(AtomString::from)
                .map(Value::from)
                .collect::<Vec<_>>(),
        ))
        .into());
    }

    let count: usize = input.take_arg()?;

    Ok(AtomRefMut::new(AtomRef::new(
        s.splitn(count, pattern.as_str())
            .into_iter()
            .map(|item| Value::from(AtomString::from(item)))
            .collect::<Vec<_>>(),
    ))
    .into())
}

pub fn string_starts_with(mut input: Input<'_>) -> Result<Value> {
    let s: AtomString = input.take_receiver()?;
    let pattern: AtomString = input.take_arg()?;

    Ok(s.starts_with(pattern.as_str()).into())
}

pub fn string_ends_with(mut input: Input<'_>) -> Result<Value> {
    let s: AtomString = input.take_receiver()?;
    let pattern: AtomString = input.take_arg()?;

    Ok(s.ends_with(pattern.as_str()).into())
}

pub fn string_contains(mut input: Input<'_>) -> Result<Value> {
    let s: AtomString = input.take_receiver()?;
    let pattern: AtomString = input.take_arg()?;

    Ok(s.contains(pattern.as_str()).into())
}

pub fn string_count(mut input: Input<'_>) -> Result<Value> {
    let s: AtomString = input.take_receiver()?;
    let pattern: AtomString = input.take_arg()?;

    Ok(Int::from(s.matches(pattern.as_str()).count()).into())
}

pub fn string_find(mut input: Input<'_>) -> Result<Value> {
    let s: AtomString = input.take_receiver()?;
    let pattern: AtomString = input.take_arg()?;

    Ok(s.find(pattern.as_str())
        .map(|index| Value::Int(index.into()))
        .into())
}

pub fn string_substr(mut input: Input<'_>) -> Result<Value> {
    let s: AtomString = input.take_receiver()?;
    let index: usize = input.take_arg()?;
    let array: AtomArray<u8> = AtomArray::from(&s.as_bytes()[index..]);

    Ok(AtomString::from(array).into())
}

pub fn string_replace(mut input: Input<'_>) -> Result<Value> {
    let s: AtomString = input.take_receiver()?;
    let pattern: AtomString = input.take_arg()?;
    let replacement: AtomString = input.take_arg()?;

    Ok(AtomString::new(s.replace(pattern.as_str(), replacement.as_str())).into())
}

pub fn string_chars(mut input: Input<'_>) -> Result<Value> {
    let s: AtomString = input.take_receiver()?;

    Ok(AtomRefMut::new(AtomRef::new(
        s.chars().into_iter().map(Value::Char).collect::<Vec<_>>(),
    ))
    .into())
}

pub fn string_bytes(mut input: Input<'_>) -> Result<Value> {
    let s: AtomString = input.take_receiver()?;

    Ok(AtomRefMut::new(AtomRef::new(
        s.bytes().into_iter().map(Value::Byte).collect::<Vec<_>>(),
    ))
    .into())
}

pub fn string_repeat(mut input: Input<'_>) -> Result<Value> {
    let s: AtomString = input.take_receiver()?;
    let count: usize = input.take_arg()?;

    Ok(AtomString::new(s.repeat(count)).into())
}

pub fn string_concat(mut input: Input<'_>) -> Result<Value> {
    let s: AtomString = input.take_receiver()?;
    let other: AtomString = input.take_arg()?;

    Ok(AtomString::new([s.as_str(), other.as_str()].concat()).into())
}

pub fn string_trim(mut input: Input<'_>) -> Result<Value> {
    let s: AtomString = input.take_receiver()?;

    Ok(AtomString::from(s.trim()).into())
}

pub fn string_len(mut input: Input<'_>) -> Result<Value> {
    let s: AtomString = input.take_receiver()?;

    Ok(Int::from(s.len()).into())
}

pub fn float_floor(mut input: Input<'_>) -> Result<Value> {
    let f: f64 = input.take_receiver()?;

    Ok(f.floor().into())
}

pub fn array_remove(mut input: Input<'_>) -> Result<Value> {
    let mut a: AtomRefMut<Vec<Value>> = input.take_receiver()?;
    let index: usize = input.take_arg()?;

    if a.len() <= index {
        return Ok(AtomNil.into());
    }

    let item = a.as_mut().remove(index);

    Ok(item)
}

pub fn array_push(mut input: Input<'_>) -> Result<Value> {
    let mut a: AtomRefMut<Vec<Value>> = input.take_receiver()?;
    let item: Value = input.take_arg()?;

    a.as_mut().push(item);

    Ok(().into())
}

pub fn array_pop(mut input: Input<'_>) -> Result<Value> {
    let mut a: AtomRefMut<Vec<Value>> = input.take_receiver()?;

    Ok(a.as_mut().pop().into())
}

pub fn array_clear(mut input: Input<'_>) -> Result<Value> {
    let mut a: AtomRefMut<Vec<Value>> = input.take_receiver()?;

    a.as_mut().clear();

    Ok(().into())
}

pub fn array_len(mut input: Input<'_>) -> Result<Value> {
    let a: AtomRefMut<Vec<Value>> = input.take_receiver()?;

    Ok(Int::from(a.len()).into())
}

pub fn fn_name(mut input: Input<'_>) -> Result<Value> {
    let func: AtomRef<Fn> = input.take_receiver()?;

    Ok(AtomString::new(format!("{}.{}", func.origin.module_name, func.name)).into())
}

pub fn int_size(mut input: Input<'_>) -> Result<Value> {
    let i: Int = input.take_receiver()?;

    Ok(Int::from(i.size()).into())
}
