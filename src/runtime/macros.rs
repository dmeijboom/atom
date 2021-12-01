macro_rules! wrap_fn {
    ($fn_ptr:expr) => {
        |args: crate::runtime::types::Input<'_>| Ok($fn_ptr(args)?.into())
    };
}

// This should stay on the bottom of the file otherwise it won't work
pub(crate) use wrap_fn;
