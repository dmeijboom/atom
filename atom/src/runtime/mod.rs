use ::std::borrow::Cow;

pub mod class;
pub mod error;
pub mod func;
pub mod module;
pub mod std;
pub mod value;

pub type Name = Cow<'static, str>;
