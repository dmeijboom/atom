use super::error::Result;

pub trait Convert<T> {
    fn convert(self) -> Result<T>;
}
