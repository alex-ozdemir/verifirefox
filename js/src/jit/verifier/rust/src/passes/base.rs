use std::fmt::Debug;

use anyhow::Error;

pub trait Pass: Debug + Send + 'static {
    fn run(&self) -> Result<(), Error>;
}

impl<T> Pass for Box<T>
where
    T: Pass + ?Sized,
{
    fn run(&self) -> Result<(), Error> {
        (**self).run()
    }
}
