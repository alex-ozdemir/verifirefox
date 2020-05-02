use std::mem;
use std::sync::Arc;

#[repr(transparent)]
pub struct Handle<T>
where
    T: ?Sized,
{
    inner: *const T,
}

impl<T> Handle<T> {
    pub fn new(value: T) -> Self {
        Arc::new(value).into()
    }
}

impl<T> Handle<T>
where
    T: ?Sized,
{
    unsafe fn to_arc(&self) -> Arc<T> {
        Arc::<T>::from_raw(self.inner)
    }
}

impl<T> From<Box<T>> for Handle<T>
where
    T: ?Sized,
{
    fn from(boxed: Box<T>) -> Self {
        Arc::<T>::from(boxed).into()
    }
}

impl<T> From<Arc<T>> for Handle<T>
where
    T: ?Sized,
{
    fn from(arc: Arc<T>) -> Self {
        Handle { inner: Arc::into_raw(arc) }
    }
}

impl<T> From<Handle<T>> for Arc<T>
where
    T: ?Sized,
{
    fn from(handle: Handle<T>) -> Self {
        let arc = unsafe { handle.to_arc() };
        mem::forget(handle);
        arc
    }
}

impl<T> Drop for Handle<T>
where
    T: ?Sized,
{
    fn drop(&mut self) {
        mem::drop(unsafe { self.to_arc() });
    }
}

impl<T> Clone for Handle<T>
where
    T: ?Sized,
{
    fn clone(&self) -> Self {
        let temp_arc = unsafe { self.to_arc() };
        let cloned_arc = temp_arc.clone();
        mem::forget(temp_arc);
        cloned_arc.into()
    }
}
