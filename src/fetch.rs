//! [`Ptr<T>`] is the main feature in this module. It is a special type of pointer
//! into a `Vec<Value>` which can be dereferenced as `T`.

use fortuples::fortuples;
use std::marker::PhantomData;

use crate::{CodePtr, Value};

/// A "pointer" containing a `usize` that may be dereferenced as a type `T`
/// It doesn't actually contain a `T` but you can use the [`Fetch`] trait to
/// conveniently fetch a type `T` from any `&[Value]`
#[derive(Debug, Clone, Copy)]
pub struct Ptr<T>(usize, PhantomData<T>);

impl<T> From<Ptr<T>> for usize {
    fn from(value: Ptr<T>) -> Self {
        value.0
    }
}
impl<T> Ptr<T> {
    pub fn new(inner: usize) -> Self {
        Self(inner, PhantomData)
    }
    pub fn inner(&self) -> usize {
      self.0
    }
    pub unsafe fn cast_to<U>(&self) -> Ptr<U> {
        Ptr::new(self.0)
    }
    pub unsafe fn offset<U>(&self, off: usize) -> Ptr<U> {
        Ptr::new(self.0 + off)
    }
}

pub trait Fetch<T> {
    fn fetch(&self, mem: &[Value]) -> T;
}
impl Fetch<Value> for Ptr<Value> {
    fn fetch(&self, mem: &[Value]) -> Value {
        mem[self.0]
    }
}
impl<T> Fetch<Ptr<T>> for Ptr<Ptr<T>> {
    fn fetch(&self, mem: &[Value]) -> Ptr<T> {
        Ptr::new(mem[self.0])
    }
}
impl Fetch<CodePtr> for Ptr<CodePtr> {
    fn fetch(&self, mem: &[Value]) -> CodePtr {
        unsafe { std::mem::transmute::<Value, CodePtr>(mem[self.0]) }
    }
}

fortuples! {
    // All false positives due to macro weirdness
    #[allow(clippy::unused_unit)]
    #[allow(unused_mut)]
    #[allow(unused_variables)]
    #[allow(unused_assignments)]
    impl Fetch<#Tuple> for Ptr<#Tuple>
    where
        #(Ptr<#Member>: Fetch<#Member>),*
    {
        fn fetch(&self, mem: &[Value]) -> #Tuple {
            let mut off = 0;
            unsafe {
              (#({ let ret = self.offset::<#Member>(off).fetch(mem); off += 1; ret }),*)
            }
        }
    }
}
