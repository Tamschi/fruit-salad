#![cfg(feature = "macros")]

use fruit_salad::Dyncast;

#[derive(Dyncast)]
#[dyncast(#![runtime_pointer_size_assertion] unsafe T)]
#[repr(transparent)]
pub struct TransparentGeneric<T>(pub T);
