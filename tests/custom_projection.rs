#![cfg(feature = "macros")]

use fruit_salad::Dyncast;

#[derive(Dyncast)]
#[dyncast(
	#![runtime_pointer_size_assertion]
	#![unsafe custom_projection =
		#![unsafe deallocate_owned]
		|this| (this as *const *mut T).read()
	]
	unsafe T
)]
#[repr(transparent)]
pub struct AssignmentStyle<T: ?Sized>(pub Box<T>);

#[derive(Dyncast)]
#[dyncast(
	#![runtime_pointer_size_assertion]
	#![unsafe custom_projection(
		#![unsafe deallocate_owned]
		|this| (this as *const *mut T).read())
	]
	unsafe T
)]
#[repr(transparent)]
pub struct ArgumentStyle<T: ?Sized>(pub Box<T>);
