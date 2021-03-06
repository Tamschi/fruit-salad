#![cfg(feature = "macros")]

use fruit_salad::Dyncast;
use std::fmt::Debug;

#[derive(Debug, Dyncast)]
#[dyncast(dyn Debug, Self)]
#[dyncast(dyn Debug, Self)]
struct Test;

#[derive(Debug, Dyncast)]
#[dyncast(dyn Debug, unsafe Test)]
struct Test2(String);

#[derive(Debug, Dyncast)]
struct NotDynamicallyDebug;

#[test]
fn dyncast() {
	let mut dyncasts: Vec<Box<dyn Dyncast>> =
		vec![Box::new(Test), Box::new(Test2("Hello!".to_string()))];

	for dyncast in &dyncasts {
		let debug = dyncast.dyncast::<dyn Debug>();
		assert!(debug.is_some());
		println!("{:#?}", debug);
	}

	dyncasts.push(Box::new(NotDynamicallyDebug));
	for dyncast in dyncasts {
		println!("{:#?}", dyncast)
	}
}
