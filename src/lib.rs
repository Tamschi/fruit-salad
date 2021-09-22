#![doc(html_root_url = "https://docs.rs/fruit-salad/0.0.1")]
#![warn(clippy::pedantic)]
#![allow(clippy::semicolon_if_nothing_returned)]

#[cfg(doctest)]
pub mod readme {
	doc_comment::doctest!("../README.md");
}
