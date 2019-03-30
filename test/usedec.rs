use *;
use ::*;
use std::option::*;
use {self, HashMap};
use ::{self, HashMap};
use std::option::Option::{Some, None};
use std::collections::hash_map::{self, HashMap};
pub use quux::foo::{bar, baz};

// fn foo<T>(_: T){}
// fn bar(map1: HashMap<String, usize>, map2: hash_map::HashMap<String, usize>){}

// fn main() {
//     // Equivalent to 'foo(vec![std::option::Option::Some(1.0f64),
//     // std::option::Option::None]);'
//     foo(vec![Some(1.0f64), None]);

//     // Both `hash_map` and `HashMap` are in scope.
//     let map1 = HashMap::new();
//     let map2 = hash_map::HashMap::new();
//     bar(map1, map2);
// }

// mod quux {
//     pub use quux::foo::{bar, baz};

//     pub mod foo {
//         pub fn bar() { }
//         pub fn baz() { }
//     }
// }

// use std::path::{self, Path, PathBuf};  // good: std is a crate name
// use crate::foo::baz::foobaz;    // good: foo is at the root of the crate

// mod foo {

//     mod example {
//         pub mod iter {}
//     }

//     use crate::foo::example::iter; // good: foo is at crate root
// //  use example::iter;      // bad: relative paths are not allowed without `self`
//     use self::baz::foobaz;  // good: self refers to module 'foo'
//     use crate::foo::bar::foobar;   // good: foo is at crate root

//     pub mod bar {
//         pub fn foobar() { }
//     }

//     pub mod baz {
//         use super::bar::foobar; // good: super refers to module 'foo'
//         pub fn foobaz() { }
//     }
// }

// fn main() {}

// use foo::example::iter;
// use ::foo::baz::foobaz;

// // use std::fs; // Error, this is ambiguous.
// use ::std::fs;  // Imports from the `std` crate, not the module below.
// use self::std::fs as self_fs;  // Imports the module below.

// mod std {
//     pub mod fs {}
// }