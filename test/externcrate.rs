extern crate pcre;

extern crate std; // equivalent to: extern crate std as std;

extern crate std as ruststd; // linking to 'std' under another name

// Importing the Cargo package hello-world
extern crate hello_world; // hyphen replaced with an underscore

extern crate proc_macro;
// Cannot reference `proc_macro` directly because it is not in the extern prelude.
// use proc_macro::TokenStream;
// Instead, you must reference the item in scope from the `extern crate`
// declaration.
use self::proc_macro::TokenStream;