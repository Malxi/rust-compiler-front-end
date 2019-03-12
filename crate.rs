//#!/usr/bin/env rustx
// Specify the crate name.
#![crate_name = "projx"]

// Specify the type of output artifact.
#![crate_type = "lib"]

// Turn on a warning.
// This can be done in any module, not just the anonymous crate module.
#![warn(non_camel_case_types)]

pub (self) mod math {
    // type Complex = (f64, f64);
    // fn sin(f: f64) -> f64 {
    //     /* ... */
    // }
    // fn cos(f: f64) -> f64 {
    //     /* ... */
    // }
    // fn tan(f: f64) -> f64 {
    //     /* ... */
    // }
}