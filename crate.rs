//#!/usr/bin/env rustx
// Specify the crate name.
#![crate_name = "projx"]

// Specify the type of output artifact.
#![crate_type = "lib"]

// Turn on a warning.
// This can be done in any module, not just the anonymous crate module.
#![warn(non_camel_case_types)]

extern crate pcre;

extern crate std; // equivalent to: extern crate std as std;

extern crate std as ruststd; // linking to 'std' under another name

use a;

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

/*const unsafe extern "C" fn name(arg: Type) -> RetType {
    // add code here
}*/

const unsafe fn name(arg: Type) -> RetType {
    // add code here
}


/* enum */
enum Animal {
    Dog,
    Cat,
}

enum Animal {
    Dog(String, f64),
    Cat { name: String, weight: f64 },
}

enum Foo {
    Bar,            // 0
    Baz = 123,      // 123
    Quux,           // 124
}

enum SharedDiscriminantError {
    SharedA = 1,
    SharedB = 1
}

enum SharedDiscriminantError2 {
    Zero,       // 0
    One,        // 1
    OneToo = 1  // 1 (collision with previous!)
}

#[repr(u8)]
enum OverflowingDiscriminantError {
    Max = 255,
    MaxPlusOne // Would be 256, but that overflows the enum.
}

#[repr(u8)]
enum OverflowingDiscriminantError2 {
    MaxMinusOne = 254, // 254
    Max,               // 255
    MaxPlusOne         // Would be 256, but that overflows the enum.
}

enum ZeroVariants {}

/* Union */
#[repr(C)]
union MyUnion {
    f1: u32,
    f2: f32,
}

/* static item */
static mut LEVELS: u32 = 0;

/* trait */
trait Seq<T> {
    fn len(&self) -> u32;
    fn elt_at(&self, n: u32) -> T;
    fn iter<F>(&self, f: F) where F: Fn(T);
}