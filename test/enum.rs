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