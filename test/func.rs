fn answer_to_life_the_universe_and_everything() -> i32 {
}

fn foo<T>(x: T) where T: Debug {}

fn foo<'a, 'aa, 'aaa, 'aaaa, T>(x: T) where T: Debug {
    // details elided
}

fn pa (a:int, b:int, c:int, d:int) {}

const unsafe extern "23333" fn xxx() {}

// Declares an extern fn, the ABI defaults to "C"
extern fn new_i32() -> i32 { }

// Declares an extern fn with "stdcall" ABI
extern "stdcall" fn new_i32_stdcall() -> i32 { }