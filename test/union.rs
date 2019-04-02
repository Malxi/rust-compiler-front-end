#[repr(C)]
union MyUnion {
    f1: u32,
    f2: f32,
}

#[repr(C)]
union U {
    i: i32,
    f: f32,
    t: i32,
    z: f32,
}