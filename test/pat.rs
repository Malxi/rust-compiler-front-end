fn pat_test() {
    let mut variable = 10;

    let x = 2;

    let SOEM(SOME(2), SOME(2), ..) = x;

    let Struct{a: x, b: y, c: z} = struct_value;

    let [a, b, c] = arr;
}