fn exp() {
    /* literal exp */
    "hello";   // string type
    '5';       // character type
    5;         // integer type

    /* operator exp */
    let shared_reference = &7;
    //let mutable_reference = &mut array;

    /* array exp */
    [1, 2, 3, 4];
    ["a", "b", "c", "d"];
    [0; 128];              // array with 128 zeros
    [0u8, 0u8, 0u8, 0u8,];
    [[1, 0, 0], [0, 1, 0], [0, 0, 1]]; // 2D array

    /* group */
    let x: i32 = 2 + 3 * 4;
    let y: i32 = (2 + 3) * 4;

    /* index exp */
    // lint is deny by default.
    //#![warn(const_err)]

    (["a", "b"])[1];

    ([1, 2, 3, 4])[2];        // Evaluates to 3

    [[1, 0, 0], [0, 1, 0], [0, 0, 1]];
    b[1][2];                  // multidimensional array indexing

    let x = (["a", "b"])[10]; // warning: index out of bounds

    let n = 10;
    let y = (["a", "b"])[n];  // panics

    let arr = ["a", "b"];
    arr[10];                  // warning: index out of bounds

    /* tuple */
    (0.0, 4.5);
    ("a", 4usize, true);
    ();
    (0,); // single-element tuple
    (0); // zero in parentheses

    /* tuple index */
    let pair = (1, 2);
    //let unit_x = Point(1.0, 0.0);

    Point {x: 10.0, y: 20.0};
    NothingInMe {};
    TuplePoint(10.0, 20.0);
    TuplePoint { 0: 10.0, 1: 20.0 }; // Results in the same value as the above line

    /* enum, call */
    let three: i32 = add(1i32, 2i32);

    /* field */
    let a = &mut x.f1; // x.f1 borrowed mutably
    let b = &x.f2;         // x.f2 borrowed immutably
    let c = &x.f2;         // Can borrow again
    let d = x.f3;           // Move out of x.f3

    /* closure */
    ten_times(|j| println("hello, {}", j));
    // With type annotations
    ten_times(|j: i32| -> () { println("hello, {}", j) });

    let word = "konnichiwa".to_owned();
    ten_times(move |j| println("{}, {}", word, j));

    /* break */
    break;
    break break 2;
    break 1+2

    /* range */
    //1..2;   // std::ops::Range
    //3..;    // std::ops::RangeFrom
    ..4;    // std::ops::RangeTo
    ..;     // std::ops::RangeFull
    //5..=6;  // std::ops::RangeInclusive
    ..=7;   // std::ops::RangeToInclusive

    /* macro */
    println!("Hello!");
    let x = vec![1,2,3];
    //type N2 = Tuple!(i32, i32);
    example!();
}


