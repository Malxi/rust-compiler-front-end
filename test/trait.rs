trait Seq<T> {
    fn len(&self) -> u32;
    fn elt_at(&self, n: u32) -> T;
    fn iter<F>(&self, f: F) where F: A+B+C;
}

trait Shape { fn area(&self) -> f64; }
trait Circle : Shape { fn radius(&self) -> f64; }

trait Shape { fn area(&self) -> f64; }
trait Circle where A: Shape { fn radius(&self) -> f64; }

unsafe trait Circle where a: Shape {
    fn radius(&self) -> f64 {
        // A = pi * r^2
        // so algebraically,
        // r = sqrt(A / pi)
        //(self.area() /std::f64::consts::PI).sqrt()
    }
}
unsafe trait T {
    fn f(i32);  // Parameter identifiers are not required.
}


// trait T {
//     fn f1((a, b): (i32, i32)) {}
//     fn f2(_: (i32, i32));  // Cannot use tuple pattern without a body.
// }