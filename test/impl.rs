impl Point {
    fn log(&self) {
        //println!("Point is at ({}, {})", self.x, self.y);
    }
}

impl Copy for Circle {}

impl Clone for Circle {
    fn clone(&self) -> Circle {  }
}

impl Shape for Circle {
    fn draw(&self, s: Surface) {  }
    fn bounding_box(&self) -> BoundingBox {
        // let r = self.radius;
        // BoundingBox {
        //     x: self.center.x - r,
        //     y: self.center.y - r,
        //     width: 2.0 * r,
        //     height: 2.0 * r,
        // }
    }
}


impl<T> Seq for Vec {
    /* ... */
}
impl Seq for u32 {
    /* Treat the integer as a sequence of bits */
}