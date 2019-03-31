/* generics */
fn test<>() {}
fn foo<'a, T>() {}
trait A<U> {}
struct Ref<'a, T> where T: 'a {}