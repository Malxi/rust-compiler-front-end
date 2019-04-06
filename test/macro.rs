macro_rules! example {
    //($i:ident) => { };
    () => {};
}

macro_rules! example {
    ($(I $i:ident)* E $e:expr) => { ($($i)-*) * $e };
}

macro_rules! pat {
    ($i:ident) => (Some($i))
}

macro_rules! Tuple {
    { $A:ty, $B:ty } => { ($A, $B) };
}

macro_rules! const_maker {
    ($t:ty, $v:tt) => { const CONST: $t = $v; };
}

macro_rules! example {
    () => { println!("Macro call in a macro!") };
}