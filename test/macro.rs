/// Possibly accepts an `token::Interpolated` expression (a pre-parsed expression
/// dropped into the token stream, which happens while parsing the result of
/// macro expansion). Placement of these is not as complex as I feared it would
/// be. The important thing is to make sure that lookahead doesn't balk at
/// `token::Interpolated` tokens.
macro_rules! maybe_whole_expr {
    ($p:expr) => {
        if let token::Interpolated(nt) = &$p.token {
            match &**nt {
                token::NtExpr(e) | token::NtLiteral(e) => {
                    let e = e.clone();
                    $p.bump();
                    return Ok(e);
                }
                token::NtPath(path) => {
                    let path = path.clone();
                    $p.bump();
                    return Ok($p.mk_expr($p.span, ExprKind::Path(None, path), ThinVec::new()));
                }
                token::NtBlock(block) => {
                    let block = block.clone();
                    $p.bump();
                    return Ok($p.mk_expr($p.span, ExprKind::Block(block, None), ThinVec::new()));
                }
                _ => {},
            };
        }
    }
}


bitflags::bitflags! {
    struct Restrictions: u8 {
        const STMT_EXPR         = 1 << 0;
        const NO_STRUCT_LITERAL = 1 << 1;
    }
}

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