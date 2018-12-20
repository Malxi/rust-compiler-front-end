as 
break 
const 
continue 
crate 
else 
enum 
extern 
false 
fn 
for 
if 
impl 
in 
let 
loop 
match 
mod 
move 
mut 
pub 
ref 
return 
self 
Self 
static 
struct 
super 
trait 
true 
type 
unsafe 
use 
where 
while 
dyn
abstract 
become 
box 
do 
final 
macro 
override 
priv 
typeof 
unsized 
virtual 
yield 
async 
await 
try 
union 
'static 

Identifier
'a'
'\''
'\n'
'\r'
'\n'
'\x41'
'\u{0045}'
'中'
// if
"ZDSADAz中文\
23321\
22222\
21212"

"\z"


"foo"; r"foo";                     // foo
"\"foo\""; r#""foo""#;             // "foo"

"foo #\"# bar";
r##"foo #"# bar"##;                // foo #"# bar

"\x52"; "R"; r"R";                 // R
"\\x52"; r"\x52";                  // \x52

b'a'  b'\x32' b'\n'

b"122222"
br"122\12s'
tttztz中"


b"foo"; br"foo";                     // foo
b"\"foo\""; br#""foo""#;             // "foo"

b"foo #\"# bar";
br##"foo #"# bar"##;                 // foo #"# bar

b"\x52"; b"R"; br"R";                // R
b"\\x52"; br"\x52";                  // \x52

123;                               // type i32
123i32;                            // type i32
123u32;                            // type u32
123_u32;                           // type u32
let a: u64 = 123;                  // type u64

0xff;                              // type i32
0xff_u8;                           // type u8

0o70;                              // type i32
0o70_i16;                          // type i16

0b1111_1111_1001_0000;             // type i32
0b1111_1111_1001_0000i64;          // type i64
0b________1;                       // type i32

0usize;                            // type usize

123.0f64;        // type f64
0.1f64;          // type f64
0.1f32;          // type f32
12E+99_f64;      // type f64
let x: f64 = 2.; // type f64

+
-
*
/
%
^
!
&
|
&&
||
<<
>>
+=
-=
*=
/=
%=
^=
&=
|=
<<=
>>=
=
==
!=
>
<
>=
<=
@
_
.
..
...
..=
,
;
:
::
->
=>
#
$
?

//! A doc comment that applies to the implicit anonymous module of this crate

pub mod outer_module {

    //!  - Inner line doc
    //!! - Still an inner line doc (but with a bang at the beginning)

    /*!  - Inner block doc */
    /*!! - Still an inner block doc (but with a bang at the beginning) */

    //   - Only a comment
    ///  - Outer line doc (exactly 3 slashes)
    //// - Only a comment

    /*   - Only a comment */
    /**  - Outer block doc (exactly) 2 asterisks */
    /*** - Only a comment */

    pub mod inner_module {}

    pub mod nested_comments {
        /* In Rust /* we can /* nest comments */ */ */

        // All three types of block comments can contain or be nested inside
        // any other type:

        /*   /* */  /** */  /*! */  */
        /*!  /* */  /** */  /*! */  */
        /**  /* */  /** */  /*! */  */
        pub mod dummy_item {}
    }

    pub mod degenerate_cases {
        // empty inner line doc
        //!

        // empty inner block doc
        /*!*/

        // empty line comment
        //

        // empty outer line doc
        ///

        // empty block comment
        /**/

        pub mod dummy_item {}

        // empty 2-asterisk block isn't a doc block, it is a block comment
        /***/

    }

    /* The next one isn't allowed because outer doc comments
       require an item that will receive the doc */
    /// Where is my item?
    /*

}