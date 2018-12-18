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