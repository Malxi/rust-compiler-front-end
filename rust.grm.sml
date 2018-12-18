functor RustLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Rust_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* rust.yacc *)

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\004\000\000\000\
\"
val actionRowNumbers =
"\001\000\000\000"
val gotoT =
"\
\\002\000\001\000\000\000\
\\000\000\
\"
val numstates = 2
val numrules = 2
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | LIFETIME of unit ->  (string) | FLOAT_LIT of unit ->  (real)
 | TUPLE_INDEX of unit ->  (int)
 | INTEGER_LIT of unit ->  (LargeInt.int)
 | RAW_BYTE_STR_LIT of unit ->  (string)
 | BYTE_STR_LIT of unit ->  (string) | BYTE_LIT of unit ->  (int)
 | RAW_STR_LIT of unit ->  (string) | STR_LIT of unit ->  (string)
 | CHAR_LIT of unit ->  (int) | IDENT of unit ->  (string)
end
type svalue = MlyValue.svalue
type result = unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 1) => true | (T 2) => true | (T 3) => true | (T 4) => true | (T 
5) => true | (T 6) => true | (T 7) => true | (T 8) => true | (T 9)
 => true | (T 10) => true | (T 11) => true | (T 12) => true | (T 13)
 => true | (T 14) => true | (T 15) => true | (T 16) => true | (T 17)
 => true | (T 18) => true | (T 19) => true | (T 20) => true | (T 21)
 => true | (T 22) => true | (T 23) => true | (T 24) => true | (T 25)
 => true | (T 26) => true | (T 27) => true | (T 28) => true | (T 29)
 => true | (T 30) => true | (T 31) => true | (T 32) => true | (T 33)
 => true | (T 34) => true | (T 35) => true | (T 37) => true | (T 38)
 => true | (T 39) => true | (T 40) => true | (T 41) => true | (T 42)
 => true | (T 43) => true | (T 44) => true | (T 45) => true | (T 46)
 => true | (T 47) => true | (T 48) => true | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "AS"
  | (T 2) => "BREAK"
  | (T 3) => "CONST"
  | (T 4) => "CONTINUE"
  | (T 5) => "CRATE"
  | (T 6) => "ELSE"
  | (T 7) => "ENUM"
  | (T 8) => "EXTERN"
  | (T 9) => "FALSE"
  | (T 10) => "FN"
  | (T 11) => "FOR"
  | (T 12) => "IF"
  | (T 13) => "IMPL"
  | (T 14) => "IN"
  | (T 15) => "LET"
  | (T 16) => "LOOP"
  | (T 17) => "MATCH"
  | (T 18) => "MOD"
  | (T 19) => "MOVE"
  | (T 20) => "MUT"
  | (T 21) => "PUB"
  | (T 22) => "REF"
  | (T 23) => "RETURN"
  | (T 24) => "SELFVALUE"
  | (T 25) => "SELFTYPE"
  | (T 26) => "STATIC"
  | (T 27) => "STRUCT"
  | (T 28) => "SUPER"
  | (T 29) => "TRAIT"
  | (T 30) => "TRUE"
  | (T 31) => "TYPE"
  | (T 32) => "UNSAFE"
  | (T 33) => "USE"
  | (T 34) => "WHERE"
  | (T 35) => "WHILE"
  | (T 36) => "DYN"
  | (T 37) => "ABSTRACT"
  | (T 38) => "BECOME"
  | (T 39) => "BOX"
  | (T 40) => "DO"
  | (T 41) => "FINAL"
  | (T 42) => "MACRO"
  | (T 43) => "OVERRIDE"
  | (T 44) => "PRIV"
  | (T 45) => "TYPEOF"
  | (T 46) => "UNSIZED"
  | (T 47) => "VIRTUAL"
  | (T 48) => "YIELD"
  | (T 49) => "ASYNC"
  | (T 50) => "AWAIT"
  | (T 51) => "TRY"
  | (T 52) => "UNION"
  | (T 53) => "STATICLIFETIME"
  | (T 54) => "IDENT"
  | (T 55) => "CHAR_LIT"
  | (T 56) => "STR_LIT"
  | (T 57) => "RAW_STR_LIT"
  | (T 58) => "BYTE_LIT"
  | (T 59) => "BYTE_STR_LIT"
  | (T 60) => "RAW_BYTE_STR_LIT"
  | (T 61) => "INTEGER_LIT"
  | (T 62) => "TUPLE_INDEX"
  | (T 63) => "FLOAT_LIT"
  | (T 64) => "LIFETIME"
  | (T 65) => "PLUS"
  | (T 66) => "MINUS"
  | (T 67) => "STAR"
  | (T 68) => "SLASH"
  | (T 69) => "PERCENT"
  | (T 70) => "CARET"
  | (T 71) => "NOT"
  | (T 72) => "AND"
  | (T 73) => "OR"
  | (T 74) => "ANDAND"
  | (T 75) => "OROR"
  | (T 76) => "SHL"
  | (T 77) => "SHR"
  | (T 78) => "PLUSEQ"
  | (T 79) => "MINUSEQ"
  | (T 80) => "STAREQ"
  | (T 81) => "SLASHEQ"
  | (T 82) => "PERCENTEQ"
  | (T 83) => "CARETEQ"
  | (T 84) => "ANDEQ"
  | (T 85) => "OREQ"
  | (T 86) => "SHLEQ"
  | (T 87) => "SHREQ"
  | (T 88) => "EQ"
  | (T 89) => "EQEQ"
  | (T 90) => "NE"
  | (T 91) => "GT"
  | (T 92) => "LT"
  | (T 93) => "GE"
  | (T 94) => "LE"
  | (T 95) => "AT"
  | (T 96) => "UNDERSCORE"
  | (T 97) => "DOT"
  | (T 98) => "DOTDOT"
  | (T 99) => "DOTDOTDOT"
  | (T 100) => "DOTDOTEQ"
  | (T 101) => "COMMA"
  | (T 102) => "SEMI"
  | (T 103) => "COLON"
  | (T 104) => "PATHSEP"
  | (T 105) => "RARROW"
  | (T 106) => "FATARROW"
  | (T 107) => "POUND"
  | (T 108) => "DOLLAR"
  | (T 109) => "QUESTION"
  | (T 110) => "LBRACE"
  | (T 111) => "RBRACE"
  | (T 112) => "LBRACKET"
  | (T 113) => "RBRACKET"
  | (T 114) => "LPARENT"
  | (T 115) => "RPARENT"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 115) $$ (T 114) $$ (T 113) $$ (T 112) $$ (T 111) $$ (T 110) $$ 
(T 109) $$ (T 108) $$ (T 107) $$ (T 106) $$ (T 105) $$ (T 104) $$ (T 
103) $$ (T 102) $$ (T 101) $$ (T 100) $$ (T 99) $$ (T 98) $$ (T 97)
 $$ (T 96) $$ (T 95) $$ (T 94) $$ (T 93) $$ (T 92) $$ (T 91) $$ (T 90)
 $$ (T 89) $$ (T 88) $$ (T 87) $$ (T 86) $$ (T 85) $$ (T 84) $$ (T 83)
 $$ (T 82) $$ (T 81) $$ (T 80) $$ (T 79) $$ (T 78) $$ (T 77) $$ (T 76)
 $$ (T 75) $$ (T 74) $$ (T 73) $$ (T 72) $$ (T 71) $$ (T 70) $$ (T 69)
 $$ (T 68) $$ (T 67) $$ (T 66) $$ (T 65) $$ (T 53) $$ (T 52) $$ (T 51)
 $$ (T 50) $$ (T 49) $$ (T 48) $$ (T 47) $$ (T 46) $$ (T 45) $$ (T 44)
 $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37)
 $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30)
 $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23)
 $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16)
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 
1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 0, ( result, defaultPos, defaultPos), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Rust_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun AS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun CONST (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun CONTINUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun CRATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun ENUM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun EXTERN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun LOOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun MATCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun MOVE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun MUT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun PUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun REF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun RETURN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun SELFVALUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun SELFTYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun STATIC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun STRUCT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun SUPER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun TRAIT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun UNSAFE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun USE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun WHERE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun DYN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun ABSTRACT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun BECOME (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun BOX (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun FINAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun MACRO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun OVERRIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun PRIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPEOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun UNSIZED (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
fun VIRTUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.VOID,p1,p2))
fun YIELD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(
ParserData.MlyValue.VOID,p1,p2))
fun ASYNC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(
ParserData.MlyValue.VOID,p1,p2))
fun AWAIT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 50,(
ParserData.MlyValue.VOID,p1,p2))
fun TRY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 51,(
ParserData.MlyValue.VOID,p1,p2))
fun UNION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 52,(
ParserData.MlyValue.VOID,p1,p2))
fun STATICLIFETIME (p1,p2) = Token.TOKEN (ParserData.LrTable.T 53,(
ParserData.MlyValue.VOID,p1,p2))
fun IDENT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 54,(
ParserData.MlyValue.IDENT (fn () => i),p1,p2))
fun CHAR_LIT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 55,(
ParserData.MlyValue.CHAR_LIT (fn () => i),p1,p2))
fun STR_LIT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 56,(
ParserData.MlyValue.STR_LIT (fn () => i),p1,p2))
fun RAW_STR_LIT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 57,(
ParserData.MlyValue.RAW_STR_LIT (fn () => i),p1,p2))
fun BYTE_LIT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 58,(
ParserData.MlyValue.BYTE_LIT (fn () => i),p1,p2))
fun BYTE_STR_LIT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 59,(
ParserData.MlyValue.BYTE_STR_LIT (fn () => i),p1,p2))
fun RAW_BYTE_STR_LIT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 60
,(ParserData.MlyValue.RAW_BYTE_STR_LIT (fn () => i),p1,p2))
fun INTEGER_LIT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 61,(
ParserData.MlyValue.INTEGER_LIT (fn () => i),p1,p2))
fun TUPLE_INDEX (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 62,(
ParserData.MlyValue.TUPLE_INDEX (fn () => i),p1,p2))
fun FLOAT_LIT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 63,(
ParserData.MlyValue.FLOAT_LIT (fn () => i),p1,p2))
fun LIFETIME (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 64,(
ParserData.MlyValue.LIFETIME (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 65,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 66,(
ParserData.MlyValue.VOID,p1,p2))
fun STAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 67,(
ParserData.MlyValue.VOID,p1,p2))
fun SLASH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 68,(
ParserData.MlyValue.VOID,p1,p2))
fun PERCENT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 69,(
ParserData.MlyValue.VOID,p1,p2))
fun CARET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 70,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 71,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 72,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 73,(
ParserData.MlyValue.VOID,p1,p2))
fun ANDAND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 74,(
ParserData.MlyValue.VOID,p1,p2))
fun OROR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 75,(
ParserData.MlyValue.VOID,p1,p2))
fun SHL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 76,(
ParserData.MlyValue.VOID,p1,p2))
fun SHR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 77,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUSEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 78,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUSEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 79,(
ParserData.MlyValue.VOID,p1,p2))
fun STAREQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 80,(
ParserData.MlyValue.VOID,p1,p2))
fun SLASHEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 81,(
ParserData.MlyValue.VOID,p1,p2))
fun PERCENTEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 82,(
ParserData.MlyValue.VOID,p1,p2))
fun CARETEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 83,(
ParserData.MlyValue.VOID,p1,p2))
fun ANDEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 84,(
ParserData.MlyValue.VOID,p1,p2))
fun OREQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 85,(
ParserData.MlyValue.VOID,p1,p2))
fun SHLEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 86,(
ParserData.MlyValue.VOID,p1,p2))
fun SHREQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 87,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 88,(
ParserData.MlyValue.VOID,p1,p2))
fun EQEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 89,(
ParserData.MlyValue.VOID,p1,p2))
fun NE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 90,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 91,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 92,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 93,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 94,(
ParserData.MlyValue.VOID,p1,p2))
fun AT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 95,(
ParserData.MlyValue.VOID,p1,p2))
fun UNDERSCORE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 96,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 97,(
ParserData.MlyValue.VOID,p1,p2))
fun DOTDOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 98,(
ParserData.MlyValue.VOID,p1,p2))
fun DOTDOTDOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 99,(
ParserData.MlyValue.VOID,p1,p2))
fun DOTDOTEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 100,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 101,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 102,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 103,(
ParserData.MlyValue.VOID,p1,p2))
fun PATHSEP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 104,(
ParserData.MlyValue.VOID,p1,p2))
fun RARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 105,(
ParserData.MlyValue.VOID,p1,p2))
fun FATARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 106,(
ParserData.MlyValue.VOID,p1,p2))
fun POUND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 107,(
ParserData.MlyValue.VOID,p1,p2))
fun DOLLAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 108,(
ParserData.MlyValue.VOID,p1,p2))
fun QUESTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 109,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 110,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 111,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 112,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 113,(
ParserData.MlyValue.VOID,p1,p2))
fun LPARENT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 114,(
ParserData.MlyValue.VOID,p1,p2))
fun RPARENT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 115,(
ParserData.MlyValue.VOID,p1,p2))
end
end
