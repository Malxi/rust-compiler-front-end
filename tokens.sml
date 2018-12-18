structure Tokens : RUST_TOKENS =
struct
  (* A "scaffold" structure for debugging lexers. *)

type linenum = int
type token = string

fun LET(i,j) = "LET   " ^ Int.toString(i)
fun IF(i,j) = "IF   " ^ Int.toString(i)
fun COMMA(i,j) = "COMMA   " ^ Int.toString(i)
fun INTEGER_LIT(c,i,j) = "INTEGER_LIT("^Int.toString(c)^")   " ^ Int.toString(i)
fun IDENTIFIER(s,i,j) = "IDENTIFIER("^s^")     " ^ Int.toString(i)
fun EOF(i,j) = "EOF   " ^ Int.toString(i)
end
