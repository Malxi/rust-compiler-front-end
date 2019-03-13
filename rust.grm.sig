signature Rust_TOKENS =
sig
type ('a,'b) token
type svalue
val ABI:  'a * 'a -> (svalue,'a) token
val LOWER_THAN_ABI:  'a * 'a -> (svalue,'a) token
val LOWER_THAN_LPARENT:  'a * 'a -> (svalue,'a) token
val SHEBANG_LINE: (string) *  'a * 'a -> (svalue,'a) token
val SHEBANG:  'a * 'a -> (svalue,'a) token
val OUTER_DOC_COMMENT: (string) *  'a * 'a -> (svalue,'a) token
val INNER_DOC_COMMENT: (string) *  'a * 'a -> (svalue,'a) token
val RPARENT:  'a * 'a -> (svalue,'a) token
val LPARENT:  'a * 'a -> (svalue,'a) token
val RBRACKET:  'a * 'a -> (svalue,'a) token
val LBRACKET:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val QUESTION:  'a * 'a -> (svalue,'a) token
val DOLLAR:  'a * 'a -> (svalue,'a) token
val POUND:  'a * 'a -> (svalue,'a) token
val FATARROW:  'a * 'a -> (svalue,'a) token
val RARROW:  'a * 'a -> (svalue,'a) token
val PATHSEP:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val SEMI:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val DOTDOTEQ:  'a * 'a -> (svalue,'a) token
val DOTDOTDOT:  'a * 'a -> (svalue,'a) token
val DOTDOT:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val UNDERSCORE:  'a * 'a -> (svalue,'a) token
val AT:  'a * 'a -> (svalue,'a) token
val LE:  'a * 'a -> (svalue,'a) token
val GE:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val GT:  'a * 'a -> (svalue,'a) token
val NE:  'a * 'a -> (svalue,'a) token
val EQEQ:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val SHREQ:  'a * 'a -> (svalue,'a) token
val SHLEQ:  'a * 'a -> (svalue,'a) token
val OREQ:  'a * 'a -> (svalue,'a) token
val ANDEQ:  'a * 'a -> (svalue,'a) token
val CARETEQ:  'a * 'a -> (svalue,'a) token
val PERCENTEQ:  'a * 'a -> (svalue,'a) token
val SLASHEQ:  'a * 'a -> (svalue,'a) token
val STAREQ:  'a * 'a -> (svalue,'a) token
val MINUSEQ:  'a * 'a -> (svalue,'a) token
val PLUSEQ:  'a * 'a -> (svalue,'a) token
val SHR:  'a * 'a -> (svalue,'a) token
val SHL:  'a * 'a -> (svalue,'a) token
val OROR:  'a * 'a -> (svalue,'a) token
val ANDAND:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val CARET:  'a * 'a -> (svalue,'a) token
val PERCENT:  'a * 'a -> (svalue,'a) token
val SLASH:  'a * 'a -> (svalue,'a) token
val STAR:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val LIFETIME: (string) *  'a * 'a -> (svalue,'a) token
val FLOAT_LIT: (real) *  'a * 'a -> (svalue,'a) token
val TUPLE_INDEX: (int) *  'a * 'a -> (svalue,'a) token
val INTEGER_LIT: (LargeInt.int) *  'a * 'a -> (svalue,'a) token
val RAW_BYTE_STR_LIT: (string) *  'a * 'a -> (svalue,'a) token
val BYTE_STR_LIT: (string) *  'a * 'a -> (svalue,'a) token
val BYTE_LIT: (int) *  'a * 'a -> (svalue,'a) token
val RAW_STR_LIT: (string) *  'a * 'a -> (svalue,'a) token
val STR_LIT: (string) *  'a * 'a -> (svalue,'a) token
val CHAR_LIT: (int) *  'a * 'a -> (svalue,'a) token
val IDENT: (string) *  'a * 'a -> (svalue,'a) token
val STATICLIFETIME:  'a * 'a -> (svalue,'a) token
val UNION:  'a * 'a -> (svalue,'a) token
val TRY:  'a * 'a -> (svalue,'a) token
val AWAIT:  'a * 'a -> (svalue,'a) token
val ASYNC:  'a * 'a -> (svalue,'a) token
val YIELD:  'a * 'a -> (svalue,'a) token
val VIRTUAL:  'a * 'a -> (svalue,'a) token
val UNSIZED:  'a * 'a -> (svalue,'a) token
val TYPEOF:  'a * 'a -> (svalue,'a) token
val PRIV:  'a * 'a -> (svalue,'a) token
val OVERRIDE:  'a * 'a -> (svalue,'a) token
val MACRO:  'a * 'a -> (svalue,'a) token
val FINAL:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val BOX:  'a * 'a -> (svalue,'a) token
val BECOME:  'a * 'a -> (svalue,'a) token
val ABSTRACT:  'a * 'a -> (svalue,'a) token
val DYN:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val WHERE:  'a * 'a -> (svalue,'a) token
val USE:  'a * 'a -> (svalue,'a) token
val UNSAFE:  'a * 'a -> (svalue,'a) token
val TYPE:  'a * 'a -> (svalue,'a) token
val TRUE:  'a * 'a -> (svalue,'a) token
val TRAIT:  'a * 'a -> (svalue,'a) token
val SUPER:  'a * 'a -> (svalue,'a) token
val STRUCT:  'a * 'a -> (svalue,'a) token
val STATIC:  'a * 'a -> (svalue,'a) token
val SELFTYPE:  'a * 'a -> (svalue,'a) token
val SELFVALUE:  'a * 'a -> (svalue,'a) token
val RETURN:  'a * 'a -> (svalue,'a) token
val REF:  'a * 'a -> (svalue,'a) token
val PUB:  'a * 'a -> (svalue,'a) token
val MUT:  'a * 'a -> (svalue,'a) token
val MOVE:  'a * 'a -> (svalue,'a) token
val MOD:  'a * 'a -> (svalue,'a) token
val MATCH:  'a * 'a -> (svalue,'a) token
val LOOP:  'a * 'a -> (svalue,'a) token
val LET:  'a * 'a -> (svalue,'a) token
val IN:  'a * 'a -> (svalue,'a) token
val IMPL:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val FOR:  'a * 'a -> (svalue,'a) token
val FN:  'a * 'a -> (svalue,'a) token
val FALSE:  'a * 'a -> (svalue,'a) token
val EXTERN:  'a * 'a -> (svalue,'a) token
val ENUM:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val CRATE:  'a * 'a -> (svalue,'a) token
val CONTINUE:  'a * 'a -> (svalue,'a) token
val CONST:  'a * 'a -> (svalue,'a) token
val BREAK:  'a * 'a -> (svalue,'a) token
val AS:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Rust_LRVALS=
sig
structure Tokens : Rust_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
