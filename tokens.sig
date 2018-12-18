signature RUST_TOKENS =
sig
    (* type ('a, 'b) token *)
    type token
    type lineNum
    type svalue

    (* EOF *)
    val EOF:        'a * 'a -> (svalue,'a) token    (* eof *)

    (* strict keywords *)
    val AS:         'a * 'a -> (svalue,'a) token    (* as *)
    val BREAK:      'a * 'a -> (svalue,'a) token    (* break *)
    val CONST:      'a * 'a -> (svalue,'a) token    (* const *)
    val CONTINUE:   'a * 'a -> (svalue,'a) token    (* continue *)
    val CRATE:      'a * 'a -> (svalue,'a) token    (* crate *)
    val ELSE:       'a * 'a -> (svalue,'a) token    (* else *)
    val ENUM:       'a * 'a -> (svalue,'a) token    (* enum *)
    val EXTERN:     'a * 'a -> (svalue,'a) token    (* extern *)
    val FALSE:      'a * 'a -> (svalue,'a) token    (* false *)
    val FN:         'a * 'a -> (svalue,'a) token    (* fn *)
    val FOR:        'a * 'a -> (svalue,'a) token    (* for *)
    val IF:         'a * 'a -> (svalue,'a) token    (* if *)
    val IMPL:       'a * 'a -> (svalue,'a) token    (* impl *)
    val IN:         'a * 'a -> (svalue,'a) token    (* in *)
    val LET:        'a * 'a -> (svalue,'a) token    (* let *)
    val LOOP:       'a * 'a -> (svalue,'a) token    (* loop *)
    val MATCH:      'a * 'a -> (svalue,'a) token    (* match *)
    val MOD:        'a * 'a -> (svalue,'a) token    (* mod *)
    val MOVE:       'a * 'a -> (svalue,'a) token    (* move *)
    val MUT:        'a * 'a -> (svalue,'a) token    (* mut *)
    val PUB:        'a * 'a -> (svalue,'a) token    (* pub *)
    val REF:        'a * 'a -> (svalue,'a) token    (* ref *)
    val RETURN:     'a * 'a -> (svalue,'a) token    (* return *)
    val SELFVALUE:  'a * 'a -> (svalue,'a) token    (* self *)
    val SELFTYPE:   'a * 'a -> (svalue,'a) token    (* Self *)
    val STATIC:     'a * 'a -> (svalue,'a) token    (* static *)
    val STRUCT:     'a * 'a -> (svalue,'a) token    (* struct *)
    val SUPER:      'a * 'a -> (svalue,'a) token    (* super *)
    val TRAIT:      'a * 'a -> (svalue,'a) token    (* trait *)
    val TRUE:       'a * 'a -> (svalue,'a) token    (* true *)
    val TYPE:       'a * 'a -> (svalue,'a) token    (* type *)
    val UNSAFE:     'a * 'a -> (svalue,'a) token    (* unsafe *)
    val USE:        'a * 'a -> (svalue,'a) token    (* use *)
    val WHERE:      'a * 'a -> (svalue,'a) token    (* where *)
    val WHILE:      'a * 'a -> (svalue,'a) token    (* while *)
    val KW_DYN:     'a * 'a -> (svalue,'a) token    (* dyn *)

    (* keywords aren't used yet *)
    val ABSTRACT:   'a * 'a -> (svalue,'a) token    (* abstract *)
    val BECOME:     'a * 'a -> (svalue,'a) token    (* become *)
    val BOX:        'a * 'a -> (svalue,'a) token    (* box *)
    val DO:         'a * 'a -> (svalue,'a) token    (* do *)
    val FINAL:      'a * 'a -> (svalue,'a) token    (* final *)
    val MACRO:      'a * 'a -> (svalue,'a) token    (* macro *)
    val OVERRIDE:   'a * 'a -> (svalue,'a) token    (* override *)
    val PRIV:       'a * 'a -> (svalue,'a) token    (* priv *)
    val TYPEOF:     'a * 'a -> (svalue,'a) token    (* typeof *)
    val UNSIZED:    'a * 'a -> (svalue,'a) token    (* unsized *)
    val VIRTUAL:    'a * 'a -> (svalue,'a) token    (* virtual *)
    val YIELD:      'a * 'a -> (svalue,'a) token    (* yield *)
    val KW_ASYNC:   'a * 'a -> (svalue,'a) token    (* async *)
    val KW_AWAIT:   'a * 'a -> (svalue,'a) token    (* await *)
    val KW_TRY:     'a * 'a -> (svalue,'a) token    (* try *)

    (* weak keywords *)
    val KW_UNION:           'a * 'a -> (svalue,'a)  (* token union *)
    val KW_STATICLIFETIME:  'a * 'a -> (svalue,'a)  (* token 'static *)
    
    (* identifiers *)
    val IDENT:              (string) * 'a * 'a -> (svalue,'a) token

    (* literals *)
    (* literals: characters and strings *)
    val CHAR_LIT:           (char) * 'a * 'a -> (svalue,'a) token
    val STR_LIT:            (string) * 'a * 'a -> (svalue,'a) token
    val RAW_STR_LIT:        (string) * 'a * 'a -> (svalue,'a) token
    val BYTE_LIT:           (int) * 'a * 'a -> (svalue,'a) token
    val BYTE_STR_LIT:       (string) * 'a * 'a -> (svalue,'a) token
    val RAW_BYTE_STR_LIT:   (string) * 'a * 'a -> (svalue,'a) token
    (* literals: ASCII escapes *)
    (* 
        \x41	7-bit character code (exactly 2 digits, up to 0x7F)
        \n	    Newline
        \r	    Carriage return
        \t	    Tab
        \\	    Backslash
        \0	    Null
    *)
    (* literals: Byte escapes *)
    (* 
        \x7F	8-bit character code (exactly 2 digits)
        \n	    Newline
        \r	    Carriage return
        \t	    Tab
        \\	    Backslash
        \0  	Null
    *)
    (* literals: Unicode escapes *)
    (* \u{7FFF}	24-bit Unicode character code (up to 6 digits) *)
    (* literals: Quote escapes *)
    (*
        \'	Single quote
        \"	Double quote
    *)
    (* literals: Numbers *)
    val INTEGER_LIT:         (int) * 'a * 'a -> (svalue,'a) token
    val DEC_LIT:             (int) * 'a * 'a -> (svalue,'a) token
    val TUPLE_INDEX:         (int) * 'a * 'a -> (svalue,'a) token
    val BIN_LIT:             (int) * 'a * 'a -> (svalue,'a) token
    val OCT_LIT:             (int) * 'a * 'a -> (svalue,'a) token
    val HEX_LIT:             (int) * 'a * 'a -> (svalue,'a) token
    val FLOAT_LIT:           (int) * 'a * 'a -> (svalue,'a) token
    val FLOAT_EXPONENT:      (int) * 'a * 'a -> (svalue,'a) token
    (* literals: Suffixes *)
    val INTEGER_SUFFIX:      (string) * 'a * 'a -> (svalue,'a) token
    val FLOAT_SUFFIX:        (string) * 'a * 'a -> (svalue,'a) token

    (* lifetimes and loop labels *)
    val LIFETIME_TOKEN:     'a * 'a -> (svalue,'a) token
    val LIFETIME_OR_LABEL:  'a * 'a -> (svalue,'a) token    

    (* punctuations *)
    val PLUS: 	    'a * 'a -> (svalue,'a) token    (* +	Addition, Trait Bounds, Macro Kleene Matcher *)
    val MINUS: 	    'a * 'a -> (svalue,'a) token    (* -	Subtraction, Negation *)
    val STAR: 	    'a * 'a -> (svalue,'a) token    (* *	Multiplication, Dereference, Raw Pointers, Macro Kleene Matcher *)
    val SLASH:	    'a * 'a -> (svalue,'a) token    (* /	Division *)
    val PERCENT:	'a * 'a -> (svalue,'a) token    (* %	Remainder *)
    val CARET:	    'a * 'a -> (svalue,'a) token    (* ^	Bitwise and Logical XOR *)
    val NOT:	    'a * 'a -> (svalue,'a) token    (* !	Bitwise and Logical NOT, Macro Calls, Inner Attributes, Never Type *)
    val AND:	    'a * 'a -> (svalue,'a) token    (* &	Bitwise and Logcal AND, Borrow, References *)
    val OR:	        'a * 'a -> (svalue,'a) token    (* |	Bitwise and Logical OR, Closures, Match *)
    val ANDAND:	    'a * 'a -> (svalue,'a) token    (* &&	Lazy AND, Borrow, References *)
    val OROR:	    'a * 'a -> (svalue,'a) token    (* ||	Lazy OR, Closures *)
    val SHL:	    'a * 'a -> (svalue,'a) token    (* <<	Shift Left, Nested Generics *)
    val SHR:	    'a * 'a -> (svalue,'a) token    (* >>	Shift Right, Nested Generics *)
    val PLUSEQ:	    'a * 'a -> (svalue,'a) token    (* +=	Addition assignment *)
    val MINUSEQ:	'a * 'a -> (svalue,'a) token    (* -=	Subtraction assignment *)
    val STAREQ:	    'a * 'a -> (svalue,'a) token    (* *=	Multiplication assignment *)
    val SLASHEQ:	'a * 'a -> (svalue,'a) token    (* /=	Division assignment *)
    val PERCENTEQ:	'a * 'a -> (svalue,'a) token    (* %=	Remainder assignment *)
    val CARETEQ:	'a * 'a -> (svalue,'a) token    (* ^=	Bitwise XOR assignment *)
    val ANDEQ:	    'a * 'a -> (svalue,'a) token    (* &=	Bitwise And assignment *)
    val OREQ:	    'a * 'a -> (svalue,'a) token    (* |=	Bitwise Or assignment *)
    val SHLEQ:	    'a * 'a -> (svalue,'a) token    (* <<=	Shift Left assignment *)
    val SHREQ:	    'a * 'a -> (svalue,'a) token    (* >>=	Shift Right assignment, Nested Generics *)
    val EQ:	        'a * 'a -> (svalue,'a) token    (* =	Assignment, Attributes, Various type definitions *)
    val EQEQ:	    'a * 'a -> (svalue,'a) token    (* ==	Equal *)
    val NE:	        'a * 'a -> (svalue,'a) token    (* !=	Not Equal *)
    val GT:	        'a * 'a -> (svalue,'a) token    (* >	Greater than, Generics, Paths *)
    val LT:	        'a * 'a -> (svalue,'a) token    (* <	Less than, Generics, Paths *)
    val GE:	        'a * 'a -> (svalue,'a) token    (* >=	Greater than or equal to, Generics *)
    val LE:	        'a * 'a -> (svalue,'a) token    (* <=	Less than or equal to *)
    val AT:	        'a * 'a -> (svalue,'a) token    (* @	Subpattern binding *)
    val UNDERSCORE:	'a * 'a -> (svalue,'a) token    (* _	Placeholder patterns, Inferred types *)
    val DOT:	    'a * 'a -> (svalue,'a) token    (* .	Field access, Tuple index *)
    val DOTDOT:	    'a * 'a -> (svalue,'a) token    (* ..	Range, Struct expressions, Wildcard patterns *)
    val DOTDOTDOT:	'a * 'a -> (svalue,'a) token    (* ...	Variadic functions *)
    val DOTDOTEQ:	'a * 'a -> (svalue,'a) token    (* ..=	Inclusive Range *)
    val COMMA:	    'a * 'a -> (svalue,'a) token    (* ,	Various separators *)
    val SEMI:	    'a * 'a -> (svalue,'a) token    (* ;	Terminator for various items and statements, Array types *)
    val COLON:	    'a * 'a -> (svalue,'a) token    (* :	Various separators *)
    val PATHSEP:	'a * 'a -> (svalue,'a) token    (* ::	Path separator *)
    val RARROW:	    'a * 'a -> (svalue,'a) token    (* ->	Function return type, Closure return type *)
    val FATARROW:	'a * 'a -> (svalue,'a) token    (* =>	Match arms, Macros *)
    val POUND:	    'a * 'a -> (svalue,'a) token    (* #	Attributes *)
    val DOLLAR:	    'a * 'a -> (svalue,'a) token    (* $	Macros *)
    val QUESTION:	'a * 'a -> (svalue,'a) token    (* ?	Question mark operator, Questionably sized *)

    (* delimiters *)
    val LBRACE:     'a * 'a -> (svalue,'a) token    (* Curly braces *)
    val RBRACE:     'a * 'a -> (svalue,'a) token    (* Curly braces *)
    val LBRACKET:   'a * 'a -> (svalue,'a) token    (* Square brackets *)
    val RBRACKET:   'a * 'a -> (svalue,'a) token    (* Square brackets *)
    val LPARENT:    'a * 'a -> (svalue,'a) token    (* Parentheses *)
    val RPARENT:    'a * 'a -> (svalue,'a) token    (* Parentheses *)
end

signature RUST_LRVALS =
sig
    structure Tokens : RUST_TOKENS
    structure ParserData:PARSER_DATA
    sharing type ParserData.Token.token = Tokens.token
    sharing type ParserData.svalue = Tokens.svalue
end