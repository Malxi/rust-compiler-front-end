(* rust.lex *)
(*
    Token list, for automatic completion.
    %term EOF
    | AS | BREAK | CONST | CONTINUE | CRATE | ELSE | ENUM | EXTERN     
        | FALSE | FN | FOR | IF | IMPL | IN | LET | LOOP | MATCH | MOD | MOVE
        | MUT | PUB | REF | RETURN | SELFVALUE | SELFTYPE | STATIC | STRUCT 
        | SUPER | TRAIT | TRUE | TYPE | UNSAFE | USE | WHERE | WHILE | DYN
    | ABSTRACT | BECOME | BOX | DO | FINAL | MACRO | OVERRIDE
        | PRIV | TYPEOF | UNSIZED | VIRTUAL | YIELD
        | ASYNC | AWAIT | TRY
    | UNION | STATICLIFETIME
    | IDENT of string
    | CHAR_LIT of int 
        | STR_LIT of string | RAW_STR_LIT of string
        | BYTE_LIT of int | BYTE_STR_LIT of string | RAW_BYTE_STR_LIT of string
        | INTEGER_LIT of LargeInt.int | TUPLE_INDEX of int 
        | FLOAT_LIT of real
    | LIFETIME of string
    | PLUS | MINUS | STAR | SLASH | PERCENT | CARET 
        | NOT | AND | OR | ANDAND | OROR | SHL | SHR
        | PLUSEQ | MINUSEQ | STAREQ | SLASHEQ | PERCENTEQ | CARETEQ | ANDEQ | OREQ
        | SHLEQ | SHREQ | EQ | EQEQ
        | NE | GT | LT | GE | LE
        | AT | UNDERSCORE | DOT | DOTDOT | DOTDOTDOT | DOTDOTEQ
        | COMMA | SEMI | COLON | PATHSEP | RARROW | FATARROW | POUND | DOLLAR | QUESTION
        | LBRACE | RBRACE | LBRACKET | RBRACKET | LPARENT | RPARENT
        | INNER_DOC_COMMENT of string | OUTER_DOC_COMMENT of string
        | SHEBANG
*)
(* 
    points:
    handle comments
    handle strings
    error handling
    end-of-file handling
    ...
*)
(*
    Input format: utf-8.
    Identifier: raw_identifier is not supported.
    Whitespace: Not include Characters 
                U+0085 (next line) 
                U+200E (left-to-right mark) 
                U+200F (right-to-left mark)
                U+2028 (line separator)
                U+2029 (paragraph separator)
    isolatedCR: A \r not followed by a \n, for some reasons, which is hard to be represented.
*)
(* user declarations *)
open Convert
type pos = int
type svalue = Tokens.svalue 
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token
type lexarg = string
type arg = lexarg

val lin = ErrorMsg.lin
val col = ErrorMsg.col
val eolpos = ref 0
fun incLine(pos) = (lin := !lin+1; col := pos::(!col))

datatype comments = InnerBlock | OuterBlock | CommonBlock;
val stateStack:(comments*int) list ref = ref []
fun statePush(state, pos) = stateStack := (state, pos)::(!stateStack)
fun statePop(state):bool = case (!stateStack) of
                    (nil) => false
                    | ((h, _)::t) => if h = state then (stateStack := t;true) else false
val doc = ref ""
val docPos = ref 0
fun docInit(pos) = if null(!stateStack) then (doc:="";docPos:=pos;()) else (doc:=(!doc);docPos:=(!docPos);())

fun error(p1, p2) = ErrorMsg.error p1
fun lexLog(pos, msg) = ErrorMsg.lexLog (pos, msg)

val lsharp = ref 0
val rsharp = ref 0

val strList = ref (nil:char list)
val strpos = ref (0:int)
fun strAppend(s:char) = strList := s::(!strList)
fun strMake() = (implode(rev(!strList)))
fun strPop(nil, _) = nil 
    | strPop(h::t, 0) = h :: t 
    | strPop(h::t, n) = strPop(t, n-1)

fun strip(s, ch) = 
    let
        val chs = rev(String.explode s)
        fun remove(nil) = nil
            | remove(h::t) = if h = ch then remove(t) else h::t
    in
        implode (rev(remove(chs)))
    end

fun toChar(text:string) = 
    let
        val cc = String.explode (text)
        val c = hd(cc)
    in
        (* app print ["char literal: ", Char.toCString c, "\n"]; *)
        c
    end

fun escape(text:string, pos:int) = 
    let
        val chs = String.explode text

        fun hex2dec (nil, v) = v
            | hex2dec (#"a"::t, v) = hex2dec(t, v*16+10)
            | hex2dec (#"A"::t, v) = hex2dec(t, v*16+10)
            | hex2dec (#"b"::t, v) = hex2dec(t, v*16+11)
            | hex2dec (#"B"::t, v) = hex2dec(t, v*16+11)
            | hex2dec (#"c"::t, v) = hex2dec(t, v*16+12)
            | hex2dec (#"C"::t, v) = hex2dec(t, v*16+12)
            | hex2dec (#"d"::t, v) = hex2dec(t, v*16+13)
            | hex2dec (#"D"::t, v) = hex2dec(t, v*16+13)
            | hex2dec (#"e"::t, v) = hex2dec(t, v*16+14)
            | hex2dec (#"E"::t, v) = hex2dec(t, v*16+14)
            | hex2dec (#"f"::t, v) = hex2dec(t, v*16+15)
            | hex2dec (#"F"::t, v) = hex2dec(t, v*16+15)
            | hex2dec (h::t, v) = hex2dec(t, v*16+ (ord h) - (ord #"0"))
        
        fun unicode (nil, v) = v
            | unicode (#"{"::t, v) = unicode(t, v)
            | unicode (#"}"::t, v) = unicode(t, v)
            | unicode (#"_"::t, v) = unicode(t, v)
            | unicode (h::t, v) = unicode(t, v*16+ (ord h) - (ord #"0"))
        (* 
            This function convert char literal to a string for Char.fromString.
            However, Char.fromString can not work when unicode point is in ordinal range of the alphabet.
        *)
        fun convert (nil) = Char.ord #"\000"
            | convert (#"x"::t) = hex2dec(t, 0)
            | convert (#"u"::t) = unicode(t, 0)
            | convert (#"n"::t) = Char.ord #"\n"
            | convert (#"r"::t) = Char.ord #"\r"
            | convert (#"t"::t) = Char.ord #"\t"
            | convert (#"0"::t) = Char.ord #"\000"
            | convert (#"\092"::t) = Char.ord #"\092"
            | convert (#"'"::t) = Char.ord #"'"
            | convert (#"\""::t) = Char.ord #"\""
            | convert (h::t) = Char.ord (toChar(implode([#"\092", h])))
    in
        case chs of
        (#"\092"::t) => convert(t)
        | _ => (ErrorMsg.error pos ("illegal escape " ^ text);0)
    end

fun toInteger(text:string) = case LargeInt.fromString(text) of 
                            SOME v => v
                            | _ => 0
 
fun toFloat(text:string) = case Real.fromString(text) of 
                            SOME v => v
                            | _ => 0.0


fun eof(fileName:string) = 
    let 
        val pos = hd(!col)
    in 
        (
            case !stateStack of
            (nil) => ()
            | ((h, cpos)::t) => 
                ErrorMsg.error cpos "Comment block unclosed."
        );
        (
            if not (!lsharp = !rsharp) then
                    ErrorMsg.error (!strpos) "String unclosed."
            else ()
        );
        lexLog(pos, "Tokens.EOF");
        Tokens.EOF(pos, pos)
    end
%%
%full
%header (functor RustLexFun(structure Tokens: Rust_TOKENS));
%arg (fileName:string);
%reject
%s LINE_COMMENT BLOCK_COMMENT INNER_LINE_DOC INNER_BLOCK_DOC OUTER_LINE_DOC OUTER_BLOCK_DOC
    LIFE_OR_CHAR STR R_STR R_STR_BEGIN R_STR_BODY 
    R_STR_END BYTE BYTE_STR BR_STR BR_STR_BEGIN BR_STR_BODY BR_STR_END SUFFIX
    POUND SHEBANG_OR_ATTR;

alpha = [a-zA-z0-9];
ident = [a-zA-Z_][a-zA-Z0-9_]*;

bin_digit = [0-1];
oct_digit = [0-7];
dec_digit = [0-9];
nz_dec_digit = [1-9];
hex_digit = [0-9a-fA-F];
dec_lit = {dec_digit}({dec_digit}|_)*;
tuple_index = (0 | {nz_dec_digit}{dec_digit}*);
bin_lit = 0b({bin_digit}|_)*{bin_digit}({bin_digit}|_)*;
oct_lit = 0o({oct_digit}|_)*{oct_digit}({oct_digit}|_)*;
hex_lit = 0x({hex_digit}|_)*{hex_digit}({hex_digit}|_)*;
integer_suffix = (u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize);
integer_lit = ({dec_lit}|{bin_lit}|{oct_lit}|{hex_lit}){integer_suffix}?;

float_exponent = (e|E)("+"|"-")?({dec_digit}|_)*{dec_digit}({dec_digit}|_)*;
float_suffix = (f32|f64);
float_lit = {dec_lit}("."|{float_exponent}|"."{dec_lit}{float_exponent}?|("."{dec_lit})?{float_exponent}?{float_suffix});   

ascii = ([\000-\127]);
ascii_char = ([^' \\ \n \r \t \128-\255]);
ascii_str = ([^\034 \n \r \t \128-\255]);
quote_escape = (\\('|\034));
ascii_escape = (\\([r t n 0 \\])|\\(x([0-7]{hex_digit})));
byte_escape = (\\([r t n 0 \\])|\\(x({hex_digit}{2})));
unicode_escape = (\\("u{"({hex_digit}_*){1,6}"}"));
str_continue = (\\"\n");
isolatedCR = (\r[^\n]);
ws = [\ \t];
eol = ("\013\010"|"\010"|"\013");
bom = ("\239\189\191");
shebang_line = ("#!"([^\[\n])*\n);

%%
<INITIAL>{eol}                    => (incLine(yypos); continue());
<INITIAL>{ws}*                    => (continue());

<INITIAL>{bom}                    => (
                                        (if not (!lin = 1) then
                                            ErrorMsg.error yypos ("Unexpected utf8 bom [INITIAL] " ^ yytext)
                                        else ());
                                        continue()
                                    );
<INITIAL>{shebang_line}           => (
                                    (* 
                                        Shebang line should occur at the first line.
                                        If not, just continue and report message.
                                    *)
                                    if (!lin = 1) then(
                                        lin := !lin + 1;
                                        lexLog(yypos, "<Shebang line>: "^yytext);
                                        Tokens.SHEBANG_LINE(strip(yytext, #"\n"), yypos, yypos+size yytext)
                                    )
                                    else (
                                        lin := !lin + 1;
                                        ErrorMsg.error yypos "Illegal Shebang line.";
                                        continue()
                                    )
                                    );
<INITIAL>"#!"                     => (lexLog(yypos, "Token.SHEBANG"); Tokens.SHEBANG(yypos, yypos+ size yytext));

<INITIAL>"//!"                    => (lexLog(yypos, "INNER_LINE_DOC"); docInit(yypos); YYBEGIN INNER_LINE_DOC; continue());
<INNER_LINE_DOC>{eol}             => (
                                        lexLog(yypos, "INNER_DOC_COMMENT: "^(!doc)); 
                                        YYBEGIN INITIAL; 
                                        incLine(yypos);
                                        Tokens.INNER_DOC_COMMENT(!doc, !docPos, yypos)
                                    );
<INNER_LINE_DOC>.                 => (doc:=(!doc)^yytext; continue());

<INITIAL>"///"                    => (lexLog(yypos, "OUTER_LINE_DOC"); docInit(yypos); YYBEGIN OUTER_LINE_DOC; continue());
<OUTER_LINE_DOC>{eol}             => (
                                        lexLog(yypos, "OUTER_DOC_COMMENT: "^(!doc)); 
                                        YYBEGIN INITIAL;
                                        incLine(yypos);
                                        Tokens.OUTER_DOC_COMMENT(!doc, !docPos, yypos)
                                    );
<OUTER_LINE_DOC>.                 => (doc:=(!doc)^yytext; continue());

<INITIAL>("//"|"////")            => (lexLog(yypos, "LINE_COMMENT"); YYBEGIN LINE_COMMENT; continue());
<LINE_COMMENT>"\n"                => (YYBEGIN INITIAL; incLine(yypos);  continue());   
<LINE_COMMENT>.                   => (continue());

<INITIAL, INNER_BLOCK_DOC, BLOCK_COMMENT, OUTER_BLOCK_DOC>"/*!"                    
                                    => (lexLog(yypos, "INNER_BLOCK_DOC"); 
                                    docInit(yypos);
                                    YYBEGIN INNER_BLOCK_DOC; 
                                    statePush(InnerBlock, yypos); 
                                    continue()
                                    );
<INNER_BLOCK_DOC>"*/"             => (
                                    (
                                        if not (statePop(InnerBlock)) then
                                            ErrorMsg.error yypos "INNER_BLOCK_DOC does not match."
                                        else ()
                                    );
                                    (
                                        case (!stateStack) 
                                            of (nil) => ((lexLog(yypos, "INNER_DOC_COMMENT: "^(!doc))); YYBEGIN INITIAL)
                                            | ((CommonBlock, _)::t) => YYBEGIN BLOCK_COMMENT
                                            | ((InnerBlock, _)::t) => YYBEGIN INNER_BLOCK_DOC
                                            | ((OuterBlock, _)::t) => YYBEGIN OUTER_BLOCK_DOC
                                    );
                                    if null(!stateStack) then 
                                    Tokens.INNER_DOC_COMMENT(!doc, !docPos, yypos)
                                    else continue()
                                );
<INITIAL, INNER_BLOCK_DOC, BLOCK_COMMENT, OUTER_BLOCK_DOC>("/**/"|"/***/")                     
=> (lexLog(yypos, "BLOCK_COMMENT"); continue());

<INITIAL, INNER_BLOCK_DOC, BLOCK_COMMENT, OUTER_BLOCK_DOC>"/**"                    
                                    => (
                                    lexLog(yypos, "OUTER_BLOCK_DOC"); 
                                    docInit(yypos); 
                                    YYBEGIN OUTER_BLOCK_DOC; 
                                    statePush(OuterBlock, yypos);
                                    continue()
                                    );
<OUTER_BLOCK_DOC>"*/"             => (
                                        (
                                            if not (statePop(OuterBlock)) then
                                                ErrorMsg.error yypos "OUTER_BLOCK_DOC does not match."
                                            else ()
                                        );
                                        (
                                           case (!stateStack) 
                                                of (nil) => ((lexLog(yypos, "OUTER_DOC_COMMENT: "^(!doc))); YYBEGIN INITIAL)
                                                | ((CommonBlock, _)::t) => YYBEGIN BLOCK_COMMENT
                                                | ((InnerBlock, _)::t) => YYBEGIN INNER_BLOCK_DOC
                                                | ((OuterBlock, _)::t) => YYBEGIN OUTER_BLOCK_DOC
                                        );
                                        if null(!stateStack) then 
                                            Tokens.OUTER_DOC_COMMENT(!doc, !docPos, yypos)
                                        else continue()
                                    );

<INITIAL, INNER_BLOCK_DOC, BLOCK_COMMENT, OUTER_BLOCK_DOC>("/*"|"/***")                     
=> (lexLog(yypos, "BLOCK_COMMENT"); YYBEGIN BLOCK_COMMENT; statePush(CommonBlock, yypos); continue());
<BLOCK_COMMENT>"*/"               => (
                                         (
                                            if not (statePop(CommonBlock)) then
                                                ErrorMsg.error yypos "BLOCK_COMMENT does not match."
                                            else ()
                                        );
                                        (
                                            case (!stateStack) 
                                                of (nil) => YYBEGIN INITIAL
                                                | ((CommonBlock, _)::t) => YYBEGIN BLOCK_COMMENT
                                                | ((InnerBlock, _)::t) => YYBEGIN INNER_BLOCK_DOC
                                                | ((OuterBlock, _)::t) => YYBEGIN OUTER_BLOCK_DOC

                                        );
                                        continue()
                                    );
<BLOCK_COMMENT>{eol}               => (incLine(yypos);  continue());
<INNER_BLOCK_DOC>{eol}             => (doc:=(!doc)^yytext; incLine(yypos);  continue());  
<OUTER_BLOCK_DOC>{eol}             => (doc:=(!doc)^yytext; incLine(yypos);  continue());  
<BLOCK_COMMENT>.                   => (continue());
<INNER_BLOCK_DOC>.                 => (doc:=(!doc)^yytext; continue());
<OUTER_BLOCK_DOC>.                 => (doc:=(!doc)^yytext; continue());  


<SUFFIX>{ident}            => (YYBEGIN INITIAL; continue());
<SUFFIX>(.|\n)             => (YYBEGIN INITIAL; REJECT());
<SUFFIX>(.|\n)             => (lexLog(yypos, "break"); YYBEGIN INITIAL; continue());

<INITIAL>"_"               => (lexLog(yypos, "<Punctuation>"^yytext); Tokens.UNDERSCORE(yypos, yypos+size yytext));
<INITIAL>"as"              => (lexLog(yypos, yytext); Tokens.AS(yypos, yypos+size yytext));
<INITIAL>"break"           => (lexLog(yypos, yytext); Tokens.BREAK(yypos, yypos+size yytext));           
<INITIAL>"const"           => (lexLog(yypos, yytext); Tokens.CONST(yypos, yypos+size yytext));        
<INITIAL>"continue"        => (lexLog(yypos, yytext); Tokens.CONTINUE(yypos, yypos+size yytext));
<INITIAL>"crate"           => (lexLog(yypos, yytext); Tokens.CARET(yypos, yypos+size yytext));
<INITIAL>"else"            => (lexLog(yypos, yytext); Tokens.ELSE(yypos, yypos+size yytext));
<INITIAL>"enum"            => (lexLog(yypos, yytext); Tokens.ENUM(yypos, yypos+size yytext));
<INITIAL>"extern"          => (lexLog(yypos, yytext); Tokens.EXTERN(yypos, yypos+size yytext));
<INITIAL>"false"           => (lexLog(yypos, yytext); Tokens.FALSE(yypos, yypos+size yytext));
<INITIAL>"fn"              => (lexLog(yypos, yytext); Tokens.FN(yypos, yypos+size yytext));
<INITIAL>"for"             => (lexLog(yypos, yytext); Tokens.FOR(yypos, yypos+size yytext));
<INITIAL>"if"              => (lexLog(yypos, yytext); Tokens.IF(yypos, yypos+size yytext));
<INITIAL>"impl"            => (lexLog(yypos, yytext); Tokens.IMPL(yypos, yypos+size yytext));
<INITIAL>"in"              => (lexLog(yypos, yytext); Tokens.IN(yypos, yypos+size yytext));
<INITIAL>"let"             => (lexLog(yypos, yytext); Tokens.LET(yypos, yypos+size yytext));
<INITIAL>"loop"            => (lexLog(yypos, yytext); Tokens.LOOP(yypos, yypos+size yytext));
<INITIAL>"match"           => (lexLog(yypos, yytext); Tokens.MATCH(yypos, yypos+size yytext));
<INITIAL>"mod"             => (lexLog(yypos, yytext); Tokens.MOD(yypos, yypos+size yytext));
<INITIAL>"move"            => (lexLog(yypos, yytext); Tokens.MOVE(yypos, yypos+size yytext));
<INITIAL>"mut"             => (lexLog(yypos, yytext); Tokens.MUT(yypos, yypos+size yytext));
<INITIAL>"pub"             => (lexLog(yypos, yytext); Tokens.PUB(yypos, yypos+size yytext));
<INITIAL>"ref"             => (lexLog(yypos, yytext); Tokens.REF(yypos, yypos+size yytext));
<INITIAL>"return"          => (lexLog(yypos, yytext); Tokens.RETURN(yypos, yypos+size yytext));
<INITIAL>"self"            => (lexLog(yypos, yytext); Tokens.SELFVALUE(yypos, yypos+size yytext));
<INITIAL>"Self"            => (lexLog(yypos, yytext); Tokens.SELFTYPE(yypos, yypos+size yytext));
<INITIAL>"static"          => (lexLog(yypos, yytext); Tokens.STATIC(yypos, yypos+size yytext));
<INITIAL>"struct"          => (lexLog(yypos, yytext); Tokens.STRUCT(yypos, yypos+size yytext));
<INITIAL>"super"           => (lexLog(yypos, yytext); Tokens.SUPER(yypos, yypos+size yytext));
<INITIAL>"trait"           => (lexLog(yypos, yytext); Tokens.TRAIT(yypos, yypos+size yytext));
<INITIAL>"true"            => (lexLog(yypos, yytext); Tokens.TRUE(yypos, yypos+size yytext));
<INITIAL>"type"            => (lexLog(yypos, yytext); Tokens.TYPE(yypos, yypos+size yytext));
<INITIAL>"unsafe"          => (lexLog(yypos, yytext); Tokens.UNSAFE(yypos, yypos+size yytext));
<INITIAL>"use"             => (lexLog(yypos, yytext); Tokens.USE(yypos, yypos+size yytext));
<INITIAL>"where"           => (lexLog(yypos, yytext); Tokens.WHERE(yypos, yypos+size yytext));
<INITIAL>"while"           => (lexLog(yypos, yytext); Tokens.WHILE(yypos, yypos+size yytext));
<INITIAL>"dyn"             => (lexLog(yypos, yytext); Tokens.DYN(yypos, yypos+size yytext));
<INITIAL>"abstract"        => (lexLog(yypos, yytext); Tokens.ABSTRACT(yypos, yypos+size yytext));
<INITIAL>"become"          => (lexLog(yypos, yytext); Tokens.BECOME(yypos, yypos+size yytext));
<INITIAL>"box"             => (lexLog(yypos, yytext); Tokens.BOX(yypos, yypos+size yytext));
<INITIAL>"do"              => (lexLog(yypos, yytext); Tokens.DO(yypos, yypos+size yytext));
<INITIAL>"final"           => (lexLog(yypos, yytext); Tokens.FINAL(yypos, yypos+size yytext));
<INITIAL>"macro"           => (lexLog(yypos, yytext); Tokens.MACRO(yypos, yypos+size yytext));
<INITIAL>"override"        => (lexLog(yypos, yytext); Tokens.OVERRIDE(yypos, yypos+size yytext));
<INITIAL>"priv"            => (lexLog(yypos, yytext); Tokens.PRIV(yypos, yypos+size yytext));
<INITIAL>"typeof"          => (lexLog(yypos, yytext); Tokens.TYPEOF(yypos, yypos+size yytext));
<INITIAL>"unsized"         => (lexLog(yypos, yytext); Tokens.UNSIZED(yypos, yypos+size yytext));
<INITIAL>"virtual"         => (lexLog(yypos, yytext); Tokens.VIRTUAL(yypos, yypos+size yytext));
<INITIAL>"yield"           => (lexLog(yypos, yytext); Tokens.YIELD(yypos, yypos+size yytext));
<INITIAL>"async"           => (lexLog(yypos, yytext); Tokens.ASYNC(yypos, yypos+size yytext));
<INITIAL>"await"           => (lexLog(yypos, yytext); Tokens.AWAIT(yypos, yypos+size yytext));
<INITIAL>"try"             => (lexLog(yypos, yytext); Tokens.TRY(yypos, yypos+size yytext));
<INITIAL>"union"           => (lexLog(yypos, yytext); Tokens.UNION(yypos, yypos+size yytext));

<INITIAL>{ident}           => (lexLog(yypos, "Tokens.IDENT "^yytext); Tokens.IDENT(yytext, yypos, yypos+size yytext));

<INITIAL>"'"                        => (YYBEGIN LIFE_OR_CHAR; lexLog(yypos, "<Char>"); continue());
<INITIAL>"static"                   => (YYBEGIN INITIAL; lexLog(yypos, yytext); Tokens.STATICLIFETIME(yypos, yypos+size yytext));
<LIFE_OR_CHAR>{ident}               => (YYBEGIN INITIAL; lexLog(yypos, yytext);
                                        (* lifetime_token or loop_label *)
                                        Tokens.LIFETIME_OR_LABEL(yytext, yypos, yypos-1+size yytext); continue());
<LIFE_OR_CHAR>{quote_escape}"'"     => (YYBEGIN INITIAL; lexLog(yypos, yytext); 
                                        Tokens.CHAR_LIT(escape(strip(yytext, #"'"), yypos), yypos, yypos-1+size yytext); continue());
<LIFE_OR_CHAR>{ascii_escape}"'"     => (YYBEGIN INITIAL; lexLog(yypos, yytext); 
                                        Tokens.CHAR_LIT(escape(strip(yytext, #"'"), yypos), yypos, yypos-1+size yytext); continue());
<LIFE_OR_CHAR>{unicode_escape}"'"   => (YYBEGIN INITIAL; lexLog(yypos, yytext); 
                                        Tokens.CHAR_LIT(escape(strip(yytext, #"'"), yypos), yypos, yypos-1+size yytext); continue());
<LIFE_OR_CHAR>."'"                  => (YYBEGIN INITIAL; lexLog(yypos, yytext); 
                                        Tokens.CHAR_LIT(Char.ord(toChar(strip(yytext, #"'"))), yypos, yypos-1+size yytext); continue());
<LIFE_OR_CHAR>[\128-\255]{2,4}"'"   => (YYBEGIN INITIAL; lexLog(yypos, yytext);
                                        Tokens.CHAR_LIT(decodeChar(strip(yytext, #"'"), UTF8), yypos, yypos-1+size yytext); continue());

<INITIAL>"\""              => (YYBEGIN STR; strList:=nil; strpos:=yypos; lexLog(yypos, "<String>"); continue());
<STR>"\""                  => (YYBEGIN INITIAL; lexLog(!strpos, "Tokens.STR_LIT "^strMake()); Tokens.STR_LIT(strMake(), !strpos, yypos));           
<STR>{quote_escape}        => (strAppend(Char.chr(escape(yytext, yypos))); continue());
<STR>{ascii_escape}        => (strAppend(Char.chr(escape(yytext, yypos))); continue());
<STR>{unicode_escape}      => (
                                (* here is a bug. This will raise a exception 
                                when unicode point is bigger than 255 *)
                                strAppend(Char.chr(escape(yytext, yypos))); 
                                continue()
                            );
<STR>{str_continue}        => (lexLog(yypos, "String \\n"); incLine(yypos);  continue());
<STR>\n                    => (strAppend(toChar(yytext)); incLine(yypos);  continue());
<STR>.                     => (strAppend(toChar(yytext)); continue());

<INITIAL>"r\""             => (YYBEGIN R_STR; strList:=nil; strpos:=yypos; lexLog(yypos, "<Raw string>"); continue());
<R_STR>"\""                => (YYBEGIN INITIAL; lexLog(!strpos,strMake()); Tokens.RAW_STR_LIT(strMake(), !strpos, yypos));
<R_STR>"\n"                => (incLine(yypos);  strAppend(toChar yytext); continue());
<R_STR>.                   => (strAppend(toChar yytext); continue());

<INITIAL>"r#"              => (
                                YYBEGIN R_STR_BEGIN;
                                strList:=nil; 
                                strpos:=yypos;
                                lsharp := 1; 
                                rsharp := 0; 
                                lexLog(yypos, "<Raw string(#)>"); 
                                continue()
                            );
<R_STR_BEGIN>"#"           => (lsharp := !lsharp+1; continue());
<R_STR_BEGIN>"\""          => (YYBEGIN R_STR_BODY; continue());
<R_STR_BEGIN>"\n"          => (incLine(yypos);  
                                ErrorMsg.error yypos ("illegal character[R_STR_BEGIN] " ^ yytext);
                                continue());
<R_STR_BEGIN>.           => (ErrorMsg.error yypos ("illegal character[R_STR_BEGIN] " ^ yytext); continue());
<R_STR_BODY>"\"#"        => (
                                lexLog(yypos, "<Raw string(#) end>");
                                app strAppend [#"\"", #"#"];
                                rsharp := 1;
                                if !rsharp = !lsharp then
                                    (YYBEGIN INITIAL;
                                    strList := strPop(!strList, !rsharp+1);
                                    lexLog(!strpos,strMake());
                                    Tokens.RAW_STR_LIT(strMake(), !strpos, yypos))
                                else
                                    (YYBEGIN R_STR_END;
                                    continue())
                            );
<R_STR_BODY>"\n"         => (incLine(yypos);  strAppend(toChar yytext); continue());
<R_STR_BODY>.            => (strAppend(toChar yytext); continue());
<R_STR_END>"#"           => (
                                strAppend(toChar yytext);
                                rsharp := !rsharp+1;
                                if !lsharp = !rsharp then
                                    (YYBEGIN INITIAL;
                                    strList := strPop(!strList, !rsharp+1);
                                    lexLog(!strpos,strMake());
                                    Tokens.RAW_STR_LIT(strMake(), !strpos, yypos))
                                else
                                    (continue())
                            );
<R_STR_END>[^#]          => (
                                strAppend(toChar yytext);
                                YYBEGIN R_STR_BODY;
                                rsharp := 0;
                                continue()
                            );

<INITIAL>"b'"              => (YYBEGIN BYTE; lexLog(yypos, "<Byte char>"); continue());
<BYTE>"\\\''"              => (
                                YYBEGIN INITIAL; 
                                lexLog(yypos, yytext); 
                                Tokens.BYTE_LIT(escape(strip(yytext, #"'"), yypos), yypos, yypos+size yytext)
                            );
<BYTE>{byte_escape}"'"     => (
                                YYBEGIN INITIAL; 
                                lexLog(yypos, yytext); 
                                Tokens.BYTE_LIT(escape(strip(yytext, #"'"), yypos), yypos, yypos+size yytext)
                            );
<BYTE>{ascii_char}"'"      => (
                                YYBEGIN INITIAL; 
                                lexLog(yypos, yytext);
                                Tokens.BYTE_LIT(Char.ord(toChar(strip(yytext, #"'"))), yypos, yypos+size yytext)
                            );
<BYTE>"\n'"                 => (YYBEGIN INITIAL; incLine(yypos);  continue());
<BYTE>."'"                  => (YYBEGIN INITIAL; ErrorMsg.error yypos ("illegal character[BYTE] " ^ yytext); continue());

<INITIAL>"b\""                  => (YYBEGIN BYTE_STR; strList:=nil; strpos:=yypos; lexLog(yypos, "<Byte string>"); continue());
<BYTE_STR>"\""                  => (YYBEGIN INITIAL; lexLog(!strpos,strMake()); Tokens.BYTE_STR_LIT(strMake(), !strpos, yypos));           
<BYTE_STR>{ascii_str}           => (strAppend(toChar(yytext)); continue());
<BYTE_STR>"\\\""                => (strAppend(Char.chr(escape(yytext, yypos))); continue());
<BYTE_STR>{byte_escape}         => (strAppend(Char.chr(escape(yytext, yypos))); continue());
<BYTE_STR>{str_continue}        => (lexLog(yypos, "String \\n"); incLine(yypos);  continue());
<BYTE_STR>"\n"                  => (strAppend(toChar(yytext)); incLine(yypos);  continue());
<BYTE_STR>.                     => (strAppend(toChar(yytext)); continue());

<INITIAL>"br\""           => (YYBEGIN BR_STR; strList:=nil; strpos:=yypos; lexLog(yypos, "<Raw byte string>"); continue());
<BR_STR>"\""              => (YYBEGIN INITIAL; lexLog(!strpos,strMake()); Tokens.RAW_BYTE_STR_LIT(strMake(), !strpos, yypos));
<BR_STR>"\n"              => (incLine(yypos);  strAppend(toChar yytext); continue());
<BR_STR>{ascii}           => (strAppend(toChar yytext); continue());
<BR_STR>.                 => (ErrorMsg.error yypos ("illegal character[BR_STR] " ^ yytext); continue());

<INITIAL>"br#"            => (YYBEGIN BR_STR_BEGIN; lsharp := 1; rsharp := 0; lexLog(yypos, "<Raw byte string(#)>"); continue());
<BR_STR_BEGIN>"#"         => (lsharp := !lsharp+1; continue());
<BR_STR_BEGIN>"\""        => (YYBEGIN BR_STR_BODY; continue());
<BR_STR_BEGIN>"\n"        => (incLine(yypos);  
                                     ErrorMsg.error yypos ("illegal character[BR_STR_BEGIN] " ^ yytext);
                                    continue());
<BR_STR_BEGIN>.           => (ErrorMsg.error yypos ("illegal character[BR_STR_BEGIN] " ^ yytext); continue());
<BR_STR_BODY>"\"#"        => (
                                app strAppend [#"\"", #"#"];
                                rsharp := 1;
                                if !rsharp = !lsharp then
                                    (YYBEGIN INITIAL;
                                    strList := strPop(!strList, !rsharp+1);
                                    lexLog(!strpos,strMake());
                                    Tokens.RAW_BYTE_STR_LIT(strMake(), !strpos, yypos))
                                else
                                    (YYBEGIN BR_STR_END;
                                    continue())
                            );
<BR_STR_BODY>"\n"         => (incLine(yypos);  strAppend(toChar yytext); continue());
<BR_STR_BODY>{ascii}      => (strAppend(toChar yytext); continue());
<BR_STR_BODY>.            => (ErrorMsg.error yypos ("illegal character[BR_STR_BODY] " ^ yytext); continue());
<BR_STR_END>"#"           => (
                                    strAppend(toChar yytext);
                                    rsharp := !rsharp+1;
                                    if !lsharp = !rsharp then
                                        (YYBEGIN INITIAL;
                                        strList := strPop(!strList, !rsharp+1);
                                        lexLog(!strpos,strMake());
                                        Tokens.RAW_STR_LIT(strMake(), !strpos, yypos))
                                    else
                                        (continue())
                            );
<BR_STR_END>[^#]          => (
                                (if (toChar yytext) < #"\128" then
                                    strAppend(toChar yytext)
                                else
                                    (ErrorMsg.error yypos ("illegal character[BR_STR_END] " ^ yytext))
                                );
                                (
                                    if (toChar yytext) = #"\n" then 
                                        (lin := !lin + 1; col := yypos :: !col)
                                    else
                                        ()
                                );
                                YYBEGIN BR_STR_BODY;
                                rsharp := 0;
                                continue()
                            );

<INITIAL>{integer_lit}     => (
                                lexLog(yypos, "<Integer> "^yytext); 
                                Tokens.INTEGER_LIT(toInteger(yytext), yypos, yypos+size yytext)
                            );
<INITIAL>{float_lit}       => (
                                lexLog(yypos, "<Float> "^yytext); 
                                Tokens.FLOAT_LIT(toFloat(yytext), yypos, yypos+size yytext)
                            );

<INITIAL>"<<="             => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.SHLEQ(yypos, yypos+size yytext));
<INITIAL>">>="             => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.SHREQ(yypos, yypos+size yytext));
<INITIAL>"..."             => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.DOTDOTDOT(yypos, yypos+size yytext));
<INITIAL>"..="             => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.DOTDOTEQ(yypos, yypos+size yytext));

<INITIAL>"&&"              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.ANDAND(yypos, yypos+size yytext));
<INITIAL>"||"              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.OROR(yypos, yypos+size yytext));
<INITIAL>"<<"              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.SHL(yypos, yypos+size yytext));
<INITIAL>">>"              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.SHR(yypos, yypos+size yytext));
<INITIAL>"+="              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.PLUSEQ(yypos, yypos+size yytext));
<INITIAL>"-="              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.MINUSEQ(yypos, yypos+size yytext));
<INITIAL>"*="              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.STAREQ(yypos, yypos+size yytext));
<INITIAL>"/="              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.SLASHEQ(yypos, yypos+size yytext));
<INITIAL>"%="              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.PERCENTEQ(yypos, yypos+size yytext));
<INITIAL>"^="              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.CARETEQ(yypos, yypos+size yytext));
<INITIAL>"&="              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.ANDEQ(yypos, yypos+size yytext));
<INITIAL>"|="              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.OREQ(yypos, yypos+size yytext));
<INITIAL>"=="              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.EQEQ(yypos, yypos+size yytext));
<INITIAL>"!="              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.NE(yypos, yypos+size yytext));
<INITIAL>">="              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.GE(yypos, yypos+size yytext));
<INITIAL>"<="              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.LE(yypos, yypos+size yytext));
<INITIAL>"->"              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.RARROW(yypos, yypos+size yytext));
<INITIAL>"=>"              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.FATARROW(yypos, yypos+size yytext));
<INITIAL>".."              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.DOTDOT(yypos, yypos+size yytext));
<INITIAL>"::"              => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.PATHSEP(yypos, yypos+size yytext));
<INITIAL>"."               => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.DOT(yypos, yypos+size yytext));
<INITIAL>"+"               => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.PLUS(yypos, yypos+size yytext));
<INITIAL>"-"               => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.MINUS(yypos, yypos+size yytext)); 
<INITIAL>"*"               => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.STAR(yypos, yypos+size yytext));
<INITIAL>"/"               => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.SLASH(yypos, yypos+size yytext));
<INITIAL>"%"               => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.PERCENT(yypos, yypos+size yytext));
<INITIAL>"#"               => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.POUND(yypos, yypos+ size yytext));
<INITIAL>"^"               => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.CARET(yypos, yypos+size yytext));
<INITIAL>"!"               => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.NOT(yypos, yypos+size yytext));
<INITIAL>"&"               => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.AND(yypos, yypos+size yytext));
<INITIAL>"|"               => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.OR(yypos, yypos+size yytext));
<INITIAL>"="               => (lexLog(yypos, "Tokens.EQ "^yytext); Tokens.EQ(yypos, yypos+size yytext));
<INITIAL>">"               => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.GT(yypos, yypos+size yytext));
<INITIAL>"<"               => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.LT(yypos, yypos+size yytext));
<INITIAL>"@"               => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.AT(yypos, yypos+size yytext));
<INITIAL>","               => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.COMMA(yypos, yypos+size yytext));
<INITIAL>";"               => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.SEMI(yypos, yypos+size yytext));
<INITIAL>":"               => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.COLON(yypos, yypos+size yytext));
<INITIAL>"$"               => (lexLog(yypos, "<Punctuation> "^yytext); Tokens.DOLLAR(yypos, yypos+size yytext));
<INITIAL>"?"               => (lexLog(yypos, "Tokens.QUESTION "^yytext); Tokens.QUESTION(yypos, yypos+size yytext));
<INITIAL>"{"               => (lexLog(yypos, "Tokens.LBRACE "^yytext); Tokens.LBRACE(yypos, yypos+size yytext));
<INITIAL>"}"               => (lexLog(yypos, "Tokens.RBRACE"^yytext); Tokens.RBRACE(yypos, yypos+size yytext));
<INITIAL>"["               => (lexLog(yypos, "Tokens.LBRACKET "^yytext); Tokens.LBRACKET(yypos, yypos+size yytext));
<INITIAL>"]"               => (lexLog(yypos, "Tokens.RBRACKET "^yytext); Tokens.RBRACKET(yypos, yypos+size yytext));
<INITIAL>"("               => (lexLog(yypos, "Tokens.LPARENT "^yytext); Tokens.LPARENT(yypos, yypos+size yytext));
<INITIAL>")"               => (lexLog(yypos, "Tokens.RPARENT "^yytext); Tokens.RPARENT(yypos, yypos+size yytext));

<INITIAL>.                 => (ErrorMsg.error yypos ("illegal character[INITIAL] " ^ yytext); continue());