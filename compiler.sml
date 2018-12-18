structure Parser : sig val parse : string -> unit  end =
struct
    fun parse fileName =
    let 
        val _ = (ErrorMsg.reset(); ErrorMsg.fileName := fileName)
        val file = TextIO.openIn fileName
        fun grab(n:int) = 
            if TextIO.endOfStream file then ""
            else TextIO.inputN(file, n)
        fun parseError(s, p1, p2) = ErrorMsg.error p1 s
        val lexer = LrParser.Stream.streamify (RustLex.makeLexer grab fileName)
        val (absyn, _) = RustParser.parse(30, lexer, parseError, ())
    in
        TextIO.closeIn file;
        absyn
    end
    handle LrParser.ParseError => raise ErrorMsg.Error
end
