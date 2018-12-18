(* glue.sml Create a lexer and a parser *)
structure RustLrVals = RustLrValsFun( structure Token = LrParser.Token)
structure RustLex = RustLexFun(structure Tokens = RustLrVals.Tokens)
structure RustParser = JoinWithArg(
    structure ParserData = RustLrVals.ParserData
    structure Lex=RustLex
    structure LrParser=LrParser
);