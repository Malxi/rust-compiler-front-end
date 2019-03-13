## Run
In sml interactive system
+ compile: CM.make "rust.cm"
+ run parser: Parser.parse "test.rs"

## Record
+ Parse SHEBANG_LINE for solve SHEBANG conflict
+ Fix grammar bug (lex bug ignores some token)
### Goal
+ To modify lex comments datatype
+ Literal parse in lex or yacc?
+ Add a suffix option for literals
+ Solve conflicts of function qualifier

## Tools
+ convert for encoding and decoding
+ printAST for printing abstract syntax tree

## lex
### Finished
+ Keywords
+ Identifier (without raw)
+ Char
+ String (unicode escape is not supported)
+ Integer (needs a convert function)
+ Float (needs a convert function)
+ Punctuation
+ Delimiter
+ Comments

## Syntax
### Finished
+ Crate
+ Attribute
+ Module
+ Extern crate
### Processing
+ Literal expression
+ Items
+ Use declaration
+ Function