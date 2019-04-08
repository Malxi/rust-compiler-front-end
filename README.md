## Run
In sml interactive system
+ compile: CM.make "rust.cm"
+ run parser: Parser.parse "test.rs"

## Record
+ Parse SHEBANG_LINE for solve SHEBANG conflict
+ Fix grammar bug (lex bug ignores some token)
+ Fix many productions (replace maybe_productions with terminals to solve shift/reduce conflicts)
+ Literal tokens take original text with suffix
### Goal
+ To modify lex comments datatype
+ Add a suffix option for literals
+ Solve conflicts of function qualifier
+ Complete simple path
+ Extra work for ambiguous tokens
(generics: GT-SHR;
generic args: GT-SHR, GT-GE, GT-SHRQG, LT-SHL)
+ Add outer attributes for operator expression will cause shift/reduce conflicts

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
+ Use declaration
+ Function
+ Type alias
+ Structs
+ Enumerations
+ Unions
+ Constant Item
+ Static Item
+ Trait
+ Implementations
+ Extern blocks
### Processing
+ Literal expression
+ Items
+ MacroInvocation
+ MacroInvocationSemi
