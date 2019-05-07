## Run
In sml interactive system
+ compile: CM.make "rust.cm"
+ run parser: Parser.parse "test.rs"

## Record
+ Parse SHEBANG_LINE for solve SHEBANG conflict
+ Fix grammar bug (lex bug ignores some token)
+ Fix many productions (replace maybe_productions with terminals to solve shift/reduce conflicts)
+ Literal tokens take original text with suffix
+ SimplePath in Macro conflicts with PathInExpression in Item, so replacing SimplePath with PathInExpression, this idea comes from rust official parser.
+ Productions of Visibility and Type have the same part in TupleFiled which cause a reduce/reduce conflicts. So set the precedence of Visibility production lower than Type production.
+ Productions of Pattern and Type have the same part in trait_func_param, so remove Type sub productions for avoiding reduce/reduce conflicts.
+ Trait Object Type One Bound production must have keyword dyn for avoiding conflicts.
+ In where clauses, "for" should have greater precedence when used as a higher ranked constraint than when used as the beginning of a
for_in_type (which is a ty). This idea comes from rust official parser.
+ In inherent implement, to remove reduce/reduce conflicts in generics and types, requiring users to provide parents delimiter around types.This idea comes from rust official parser.
+ TypePath contains PathInExpression, rewrite MarcoInvocation for avoiding conflicts, maybe need to check TypePathFn.
+ Use "vis_item" replace "item" for avoiding conflicts in Statement production.
+ Ambiguous tokens (generics: GT-SHR;generic args: GT-SHR, GT-GE, GT-SHRQG) are parsed in yacc. 
### Goal
+ To modify lex comments datatype
+ Add a suffix option for literals
+ Add outer attributes for operator expression will cause shift/reduce conflicts
+ Extra work for ambiguous tokens
(Float Literal: 1.; Range: 1..2)
+ Ambiguous tokens: LT-SHL

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
+ Type
### Processing
+ Literal expression
+ Items
+ MacroInvocation
+ MacroInvocationSemi
