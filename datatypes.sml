signature DATATYPES =
sig
    datatype A = A of Crate
    and Crate = Crate of Shebang
    and Shebang = Shebang of string
    and Path = SimplePath of string list   
    and Numeric = U8 of Word8.word | U16 of Word.word | U32 of Word32.word 
                    | U64 of Word64.word | U128 of LargeInt.int
                    | I8 of Word8.word | I16 of Word.word | I32 of Word32.word 
                    | I64 of Word64.word | I128 of LargeInt.int
                    | usize of Word.word | isize of Word.word
end

structure DataTypes:DATATYPES =
struct
    datatype A = A of Crate
    and Crate = Crate of Shebang
    and Shebang = Shebang of string
    and Path = SimplePath of string list   
    and Numeric = U8 of Word8.word | U16 of Word.word | U32 of Word32.word 
                    | U64 of Word64.word | U128 of LargeInt.int
                    | I8 of Word8.word | I16 of Word.word | I32 of Word32.word 
                    | I64 of Word64.word | I128 of LargeInt.int
                    | usize of Word.word | isize of Word.word
end