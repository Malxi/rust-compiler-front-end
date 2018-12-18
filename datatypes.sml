signature DATATYPES =
sig
    datatype Numeric = U8 of Word8.word | U16 of Word16.word | U32 of Word32.word 
                    | U64 of Word64.word | U128 of LargeInt.int
                    | I8 of Word8.word | I16 of Word16.word | I32 of Word32.word 
                    | I64 of Word64.word | I128 of Word128.word
                    | usize of Word.word | isize of Word.word
end

structure Datatypes:DATATYPES =
struct
    datatype Numeric =  U8 of Word8.word | U16 of Word16.word | U32 of Word32.word 
                    | U64 of Word64.word | U128 of LargeInt.int
                    | I8 of Word8.word | I16 of Word16.word | I32 of Word32.word 
                    | I64 of Word64.word | I128 of Word128.word
                    | usize of Word.word | isize of Word.word
end