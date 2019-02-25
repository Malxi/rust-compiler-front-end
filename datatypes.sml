signature DATATYPES =
sig
    datatype A = A of Crate
    and Crate = Crate of (Shebang * InnerAttribute list * Item list)
    and Shebang = Shebang of string option
    and InnerAttribute = InnerAttribute of MetaItem
    and OuterAttribute = OuterAttribute of MetaItem
    and MetaItem = AttrName of SimplePath | AttrKVPair of SimplePath * LiteralExpression | AttrSubs of SimplePath * MetaSeq option
    and MetaSeq = MetaSeq of MetaItemInner list
    and LiteralExpression = LiteralExpression of string
    and MetaItemInner = MetaItem of MetaItem | MetaLit of LiteralExpression
    and SimplePath = SimplePath of string list
    and Item = Item of (OuterAttribute list * VisItem option * MarcoItem option)
    and VisItem = VisItem
    and Visibility = CrateVis | SelfVis | SuperVis | InVis of SimplePath
    and Module = Module of (string * InnerAttribute option * Item option)
    and ExternCrate = ExternCrate of (string * string option)
    and UseDeclaration = UseDeclaration of UseTree
    and UseTree = UseSingle of SimplePath list | UseMultiple of (SimplePath list * UseTree list) | UseAlias of (SimplePath * string)
    and Functions = Functions
    and TypeAlias = TypeAlias of {ident:string, generic:string, whereClause:string, typ: string}
    and Struct = Struct
    and Enumeration = Enumeration
    and Union = Union
    and ConstantItem = ConstantItem
    and StaticItem = StaticItem
    and Trait = Trait
    and Implementation = Implementation
    and ExternBlock = ExternBlock
    and MarcoItem = MarcoItem
    and Numeric = U8 of Word8.word | U16 of Word.word | U32 of Word32.word 
                    | U64 of Word64.word | U128 of LargeInt.int
                    | I8 of Word8.word | I16 of Word.word | I32 of Word32.word 
                    | I64 of Word64.word | I128 of LargeInt.int
                    | usize of Word.word | isize of Word.word
end

structure DataTypes:DATATYPES =
struct
    datatype A = A of Crate
    and Crate = Crate of (Shebang * InnerAttribute list * Item list)
    and Shebang = Shebang of string option
    and InnerAttribute = InnerAttribute of MetaItem
    and OuterAttribute = OuterAttribute of MetaItem
    and MetaItem = AttrName of SimplePath | AttrKVPair of SimplePath * LiteralExpression | AttrSubs of SimplePath * MetaSeq option
    and MetaSeq = MetaSeq of MetaItemInner list
    and LiteralExpression = LiteralExpression of string
    and MetaItemInner = MetaItem of MetaItem | MetaLit of LiteralExpression
    and SimplePath = SimplePath of string list
    and Item = Item of (OuterAttribute list * VisItem option * MarcoItem option)
    and VisItem = VisItem
    and Visibility = CrateVis | SelfVis | SuperVis | InVis of SimplePath
    and Module = Module of (string * InnerAttribute option * Item option)
    and ExternCrate = ExternCrate of (string * string option)
    and UseDeclaration = UseDeclaration of UseTree
    and UseTree = UseSingle of SimplePath list | UseMultiple of (SimplePath list * UseTree list) | UseAlias of (SimplePath * string)
    and Functions = Functions
    and TypeAlias = TypeAlias of {ident: string, generic:string, whereClause: string, typ: string}
    and Struct = Struct
    and Enumeration = Enumeration
    and Union = Union
    and ConstantItem = ConstantItem
    and StaticItem = StaticItem
    and Trait = Trait
    and Implementation = Implementation
    and ExternBlock = ExternBlock
    and MarcoItem = MarcoItem
    and Numeric = U8 of Word8.word | U16 of Word.word | U32 of Word32.word 
                    | U64 of Word64.word | U128 of LargeInt.int
                    | I8 of Word8.word | I16 of Word.word | I32 of Word32.word 
                    | I64 of Word64.word | I128 of LargeInt.int
                    | usize of Word.word | isize of Word.word
end