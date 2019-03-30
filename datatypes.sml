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
    and SimplePath = SimplePath of PathSeg list
    and PathSeg = IDPat of string | SuperPat | SelfPat | CratePat | DCratePat | DefaultPat
    and Item = VisItemType of (OuterAttribute list * VisItem) | MarcoItemType of MarcoItem
    and VisItem = VisItem of (Visibility * ItemType)
    and ItemType = 
        Module of (string * ModuleBody option)
        | ExternCrate of (string * string option)
        | UseDeclaration of UseTree 
        | Function
        | TypeAlias (* of {ident:string, generic:string, whereClause:string, typ: string} *)
        | Struct
        | Enumeration
        | Union
        | ConstantItem
        | StaticItem
        | Trait
        | Implementation
        | ExternBlock
    and ModuleBody = ModuleBody of InnerAttribute list * Item list
    and Visibility = DefaultVis | PubVis | CrateVis | SelfVis | SuperVis | InVis of SimplePath
    and UseTree = UseWildCard of SimplePath option | UseMultiple of (SimplePath option * UseTree list) | UseAlias of (SimplePath * string option)
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
    and SimplePath = SimplePath of PathSeg list
    and PathSeg = IDPat of string | SuperPat | SelfPat | CratePat | DCratePat | DefaultPat
    and Item = VisItemType of (OuterAttribute list * VisItem) | MarcoItemType of MarcoItem
    and VisItem = VisItem of (Visibility * ItemType)
    and ItemType = 
        Module of (string * ModuleBody option)
        | ExternCrate of (string * string option)
        | UseDeclaration of UseTree 
        | Function
        | TypeAlias (* of {ident:string, generic:string, whereClause:string, typ: string} *)
        | Struct
        | Enumeration
        | Union
        | ConstantItem
        | StaticItem
        | Trait
        | Implementation
        | ExternBlock
    and ModuleBody = ModuleBody of InnerAttribute list * Item list
    and Visibility = DefaultVis | PubVis | CrateVis | SelfVis | SuperVis | InVis of SimplePath
    and UseTree = UseWildCard of SimplePath option | UseMultiple of (SimplePath option * UseTree list) | UseAlias of (SimplePath * string option)
    and MarcoItem = MarcoItem
    and Numeric = U8 of Word8.word | U16 of Word.word | U32 of Word32.word 
                    | U64 of Word64.word | U128 of LargeInt.int
                    | I8 of Word8.word | I16 of Word.word | I32 of Word32.word 
                    | I64 of Word64.word | I128 of LargeInt.int
                    | usize of Word.word | isize of Word.word
end