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
    and PathSeg = IDPat of Identifer | SuperPat | SelfPat | CratePat | DCratePat | DefaultPat
    and Item = VisItemType of (OuterAttribute list * VisItem) | MarcoItemType of MarcoItem
    and VisItem = VisItem of (Visibility * ItemType)
    and ItemType = 
        Module of (Identifer * ModuleBody option)
        | ExternCrate of (Identifer * Identifer option)
        | UseDeclaration of UseTree 
        | Function of {qualifier:FunctionQualifier list, name:Identifer, generic:Generics option, 
                        params:FunctionParam list, ret:Type option, 
                        wh:WhereClause option, be:BlockExpression}
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
    and UseTree = UseAll of SimplePath option | UseList of (SimplePath option * UseTree list) | UseAlias of (SimplePath * Identifer option)
    and MarcoItem = MarcoItem
    and Identifer = Identifer of string
    and FunctionQualifier = ConstFQ | UnsafeFQ | ExternFQ of Abi option
    and Abi = Abi of string
    and Generics = Generics of GenericParams
    and GenericParams = GenericParams of LifetimeParams * TypeParams
    and TypeParams = TypeParams of TypeParam list
    and TypeParam = TypeParam of (OuterAttribute option * Identifer * TypeParamBounds option * Type option)
    and FunctionParam = FunctionParam
    and WhereClause = WhereClause of WhereClauseItem list
    and WhereClauseItem = LifetimeWhereClauseItem of (Lifetime * LifetimeBounds) 
                        | TypeBoundWhereClauseItem of (ForLifetimes option * Type * TypeParamBounds option)
    and Lifetime = LifetimeOrLabel of string | StaticLifetime
    and TraitBound = TraitBound of (Sized option * ForLifetimes option * TypePath)
    and Sized = Sized
    and LifetimeBounds = LifetimeBounds of Lifetime list
    and ForLifetimes = ForLifetimes of LifetimeParams
    and LifetimeParams = LifetimeParams of LifetimeParam list
    and LifetimeParam = LifetimeParam of (OuterAttribute option * Lifetime * LifetimeBounds option)
    and TypeParamBounds = TypeParamBounds of TypeParamBound list
    and TypeParamBound = LTB of Lifetime | TB of TraitBound
    and TypePath = TypePath
    and Pattern = Pattern
    and Type = Type
    and BlockExpression = BlockExpression
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
    and PathSeg = IDPat of Identifer | SuperPat | SelfPat | CratePat | DCratePat | DefaultPat
    and Item = VisItemType of (OuterAttribute list * VisItem) | MarcoItemType of MarcoItem
    and VisItem = VisItem of (Visibility * ItemType)
    and ItemType = 
        Module of (Identifer * ModuleBody option)
        | ExternCrate of (Identifer * Identifer option)
        | UseDeclaration of UseTree 
        | Function of {qualifier:FunctionQualifier list, name:Identifer, generic:Generics option, 
                        params:FunctionParam list, ret:Type option, 
                        wh:WhereClause option, be:BlockExpression}
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
    and UseTree = UseAll of SimplePath option | UseList of (SimplePath option * UseTree list) | UseAlias of (SimplePath * Identifer option)
    and MarcoItem = MarcoItem
    and Identifer = Identifer of string
    and FunctionQualifier = ConstFQ | UnsafeFQ | ExternFQ of Abi option
    and Abi = Abi of string
    and Generics = Generics of GenericParams
    and GenericParams = GenericParams of LifetimeParams * TypeParams
    and TypeParams = TypeParams of TypeParam list
    and TypeParam = TypeParam of (OuterAttribute option * Identifer * TypeParamBounds option * Type option)
    and FunctionParam = FunctionParam
    and WhereClause = WhereClause of WhereClauseItem list
    and WhereClauseItem = LifetimeWhereClauseItem of (Lifetime * LifetimeBounds) 
                        | TypeBoundWhereClauseItem of (ForLifetimes option * Type * TypeParamBounds option)
    and Lifetime = LifetimeOrLabel of string | StaticLifetime
    and TraitBound = TraitBound of (Sized option * ForLifetimes option * TypePath)
    and Sized = Sized
    and LifetimeBounds = LifetimeBounds of Lifetime list
    and ForLifetimes = ForLifetimes of LifetimeParams
    and LifetimeParams = LifetimeParams of LifetimeParam list
    and LifetimeParam = LifetimeParam of (OuterAttribute option * Lifetime * LifetimeBounds option)
    and TypeParamBounds = TypeParamBounds of TypeParamBound list
    and TypeParamBound = LTB of Lifetime | TB of TraitBound
    and TypePath = TypePath
    and Pattern = Pattern
    and Type = Type
    and BlockExpression = BlockExpression
    and Numeric = U8 of Word8.word | U16 of Word.word | U32 of Word32.word 
                    | U64 of Word64.word | U128 of LargeInt.int
                    | I8 of Word8.word | I16 of Word.word | I32 of Word32.word 
                    | I64 of Word64.word | I128 of LargeInt.int
                    | usize of Word.word | isize of Word.word
end