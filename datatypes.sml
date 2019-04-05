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
        | TypeAlias of (Identifer * Generics option * WhereClause option * Type)
        | Struct of StructType
        | Enumeration of (Identifer * Generics option * WhereClause option * EnumItem list)
        | Union of (Identifer * Generics option * WhereClause option * StructField list)
        | ConstantItem of (Identifer * Type * Expression)
        | StaticItem of (Mutability * Identifer * Type * Expression)
        | Trait of {unsafe: Unsafe option, name:Identifer, generic:Generics option, 
                    tyb:TypeParamBounds option, wh:WhereClause option, 
                    traitItems: TraitItem list}
        | InherentImpl of {generic:Generics option, ty:Type, wh:WhereClause option, 
                        innerAttrs: InnerAttribute list, implItems:InherentImplItem list
                        }
        | TraitImpl of {
            unsafe:Unsafe option, generic:Generics option, neg:bool, typath:TypePath, ty:Type,
            wh:WhereClause option, innerAttrs:InnerAttribute list, implItems: TraitImplItem list
            }
        | ExternBlock
    and StructType = StructStruct of (Identifer * Generics option * WhereClause option * StructField list)
                    | UnitStruct of (Identifer * Generics option * WhereClause option)
                    | TupleStruct of (Identifer * Generics option * TupleField list * WhereClause option)
    and StructField = StructField of (OuterAttribute list * Visibility * Identifer * Type)
    and TupleField = TupleField of (OuterAttribute list * Visibility * Type)
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
    and FunctionParam = FunctionParam of (Pattern * Type)
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

    and EnumItem = EnumItem of (OuterAttribute list * Identifer * EnumItemType option)
    and EnumItemType = EnumItemTuple of TupleField list 
                    | EnumItemStruct of StructField list 
                    | EnumItemDiscriminant of Expression
    and Expression = Expression

    and Mutability = Mut | NonMut

    and TraitItem = TraitItem of (OuterAttribute list * TraitItemType)
    and TraitItemType = TraitFunc of (TraitFuncDecl * BlockExpression option) 
                        | TraitMethod of (TraitMethodDecl * BlockExpression option)
                        | TraitConst of Identifer * Type * Expression option
                        | TraitType of Identifer * TypeParamBounds option
                        | TraitMIS of MacroInvocationSemi
    and TraitFuncDecl = TraitFuncDecl of {qualifier:FunctionQualifier list, name:Identifer, generic:Generics option, 
                        params:TraitFunctionParam list, ret:Type option, 
                        wh:WhereClause option}
    and TraitFunctionParam = TraitFunctionParam of (Pattern option * Type)
    and TraitMethodDecl = TraitMethodDecl of {qualifier:FunctionQualifier list, name:Identifer, generic:Generics option, 
                        selfParam: SelfParam, params:TraitFunctionParam list, ret:Type option, 
                        wh:WhereClause option}
    and SelfParam = SelfParamLT of (Lifetime option * Mutability) | SelfParamTY of (Mutability * Type option)

    and MacroInvocationSemi = MacroInvocationSemi

    and Unsafe = Unsafe

    and InherentImplItem = InherentImplItemMarco of (OuterAttribute list * MacroInvocationSemi)
                            | InherentImplItemType of (OuterAttribute list * Visibility option * ItemType)
                            | InherentImplItemMethod of (OuterAttribute list * Visibility option * Method)
    and TraitImplItem = TraitImplItemMarco of (OuterAttribute list * MacroInvocationSemi)
                        | TraitImplItemType of (OuterAttribute list * Visibility option * ItemType)
                        | TraitImplItemMethod of (OuterAttribute list * Visibility option * Method)
    and Method = Method

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
        | TypeAlias of (Identifer * Generics option * WhereClause option * Type)
        | Struct of StructType
        | Enumeration of (Identifer * Generics option * WhereClause option * EnumItem list)
        | Union of (Identifer * Generics option * WhereClause option * StructField list)
        | ConstantItem of (Identifer * Type * Expression)
        | StaticItem of (Mutability * Identifer * Type * Expression)
        | Trait of {unsafe: Unsafe option, name:Identifer, generic:Generics option, 
                    tyb:TypeParamBounds option, wh:WhereClause option, 
                    traitItems: TraitItem list}
        | InherentImpl of {generic:Generics option, ty:Type, wh:WhereClause option, 
                        innerAttrs: InnerAttribute list, implItems:InherentImplItem list
                        }
        | TraitImpl of {
            unsafe:Unsafe option, generic:Generics option, neg:bool, typath:TypePath, ty:Type,
            wh:WhereClause option, innerAttrs:InnerAttribute list, implItems: TraitImplItem list
            }
        | ExternBlock
    and StructType = StructStruct of (Identifer * Generics option * WhereClause option * StructField list)
                    | UnitStruct of (Identifer * Generics option * WhereClause option)
                    | TupleStruct of (Identifer * Generics option * TupleField list * WhereClause option)
    and StructField = StructField of (OuterAttribute list * Visibility * Identifer * Type)
    and TupleField = TupleField of (OuterAttribute list * Visibility * Type)
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
    and FunctionParam = FunctionParam of (Pattern * Type)
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

    and EnumItem = EnumItem of (OuterAttribute list * Identifer * EnumItemType option)
    and EnumItemType = EnumItemTuple of TupleField list 
                    | EnumItemStruct of StructField list 
                    | EnumItemDiscriminant of Expression
    and Expression = Expression

    and Mutability = Mut | NonMut

    and TraitItem = TraitItem of (OuterAttribute list * TraitItemType)
    and TraitItemType = TraitFunc of (TraitFuncDecl * BlockExpression option) 
                        | TraitMethod of (TraitMethodDecl * BlockExpression option)
                        | TraitConst of Identifer * Type * Expression option
                        | TraitType of Identifer * TypeParamBounds option
                        | TraitMIS of MacroInvocationSemi
    and TraitFuncDecl = TraitFuncDecl of {qualifier:FunctionQualifier list, name:Identifer, generic:Generics option, 
                        params:TraitFunctionParam list, ret:Type option, 
                        wh:WhereClause option}
    and TraitFunctionParam = TraitFunctionParam of (Pattern option * Type)
    and TraitMethodDecl = TraitMethodDecl of {qualifier:FunctionQualifier list, name:Identifer, generic:Generics option, 
                        selfParam: SelfParam, params:TraitFunctionParam list, ret:Type option, 
                        wh:WhereClause option}
    and SelfParam = SelfParamLT of (Lifetime option * Mutability) | SelfParamTY of (Mutability * Type option)

    and MacroInvocationSemi = MacroInvocationSemi

    and Unsafe = Unsafe

    and InherentImplItem = InherentImplItemMarco of (OuterAttribute list * MacroInvocationSemi)
                            | InherentImplItemType of (OuterAttribute list * Visibility option * ItemType)
                            | InherentImplItemMethod of (OuterAttribute list * Visibility option * Method)
    and TraitImplItem = TraitImplItemMarco of (OuterAttribute list * MacroInvocationSemi)
                        | TraitImplItemType of (OuterAttribute list * Visibility option * ItemType)
                        | TraitImplItemMethod of (OuterAttribute list * Visibility option * Method)
    and Method = Method

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