signature DATATYPES =
sig
    datatype A = A of Crate
    and Crate = Crate of (Shebang * InnerAttribute list * Item list)
    and Shebang = Shebang of string option
    and InnerAttribute = InnerAttribute of MetaItem
    and OuterAttribute = OuterAttribute of MetaItem
    and MetaItem = AttrName of SimplePath | AttrKVPair of SimplePath * LiteralExpression 
                    | AttrSubs of SimplePath * MetaItemInner list
    and LiteralExpression = LiteralExpression of TokenType
    and MetaItemInner = MetaItem of MetaItem | MetaLit of LiteralExpression
    and SimplePath = SimplePath of PathSeg list
    and PathInExpression = PathInExpression of PathSeg list
    and QualifiedPathInExpression = QualifiedPathInExpression of Type * TypePath option * PathSeg list
    and QualifiedPathInType = QualifiedPathInType of Type * TypePath option * PathSeg list
    and TypePath = TypePath of PathSeg list
    and PathSeg = IDPS of Identifer | SuperPS | SelfValuePS | CratePS | DCratePS | DefaultPS | SelfTypePS 
                | GenericPS of GenericArgs | TypePathFnPS of TypePathFn
    and GenericArgs = GenericArgs of Lifetime list * Type list * Binding list
    and Binding = Binding of (Identifer * Type)
    and TypePathFn = TypePathFn of (Type list * Type option)
    and Item = VisItemType of (OuterAttribute list * VisItem) | MacroItemType of MacroItem
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
        | ExternBlock of (Abi option * InnerAttribute list * ExternalItem list)
    and StructType = StructStruct of (Identifer * Generics option * WhereClause option * StructField list)
                    | UnitStruct of (Identifer * Generics option * WhereClause option)
                    | TupleStruct of (Identifer * Generics option * TupleField list * WhereClause option)
    and StructField = StructField of (OuterAttribute list * Visibility * Identifer * Type)
    and TupleField = TupleField of (OuterAttribute list * Visibility * Type)
    and ModuleBody = ModuleBody of InnerAttribute list * Item list
    and Visibility = DefaultVis | PubVis | CrateVis | SelfVis | SuperVis | InVis of SimplePath
    and UseTree = UseAll of SimplePath option | UseList of (SimplePath option * UseTree list) | UseAlias of (SimplePath * Identifer option)
    and MacroItem = MacroInvocationSemi of PathInExpression * TokenTree
                    | MacroInvocation of PathInExpression * TokenTree 
                    | MacroRulesDefinition of PathInExpression * Identifer * MacroRulesDef
    and TokenTree = DTokenTree of Delim * TokenTree list | SToken of TokenType
    and Delim = ParentDelim | BracketDelim | BraceDelim
    and MacroRulesDef = MacroRulesDef of Delim * MacroRule list
    and MacroRule = MacroRule of MacroMatcher * TokenTree
    and MacroMatcher = MacroMatcher of Delim * MacroMatch list
    and MacroMatch = MMTK of TokenType | MMer of MacroMatcher 
                | MMBD of Identifer * Identifer | MMs of MacroMatch list * TokenType option * MacroKleeneOp
    and MacroKleeneOp = KleeneStar | KleenePlus | KleeneQues
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
                        | TraitMIS of MacroItem
    and TraitFuncDecl = TraitFuncDecl of {qualifier:FunctionQualifier list, name:Identifer, generic:Generics option, 
                        params:TraitFunctionParam list, ret:Type option, 
                        wh:WhereClause option}
    and TraitFunctionParam = TraitFunctionParam of (Pattern option * Type)
    and TraitMethodDecl = TraitMethodDecl of {qualifier:FunctionQualifier list, name:Identifer, generic:Generics option, 
                        selfParam: SelfParam, params:TraitFunctionParam list, ret:Type option, 
                        wh:WhereClause option}
    and SelfParam = SelfParamLT of (Lifetime option * Mutability) | SelfParamTY of (Mutability * Type option)

    and Unsafe = Unsafe

    and InherentImplItem = InherentImplItemMacro of (OuterAttribute list * MacroItem)
                            | InherentImplItemType of (OuterAttribute list * Visibility * ItemType)
                            | InherentImplItemMethod of (OuterAttribute list * Visibility * Method)
    and TraitImplItem = TraitImplItemMacro of (OuterAttribute list * MacroItem)
                        | TraitImplItemType of (OuterAttribute list * Visibility * ItemType)
                        | TraitImplItemMethod of (OuterAttribute list * Visibility * Method)
    and Method = Method of {qualifier:FunctionQualifier list, name:Identifer, generic:Generics option, 
                        selfParam: SelfParam, params:FunctionParam list, ret:Type option, 
                        wh:WhereClause option, be:BlockExpression}

    and ExternalItem = ExternalItem of (OuterAttribute list * Visibility * ExternalItemType)
    and ExternalItemType = ExternalStaticItem of (Mutability * Identifer * Type)
                            | ExternalFunctionItem of {name:Identifer, generic:Generics option, 
                            params:ExternFunctionParameter, ret:Type option, wh:WhereClause option}
    and ExternFunctionParameter = ExternFunctionParameter of {params:NamedFunctionParam list, var:bool}
    and NamedFunctionParam = NamedFunctionParam of (Identifer option * Type)

    and Pattern = LiteralPattern of Minus option * TokenType * Pos
                | IdentiferPattern of BindingMode * Identifer * Pattern option
                | WildcardPattern of Pos
                | RangePatternDDE of RangePatternBound * RangePatternBound
                | RangePatternDDD of RangePatternBound * RangePatternBound
                | ReferencePattern of Borrow * Mutability * Pattern * Pos
                | StructPattern of PathInExpression * StructPatternElements
                | TupleStructPattern of PathInExpression * Pattern list
                | TupleStructPatternDD of PathInExpression * Pattern list * Pattern list
                | TupleORGroupPattern of Pattern list * Pos 
                | TupleORGroupPatternDD of Pattern list * Pattern list * Pos
                | SlicePattern of Pattern list * Pos
                | PathPat of PathInExpression 
                | QPathPat of QualifiedPathInExpression
    and Minus = Minus of Pos
    and BindingMode = BindingMode of Ref option * Mutability * Pos
    and Ref = Ref of Pos 
    and RangePatternBound = RPBLit of Minus option * TokenType * Pos
                        | RPBPath of PathInExpression 
                        | RPBQPath of QualifiedPathInExpression
    and Borrow = BOnce of Pos | BTwice of Pos
    and StructPatternElements = StructPatternElements of StructPatternField list * StructPatternEtCetera option
    and StructPatternField = SPFTPIND of OuterAttribute list * TokenType * Pattern * Pos
                            | SPFIBD of OuterAttribute list * Identifer * Pattern * Pos
                            | SPFID of OuterAttribute list * Ref option * Mutability * Identifer * Pos
    and StructPatternEtCetera = StructPatternEtCetera of OuterAttribute list * Pos

    and Type = Type
    and BlockExpression = BlockExpression

    and TokenType = TKAS of Pos | TKBREAK of Pos | TKCONST of Pos | TKCONTINUE of Pos | TKCRATE of Pos
                | TKELSE of Pos | TKENUM of Pos | TKEXTERN of Pos | TKFALSE of Pos | TKFN of Pos
                | TKFOR of Pos | TKIF of Pos | TKIMPL of Pos | TKIN of Pos| TKLET of Pos| TKLOOP of Pos
                | TKMATCH of Pos| TKMOD of Pos| TKMOVE of Pos| TKMUT of Pos| TKPUB of Pos| TKREF of Pos
                | TKRETURN of Pos| TKSELFVALUE of Pos| TKSELFTYPE of Pos| TKSTATIC of Pos| TKSTRUCT of Pos
                | TKSUPER of Pos| TKTRAIT of Pos| TKTRUE of Pos| TKTYPE of Pos| TKUNSAFE of Pos| TKUSE of Pos
                | TKWHERE of Pos| TKWHILE of Pos| TKDYN of Pos| TKABSTRACT of Pos| TKBECOME of Pos
                | TKBOX of Pos| TKDO of Pos| TKFINAL of Pos| TKMACRO of Pos | TKOVERRIDE of Pos| TKPRIV of Pos
                | TKTYPEOF of Pos| TKUNSIZED of Pos| TKVIRTUAL of Pos| TKYIELD of Pos| TKASYNC of Pos
                | TKAWAIT of Pos | TKTRY of Pos| TKUNION of Pos | TKSTATICLIFETIME of Pos
                | TKIDENT of string * Pos | TKCHAR_LIT of string * Pos 
                | TKSTR_LIT of string *  Pos| TKRAW_STR_LIT of string *  Pos| TKBYTE_LIT of string * Pos
                | TKBYTE_STR_LIT of string * Pos| TKRAW_BYTE_STR_LIT of string * Pos| TKINTEGER_LIT of string * Pos
                | TKTUPLE_INDEX of string * Pos | TKFLOAT_LIT of string * Pos
                | TKLIFETIME_OR_LABEL of string * Pos| TKLIFETIME_TOKEN of string * Pos
                | TKMINUS of Pos| TKSLASH of Pos| TKPERCENT of Pos| TKCARET of Pos
                | TKNOT of Pos| TKAND of Pos| TKOR of Pos| TKANDAND of Pos| TKOROR of Pos| TKSHL of Pos| TKSHR of Pos| TKPLUSEQ of Pos
                | TKMINUSEQ of Pos| TKSTAREQ of Pos| TKSLASHEQ of Pos| TKPERCENTEQ of Pos| TKCARETEQ of Pos| TKANDEQ of Pos| TKOREQ of Pos
                | TKSHLEQ of Pos| TKSHREQ of Pos| TKEQ of Pos| TKEQEQ of Pos| TKNE of Pos| TKGT of Pos| TKLT of Pos| TKGE of Pos
                | TKLE of Pos| TKAT of Pos| TKUNDERSCORE of Pos| TKDOT of Pos| TKDOTDOT of Pos| TKDOTDOTDOT of Pos| TKDOTDOTEQ of Pos| TKCOMMA of Pos
                | TKSEMI of Pos| TKCOLON of Pos| TKPATHSEP of Pos| TKRARROW of Pos| TKFATARROW of Pos| TKPOUND of Pos
                | TKINNER_DOC_COMMENT of string * Pos | TKOUTER_DOC_COMMENT of string * Pos
                | TKSHEBANG of Pos| TKDOLLAR of Pos| TKPLUS of Pos| TKSTAR of Pos| TKQUESTION of Pos
                
    and Pos = Pos of int

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
    and MetaItem = AttrName of SimplePath | AttrKVPair of SimplePath * LiteralExpression 
                    | AttrSubs of SimplePath * MetaItemInner list
    and LiteralExpression = LiteralExpression of TokenType
    and MetaItemInner = MetaItem of MetaItem | MetaLit of LiteralExpression
    and SimplePath = SimplePath of PathSeg list
    and PathInExpression = PathInExpression of PathSeg list
    and QualifiedPathInExpression = QualifiedPathInExpression of Type * TypePath option * PathSeg list
    and QualifiedPathInType = QualifiedPathInType of Type * TypePath option * PathSeg list
    and TypePath = TypePath of PathSeg list
    and PathSeg = IDPS of Identifer | SuperPS | SelfValuePS | CratePS | DCratePS | DefaultPS | SelfTypePS 
                | GenericPS of GenericArgs | TypePathFnPS of TypePathFn
    and GenericArgs = GenericArgs of Lifetime list * Type list * Binding list
    and Binding = Binding of (Identifer * Type)
    and TypePathFn = TypePathFn of (Type list * Type option)
    and Item = VisItemType of (OuterAttribute list * VisItem) | MacroItemType of MacroItem
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
        | ExternBlock of (Abi option * InnerAttribute list * ExternalItem list)
    and StructType = StructStruct of (Identifer * Generics option * WhereClause option * StructField list)
                    | UnitStruct of (Identifer * Generics option * WhereClause option)
                    | TupleStruct of (Identifer * Generics option * TupleField list * WhereClause option)
    and StructField = StructField of (OuterAttribute list * Visibility * Identifer * Type)
    and TupleField = TupleField of (OuterAttribute list * Visibility * Type)
    and ModuleBody = ModuleBody of InnerAttribute list * Item list
    and Visibility = DefaultVis | PubVis | CrateVis | SelfVis | SuperVis | InVis of SimplePath
    and UseTree = UseAll of SimplePath option | UseList of (SimplePath option * UseTree list) | UseAlias of (SimplePath * Identifer option)
    and MacroItem = MacroInvocationSemi of PathInExpression * TokenTree
                    | MacroInvocation of PathInExpression * TokenTree 
                    | MacroRulesDefinition of PathInExpression * Identifer * MacroRulesDef
    and TokenTree = DTokenTree of Delim * TokenTree list | SToken of TokenType
    and Delim = ParentDelim | BracketDelim | BraceDelim
    and MacroRulesDef = MacroRulesDef of Delim * MacroRule list
    and MacroRule = MacroRule of MacroMatcher * TokenTree
    and MacroMatcher = MacroMatcher of Delim * MacroMatch list
    and MacroMatch = MMTK of TokenType | MMer of MacroMatcher 
                | MMBD of Identifer * Identifer | MMs of MacroMatch list * TokenType option * MacroKleeneOp
    and MacroKleeneOp = KleeneStar | KleenePlus | KleeneQues
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
                        | TraitMIS of MacroItem
    and TraitFuncDecl = TraitFuncDecl of {qualifier:FunctionQualifier list, name:Identifer, generic:Generics option, 
                        params:TraitFunctionParam list, ret:Type option, 
                        wh:WhereClause option}
    and TraitFunctionParam = TraitFunctionParam of (Pattern option * Type)
    and TraitMethodDecl = TraitMethodDecl of {qualifier:FunctionQualifier list, name:Identifer, generic:Generics option, 
                        selfParam: SelfParam, params:TraitFunctionParam list, ret:Type option, 
                        wh:WhereClause option}
    and SelfParam = SelfParamLT of (Lifetime option * Mutability) | SelfParamTY of (Mutability * Type option)

    and Unsafe = Unsafe

    and InherentImplItem = InherentImplItemMacro of (OuterAttribute list * MacroItem)
                            | InherentImplItemType of (OuterAttribute list * Visibility * ItemType)
                            | InherentImplItemMethod of (OuterAttribute list * Visibility * Method)
    and TraitImplItem = TraitImplItemMacro of (OuterAttribute list * MacroItem)
                        | TraitImplItemType of (OuterAttribute list * Visibility * ItemType)
                        | TraitImplItemMethod of (OuterAttribute list * Visibility * Method)
    and Method = Method of {qualifier:FunctionQualifier list, name:Identifer, generic:Generics option, 
                        selfParam: SelfParam, params:FunctionParam list, ret:Type option, 
                        wh:WhereClause option, be:BlockExpression}

    and ExternalItem = ExternalItem of (OuterAttribute list * Visibility * ExternalItemType)
    and ExternalItemType = ExternalStaticItem of (Mutability * Identifer * Type)
                            | ExternalFunctionItem of {name:Identifer, generic:Generics option, 
                            params:ExternFunctionParameter, ret:Type option, wh:WhereClause option}
    and ExternFunctionParameter = ExternFunctionParameter of {params:NamedFunctionParam list, var:bool}
    and NamedFunctionParam = NamedFunctionParam of (Identifer option * Type)

    and Pattern = LiteralPattern of Minus option * TokenType * Pos
                | IdentiferPattern of BindingMode * Identifer * Pattern option
                | WildcardPattern of Pos
                | RangePatternDDE of RangePatternBound * RangePatternBound
                | RangePatternDDD of RangePatternBound * RangePatternBound
                | ReferencePattern of Borrow * Mutability * Pattern * Pos
                | StructPattern of PathInExpression * StructPatternElements
                | TupleStructPattern of PathInExpression * Pattern list
                | TupleStructPatternDD of PathInExpression * Pattern list * Pattern list
                | TupleORGroupPattern of Pattern list * Pos 
                | TupleORGroupPatternDD of Pattern list * Pattern list * Pos
                | SlicePattern of Pattern list * Pos
                | PathPat of PathInExpression 
                | QPathPat of QualifiedPathInExpression
    and Minus = Minus of Pos
    and BindingMode = BindingMode of Ref option * Mutability * Pos
    and Ref = Ref of Pos 
    and RangePatternBound = RPBLit of Minus option * TokenType * Pos
                        | RPBPath of PathInExpression 
                        | RPBQPath of QualifiedPathInExpression
    and Borrow = BOnce of Pos | BTwice of Pos
    and StructPatternElements = StructPatternElements of StructPatternField list * StructPatternEtCetera option
    and StructPatternField = SPFTPIND of OuterAttribute list * TokenType * Pattern * Pos
                            | SPFIBD of OuterAttribute list * Identifer * Pattern * Pos
                            | SPFID of OuterAttribute list * Ref option * Mutability * Identifer * Pos
    and StructPatternEtCetera = StructPatternEtCetera of OuterAttribute list * Pos

    and Type = Type
    and BlockExpression = BlockExpression

    and TokenType = TKAS of Pos | TKBREAK of Pos | TKCONST of Pos | TKCONTINUE of Pos | TKCRATE of Pos
                | TKELSE of Pos | TKENUM of Pos | TKEXTERN of Pos | TKFALSE of Pos | TKFN of Pos
                | TKFOR of Pos | TKIF of Pos | TKIMPL of Pos | TKIN of Pos| TKLET of Pos| TKLOOP of Pos
                | TKMATCH of Pos| TKMOD of Pos| TKMOVE of Pos| TKMUT of Pos| TKPUB of Pos| TKREF of Pos
                | TKRETURN of Pos| TKSELFVALUE of Pos| TKSELFTYPE of Pos| TKSTATIC of Pos| TKSTRUCT of Pos
                | TKSUPER of Pos| TKTRAIT of Pos| TKTRUE of Pos| TKTYPE of Pos| TKUNSAFE of Pos| TKUSE of Pos
                | TKWHERE of Pos| TKWHILE of Pos| TKDYN of Pos| TKABSTRACT of Pos| TKBECOME of Pos
                | TKBOX of Pos| TKDO of Pos| TKFINAL of Pos| TKMACRO of Pos | TKOVERRIDE of Pos| TKPRIV of Pos
                | TKTYPEOF of Pos| TKUNSIZED of Pos| TKVIRTUAL of Pos| TKYIELD of Pos| TKASYNC of Pos
                | TKAWAIT of Pos | TKTRY of Pos| TKUNION of Pos | TKSTATICLIFETIME of Pos
                | TKIDENT of string * Pos | TKCHAR_LIT of string * Pos 
                | TKSTR_LIT of string *  Pos| TKRAW_STR_LIT of string *  Pos| TKBYTE_LIT of string * Pos
                | TKBYTE_STR_LIT of string * Pos| TKRAW_BYTE_STR_LIT of string * Pos| TKINTEGER_LIT of string * Pos
                | TKTUPLE_INDEX of string * Pos | TKFLOAT_LIT of string * Pos
                | TKLIFETIME_OR_LABEL of string * Pos| TKLIFETIME_TOKEN of string * Pos
                | TKMINUS of Pos| TKSLASH of Pos| TKPERCENT of Pos| TKCARET of Pos
                | TKNOT of Pos| TKAND of Pos| TKOR of Pos| TKANDAND of Pos| TKOROR of Pos| TKSHL of Pos| TKSHR of Pos| TKPLUSEQ of Pos
                | TKMINUSEQ of Pos| TKSTAREQ of Pos| TKSLASHEQ of Pos| TKPERCENTEQ of Pos| TKCARETEQ of Pos| TKANDEQ of Pos| TKOREQ of Pos
                | TKSHLEQ of Pos| TKSHREQ of Pos| TKEQ of Pos| TKEQEQ of Pos| TKNE of Pos| TKGT of Pos| TKLT of Pos| TKGE of Pos
                | TKLE of Pos| TKAT of Pos| TKUNDERSCORE of Pos| TKDOT of Pos| TKDOTDOT of Pos| TKDOTDOTDOT of Pos| TKDOTDOTEQ of Pos| TKCOMMA of Pos
                | TKSEMI of Pos| TKCOLON of Pos| TKPATHSEP of Pos| TKRARROW of Pos| TKFATARROW of Pos| TKPOUND of Pos
                | TKINNER_DOC_COMMENT of string * Pos | TKOUTER_DOC_COMMENT of string * Pos
                | TKSHEBANG of Pos| TKDOLLAR of Pos| TKPLUS of Pos| TKSTAR of Pos| TKQUESTION of Pos
                
    and Pos = Pos of int

    and Numeric = U8 of Word8.word | U16 of Word.word | U32 of Word32.word 
                    | U64 of Word64.word | U128 of LargeInt.int
                    | I8 of Word8.word | I16 of Word.word | I32 of Word32.word 
                    | I64 of Word64.word | I128 of LargeInt.int
                    | usize of Word.word | isize of Word.word
end