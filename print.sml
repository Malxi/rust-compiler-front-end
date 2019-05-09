(* print.sml *)
structure PrintAST: 
    sig val print: TextIO.outstream * DataTypes.Crate -> unit end =
struct
    structure A = DataTypes
    fun print (stream, ast) = 
        let
            fun out(s) = TextIO.output(stream, s)
            fun outln(s) = (out s; out "\n")
            fun indent(0) = ()
                | indent(n) = (out " "; indent(n-1))
            fun nextLine(d) = (outln ""; indent d)

            fun outList d f l r = 
                let
                    fun sep r d = if r then nextLine d else out ""
                    val ind = if r then d else 0
                    val inc = if r then 0 else 0
                    fun travel d f [a] = (f(a, d+inc))
                        | travel d f (h::t) = (f(h, d+inc); out ", "; sep r d; travel (d+inc) f t)
                        | travel d f nil = ()

                    fun start d f [a] = (f(a, d))
                        | start d f (h::t) = (f(h, d); out ", "; sep r d; travel (d+inc) f t)
                        | start d f nil = ()
                in
                    (out "["; start ind f l; out "]")
                end

            fun Token(A.TKSTR_LIT(s, pos), d) = (out ("Token.STR_LIT("^s^")"))
                | Token(A.TKRAW_STR_LIT(s, pos), d) = (out ("Token.RAW_STR_LIT("^s^")"))
                | Token(_, d) = (out "Token()")

            fun Crate(A.Crate(shebang, innerAttrs, items), d) = 
                    (indent d; outln "Crate ("; Shebang(shebang, d+1); outln ","; 
                    outList (d+1) InnerAttribute innerAttrs true; outln ",";
                    outList (d+1) Item items true;
                    outln "\n)")
            and Shebang(A.Shebang(SOME s), d) = (out("Shebang ("^s^")"))
                | Shebang(A.Shebang(NONE), d) = (out("Shebang (NONE)"))
            and InnerAttribute(A.InnerAttribute(innerAttr), d) = 
                    (indent d; out "InnerAttribute("; MetaItem(innerAttr, 0); out ")")
            and OuterAttribute(A.OuterAttribute(outerAttr), d) = 
                    (indent d; out "OuterAttribute("; MetaItem(outerAttr, 0); out ")")
            and MetaItem(A.AttrName simplePath, d) = 
                    (indent d; out "AttrName ("; SimplePath(simplePath, d); out ")")
                | MetaItem (A.AttrKVPair(simplePath, literalExpression), d) = 
                    (indent d; out "AttrKVPair ("; SimplePath(simplePath, d); out "="; Expression(literalExpression, 0); out ")")
                | MetaItem (A.AttrSubs(simplePath, metaSeq), d) = 
                    (indent d; out "AttrSubs ("; 
                    SimplePath(simplePath, d); out "("; 
                    (indent d; outList (0) MetaItemInner metaSeq true);
                    out ")")
            and SimplePath (A.SimplePath(pathList), d) = 
                (indent d; out "SimplePath ("; outList (d) PathSeg pathList false; out ")")
            and PathInExpression(A.PathInExpression(pathList), d) = 
                (indent d; out "PathInExpression ("; outList (d) PathSeg pathList false; out ")")
            and QualifiedPathInExpression(A.QualifiedPathInExpression(ty, NONE, pathList), d) =
                (indent d; out "QualifiedPathInExpression ("; Type(ty, d);outList (d) PathSeg pathList false; out ")")
                | QualifiedPathInExpression(A.QualifiedPathInExpression(ty, SOME(typ), pathList), d) =
                (indent d; out "QualifiedPathInExpression ("; Type(ty, d); out "as"; TypePath(typ, d);
                outList (d) PathSeg pathList false; out ")")
            and QualifiedPathInType(A.QualifiedPathInType(ty, NONE, pathList), d) =
                (indent d; out "QualifiedPathInType ("; Type(ty, d);outList (d) PathSeg pathList false; out ")")
                | QualifiedPathInType(A.QualifiedPathInType(ty, SOME(typ), pathList), d) =
                (indent d; out "QualifiedPathInType ("; Type(ty, d); out "as"; TypePath(typ, d);
                outList (d) PathSeg pathList false; out ")")
            and PathSeg(A.IDPS(id), d) = (Identifer(id, d))
                | PathSeg(A.SelfValuePS, d) = (out ("self"))
                | PathSeg(A.CratePS, d) = (out ("crate"))
                | PathSeg(A.DCratePS, d) = (out ("$crate"))
                | PathSeg(A.SuperPS, d) = (out ("super"))
                | PathSeg(A.DefaultPS, d) = (out ("root"))
                | PathSeg(A.SelfTypePS, d) = (out ("Self"))
                | PathSeg(A.GenericPS(genericArgs), d) = GenericArgs(genericArgs, d)
                | PathSeg(A.TypePathFnPS(typePathfn), d) = (out ("Self"))
            and GenericArgs(A.GenericArgs(lts, tys, bingds), d) = 
                (
                    out "GenericArgs ("; 
                    outList (d) Lifetime lts false; 
                    out ",";
                    outList (d) Type tys false;
                    out ",";
                    outList (d) Binding bingds false;
                    out ")"
                )
            and Binding(A.Binding(id, ty), d) = (Identifer(id, d+1); out ":"; Type(ty, d+1))
            and TypePathFn(A.TypePathFn(tys, NONE), d) = 
                    (out "TypePathFn ("; outList (d) Type tys false; out ")")
                | TypePathFn(A.TypePathFn(tys, SOME(ty)), d) = 
                    (out "TypePathFn ("; outList (d) Type tys false; out "->"; Type(ty, d+1); out ")")
            and MetaItemInner(A.MetaItem(metaItem), d) = 
                    (indent d; out "MetaItemInner ("; MetaItem(metaItem, d);out ")")
                | MetaItemInner(A.MetaLit(literalExpression), d) = 
                    (indent d; out "MetaItemInner ("; Expression(literalExpression, d) ;out ")")
            and Item(A.VisItemType(outerAttrs, visItem), d) = 
                    (out "VisItemType ("; nextLine(d); VisItem(visItem, d+1); out ")")
                | Item(A.MacroItemType(marcoItem), d) = 
                     (out "MacroItemType ("; nextLine(d); MacroItem(marcoItem, d+1); out ")")
            and VisItem(A.VisItem(visibility, itemType), d) =
                (out "VisItem (";
                nextLine(d);
                out "Visibility (";
                Visibility(visibility, d+1);
                out ")";
                nextLine(d);
                ItemType(itemType, d+1);
                nextLine(d); 
                out ")")
            and Visibility(A.DefaultVis, d) = (out "default")
                | Visibility(A.PubVis, d) = (out "pub")
                | Visibility(A.CrateVis, d) = (out "crate")
                | Visibility(A.SelfVis, d) = (out "self")
                | Visibility(A.SuperVis, d) = (out "super")
                | Visibility(A.InVis(vis), d) = (out "in"; SimplePath(vis, 0))
            and ItemType(A.Module(id, SOME(mbd)), d) = 
                    (out "Module (";
                    nextLine(d);
                    Identifer(id, 0);
                    nextLine(d);
                    ModuleBody(mbd, d+1);
                    out ")")
                | ItemType(A.Module(id, NONE), d) = 
                    (out "Module "; Identifer(id, 0); out ",")
                | ItemType(A.ExternCrate(id, NONE), d) =
                    (out "Extern Crate ("; Identifer(id, 0); out ")")
                | ItemType(A.ExternCrate(id1, SOME(id2)), d) =
                    (out "Extern Crate ("; Identifer(id1, 0); out " as "; Identifer(id2, 0); out ")")
                | ItemType(A.UseDeclaration useDecl, d) =
                    ("UseDeclaration ("; UseTree(useDecl, 0); out ")")
                | ItemType(A.Function(func), d) = 
                     let
                        val {qualifier=qualifier, name=name, generic=generic, params=params, ret=ret, wh=wh, be=be} = func
                    in
                        (out "Function (";
                        nextLine(d);
                        out "qualifier: ";
                        outList (d+1) FunctionQualifier qualifier false;
                        nextLine(d);
                        out "name: ";
                        Identifer(name, 0);
                        nextLine(d);
                        out "generics: ";
                        (fn SOME(g) => (nextLine(d); Generics(g, d)) | NONE => ()) generic;
                        nextLine(d);
                        out "param: ";
                        outList (d+1) FunctionParam params false;
                        nextLine(d);
                        out "ret: ";
                        TypeOption(ret, d+1);
                        nextLine(d);
                        out "wh: ";
                        WhereClauseOption(wh, d+1);
                        nextLine(d);
                        out "be: ";
                        nextLine(d);
                        Expression(be, d+1);
                        nextLine(d);
                        out ")"
                        )
                    end
                | ItemType(A.TypeAlias(id, mgen, mwh, ty), d) =
                    (out "TypeAlias (";
                    Identifer(id, 0);
                    out ",";
                    GenericsOption(mgen, d+1);
                    out ",";
                    WhereClauseOption(mwh, d+1);
                    out ",";
                    Type(ty, d+1);
                    out ")"
                    )
                | ItemType(A.Struct(st), d) = (StructType(st, d))
                | ItemType(A.Enumeration(id, mgen, mwh, eitList), d) = 
                    (
                        out "Enumeration (";
                        nextLine(d);
                        out "name: ";
                        Identifer(id, d);
                        nextLine(d);
                        out "generic: ";
                        (fn SOME(g) => (nextLine(d); Generics(g, d)) | NONE => ()) mgen;
                        nextLine(d);
                        out "where: ";
                        WhereClauseOption(mwh, d);
                        nextLine(d);
                        out "enumItems: ";
                        outList (d+1) EnumItem eitList false;
                        nextLine(d);
                        out ")"
                    )
                | ItemType (A.Union(id, mgen, mwh, sfList), d) =
                    (
                        out "Union (";
                        nextLine(d);
                        out "generics: ";
                        (fn SOME(g) => (nextLine(d); Generics(g, d)) | NONE => ()) mgen;
                        nextLine(d);
                        out "where: ";
                        WhereClauseOption(mwh, d);
                        nextLine(d);
                        out "struct fields: ";
                        outList (d) StructField sfList false;
                        nextLine(d);
                        out ")"
                    )
                | ItemType(A.ConstantItem(id, ty, exp), d) =
                    (out "ConstantItem ("; Identifer(id, d); out ","; Type(ty, d); out ","; Expression(exp, d); out ")")
                | ItemType(A.StaticItem(m, id, ty, exp), d) =
                    (out "StaticItem ("; Mutability(m, d); out ","; Identifer(id, d); out ","; Type(ty, d); out ","; Expression(exp, d); out ")")
                | ItemType(A.Trait(trait), d) = 
                    let
                        val {unsafe=unsafe, name=name, generic=generic, tyb=tyb, wh=wh, traitItems=traitItems} = trait
                    in
                        out "Trait (";
                        nextLine(d);
                        out "unsafe: ";
                        UnsafeOption(unsafe, d+1);
                        nextLine(d);
                        out "name: ";
                        Identifer(name, d+1);
                        nextLine(d);
                        out "generics: ";
                        (fn SOME(g) => (nextLine(d); Generics(g, d)) | NONE => ()) generic;
                        nextLine(d);
                        out "type param bounds: ";
                        TypeParamBoundsOption(tyb, d+1);
                        nextLine(d);
                        out "where: ";
                        WhereClauseOption(wh, d+1);
                        nextLine(d);
                        out "trait items: ";
                        nextLine(d);
                        outList (d+1) TraitItem traitItems true;
                        nextLine(d);
                        out ")"
                    end
                | ItemType(A.InherentImpl(ihti), d) =
                    let
                        val {generic=generic, ty=ty, wh=wh, innerAttrs=innerAttrs, implItems=implItems} = ihti
                    in
                        out "InherentImpl (";
                        nextLine(d);
                        out "generic:";
                        (fn SOME(g) => (nextLine(d); Generics(g, d)) | NONE => ()) generic;
                        nextLine(d);
                        out "type: ";
                        Type(ty, d+1);
                        nextLine(d);
                        outList (d+1) InnerAttribute innerAttrs false;
                        nextLine(d);
                        out "items: ";
                        nextLine(d);
                        outList (d+1) InherentImplItem implItems true;
                        nextLine(d);
                        out ")"
                    end
                | ItemType(A.TraitImpl(traitImpl), d) =
                    let
                        val {unsafe=unsafe, generic=generic, neg=neg, typath=typath, ty=ty, wh=wh, innerAttrs=innerAttrs, implItems=implItems} = traitImpl
                    in
                        out "TraitImpl (";
                        nextLine(d);
                        out "unsafe: ";
                        UnsafeOption(unsafe, d+1);
                        nextLine(d);
                        out "generic:";
                        (fn SOME(g) => (nextLine(d); Generics(g, d)) | NONE => ()) generic;
                        nextLine(d);
                        out "neg: ";
                        (if neg then out "true" else out "false");
                        nextLine(d);
                        out "type path: ";
                        TypePath(typath, d+1);
                        nextLine(d);
                        out "type: ";
                        Type(ty, d+1);
                        nextLine(d);
                        outList (d+1) InnerAttribute innerAttrs false;
                        nextLine(d);
                        out "items: ";
                        nextLine(d);
                        outList (d+1) TraitImplItem implItems true;
                        nextLine(d);
                        out ")"
                    end
                | ItemType(A.ExternBlock(mabi, innerAttrs, etiList), d) =
                    (
                        out ("ExternBlock (");
                        nextLine(d);
                        outList (d+1) InnerAttribute innerAttrs false;
                        nextLine(d);
                        outList (d+1) ExternalItem etiList true;
                        out ")"
                    )
            and ModuleBody(A.ModuleBody(innerAttrs, items), d) = 
                (out "Body (";
                nextLine(d);
                out "name: ";
                outList (d+1) InnerAttribute innerAttrs true;
                nextLine(d);
                out "items: ";
                outList (d+1) Item items true;
                nextLine(d);
                out ")")
            and UseTree(A.UseAll(SOME(simplePath)), d) = 
                    (indent d; out "UseAll ("; SimplePath(simplePath, d); out ")")
                | UseTree(A.UseAll(NONE), d) = 
                    (indent d; out "UseAll ("; out ")")
                | UseTree(A.UseList(SOME(simplePath), useTreeList), d) = 
                    (indent d; out "UseList ("; SimplePath(simplePath, d); outList (d) UseTree useTreeList false; out ")")
                | UseTree(A.UseList(NONE, useTreeList), d) = 
                    (indent d; out "UseList ("; outList (d) UseTree useTreeList false; out ")")
                | UseTree(A.UseAlias(simplePath, SOME(id)), d) = 
                    (indent d; out "UseAlias ("; SimplePath(simplePath, d); out " as "; Identifer(id, d); out ")")
                | UseTree(A.UseAlias(simplePath, NONE), d) = 
                    (indent d; out "UseAlias ("; SimplePath(simplePath, d); out ")")
            and Identifer(A.Identifer(id), d) = 
                (out id)
            and FunctionQualifier(A.ConstFQ, d) = (indent d; out "const")
                | FunctionQualifier(A.UnsafeFQ, d) = (indent d; out "unsafe")
                | FunctionQualifier(A.ExternFQ(SOME(abi)), d) = (indent d; out "extern("; Abi(abi, 0); out ")")
                | FunctionQualifier(A.ExternFQ(NONE), d) = (indent d; out "extern")
            and Abi(A.Abi(s), d) = (indent d; out s)
            and Generics(A.Generics(generics), d) = 
                (out "Generics (";
                GenericParams(generics, d+1);
                nextLine(d);
                out ")"
                )
            and GenericParams(A.GenericParams(lifetimeParams, typeParams), d) =
                (nextLine(d);
                LifetimeParams(lifetimeParams, d+1);
                nextLine(d);
                TypeParams(typeParams, d+1))
            and LifetimeParams(A.LifetimeParams(ltpList), d) =
                (outList (d+1) LifetimeParam ltpList false)
            and LifetimeParam(A.LifetimeParam(NONE, lt, NONE), d) =
                    (indent d; out "LifetimeParam ("; Lifetime(lt, d); out ")")
                | LifetimeParam(A.LifetimeParam(NONE, lt, SOME(ltbs)), d) = 
                    (indent d; out "LifetimeParam ("; Lifetime(lt, d); outln ","; LifetimeBounds(ltbs, d); out ")")
                | LifetimeParam(A.LifetimeParam(SOME(outerAttr), lt, NONE), d) =
                    (indent d; out "LifetimeParam ("; OuterAttribute(outerAttr, d); outln ","; Lifetime(lt, d); out ")")
                | LifetimeParam(A.LifetimeParam(SOME(outerAttr), lt, SOME(ltbs)), d) =
                    (indent d; out "LifetimeParam ("; OuterAttribute(outerAttr, d); outln ","; Lifetime(lt, d); outln ","; LifetimeBounds(ltbs, d); out ")")
            and Lifetime(A.LifetimeOrLabel(s), d) = (indent d; out "'"; out s)
                | Lifetime(A.StaticLifetime, d) = (indent d; out "'"; out "static")
            and LifetimeBounds(A.LifetimeBounds(ltList), d) = 
                (indent d; outList (d+1) Lifetime ltList false)
            and TypeParams(A.TypeParams(tpList), d) = 
                (outList (d+1) TypeParam tpList false)
            and TypeParam(A.TypeParam(moutAttr, id, mtpbs, mty), d) =
                (indent d; out "TypeParam (";
                OuterAttributeOption(moutAttr, d);
                out ",";
                Identifer(id, d);
                out ",";
                TypeParamBoundsOption(mtpbs, d);
                out ",";
                TypeOption(mty, d);
                out ")"
                )
            and TypeParamBounds(A.TypeParamBounds(tpbList), d) = 
                (outList (d+1) TypeParamBound tpbList false)
            and TypeParamBound(A.LTB(lt), d) = Lifetime(lt, d)
                | TypeParamBound(A.TB(tb), d) = TraitBound(tb, d)
            and TraitBound(A.TraitBound(NONE, NONE, tyPath), d) =
                    (indent d; out "TraitBound ("; TypePath(tyPath, d); out ")")
                | TraitBound(A.TraitBound(NONE, SOME(flt), tyPath), d) =
                    (indent d; out "TraitBound ("; ForLifetimes(flt, d); outln ","; TypePath(tyPath, d); out ")")
                | TraitBound(A.TraitBound(SOME(sized), NONE, tyPath), d) =
                    (indent d; out "TraitBound ("; Sized(sized, d); outln ","; TypePath(tyPath, d); out ")")
                | TraitBound(A.TraitBound(SOME(sized), SOME(flt), tyPath), d) =
                    (indent d; out "TraitBound ("; Sized(sized, d); outln ","; ForLifetimes(flt, d); outln ","; TypePath(tyPath, d); out ")")
            and Sized(A.Sized, d) = out "?"
            and TypePath(A.TypePath(pathList), d) = 
                (indent d; out "TypePath ("; outList (d) PathSeg pathList false; out ")")
            and Type(A.TypeNoBoundsT(tnbt), d) = 
                (
                    TypeNoBounds(tnbt, d)
                )
                | Type(A.ImplTraitType(tpbs, pos), d) = 
                (
                    out "ImplTraitType (";
                    TypeParamBounds(tpbs, d);
                    out ")"
                )
                | Type(A.TraitObjectType(tpbs, pos), d) = 
                (
                    out "TraitObjectType (";
                    TypeParamBounds(tpbs, d);
                    out ")"
                )
            and TypeNoBounds(A.ParenthesizedType(typ, pos), d) =
                (
                    out "ParenthesizedType (";
                    Type(typ, d);
                    out ")"
                )
                | TypeNoBounds(A.ImplTraitTypeOneBound(tb, pos), d) =
                (
                    out "ImplTraitTypeOneBound (";
                    TraitBound(tb, d);
                    out ")"
                )
                | TypeNoBounds(A.TraitObjectTypeOneBound(tb, pos), d) =
                (
                    out "TraitObjectTypeOneBound (";
                    TraitBound(tb, d);
                    out ")"
                )
                | TypeNoBounds(A.TNBTypePath(typath), d) =
                (
                    TypePath(typath, d)
                )
                | TypeNoBounds(A.TupleType(typList, pos), d) =
                (
                    out "TupleType (";
                    outList (d) Type typList false;
                    out ")"
                )
                | TypeNoBounds(A.NeverTuple(pos), d) =
                (
                    out "NeverTuple ()"
                )
                | TypeNoBounds(A.RawPointerType(rptm, tndt, pos), d) =
                (
                    out "RawPointerType (";
                    (fn A.ConstMod(pos) => out "const" | A.MutMod(pos) => out "mut") rptm;
                    out " ";
                    TypeNoBounds(tndt, d);
                    out ")"
                )
                | TypeNoBounds(A.ReferenceType(mlt, mut, tndt, pos), d) =
                (
                    out "ReferenceType (";
                    (fn SOME(lt) => (Lifetime(lt, d); out " ") | NONE => ()) mlt;
                    TypeNoBounds(tndt, d);
                    out ")"
                )
                | TypeNoBounds(A.ArrayType(ty, expr, pos), d) =
                (
                    out "ArrayType (";
                    Type(ty, d);
                    Expression(expr, d);
                    out ")"
                )
                | TypeNoBounds(A.SliceType(ty, pos), d) =
                (
                    out "SliceType (";
                    Type(ty, d);
                    out ")"
                )
                | TypeNoBounds(A.InferredType(pos), d) =
                (
                    out "InferredType ()"
                )
                | TypeNoBounds(A.TNBQPathInType(qpath), d) =
                (
                    QualifiedPathInType(qpath, d)
                )
                | TypeNoBounds(A.BareFunctionType(bft), d) =
                    let
                        val {forlifetimes=forlifetimes, qualifier=qualifier, params=params, var=var, ret=ret} = bft
                    in
                        out "BareFunctionType (";
                        out ",";
                        ForLifetimesOption(forlifetimes, d);
                        out ",";
                        outList (d) FunctionQualifier qualifier false;
                        out ",";
                        outList (d) MaybeNamedParam params false;
                        out ",";
                        (fn true => out "var" | false => out "nonvar") var;
                        out ",";
                        (fn SOME(tbdt) => TypeNoBounds(tbdt, d) | NONE => ()) ret;
                        out ")"
                    end
                | TypeNoBounds(A.TNBMacro(typath, tokenTree), d) =
                (
                    out "MacroInvocation (";
                    TypePath(typath, d);
                    out ",";
                    TokenTree(tokenTree, d);
                    out ")"
                )
            and MaybeNamedParam(A.MaybeNamedParamID(id, ty, pos), d) =
                (
                    out "MaybeNamedParam (";
                    Identifer(id, d);
                    out ":";
                    Type(ty, d);
                    out ")"
                )
                | MaybeNamedParam(A.MaybeNamedParamWD(wc, ty, pos), d) =
                (
                    out "MaybeNamedParam (";
                    out "_";
                    out ":";
                    Type(ty, d);
                    out ")"
                )
                | MaybeNamedParam(A.MaybeNamedParamTY(ty), d) =
                (
                    out "MaybeNamedParam (";

                    Type(ty, d);
                    out ")"
                )
            and ForLifetimes(A.ForLifetimes(ltps), d) =
                LifetimeParams(ltps, d)
            and FunctionParam(A.FunctionParam(pat, ty), d) =
                (Pattern(pat, d); out ":"; Type(ty, d))
            and Pattern(A.LiteralPattern(mminus, tk, pos), d) =
                (
                    out "LiteralPattern (";
                    (fn SOME(minus) => out "-" | NONE => ()) mminus;
                    Token(tk, d);
                    out ")"
                )
                | Pattern(A.IdentiferPattern(bdm, id, mpat), d) =
                (
                    out "IdentiferPattern (";
                    BindingMode(bdm, d);
                    Identifer(id, d);
                    (fn SOME(pat) => Pattern(pat, d) | NONE => ()) mpat;
                    out ")"
                )
                | Pattern(A.WildcardPattern(pos), d) =
                (
                    out "WildcardPattern ()"
                )
                | Pattern(A.RangePatternDDE(rpb1, rpb2), d) =
                (
                    out "RangePattern (";
                    RangePatternBound(rpb1, d);
                    out "..=";
                    RangePatternBound(rpb2, d);
                    out ")"
                )
                | Pattern(A.RangePatternDDD(rpb1, rpb2), d) =
                (
                    out "RangePattern (";
                    RangePatternBound(rpb1, d);
                    out "...";
                    RangePatternBound(rpb2, d);
                    out ")"
                )
                | Pattern(A.ReferencePattern(borrow, mut, pat, pos), d) =
                (
                    out "ReferencePattern (";
                    Borrow(borrow, d);
                    Mutability(mut, d);
                    Pattern(pat, d);
                    out ")"
                )
                | Pattern(A.StructPattern(path, spes), d) =
                (
                    out "StructPattern (";
                    PathInExpression(path, d);
                    out ",";
                    StructPatternElements(spes, d);
                    out ")"
                )
                | Pattern(A.TupleStructPattern(path, patterns), d) =
                (
                    out "TupleStructPattern (";
                    PathInExpression(path, d);
                    out ",";
                    outList (d) Pattern patterns false;
                    out ")"
                )
                | Pattern(A.TupleStructPatternDD(path, patterns1, patterns2), d) =
                (
                    out "TupleStructPattern (";
                    PathInExpression(path, d);
                    out ",";
                    outList (d) Pattern patterns1 false;
                    out "..";
                    outList (d) Pattern patterns2 false;
                    out ")"
                )
                | Pattern(A.TupleORGroupPattern(patterns, pos), d) =
                (
                    out "TupleORGroupPattern (";
                    outList (d) Pattern patterns false;
                    out ")"
                )
                | Pattern(A.TupleORGroupPatternDD(patterns1, patterns2, pos), d) =
                (
                    out "TupleORGroupPattern (";
                    outList (d) Pattern patterns1 false;
                    out "..";
                    outList (d) Pattern patterns2 false;
                    out ")"
                )
                | Pattern(A.SlicePattern(patterns, pos), d) =
                (
                    out "SlicePattern (";
                    outList (d) Pattern patterns false;
                    out ")"
                )
                | Pattern(A.PathPat(path), d) =
                (
                    out "PathPattern  (";
                    PathInExpression(path, d);
                    out ")"
                )
                | Pattern(A.QPathPat(path), d) =
                (
                    out "PathPattern  (";
                    QualifiedPathInExpression(path, d);
                    out ")"
                )
            and BindingMode(A.BindingMode(mr, mut, pos), d) =
                (
                    (fn SOME(r) => out "ref " | NONE => ()) mr;
                    Mutability(mut, d)
                )
            and RangePatternBound(A.RPBLit(mminus, tk, pos), d) =
                (
                    (fn SOME(minus) => out "-" | NONE => ()) mminus;
                    Token(tk, d)
                )
                | RangePatternBound(A.RPBPath(path), d) =
                (
                    PathInExpression(path, d)
                )
                | RangePatternBound(A.RPBQPath(path), d) =
                (
                    QualifiedPathInExpression(path, d)
                )
            and Borrow(A.BOnce(pos), d) = out "&"
                | Borrow(A.BTwice(pos), d) = out "&&"
            and StructPatternElements(A.StructPatternElements(spfs, mcetera), d) =
                (
                    outList (d) StructPatternField spfs false;
                    out ",";
                    (fn SOME(cetera) => StructPatternEtCetera(cetera, d) | NONE => ()) mcetera
                )
            and StructPatternField(A.SPFTPIND(outerAttrs, tk, pat, pos), d) = 
                (
                    out "StructPatternField (";
                    outList (d) OuterAttribute outerAttrs false;
                    out ",";
                    Token(tk, d);
                    out ":";
                    Pattern(pat, d);
                    out ")"
                )
                | StructPatternField(A.SPFIBD(outerAttrs, id, pat, pos), d) = 
                (
                    out "StructPatternField (";
                    outList (d) OuterAttribute outerAttrs false;
                    out ",";
                    Identifer(id, d);
                    out ":";
                    Pattern(pat, d);
                    out ")"
                )
                | StructPatternField(A.SPFID(outerAttrs, mr, mut, id, pos), d) = 
                (
                    out "StructPatternField (";
                    (fn SOME(r) => out "ref " | NONE => ()) mr;
                    Mutability(mut, d);
                    Identifer(id, d);
                    out ")"
                )
            and StructPatternEtCetera(A.StructPatternEtCetera(outerAttrs, pos), d) =
                (
                    out "StructPatternEtCetera (";
                    outList (d) OuterAttribute outerAttrs false;
                    out ")"
                )
            and WhereClause(A.WhereClause(wciList), d) =
                (outList (d+1) WhereClauseItem wciList false)
            and WhereClauseItem(A.LifetimeWhereClauseItem(lt, ltbs), d) =
                    (out "LifetimeWhereClauseItem (";
                    Lifetime(lt, 0);
                    out ",";
                    LifetimeBounds(ltbs, d);
                    out ")"
                    )
                | WhereClauseItem(A.TypeBoundWhereClauseItem(mflt, ty, mtpbs), d) =
                    (out "TypeBoundWhereClauseItem (";
                    ForLifetimesOption(mflt, d);
                    out ",";
                    Type(ty, d);
                    out ",";
                    TypeParamBoundsOption(mtpbs, d);
                    out ")"
                    )
            and StructType(A.StructStruct(id, mgen, mwh, sfList), d) = 
                (
                    out "StructStruct (";
                    nextLine(d);
                    out "name: ";
                    Identifer(id, d+1);
                    nextLine(d);
                    out "generic: ";
                    (fn SOME(g) => (nextLine(d); Generics(g, d)) | NONE => ()) mgen;
                    nextLine(d);
                    out "where: ";
                    WhereClauseOption(mwh, d+1);
                    nextLine(d);
                    out "struct filed: ";
                    outList (d+1) StructField sfList false;
                    nextLine(d);
                    out ")"
                )
                | StructType(A.UnitStruct(id, mgen, mwh), d) = 
                (
                    out "UnitStruct (";
                    nextLine(d);
                    out "name: ";
                    Identifer(id, d+1);
                    nextLine(d);
                    out "generic: ";
                    (fn SOME(g) => (nextLine(d); Generics(g, d)) | NONE => ()) mgen;
                    nextLine(d);
                    out "where: ";
                    WhereClauseOption(mwh, d+1);
                    nextLine(d);
                    out ")"
                )
                | StructType(A.TupleStruct(id, mgen, tpList, mwh), d) = 
                (
                    out "TupleStruct (";
                    nextLine(d);
                    out "name: ";
                    Identifer(id, d+1);
                    nextLine(d);
                    out "generic: ";
                    (fn SOME(g) => (nextLine(d); Generics(g, d)) | NONE => ()) mgen;
                    nextLine(d);
                    out "tuple filed: ";
                    outList (d+1) TupleField tpList false;
                    nextLine(d);
                    out "where: ";
                    WhereClauseOption(mwh, d+1);
                    nextLine(d);
                    out ")"
                )
            and TupleField(A.TupleField(outerAttrs, vis, ty), d) =
                (
                    out "TupleField (";
                    outList (d+1) OuterAttribute outerAttrs false;
                    out ",";
                    Visibility(vis, d+1);
                    out ",";
                    Type(ty, d);
                    out ")"
                )
            and StructField(A.StructField(outerAttrs, vis, id, ty), d) =
                (
                    out "StructField (";
                    outList (d+1) OuterAttribute outerAttrs false;
                    out ",";
                    Visibility(vis, d+1);
                    out ",";
                    Identifer (id, d+1);
                    out ",";
                    Type(ty, d);
                    out ")"
                )
            and EnumItemType(A.EnumItemTuple(tfList), d) =
                    (outList (d+1) TupleField tfList false)
                | EnumItemType(A.EnumItemStruct(sfList), d) =
                    (outList (d+1) StructField sfList false)
                |EnumItemType(A.EnumItemDiscriminant(exp), d) =
                    (Expression(exp, d))
            and EnumItem(A.EnumItem(outerAttrs, id, SOME(et)), d) =
                (
                    out "EnumItem (";
                    outList (d) OuterAttribute outerAttrs false;
                    out ",";
                    Identifer(id, d);
                    out ",";
                    EnumItemType(et, d);
                    out ")"
                )
                | EnumItem(A.EnumItem(outerAttrs, id, NONE), d) =
                (
                    out "EnumItem (";
                    outList (d) OuterAttribute outerAttrs false;
                    out ",";
                    Identifer(id, d);
                    out ")"
                )
            and Mutability(A.Mut, d) = (out "mut")
                | Mutability(A.NonMut, d) = (out "non-mut")
            and Unsafe(A.Unsafe, d) = out "unsafe"
            and TraitItem(A.TraitItem(outerAttrs, tity), d) =
                (
                    out "TraitItem (";
                    nextLine(d);
                    outList (d+1) OuterAttribute outerAttrs false;
                    out ",";
                    nextLine(d);
                    TraitItemType(tity, d+1);
                    out ")"
                )
            and TraitItemType(A.TraitFunc(tfdecl, mbexp), d) = 
                (
                    TraitFuncDecl(tfdecl, d+1);
                    nextLine(d);
                    ExpressionOption(mbexp, d+1)
                )
                | TraitItemType(A.TraitMethod(tmdecl, mbexp), d) = 
                (
                    TraitMethodDecl(tmdecl, d+1);
                    nextLine(d);
                    ExpressionOption(mbexp, d+1)
                )
                | TraitItemType(A.TraitConst(id, ty, mexp), d) = (
                    Identifer(id, d+1);
                    out ",";
                    Type(ty, d+1);
                    ExpressionOption(mexp, d+1)
                )
                | TraitItemType(A.TraitType(id, mtybs), d) = 
                (
                    Identifer(id, d+1);
                    out ",";
                    TypeParamBoundsOption(mtybs, d+1)
                )
                | TraitItemType(A.TraitMIS(mis), d) = (MacroItem(mis, d+1))
            and TraitFuncDecl(A.TraitFuncDecl(tfdecl), d) =
                let
                    val {qualifier=qualifier, name=name, generic=generic, params=params, ret=ret, wh=wh} = tfdecl
                in
                    out "TraitFuncDecl (";
                    nextLine(d);
                    out "qualifier: ";
                    outList (d+1) FunctionQualifier qualifier false;
                    nextLine(d);
                    out "name: ";
                    Identifer(name, 0);
                    nextLine(d);
                    out "generics: ";
                    (fn SOME(g) => (nextLine(d); Generics(g, d)) | NONE => ()) generic;
                    nextLine(d);
                    out "param: ";
                    outList (d+1) TraitFunctionParam params false;
                    nextLine(d);
                    out "ret: ";
                    TypeOption(ret, d+1);
                    nextLine(d);
                    out "wh: ";
                    WhereClauseOption(wh, d+1);
                    nextLine(d);
                    out ")"
                end
            and TraitMethodDecl(A.TraitMethodDecl(tmdecl), d) =
                let
                    val {qualifier=qualifier, name=name, generic=generic, selfParam=selfParam, params=params, ret=ret, wh=wh} = tmdecl
                in
                    out "TraitFuncDecl (";
                    nextLine(d);
                    out "qualifier: ";
                    outList (d+1) FunctionQualifier qualifier false;
                    nextLine(d);
                    out "name: ";
                    Identifer(name, 0);
                    nextLine(d);
                    out "generics: ";
                    (fn SOME(g) => (nextLine(d); Generics(g, d)) | NONE => ()) generic;
                    nextLine(d);
                    out "selfparam: ";
                    SelfParam(selfParam, d);
                    nextLine(d);
                    out "param: ";
                    outList (d+1) TraitFunctionParam params false;
                    nextLine(d);
                    out "ret: ";
                    TypeOption(ret, d+1);
                    nextLine(d);
                    out "wh: ";
                    WhereClauseOption(wh, d+1);
                    nextLine(d);
                    out ")"
                end
            and TraitFunctionParam(A.TraitFunctionParam(SOME(pat), ty), d) =
                (Pattern(pat, d); out ":"; Type(ty, d))
                | TraitFunctionParam(A.TraitFunctionParam(NONE, ty), d) =
                (Type(ty, d))
            and SelfParam(A.SelfParamLT(SOME(lt), mut), d) =
                    (Lifetime(lt, d+1); out ","; Mutability (mut, d+1))
                | SelfParam(A.SelfParamLT(NONE, mut), d) =
                    (Mutability (mut, d+1))
                | SelfParam(A.SelfParamTY(mut, mty), d) =
                    (Mutability (mut, d+1); TypeOption(mty, d+1))
            and Expression(A.BlockExpr, d) = out "BlockExpr()"
                | Expression(_, d) = out "Expression()"
            and MacroItem(A.MacroInvocationSemi(path, tokenTree), d) = 
                (
                    out "MacroInvocationSemi (";
                    PathInExpression(path, d);
                    out ",";
                    TokenTree(tokenTree, d);
                    out ")"
                )
                | MacroItem(A.MacroInvocation(path, tokenTree), d) =
                (
                    out "MacroInvocation (";
                    PathInExpression(path, d);
                    out ",";
                    TokenTree(tokenTree, d);
                    out ")"
                )
                | MacroItem(A.MacroRulesDefinition(path, id, mrd), d) =
                (
                    out "MacroRulesDefinition (";
                    PathInExpression(path, d);
                    out ",";
                    Identifer(id, d);
                    out ",";
                    MacroRulesDef(mrd, d);
                    out ")"
                )
            and MacroRulesDef(A.MacroRulesDef(delim, mrlst), d) =
                (
                    Delim(delim, d);
                    out ",";
                    outList (d) MacroRule mrlst false
                )
            and MacroRule(A.MacroRule(matcher, tokenTree), d) =
                (
                    out "MacroRule (";
                    MacroMatcher(matcher, d);
                    out ",";
                    TokenTree(tokenTree, d)
                )
            and MacroMatcher(A.MacroMatcher(delim, matchList), d) =
                (
                    out "MacroMatcher (";
                    Delim(delim, d);
                    out ",";
                    outList (d) MacroMatch matchList false;
                    out ")"
                )
            and MacroMatch(A.MMTK(tk), d) = Token(tk, d)
                | MacroMatch(A.MMer(matcher), d) = MacroMatcher(matcher, d)
                | MacroMatch(A.MMBD(id1, id2), d) = (Identifer(id1, d); out ":"; Identifer(id2, d))
                | MacroMatch(A.MMs(matchList, NONE, kleene), d) =
                (
                    outList (d) MacroMatch matchList false;
                    out ",";
                    MacroKleeneOp(kleene, d)
                )
                | MacroMatch(A.MMs(matchList, SOME(tk), kleene), d) =
                (
                    outList (d) MacroMatch matchList false;
                    out ",";
                    Token(tk, d);
                    out ",";
                    MacroKleeneOp(kleene, d)
                )
            and MacroKleeneOp(A.KleeneStar, d) = out "*"
                | MacroKleeneOp(A.KleenePlus, d) = out "+"
                | MacroKleeneOp(A.KleeneQues, d) = out "?"
            and Delim(A.ParentDelim, d) = out "ParentDelim"
                | Delim(A.BracketDelim, d) = out "BracketDelim"
                | Delim(A.BraceDelim, d) = out "BraceDelim"
            and TokenTree(A.DTokenTree(delim, tokenTreeList), d) = 
                (outList (d) TokenTree tokenTreeList false)
                | TokenTree(A.SToken(tk), d) = Token(tk, d)
            and InherentImplItem(A.InherentImplItemMacro(outerAttr, mis), d) = 
                (
                    out "InherentImplItemMacro (";
                    nextLine(d);
                    outList (d+1) OuterAttribute outerAttr false;
                    out ",";
                    nextLine(d);
                    MacroItem(mis, d+1);
                    nextLine(d);
                    out ")"
                )
                | InherentImplItem(A.InherentImplItemType(outerAttr, vis, it), d) =
                (
                    out "InherentImplItemType (";
                    nextLine(d);
                    outList (d+1) OuterAttribute outerAttr false;
                    out ",";
                    nextLine(d);
                    Visibility(vis, d+1);
                    out ",";
                    nextLine(d);
                    ItemType(it, d+1);
                    nextLine(d);
                    out ")"
                    
                )
                | InherentImplItem(A.InherentImplItemMethod(outerAttr, vis, method), d) =
                (
                    out "InherentImplItemMethod (";
                    nextLine(d);
                    outList (d+1) OuterAttribute outerAttr false;
                    out ",";
                    nextLine(d);
                    Visibility(vis, d+1);
                    out ",";
                    nextLine(d);
                    Method(method, d+1);
                    nextLine(d);
                    out ")"
                )
            and TraitImplItem(A.TraitImplItemMacro(outerAttr, mis), d) = 
                (
                    out "TraitImplItemMacro (";
                    nextLine(d);
                    outList (d+1) OuterAttribute outerAttr false;
                    out ",";
                    nextLine(d);
                    MacroItem(mis, d+1);
                    nextLine(d);
                    out ")"
                )
                | TraitImplItem(A.TraitImplItemType(outerAttr, vis, it), d) =
                (
                    out "TraitImplItemType (";
                    nextLine(d);
                    outList (d+1) OuterAttribute outerAttr false;
                    out ",";
                    nextLine(d);
                    Visibility(vis, d+1);
                    out ",";
                    nextLine(d);
                    ItemType(it, d+1);
                    nextLine(d);
                    out ")"
                )
                | TraitImplItem(A.TraitImplItemMethod(outerAttr, vis, method), d) =
                (
                    out "TraitImplItemMethod (";
                    nextLine(d);
                    outList (d+1) OuterAttribute outerAttr false;
                    out ",";
                    nextLine(d);
                    Visibility(vis, d+1);
                    out ",";
                    nextLine(d);
                    Method(method, d+1);
                    nextLine(d);
                    out ")"
                )
            and Method(A.Method(method), d) = 
                let
                    val {qualifier=qualifier, name=name, generic=generic, selfParam=selfParam, params=params, ret=ret, wh=wh, be=be} = method
                in
                    out "Method (";
                    nextLine(d);
                    out "qualifier: ";
                    outList (d+1) FunctionQualifier qualifier false;
                    nextLine(d);
                    out "name: ";
                    Identifer(name, 0);
                    nextLine(d);
                    out "generics: ";
                    (fn SOME(g) => (nextLine(d); Generics(g, d)) | NONE => ()) generic;
                    nextLine(d);
                    out "selfparam: ";
                    SelfParam(selfParam, d);
                    nextLine(d);
                    out "param: ";
                    outList (d+1) FunctionParam params false;
                    nextLine(d);
                    out "ret: ";
                    TypeOption(ret, d+1);
                    nextLine(d);
                    out "wh: ";
                    WhereClauseOption(wh, d+1);
                    nextLine(d);
                    out "be: ";
                    nextLine(d);
                    Expression(be, d+1);
                    nextLine(d);
                    out ")"
                end
            and ExternalItem(A.ExternalItem(outerAttrs, vis, etity), d) =
                (
                    out "ExternalItem (";
                    nextLine(d);
                    outList (d+1) OuterAttribute outerAttrs false;
                    Visibility(vis, d+1);
                    nextLine(d);
                    ExternalItemType(etity, d+1);
                    nextLine(d);
                    out ")"
                )
            and ExternalItemType(A.ExternalStaticItem(mut, id, ty), d) =
                (
                    out "ExternalStaticItem (";
                    nextLine(d);
                    Mutability(mut, d);
                    out ",";
                    Identifer(id, d);
                    out ",";
                    Type(ty, d);
                    nextLine(d);
                    out ")"
                )
                | ExternalItemType(A.ExternalFunctionItem(efi), d) =
                let
                    val {name=name, generic=generic, params=params, ret=ret, wh=wh} = efi
                in
                    out "ExternalFunctionItem (";
                    nextLine(d);
                    out "name: ";
                    Identifer(name, d);
                    nextLine(d);
                    out "generics: ";
                    (fn SOME(g) => (nextLine(d); Generics(g, d)) | NONE => ()) generic;
                    nextLine(d);
                    out "param: ";
                    ExternFunctionParameter(params, d+1);
                    nextLine(d);
                    out "ret: ";
                    TypeOption(ret, d+1);
                    nextLine(d);
                    out "wh: ";
                    WhereClauseOption(wh, d+1);
                    nextLine(d);
                    out ")"
                end
            and ExternFunctionParameter(A.ExternFunctionParameter(efps), d) =
                let
                    val {params=params, var=var} = efps
                in
                    outList (d+1) NamedFunctionParam params false;
                    out ",";
                    (fn true => out "true" | false => out "false") var
                end
            and NamedFunctionParam(A.NamedFunctionParam(mid, ty), d) =
                (
                    (fn SOME(id) => (Identifer(id, d+1); out ":") | NONE => ()) mid;
                    Type(ty, d+1)
                )
            and ExpressionOption(SOME(be), d) = Expression(be, d)
                | ExpressionOption(NONE, d) = ()
            and UnsafeOption(SOME(s), d) = Unsafe(s, d)
                | UnsafeOption(NONE, d) = ()
            and OuterAttributeOption(SOME(outAttr), d) = (OuterAttribute(outAttr, d))
                | OuterAttributeOption(NONE, d) = ()
            and TypeParamBoundsOption(SOME(tpbs), d) = (TypeParamBounds(tpbs, d))
                | TypeParamBoundsOption(NONE, d) = ()
            and TypeOption(SOME(ty), d) = (Type(ty, d))
                | TypeOption(NONE, d) = ()
            and ForLifetimesOption(SOME(flt), d) = ForLifetimes(flt, d)
                | ForLifetimesOption(NONE, d) = ()
            and WhereClauseOption(SOME(wh), d) = WhereClause(wh, d)
                | WhereClauseOption(NONE, d) = ()
            and GenericsOption(SOME(g), d) = (Generics(g, d))
                | GenericsOption(NONE, d) = ()
            and VisibilityOption(SOME(v), d) = Visibility(v, d)
                | VisibilityOption(NONE, d) = ()
            and AbiOption(SOME(a), d) = Abi(a, d)
                | AbiOption(NONE, d) = ()
        in
            Crate(ast, 0)
        end
end
