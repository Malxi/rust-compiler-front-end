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

            fun outList d f l r = 
                let
                    val sep = if r then "\n" else ""
                    val ind = if r then d else 0
                    val inc = if r then 0 else 0
                    fun travel d f [a] = (f(a, d+inc))
                        | travel d f (h::t) = (f(h, d+inc); out ", "; out sep; travel (d+inc) f t)
                        | travel d f nil = ()

                    fun start d f [a] = (f(a, 0))
                        | start d f (h::t) = (f(h, 0); out ", "; out sep; travel (d+inc) f t)
                        | start d f nil = ()
                in
                    (out "["; start ind f l; out "]")
                end

            fun nextLine(d) = (outln ""; indent d)

            fun Crate(A.Crate(shebang, innerAttrs, items), d) = 
                    (indent d; outln "Crate ("; Shebang(shebang, d+1); outln ","; 
                    outList (d+1) InnerAttribute innerAttrs true; outln ",";
                    outList (d+1) Item items true;
                    outln "\n)")
            and Shebang(A.Shebang(SOME s), d) = (indent d; out("Shebang ("^s^")"))
                | Shebang(A.Shebang(NONE), d) = (indent d; out("Shebang (NONE)"))
            and InnerAttribute(A.InnerAttribute(innerAttr), d) = 
                    (indent d; out "InnerAttribute("; MetaItem(innerAttr, 0); out ")")
            and OuterAttribute(A.OuterAttribute(outerAttr), d) = 
                    (indent d; out "OuterAttribute("; MetaItem(outerAttr, 0); out ")")
            and MetaItem(A.AttrName simplePath, d) = 
                    (indent d; out "AttrName ("; SimplePath(simplePath, d); out ")")
                | MetaItem (A.AttrKVPair(simplePath, literalExpression), d) = 
                    (indent d; out "AttrKVPair ("; SimplePath(simplePath, d); out "="; LiteralExpression(literalExpression, 0); out ")")
                | MetaItem (A.AttrSubs(simplePath, metaSeq), d) = 
                    (indent d; out "AttrSubs ("; SimplePath(simplePath, d); out "("; MetaSeq(metaSeq, d); out ")")
            and SimplePath (A.SimplePath(pathList), d) = 
                (indent d; out "SimplePath ("; outList (d) PathSeg pathList false; out ")")
            and PathSeg(A.IDPat(id), d) = (Identifer(id, d))
                | PathSeg(A.SelfPat, d) = (out ("self"))
                | PathSeg(A.CratePat, d) = (out ("crate"))
                | PathSeg(A.DCratePat, d) = (out ("$crate"))
                | PathSeg(A.SuperPat, d) = (out ("super"))
                | PathSeg(A.DefaultPat, d) = (out ("root"))
            and LiteralExpression (A.LiteralExpression s, d) = (indent d; out s)
            and MetaSeq((SOME metaSeq), d) = 
                    let 
                        fun helper((A.MetaSeq metaItemInnerList), d) = (indent d; outList (0) MetaItemInner metaItemInnerList true)
                    in 
                        helper(metaSeq, d)
                    end
                | MetaSeq ((NONE), d) = ()
            and MetaItemInner(A.MetaItem(metaItem), d) = 
                    (indent d; out "MetaItemInner ("; MetaItem(metaItem, d);out ")")
                | MetaItemInner(A.MetaLit(literalExpression), d) = 
                    (indent d; out "MetaItemInner ("; LiteralExpression(literalExpression, d) ;out ")")
            and Item(A.VisItemType(outerAttrs, visItem), d) = 
                    (indent d; out "VisItemType("; nextLine(d+1); VisItem(visItem, d+1); out ")")
                | Item(A.MarcoItemType(marcoItem), d) = 
                    (indent d; out "MarcoItemType("; out ")")
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
                        nextLine(d+1);
                        out "qualifier: ";
                        outList (d+1) FunctionQualifier qualifier false;
                        nextLine(d+1);
                        out "name: ";
                        Identifer(name, 0);
                        nextLine(d+1);
                        out "generics: ";
                        GenericsOption(generic, d+1);
                        nextLine(d+1);
                        out "param: ";
                        outList (d+1) FunctionParam params false;
                        nextLine(d+1);
                        out "ret: ";
                        TypeOption(ret, d+1);
                        nextLine(d+1);
                        out "wh: ";
                        WhereClauseOption(wh, d+1);
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
                | ItemType (_, d) =
                    (out "ItemType()")
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
                (indent d; out id)
            and FunctionQualifier(A.ConstFQ, d) = (indent d; out "const")
                | FunctionQualifier(A.UnsafeFQ, d) = (indent d; out "unsafe")
                | FunctionQualifier(A.ExternFQ(SOME(abi)), d) = (indent d; out "extern("; Abi(abi, 0); out ")")
                | FunctionQualifier(A.ExternFQ(NONE), d) = (indent d; out "extern")
            and Abi(A.Abi(s), d) = (indent d; out s)
            and Generics(A.Generics(generics), d) = 
                (out "Generics (";
                GenericParams(generics, d+1);
                outln ""; indent d; out ")"
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
                (indent d; outList (d+1) TypeParamBound tpbList false)
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
            and TypePath(A.TypePath, d) = out "type path"
            and Type(A.Type, d) = out "type"
            and ForLifetimes(A.ForLifetimes(ltps), d) =
                LifetimeParams(ltps, d)
            and FunctionParam(A.FunctionParam(pat, ty), d) =
                (Pattern(pat, d); out ":"; Type(ty, d))
            and Pattern(A.Pattern, d) = out "pattern"
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
        in
            Crate(ast, 0)
        end
end
