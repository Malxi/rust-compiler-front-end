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
                    val sep = if r = 1 then "\n" else ""
                    fun travel d f [a] = (f(a, d+r))
                        | travel d f (h::t) = (f(h, d+r); out ", "; out sep; travel (d+r) f t)
                        | travel d f nil = ()

                    fun start d f [a] = (f(a, 0))
                        | start d f (h::t) = (f(h, 0); out ", "; out sep; travel (d+r) f t)
                        | start d f nil = ()
                in
                    (indent d; out "["; start d f l; out "]")
                end

            fun Crate(A.Crate(shebang, innerAttrs, items), d) = 
                    (indent d; outln "Crate ("; Shebang(shebang, d+1); outln ","; 
                    outList (d+1) InnerAttribute innerAttrs 1; outln ",";
                    outList (d+1) Item items 1;
                    outln "\n)")
            and Shebang(A.Shebang(SOME s), d) = (indent d; out("Shebang ("^s^")"))
                | Shebang(A.Shebang(NONE), d) = (indent d; out("Shebang (NONE)"))
            and InnerAttribute(A.InnerAttribute(innerAttr), d) = 
                    (indent d; out "InnerAttribute("; MetaItem(innerAttr, 0); out ")")
            and MetaItem(A.AttrName simplePath, d) = 
                    (indent d; out "AttrName ("; SimplePath(simplePath, d); out ")")
                | MetaItem (A.AttrKVPair(simplePath, literalExpression), d) = 
                    (indent d; out "AttrKVPair ("; SimplePath(simplePath, d); out "="; LiteralExpression(literalExpression, 0); out ")")
                | MetaItem (A.AttrSubs(simplePath, metaSeq), d) = 
                    (indent d; out "AttrSubs ("; SimplePath(simplePath, d); out "("; MetaSeq(metaSeq, d); out ")")
            and SimplePath (A.SimplePath(pathList), d) = 
                (indent d; out "SimplePath ("; outList (d) PathSeg pathList 0; out ")")
            and PathSeg(A.IDPat(id), d) = (Identifer(id, d))
                | PathSeg(A.SelfPat, d) = (out ("self"))
                | PathSeg(A.CratePat, d) = (out ("crate"))
                | PathSeg(A.DCratePat, d) = (out ("$crate"))
                | PathSeg(A.SuperPat, d) = (out ("super"))
                | PathSeg(A.DefaultPat, d) = (out ("root"))
            and LiteralExpression (A.LiteralExpression s, d) = (indent d; out s)
            and MetaSeq((SOME metaSeq), d) = 
                    let 
                        fun helper((A.MetaSeq metaItemInnerList), d) = (indent d; outList (0) MetaItemInner metaItemInnerList 1)
                    in 
                        helper(metaSeq, d)
                    end
                | MetaSeq ((NONE), d) = ()
            and MetaItemInner(A.MetaItem(metaItem), d) = 
                    (indent d; out "MetaItemInner ("; MetaItem(metaItem, d);out ")")
                | MetaItemInner(A.MetaLit(literalExpression), d) = 
                    (indent d; out "MetaItemInner ("; LiteralExpression(literalExpression, d) ;out ")")
            and Item(A.VisItemType(outerAttrs, visItem), d) = 
                    (indent d; out "VisItemType("; outln ""; VisItem(visItem, d+1); out ")")
                | Item(A.MarcoItemType(marcoItem), d) = 
                    (indent d; out "MarcoItemType("; out ")")
            and VisItem(A.VisItem(visibility, itemType), d) =
                (indent d; out "VisItem ("; Visibility(visibility, 0); outln ""; ItemType(itemType, d); out ")")
            and Visibility(A.DefaultVis, d) = (indent d; out "<default>")
                | Visibility(A.PubVis, d) = (indent d; out "<pub>")
                | Visibility(A.CrateVis, d) = (indent d; out "<crate>")
                | Visibility(A.SelfVis, d) = (indent d; out "<self>")
                | Visibility(A.SuperVis, d) = (indent d; out "<super>")
                | Visibility(A.InVis(vis), d) = (indent d; out "<in>"; SimplePath(vis, d))
            and ItemType(A.Module(id, SOME(mbd)), d) = 
                    (indent (d+1); 
                    out "Module "; Identifer(id, 0); outln ""; 
                    ModuleBody(mbd, d+1);
                    out ",")
                | ItemType(A.Module(id, NONE), d) = 
                    (indent d; out "Module "; Identifer(id, 0); out ",")
                | ItemType(A.UseDeclaration useDecl, d) =
                    (indent d; "UseDeclaration ("; UseTree(useDecl, d); out ")")
                | ItemType (_, d) =
                    (indent d; out "ItemType")
            and ModuleBody(A.ModuleBody(innerAttrs, items), d) = 
                (indent d; outln "Body {";
                outList (d+1) InnerAttribute innerAttrs 1;
                outln ",";
                outList (d+1) Item items 1;
                outln "";
                indent d; out "}")
            and UseTree(A.UseAll(SOME(simplePath)), d) = 
                    (indent d; out "UseAll ("; SimplePath(simplePath, d); out ")")
                | UseTree(A.UseAll(NONE), d) = 
                    (indent d; out "UseAll ("; out ")")
                | UseTree(A.UseList(SOME(simplePath), useTreeList), d) = 
                    (indent d; out "UseList ("; SimplePath(simplePath, d); outList (d) UseTree useTreeList 0; out ")")
                | UseTree(A.UseList(NONE, useTreeList), d) = 
                    (indent d; out "UseList ("; outList (d) UseTree useTreeList 0; out ")")
                | UseTree(A.UseAlias(simplePath, SOME(id)), d) = 
                    (indent d; out "UseAlias ("; SimplePath(simplePath, d); out " as "; Identifer(id, d); out ")")
                | UseTree(A.UseAlias(simplePath, NONE), d) = 
                    (indent d; out "UseAlias ("; SimplePath(simplePath, d); out ")")
            and Identifer(A.Identifer(id), d) = 
                (indent d; out id)
        in
            Crate(ast, 0)
        end
end
