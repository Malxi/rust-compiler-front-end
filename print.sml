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

            fun outList d f l = 
                let
                    fun travel d f [a] = (f(a, d+1))
                        | travel d f (h::t) = (f(h, d+1); outln ", "; travel (d+1) f t)
                        | travel d f nil = ()

                    fun start d f [a] = (f(a, 0))
                        | start d f (h::t) = (f(h, 0); outln ","; travel (d+1) f t)
                        | start d f nil = ()
                in
                    (indent d; out "["; start d f l;out "]")
                end

            fun Crate(A.Crate(shebang, innerAttrs), d) = 
                    (indent d; outln "Crate ("; Shebang(shebang, d+1); outln ","; outList (d+1) InnerAttribute innerAttrs; outln "\n)")
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
                (indent d; out "SimplePath ("; outList (0) Path pathList; out ")")
            and Path(s, d) = (out (s))
            and LiteralExpression (A.LiteralExpression s, d) = (indent d; out s)
            and MetaSeq((SOME metaSeq), d) = 
                    let 
                        fun helper((A.MetaSeq metaItemInnerList), d) = (indent d; outList (0) MetaItemInner metaItemInnerList)
                    in 
                        helper(metaSeq, d)
                    end
                | MetaSeq ((NONE), d) = ()
            and MetaItemInner(A.MetaItem(metaItem), d) = 
                    (indent d; out "MetaItemInner ("; MetaItem(metaItem, d);out ")")
                | MetaItemInner(A.MetaLit(literalExpression), d) = 
                    (indent d; out "MetaItemInner ("; LiteralExpression(literalExpression, d) ;out ")")

        in
            Crate(ast, 0)
        end
end
