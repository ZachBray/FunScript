module internal FunScript.UnionTypes

open AST
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection

let private ignoredUnions =
   set [
      //typeof<obj list>.Name
      typeof<obj option>.Name
   ]

let private creation =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      function
      | Patterns.NewUnionCase(uci, exprs) ->
        let decls, refs = Reflection.getDeclarationAndReferences (|Split|) exprs
        if ignoredUnions.Contains uci.DeclaringType.Name then
            let propNames = Reflection.getCaseVars uci |> List.map (fun (var,_) -> var.Name)
            let fields = 
                ("Tag", Number(float uci.Tag)) ::
                (List.zip propNames refs)
            // TODO: What about comparison?
            [ yield! decls |> Seq.concat 
              yield returnStrategy.Return <| Object fields ]
        else
            let cons = Reflection.getUnionCaseConstructorVar compiler uci
            [ yield! decls |> Seq.concat 
              yield returnStrategy.Return <| New(cons, refs) ]
      | _ -> []

let private matching =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
    function
    | Patterns.UnionCaseTest(Split(objDecl, objRef), uci) ->
        [ yield! objDecl
          yield returnStrategy.Return <| BinaryOp(PropertyGet(objRef, "Tag"), "==", Number(float uci.Tag))
        ]
    | Patterns.Call(None, mi, [Split(objDecl, objRef)]) when 
            FSharpType.IsUnion mi.DeclaringType && 
            mi.Name = "GetTag" && 
            mi.ReturnType = typeof<int> ->
        [ yield! objDecl
          yield returnStrategy.Return <| PropertyGet(objRef, "Tag")
        ]
    | Patterns.Call(None, mi, [arg]) as e when 
            mi.DeclaringType.Name.Contains "FSharpOption" &&
            mi.Name = "Some" ->
        let cases = FSharpType.GetUnionCases e.Type
        let someCase = cases.[1]
        compiler.Compile returnStrategy (Expr.NewUnionCase(someCase, [arg]))
    | Patterns.Call(None, mi, []) as e when 
            mi.DeclaringType.Name.Contains "FSharpOption" &&
            mi.Name = "get_None" ->
        let cases = FSharpType.GetUnionCases e.Type
        let noneCase = cases.[0]
        compiler.Compile returnStrategy (Expr.NewUnionCase(noneCase, []))
    | Patterns.PropertyGet(None, pi, []) as e when 
        pi.DeclaringType.Name.Contains "FSharpOption" &&
        pi.Name = "None" ->
        let cases = FSharpType.GetUnionCases e.Type
        let noneCase = cases.[0]
        compiler.Compile returnStrategy (Expr.NewUnionCase(noneCase, []))
    | _ -> []

let components = [ 
   creation
   matching
]