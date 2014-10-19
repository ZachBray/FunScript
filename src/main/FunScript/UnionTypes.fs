module internal FunScript.UnionTypes

open AST
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection

let private getCaseConsVars caseType = 
   Objects.getFields caseType
   |> Seq.map (fun (name, t) -> Var(name, typeof<obj>), t)
   |> Seq.toList

let private getCaseVars (uci:UnionCaseInfo) =
   let mi, t = Quote.getCaseMethodInfo uci
   if mi.GetParameters().Length = 0 then []
   else getCaseConsVars t

let private ignoredUnions =
   set [
      //typeof<obj list>.Name
      typeof<obj option>.Name
   ]

let private createConstructor uci compiler =
   let vars = getCaseVars uci |> List.map fst
   let this = Var("__this", typeof<obj>)
   vars, Block [  
      yield CopyThisToVar(this)
      yield Assign(PropertyGet(Reference this, "Tag"), Number(float uci.Tag))
      yield Assign(PropertyGet(Reference this, "_CaseName"), String uci.Name)
      for var in vars do yield Assign(PropertyGet(Reference this, var.Name), Reference var)
   ]

let private creation =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
      function
      | Patterns.NewUnionCase(uci, exprs) when ignoredUnions.Contains uci.DeclaringType.Name ->
         let decls, refs = 
            exprs 
            |> List.map (fun (Split(valDecl, valRef)) -> valDecl, valRef)
            |> List.unzip
         let propNames = getCaseVars uci |> List.map (fun (var,_) -> var.Name)
         let fields = 
            ("Tag", Number(float uci.Tag)) ::
            (List.zip propNames refs)
         // TODO: What about comparison?
         [ yield! decls |> Seq.concat 
           yield returnStategy.Return <| Object fields
         ]
      | Patterns.NewUnionCase(uci, exprs) ->
         let decls, refs = 
            exprs 
            |> List.map (fun (Split(valDecl, valRef)) -> valDecl, valRef)
            |> List.unzip
         let name = Reflection.getUnionCaseConstructorName compiler uci
         let cons = 
            compiler.DefineGlobal name (fun var -> 
               [Assign(Reference var, Lambda <| createConstructor uci compiler)])
         [ yield! decls |> Seq.concat 
           yield returnStategy.Return <| New(cons, refs)
         ]
      | _ -> []

let private matching =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
    function
    | Patterns.UnionCaseTest(Split(objDecl, objRef), uci) ->
        [ yield! objDecl
          yield returnStategy.Return <| BinaryOp(PropertyGet(objRef, "Tag"), "==", Number(float uci.Tag))
        ]
    | Patterns.Call(None, mi, [Split(objDecl, objRef)]) when 
            FSharpType.IsUnion mi.DeclaringType && 
            mi.Name = "GetTag" && 
            mi.ReturnType = typeof<int> ->
        [ yield! objDecl
          yield returnStategy.Return <| PropertyGet(objRef, "Tag")
        ]
    | Patterns.Call(None, mi, [arg]) as e when 
            mi.DeclaringType.Name.Contains "FSharpOption" &&
            mi.Name = "Some" ->
        let cases = FSharpType.GetUnionCases e.Type
        let someCase = cases.[1]
        compiler.Compile returnStategy (Expr.NewUnionCase(someCase, [arg]))
    | Patterns.Call(None, mi, []) as e when 
            mi.DeclaringType.Name.Contains "FSharpOption" &&
            mi.Name = "get_None" ->
        let cases = FSharpType.GetUnionCases e.Type
        let noneCase = cases.[0]
        compiler.Compile returnStategy (Expr.NewUnionCase(noneCase, []))
    | Patterns.PropertyGet(None, pi, []) as e when 
        pi.DeclaringType.Name.Contains "FSharpOption" &&
        pi.Name = "None" ->
        let cases = FSharpType.GetUnionCases e.Type
        let noneCase = cases.[0]
        compiler.Compile returnStategy (Expr.NewUnionCase(noneCase, []))
    | _ -> []

let components = [ 
   creation
   matching
]