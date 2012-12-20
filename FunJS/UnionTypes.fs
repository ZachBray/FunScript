module internal FunJS.UnionTypes

open AST
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection

let private getCaseConsVars caseType = 
   Objects.getFields caseType
   |> Seq.map fst
   |> Seq.map (fun name -> Var(name, typeof<obj>))
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
   let vars = getCaseVars uci
   vars, Block [
      yield Assign(PropertyGet(This, "Tag"), String uci.Name)
      for var in vars do yield Assign(PropertyGet(This, var.Name), Reference var)
   ]

let private creation =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
      function
      | Patterns.NewUnionCase(uci, exprs) when ignoredUnions.Contains uci.DeclaringType.Name ->
         let decls, refs = 
            exprs 
            |> List.map (fun (Split(valDecl, valRef)) -> valDecl, valRef)
            |> List.unzip
         let propNames = getCaseVars uci |> List.map (fun var -> var.Name)
         let fields = 
            ("Tag", String uci.Name) ::
            (List.zip propNames refs)
         [ yield! decls |> Seq.concat 
           yield returnStategy.Return <| Object fields
         ]
      | Patterns.NewUnionCase(uci, exprs) ->
         let decls, refs = 
            exprs 
            |> List.map (fun (Split(valDecl, valRef)) -> valDecl, valRef)
            |> List.unzip
         let name = JavaScriptNameMapper.mapType uci.DeclaringType + "_" + uci.Name
         let cons = 
            compiler.DefineGlobal name (fun var -> 
               [ 
                  yield Assign(Reference var, Lambda <| createConstructor uci compiler)
                  let methods = 
                     compiler |> Objects.genInstanceMethods uci.DeclaringType
                  let proto = PropertyGet(Reference var, "prototype")
                  for name, lambda in methods do
                     yield Assign(PropertyGet(proto, name), lambda)
               ]                    
            )
         [ yield! decls |> Seq.concat 
           yield returnStategy.Return <| New(cons.Name, refs)
         ]
      | _ -> []

let private matching =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.UnionCaseTest(Split(objDecl, objRef), uci) ->
         [ yield! objDecl
           yield returnStategy.Return <| BinaryOp(PropertyGet(objRef, "Tag"), "==", String(uci.Name))
         ]
      | _ -> []

let components = [ 
   creation
   matching
]