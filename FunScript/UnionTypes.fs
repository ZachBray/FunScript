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

let genComparisonFunc (uci : UnionCaseInfo) =
   
   let that = Var("that", typeof<obj>)
   let diff = Var("diff", typeof<obj>)
   
   
   let typeMap =
      if uci.DeclaringType.IsGenericType then
         let genericTypeArgs = Reflection.getGenericTypeArgs (uci.DeclaringType.GetGenericTypeDefinition())
         let realTypeArgs = Reflection.getGenericTypeArgs uci.DeclaringType
         Array.map2 (fun (genericType : System.Type) realType ->
            genericType.Name, realType) genericTypeArgs realTypeArgs
         |> Map.ofArray
      else Map.empty

   let compareThen (t : System.Type) name cont =
      let t =
         if t.IsGenericParameter then typeMap.[t.Name]
         else t
      let thisField = PropertyGet(This, name)
      let thatField = PropertyGet(Reference that, name)
      let decls, compareExpr = Comparison.compareCall t thisField t thatField |> Option.get
      [  yield! decls
         yield Assign(Reference diff, compareExpr)
         yield IfThenElse(
            BinaryOp(Reference diff, "!=", Number 0.),
            Block [ Return <| Reference diff ],
            Block cont)
      ]
   let body =
      compareThen typeof<string> "Tag" (
         List.foldBack (fun (var : Var, t) acc ->
            compareThen t var.Name acc
         ) (getCaseVars uci) [ Return <| Number 0. ]
      )
      
   Lambda(
      [that],
      Block <| DeclareAndAssign(diff, Number 0.) :: body
   )

let genComparisonMethods t =
    // TODO: What about overriden comparability
    let func = genComparisonFunc t
    [ "CompareTo", func ]

let private ignoredUnions =
   set [
      //typeof<obj list>.Name
      typeof<obj option>.Name
   ]

let private createConstructor uci compiler =
   let vars = getCaseVars uci |> List.map fst
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
         let propNames = getCaseVars uci |> List.map (fun (var,_) -> var.Name)
         let fields = 
            ("Tag", String uci.Name) ::
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
         let typeArgs = Reflection.getGenericTypeArgs uci.DeclaringType
         let specialization = Reflection.getSpecializationString compiler typeArgs
         let name = JavaScriptNameMapper.mapType uci.DeclaringType + "_" + uci.Name + specialization
         let cons = 
            compiler.DefineGlobal name (fun var -> 
               [ 
                  yield Assign(Reference var, Lambda <| createConstructor uci compiler)
                  let comparisonMethods = genComparisonMethods uci
                  let proto = PropertyGet(Reference var, "prototype")
                  for name, lambda in comparisonMethods do
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