module internal FunScript.RecordTypes

open AST
open Quote
open Microsoft.FSharp.Quotations
open System.Reflection

let genComparisonFunc t =
   let fields = Objects.getFields t
   let that = Var("that", typeof<obj>)
   let diff = Var("diff", typeof<obj>)
      
   let body =
      List.foldBack (fun (name, t) acc ->
         let thisField = PropertyGet(This, name)
         let thatField = PropertyGet(Reference that, name)
         let compareExpr = Comparison.compareCall thisField thatField
         [  Assign(Reference diff, compareExpr)
            IfThenElse(
               BinaryOp(Reference diff, "!=", Number 0.),
               Block [ Return <| Reference diff ],
               Block acc)
         ]) fields [ Return <| Number 0. ]
      
   Lambda(
      [that],
      Block <| Declare [diff] :: body
   )

let genComparisonMethods t =
    // TODO: What about overriden comparability
    let func = genComparisonFunc t
    [ "CompareTo", func ]

let private getRecordVars recType =
   Objects.getFields recType
   |> Seq.map fst
   |> Seq.map (fun name -> Var(name, typeof<obj>))
   |> Seq.toList

let private createConstructor recType compiler =
   let vars = getRecordVars recType
   vars, Block [  
      for var in vars do yield Assign(PropertyGet(This, var.Name), Reference var)
   ]

let private creation =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
      function
      | Patterns.NewRecord(recType, exprs) when recType.Name = typeof<Ref<obj>>.Name ->
         let decls, refs = 
            exprs 
            |> List.map (fun (Split(valDecl, valRef)) -> valDecl, valRef)
            |> List.unzip
         let propNames = getRecordVars recType |> List.map (fun v -> v.Name)
         let fields = List.zip propNames refs
         [  yield! decls |> Seq.concat 
            yield returnStategy.Return <| Object fields
         ]
      | PatternsExt.NewRecord(recType, exprs) ->
         let decls, refs = 
            exprs 
            |> List.map (fun (Split(valDecl, valRef)) -> valDecl, valRef)
            |> List.unzip
         let ci = 
            recType.GetConstructors(
               BindingFlags.Public ||| 
               BindingFlags.NonPublic ||| 
               BindingFlags.Instance).[0]
         let name = JavaScriptNameMapper.mapMethod ci
         let cons = 
            compiler.DefineGlobal name (fun var -> 
               [ 
                  yield Assign(Reference var, Lambda <| createConstructor recType compiler) 
                  let methods = compiler |> Objects.genInstanceMethods recType
                  let comparisonMethods = genComparisonMethods recType
                  let proto = PropertyGet(Reference var, "prototype")
                  for name, lambda in Seq.append methods comparisonMethods do
                     yield Assign(PropertyGet(proto, name), lambda)
               ]
            )
         [ yield! decls |> Seq.concat 
           yield returnStategy.Return <| New(cons.Name, refs)
         ]
      | _ -> []

let components = [ creation ]