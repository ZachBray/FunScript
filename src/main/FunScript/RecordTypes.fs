module internal FunScript.RecordTypes

open AST
open Quote
open Microsoft.FSharp.Quotations
open System.Reflection

let private getRecordVars recType =
   Objects.getFields recType
   |> Seq.map fst
   |> Seq.map (fun name -> Var(name, typeof<obj>))
   |> Seq.toList

let private createConstructor recType compiler =
   let vars = getRecordVars recType
   let this = Var("__this", typeof<obj>)
   vars, Block [  
      yield CopyThisToVar(this)
      for var in vars do yield Assign(PropertyGet(Reference this, var.Name), Reference var)
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
         let name = Reflection.getRecordConstructorName compiler recType
         let cons = 
            compiler.DefineGlobal recType name (fun var -> 
               [Assign(Reference var, Lambda <| createConstructor recType compiler)])
         [ yield! decls |> Seq.concat 
           yield returnStategy.Return <| New(cons, refs)
         ]
      | _ -> []

let components = [ creation ]