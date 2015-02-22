module internal FunScript.RecordTypes

open AST
open Quote
open Microsoft.FSharp.Quotations
open System.Reflection


let private createConstructor recType compiler =
   let vars = Reflection.getRecordVars recType
   let this = Var("__this", typeof<obj>)
   vars, Block [  
      yield CopyThisToVar(this)
      for var in vars do yield Assign(PropertyGet(Reference this, var.Name), Reference var)
   ]

let private creation =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      function
      | Patterns.NewRecord(recType, exprs) ->
         let decls, refs = Reflection.getDeclarationAndReferences (|Split|) exprs
         if recType.Name = typeof<Ref<obj>>.Name then
            let propNames = Reflection.getRecordVars recType |> List.map (fun v -> v.Name)
            let fields = List.zip propNames refs
            [ yield! decls |> Seq.concat 
              yield returnStrategy.Return <| Object fields ]
         else
            let cons = Reflection.getRecordConstructorVar compiler recType
            [ yield! decls |> Seq.concat 
              yield returnStrategy.Return <| New(cons, refs) ]
      | _ -> []

let components = [ creation ]