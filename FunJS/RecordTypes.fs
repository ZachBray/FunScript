module internal FunJS.RecordTypes

open AST
open Microsoft.FSharp.Quotations

let private creation =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.NewRecord(recType, exprs) ->
         let propNames = 
            let props = 
               recType.GetProperties() 
               |> Seq.map (fun pi -> (pi.Name.ToLower(), pi.PropertyType.GUID), pi)
               |> Map.ofSeq
            recType.GetConstructors().[0].GetParameters()
            |> Seq.choose (fun pi ->
               props |> Map.tryFind (pi.Name.ToLower(), pi.ParameterType.GUID))
            |> Seq.map (fun pi -> pi.Name)
            |> Seq.toList
            
         let decls, refs = 
            exprs 
            |> List.map (fun (Split(valDecl, valRef)) -> valDecl, valRef)
            |> List.unzip
         let fields = List.zip propNames refs
         [ yield! decls |> Seq.concat 
           yield returnStategy.Return <| Object fields
         ]
      | _ -> []

let components = [ 
   creation
]