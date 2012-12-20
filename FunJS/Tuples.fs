module internal FunJS.Tuples

open AST
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection

let private creation =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.NewTuple(exprs) ->
         let decls, refs = 
            exprs 
            |> List.map (fun (Split(valDecl, valRef)) -> valDecl, valRef)
            |> List.unzip
         let propNames =
            match refs.Length with
            | 1 -> [ "Value" ]
            | n -> refs |> List.mapi (fun i _ -> sprintf "Item%i" (i + 1))
         let fields = List.zip propNames refs
         let fieldTypes = exprs |> List.map (fun e -> e.Type) |> List.toArray
         let tupleType = FSharpType.MakeTupleType fieldTypes
         let compareFunc = Objects.genComparisonFunc tupleType
         let fields = ("CompareTo", compareFunc)::fields
         [  yield! decls |> Seq.concat 
            yield returnStategy.Return <| Object fields
         ]
      | _ -> []

let private getIndex =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.TupleGet(Split(valDecl, valRef), i) ->
         [ yield! valDecl
           yield returnStategy.Return <| PropertyGet(valRef, sprintf "Item%i" (i+1))
         ]
      | _ -> []

let components = [ 
   creation
   getIndex
   CompilerComponent.unary <@ fst @> (fun arg -> PropertyGet(arg, "Item1"))
   CompilerComponent.unary <@ snd @> (fun arg -> PropertyGet(arg, "Item2"))
]