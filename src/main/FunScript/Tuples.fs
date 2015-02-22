module internal FunScript.Tuples

open AST
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection

let itemsPropName = "Items"
let getItem i ref = IndexGet(PropertyGet(ref, itemsPropName), Number(float i))

let private creation =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
      function
      | Patterns.NewTuple(exprs) ->
         let decls, refs = 
            exprs 
            |> List.map (fun (Split(valDecl, valRef)) -> valDecl, valRef)
            |> List.unzip
         let typeArgs = exprs |> List.map (fun x -> x.Type)
         let cons = Reflection.getTupleConstructorVar compiler typeArgs
         [  yield! decls |> Seq.concat 
            yield returnStategy.Return <| New(cons, refs) ]
      | _ -> []

let private getIndex =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.TupleGet(Split(valDecl, valRef), i) ->
         [ yield! valDecl
           yield returnStategy.Return (valRef |> getItem i)
         ]
      | _ -> []

let components = [ 
   creation
   getIndex
   CompilerComponent.unary <@ fst @> (fun arg -> arg |> getItem 0)
   CompilerComponent.unary <@ snd @> (fun arg -> arg |> getItem 1)
   CompilerComponent.unary 
      <@ Microsoft.FSharp.Reflection.FSharpValue.GetTupleFields @> 
      (fun arg -> PropertyGet(arg, itemsPropName))
   CompilerComponent.binary 
      <@ fun obj i -> Microsoft.FSharp.Reflection.FSharpValue.GetTupleField(obj, i) @> 
      (fun arg i -> IndexGet(PropertyGet(arg, itemsPropName), i))
]