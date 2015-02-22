module internal FunScript.Tuples

open AST
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection

let itemsPropName = "Items"
let getItem i ref = IndexGet(PropertyGet(ref, itemsPropName), Number(float i))

let private creation =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      function
      | Patterns.NewTuple(exprs) ->
         let decls, refs = Reflection.getDeclarationAndReferences (|Split|) exprs
         let typeArgs = exprs |> List.map (fun x -> x.Type)
         let cons = Reflection.getTupleConstructorVar compiler typeArgs
         [  yield! decls |> Seq.concat 
            yield returnStrategy.Return <| New(cons, refs) ]
      | _ -> []

let private getIndex =
   CompilerComponent.create <| fun (|Split|) _ returnStrategy ->
      function
      | Patterns.TupleGet(Split(valDecl, valRef), i) ->
         [ yield! valDecl
           yield returnStrategy.Return (valRef |> getItem i)
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