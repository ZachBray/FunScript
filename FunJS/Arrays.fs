module internal FunJS.Arrays

open AST
open Microsoft.FSharp.Quotations

let private creation =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.NewArray(arrayType, exprs) ->
         let decls, refs = 
            exprs 
            |> List.map (fun (Split(valDecl, valRef)) -> valDecl, valRef)
            |> List.unzip
         [ yield! decls |> Seq.concat 
           yield returnStategy.Return <| Array refs
         ]
      | _ -> []

let private getIndex = 
   CompilerComponent.binary 
      <@ fun (xs:_ []) i -> xs.[i] @> 
      (fun array index -> IndexGet(array, index))

let private setIndex = 
   CompilerComponent.ternaryStatement
      <@ fun (xs:_ []) i v -> xs.[i] <- v @> 
      (fun array index value -> Assign(IndexGet(array, index), value))

let components = 
   [ 
      [
         creation
         getIndex
         setIndex
         ExpressionReplacer.createGetter <@ fun (xs:_ []) -> xs.Length @> <@ Core.Array.BoxedLength @>
         ExpressionReplacer.create <@ Array.toList @> <@ Core.List.OfArray @>
         ExpressionReplacer.create <@ Array.ofList @> <@ Core.List.ToArray @>
      ]
      ExpressionReplacer.createModuleMapping 
            "FSharp.Core" "Microsoft.FSharp.Collections.ArrayModule"
            "FunJS" "FunJS.Core.Array"
   ] |> List.concat