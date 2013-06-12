module internal FunScript.Maps

open AST
open System.Collections
open System.Collections.Generic
open Microsoft.FSharp.Quotations


let components = 
   [
      [
         ExpressionReplacer.createUnsafe 
            <@ fun (kvp:KeyValuePair<_,_>) -> kvp.Key @> 
            <@ fun (kvp:Core.LanguagePrimitives.KeyValuePair<_,_>) -> kvp.Key @> 
         ExpressionReplacer.createUnsafe 
            <@ fun (kvp:KeyValuePair<_,_>) -> kvp.Value @> 
            <@ fun (kvp:Core.LanguagePrimitives.KeyValuePair<_,_>) -> kvp.Value @> 
         ExpressionReplacer.createUnsafe 
            <@ fun (xs:_ seq) -> Map(xs) @> 
            <@ fun (xs:_ seq) -> Core.Map.ofSeq @>
      ]

      ExpressionReplacer.createTypeMethodMappings
         typeof<Microsoft.FSharp.Collections.Map<_,_>>
         typeof<Core.Map.Map<_,_>>

      ExpressionReplacer.createModuleMapping
         "FSharp.Core" "Microsoft.FSharp.Collections.MapModule"
         "FunScript" "FunScript.Core.Map"
   ] |> List.concat