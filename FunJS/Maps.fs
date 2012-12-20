module internal FunJS.Maps

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
         ExpressionReplacer.createUnsafe 
            <@ fun (xs:Map<_,_>) -> xs.Add @> 
            <@ fun (xs:Core.Map.Map<_,_>) -> xs.Add @>
         ExpressionReplacer.createUnsafe 
            <@ fun (xs:Map<_,_>) -> xs.ContainsKey @> 
            <@ fun (xs:Core.Map.Map<_,_>) -> xs.ContainsKey @>
         ExpressionReplacer.createUnsafe 
            <@ fun (xs:Map<_,_>) -> xs.Count @> 
            <@ fun (xs:Core.Map.Map<_,_>) -> xs.Count @>
         ExpressionReplacer.createUnsafe 
            <@ fun (xs:Map<_,_>) -> xs.IsEmpty @> 
            <@ fun (xs:Core.Map.Map<_,_>) -> xs.IsEmpty @>
         ExpressionReplacer.createUnsafe 
            <@ fun (xs:Map<_,_>) -> xs.TryFind @> 
            <@ fun (xs:Core.Map.Map<_,_>) -> xs.TryFind @>
         ExpressionReplacer.createUnsafe 
            <@ fun (xs:Map<_,_>) k -> xs.[k] @> 
            <@ fun (xs:Core.Map.Map<_,_>) k -> xs.[k] @>
         ExpressionReplacer.createUnsafe 
            <@ fun (xs:Map<_,_>) -> xs.Remove @> 
            <@ fun (xs:Core.Map.Map<_,_>) -> xs.Remove @>
         ExpressionReplacer.createUnsafe <@ fun xs -> set xs @> <@ fun xs -> FunJS.Core.Set.ofSeq xs @>
      ]
      ExpressionReplacer.createModuleMapping
         "FSharp.Core" "Microsoft.FSharp.Collections.MapModule"
         "FunJS" "FunJS.Core.Map"
   ] |> List.concat