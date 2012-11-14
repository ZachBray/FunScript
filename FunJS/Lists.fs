module internal FunJS.Lists

let components = 
   [
      [
         ExpressionReplacer.createGetter <@ fun (xs:_ list) -> xs.Head @> <@ Core.List.Head @>
         ExpressionReplacer.createGetter <@ fun (xs:_ list) -> xs.Tail @> <@ Core.List.Tail @>
         ExpressionReplacer.create <@ List.Cons @> <@ Core.List.Cons @>
         ExpressionReplacer.create <@ (@) @> <@ Core.List.Append @>
         ExpressionReplacer.createGetter <@ List.Empty @> <@ Core.List.Empty @>
         ExpressionReplacer.createGetter <@ fun (xs:_ list) -> xs.IsEmpty @> <@ Core.List.IsEmpty @>
         ExpressionReplacer.createGetter <@ fun (xs:_ list) i -> xs.Item i @> <@ Core.List.Get @>
         ExpressionReplacer.createGetter <@ fun (xs:_ list) -> xs.Length @> <@ Core.List.Length @>
      ]
      ExpressionReplacer.createModuleMapping 
         "FSharp.Core" "Microsoft.FSharp.Collections.ListModule"
         "FunJS" "FunJS.Core.List"
   ] |> List.concat