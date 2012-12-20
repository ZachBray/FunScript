module internal FunJS.Lists

open AST
open Microsoft.FSharp.Quotations

let components = 
   [
      [
         ExpressionReplacer.createUnsafe <@ fun (xs:_ list) -> xs.Head @> <@ Core.List.Head @>
         ExpressionReplacer.createUnsafe <@ fun (xs:_ list) -> xs.Tail @> <@ Core.List.Tail @>
         ExpressionReplacer.createUnsafe <@ [] @> <@ Core.List.Empty @>
         ExpressionReplacer.createUnsafe <@ fun x xs -> x::xs @> <@ Core.List.CreateCons @>
         ExpressionReplacer.createUnsafe <@ List.Cons @> <@ Core.List.CreateCons @>
         ExpressionReplacer.createUnsafe <@ (@) @> <@ Core.List.Append @>
         ExpressionReplacer.create <@ List.toSeq @> <@ Core.Seq.OfList @>
         ExpressionReplacer.create <@ List.ofSeq @> <@ Core.Seq.ToList @>
         ExpressionReplacer.createUnsafe <@ List.Empty @> <@ Core.List.Empty @>
         ExpressionReplacer.createUnsafe <@ fun (xs:_ list) -> xs.IsEmpty @> <@ Core.List.IsEmpty @>
         ExpressionReplacer.createUnsafe <@ fun (xs:_ list) i -> xs.Item i @> <@ Core.List.Get @>
         ExpressionReplacer.createUnsafe <@ fun (xs:_ list) -> xs.Length @> <@ Core.List.Length @>
      ]
      ExpressionReplacer.createModuleMapping 
         "FSharp.Core" "Microsoft.FSharp.Collections.ListModule"
         "FunJS" "FunJS.Core.List"
   ] |> List.concat