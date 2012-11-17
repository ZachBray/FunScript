[<FunJS.JS>]
module internal FunJS.Lists

open AST
open Microsoft.FSharp.Quotations

type 'a ListRepresentation = 
   | Empty
   | Cons of 'a * 'a list

   member list.GetEnumerator() =
      let xs = Core.Seq.OfList(unbox list)
      xs.GetEnumerator()

let components = 
   [
      [
//         ExpressionReplacer.create <@ fun x y -> x::y @> <@ Cons @>
//         ExpressionReplacer.create <@ [] @> <@ Empty @>
         ExpressionReplacer.create <@ fun (xs:_ list) -> xs.Head @> <@ Core.List.Head @>
         ExpressionReplacer.create <@ fun (xs:_ list) -> xs.Tail @> <@ Core.List.Tail @>
         ExpressionReplacer.create <@ List.Cons @> <@ Core.List.Cons @>
         ExpressionReplacer.create <@ (@) @> <@ Core.List.Append @>
         ExpressionReplacer.create <@ List.toSeq @> <@ Core.Seq.OfList @>
         ExpressionReplacer.create <@ List.ofSeq @> <@ Core.Seq.ToList @>
         ExpressionReplacer.create <@ List.Empty @> <@ Core.List.Empty @>
         ExpressionReplacer.create <@ fun (xs:_ list) -> xs.IsEmpty @> <@ Core.List.IsEmpty @>
         ExpressionReplacer.create <@ fun (xs:_ list) i -> xs.Item i @> <@ Core.List.Get @>
         ExpressionReplacer.create <@ fun (xs:_ list) -> xs.Length @> <@ Core.List.Length @>
      ]
      ExpressionReplacer.createModuleMapping 
         "FSharp.Core" "Microsoft.FSharp.Collections.ListModule"
         "FunJS" "FunJS.Core.List"
   ] |> List.concat