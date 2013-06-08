module internal FunScript.Seqs

open AST
open System.Collections
open Microsoft.FSharp.Quotations

//TODO: Replace with a lenient expression replacer but first need to come up with
// a solution for matching expressions as equal when the generic arguments differ slightly.
// Perhaps a call to a generic method through reflection? (Sloooooow!!)
let private toSeq =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      function
      | Patterns.Coerce(expr, t) 
         when t.Name = typeof<seq<obj>>.Name || t.Name = typeof<IEnumerable>.Name ->
         match expr with
         | expr when expr.Type.IsArray ->
            let mi, _ = Quote.toMethodInfoFromLambdas <@@ Core.Seq.OfArray @@>
            let specificMi = mi.MakeGenericMethod [|expr.Type.GetElementType()|]
            compiler.Compile returnStrategy (Expr.Call(specificMi, [expr]))
         | expr when expr.Type.Name = typeof<list<obj>>.Name ->
            let mi, _ = Quote.toMethodInfoFromLambdas <@@ Core.Seq.OfList @@>
            let specificMi = mi.MakeGenericMethod(expr.Type.GetGenericArguments())
            compiler.Compile returnStrategy (Expr.Call(specificMi, [expr]))
         | _ -> []         
      | _ -> []

let components = 
   [
      [
         toSeq
         //ExpressionReplacer.create <@ seq @> <@ (fun xs -> xs :> seq<_>) @>
      ]
      ExpressionReplacer.createModuleMapping 
         "FSharp.Core" "Microsoft.FSharp.Collections.SeqModule"
         "FunScript" "FunScript.Core.Seq"
   ] |> List.concat
