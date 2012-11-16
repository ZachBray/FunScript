module internal FunJS.Seqs

open AST
open Microsoft.FSharp.Quotations

//TODO: Replace with a lenient expression replacer but first need to come up with
// a solution for matching expressions as equal when the generic arguments differ slightly.
// Perhaps a call to a generic method through reflection? (Sloooooow!!)
let private toSeq =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      function
      | Patterns.Coerce(expr, t) 
         when t.Name = typeof<seq<obj>>.Name &&
              expr.Type.Name = typeof<list<obj>>.Name ->
         let mi = CompilerComponent.Quote.toMethodInfo <@@ Core.Seq.OfList @@>
         let specificMi = mi.MakeGenericMethod(expr.Type.GetGenericArguments())
         compiler.Compile returnStrategy (Expr.Call(specificMi, [expr]))
      | Patterns.Coerce(expr, t) 
         when t.Name = typeof<seq<obj>>.Name &&
              expr.Type.IsArray ->
         let mi = CompilerComponent.Quote.toMethodInfo <@@ Core.Seq.OfArray @@>
         let specificMi = mi.MakeGenericMethod [|expr.Type.GetElementType()|]
         compiler.Compile returnStrategy (Expr.Call(specificMi, [expr]))
      | _ -> []

let components = 
   [
      [
         toSeq
      ]
      ExpressionReplacer.createModuleMapping 
         "FSharp.Core" "Microsoft.FSharp.Collections.SeqModule"
         "FunJS" "FunJS.Core.Seq"
   ] |> List.concat
