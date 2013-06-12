module internal FunScript.Sets

open AST
open System.Collections
open Microsoft.FSharp.Quotations


let components = 
   [
      [
//         ExpressionReplacer.createUnsafe
//            <@ fun xs ys -> Set.op_Addition(xs, ys) @>
//            <@ fun xs ys -> Core.Set.Set<_>.op_Addition(xs, ys) @>
//         ExpressionReplacer.createUnsafe
//            <@ fun xs ys -> Set.op_Subtraction(xs, ys) @>
//            <@ fun xs ys -> Core.Set.Set<_>.op_Subtraction(xs, ys) @>
//         ExpressionReplacer.createUnsafe 
//            <@ fun (xs:_ Set) -> xs.Add @> 
//            <@ fun (xs:_ Core.Set.Set) -> xs.Add @>
//         ExpressionReplacer.createUnsafe 
//            <@ fun (xs:_ Set) -> xs.Contains @> 
//            <@ fun (xs:_ Core.Set.Set) -> xs.Contains @>
//         ExpressionReplacer.createUnsafe 
//            <@ fun (xs:_ Set) -> xs.Count @> 
//            <@ fun (xs:_ Core.Set.Set) -> xs.Count @>
//         ExpressionReplacer.createUnsafe 
//            <@ fun (xs:_ Set) -> xs.IsEmpty @> 
//            <@ fun (xs:_ Core.Set.Set) -> xs.IsEmpty @>
//         ExpressionReplacer.createUnsafe 
//            <@ fun (xs:_ Set) -> xs.IsProperSubsetOf @> 
//            <@ fun (xs:_ Core.Set.Set) -> xs.IsProperSubsetOf @>
//         ExpressionReplacer.createUnsafe 
//            <@ fun (xs:_ Set) -> xs.IsProperSupersetOf @> 
//            <@ fun (xs:_ Core.Set.Set) -> xs.IsProperSupersetOf @>
//         ExpressionReplacer.createUnsafe 
//            <@ fun (xs:_ Set) -> xs.IsSubsetOf @> 
//            <@ fun (xs:_ Core.Set.Set) -> xs.IsSubsetOf @>
//         ExpressionReplacer.createUnsafe 
//            <@ fun (xs:_ Set) -> xs.IsSupersetOf @> 
//            <@ fun (xs:_ Core.Set.Set) -> xs.IsSupersetOf @>
//         ExpressionReplacer.createUnsafe 
//            <@ fun (xs:_ Set) -> xs.MaximumElement @> 
//            <@ fun (xs:_ Core.Set.Set) -> xs.MaximumElement @>
//         ExpressionReplacer.createUnsafe 
//            <@ fun (xs:_ Set) -> xs.MinimumElement @> 
//            <@ fun (xs:_ Core.Set.Set) -> xs.MinimumElement @>
//         ExpressionReplacer.createUnsafe 
//            <@ fun (xs:_ Set) -> xs.Remove @> 
//            <@ fun (xs:_ Core.Set.Set) -> xs.Remove @>
         ExpressionReplacer.createUnsafe <@ fun xs -> set xs @> <@ fun xs -> FunScript.Core.Set.ofSeq xs @>
      ]

      ExpressionReplacer.createTypeMethodMappings
         typeof<Microsoft.FSharp.Collections.Set<_>>
         typeof<Core.Set.Set<_>>

      ExpressionReplacer.createModuleMapping
         "FSharp.Core" "Microsoft.FSharp.Collections.SetModule"
         "FunScript" "FunScript.Core.Set"
   ] |> List.concat