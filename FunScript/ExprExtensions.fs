module internal FunScript.Expr

open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations

let memoize f =
    let cache = Dictionary(HashIdentity.Reference)
    fun x ->
        match cache.TryGetValue x with
        | true, v -> v
        | false, _ ->
            let v = f x
            cache.Add(x, v)
            v

let iter f expr =
   let rec iter expr =
      f expr
      match expr with
      | ExprShape.ShapeVar _ -> ()
      | ExprShape.ShapeLambda(_, expr) -> iter expr
      | ExprShape.ShapeCombination (_, exprs) ->
         exprs |> List.iter iter
   iter expr

let exists f expr =
   let rec exists expr =
      f expr ||
         match expr with
         | ExprShape.ShapeVar _ -> false
         | ExprShape.ShapeLambda(_, expr) -> exists expr
         | Patterns.Let(_, exprA, exprB) -> exists exprA || exists exprB
         | ExprShape.ShapeCombination (_, exprs) ->
            exprs |> List.exists exists
   exists expr

let tryGetReflectedDefinition : MethodBase -> Expr option =
    memoize Expr.TryGetReflectedDefinition