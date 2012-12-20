module internal FunJS.Expr

open Microsoft.FSharp.Quotations

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