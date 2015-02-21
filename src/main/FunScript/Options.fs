module internal FunScript.Options

open Microsoft.FSharp.Quotations

[<FunScript.JS>]
module Replacements =

    let createNullable x = x
    let hasValue x = not(obj.ReferenceEquals(x, null))
    let getValue x = x

    let createEmptyNullable =
       CompilerComponent.create <| fun split compiler returnStrategy ->
          function
          | Patterns.DefaultValue t when t.Name.StartsWith("Nullable") ->
            [ returnStrategy.Return <| AST.JSExpr.Null ]
          | _ -> []

let components = 
   [
      [
         ExpressionReplacer.create <@ fun (maybe:_ option) -> maybe.IsNone @> <@ FunScript.Core.Option.IsNone @>
         ExpressionReplacer.create <@ fun (maybe:_ option) -> maybe.IsSome @> <@ FunScript.Core.Option.IsSome @>
         ExpressionReplacer.createUnsafe <@ fun (maybe:_ option) -> maybe.Value @> <@ FunScript.Core.Option.GetValue @>
         
         Replacements.createEmptyNullable
         ExpressionReplacer.createUnsafe <@ fun x -> System.Nullable(x) @> <@ Replacements.createNullable @>
         ExpressionReplacer.createUnsafe <@ fun (x:_ System.Nullable) -> x.HasValue @> <@ Replacements.hasValue @>
         ExpressionReplacer.createUnsafe <@ fun (x:_ System.Nullable) -> x.Value @> <@ Replacements.getValue @>
      ]
      ExpressionReplacer.createModuleMapping
         "FSharp.Core" "Microsoft.FSharp.Core.OptionModule"
         "FunScript" "FunScript.Core.Option"

   ] |> List.concat