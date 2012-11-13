module internal FunJS.Comparison

open AST
open Microsoft.FSharp.Quotations

[<JS>]
module Replacements =
   [<Inline>]
   let min x y = if y < x then y else x

   [<Inline>]
   let max x y = if y > x then y else x

let components = [ 
   CompilerComponent.binaryOp <@ (=) @> "=="
   CompilerComponent.binaryOp <@ (<>) @> "!="
   CompilerComponent.binaryOp <@ (>) @> ">"
   CompilerComponent.binaryOp <@ (>=) @> ">="
   CompilerComponent.binaryOp <@ (<) @> "<"
   CompilerComponent.binaryOp <@ (<=) @> "<="
   ExpressionReplacer.create <@ min @> <@ Replacements.min @>
   ExpressionReplacer.create <@ max @> <@ Replacements.max @>
]