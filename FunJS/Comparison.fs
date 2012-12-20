module internal FunJS.Comparison

open AST
open Microsoft.FSharp.Quotations
open System

[<JS; Inline>]
module Replacements =
   
   let min x y = if y < x then y else x

   let max x y = if y > x then y else x

let compareCall exprA exprB =
   Apply(PropertyGet(exprA, "CompareTo"), [exprB])

let compareOp op exprA exprB =
   BinaryOp(compareCall exprA exprB, op, Number 0.)

let components = [ 
   CompilerComponent.binary <@ (=) @> (compareOp "==")
   CompilerComponent.binary <@ (<>) @> (compareOp "!=")
   CompilerComponent.binary <@ (>) @> (compareOp ">")
   CompilerComponent.binary <@ (>=) @> (compareOp ">=")
   CompilerComponent.binary <@ (<) @> (compareOp "<")
   CompilerComponent.binary <@ (<=) @> (compareOp "<=")
   CompilerComponent.binary <@ compare @> compareCall
   CompilerComponent.binary <@ Unchecked.compare @> compareCall
   ExpressionReplacer.create <@ min @> <@ Replacements.min @>
   ExpressionReplacer.create <@ max @> <@ Replacements.max @>
]