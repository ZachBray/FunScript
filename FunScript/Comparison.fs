module internal FunScript.Comparison

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

let private isNumericType (t:System.Type) =
   Reflection.primitiveTypes.Contains t.FullName

let private numericInfix op _ lhsT lhs rhsT rhs =
   if isNumericType lhsT && isNumericType rhsT then
      Some(BinaryOp(lhs, op, rhs))
   else
      Some(BinaryOp(compareCall lhs rhs, op, Number 0.))

let private compareTyped =
   CompilerComponent.generateArity <@ compare @> (fun mi (|Split|) ->
      function
      | [Split(declLHS, refLHS) as exprA; Split(declRHS, refRHS) as exprB] ->
         if isNumericType exprA.Type && isNumericType exprB.Type then
            let diff = Var("diff", typeof<obj>)
            let assignment =
               IfThenElse(
                  BinaryOp(refLHS, "<", refRHS), 
                  Block [ Assign(Reference diff, Number -1.) ],
                  Block [
                     IfThenElse(
                        BinaryOp(refLHS, "==", refRHS),
                        Block [ Assign(Reference diff, Number 0.) ],
                        Block [ Assign(Reference diff, Number 1.) ])
                  ])
            Some([declLHS; declRHS; [Declare [diff]; assignment]], Reference diff)
         else Some([declLHS; declRHS], compareCall refLHS refRHS)  
      | _ -> None)
   

let components = [ 
   CompilerComponent.binaryTyped <@ (=) @> (numericInfix "==")
   CompilerComponent.binaryTyped <@ (<>) @> (numericInfix "!=")
   CompilerComponent.binaryTyped <@ (>) @> (numericInfix ">")
   CompilerComponent.binaryTyped <@ (>=) @> (numericInfix ">=")
   CompilerComponent.binaryTyped <@ (<) @> (numericInfix "<")
   CompilerComponent.binaryTyped <@ (<=) @> (numericInfix "<=")
   CompilerComponent.binary <@ compare @> compareCall
   CompilerComponent.binary <@ Unchecked.compare @> compareCall
   ExpressionReplacer.create <@ min @> <@ Replacements.min @>
   ExpressionReplacer.create <@ max @> <@ Replacements.max @>
]