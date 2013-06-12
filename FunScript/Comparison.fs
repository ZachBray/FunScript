module internal FunScript.Comparison

open AST
open Microsoft.FSharp.Quotations
open System

[<JS; Inline>]
module Replacements =
   
   let min x y = if y < x then y else x

   let max x y = if y > x then y else x

let private isPrimitiveType (t:System.Type) =
   Reflection.primitiveTypes.Contains t.FullName

type private TypeKind =
   | Primitive
   | Custom
   | Array of TypeKind

let rec private kindOf (t:System.Type) =
   if isPrimitiveType t then Primitive
   elif t.IsArray then 
      if t.GetArrayRank() <> 1 then
         failwith "Unsupported array rank."
      Array(kindOf (t.GetElementType()))
   else Custom

let private comparePrimitive refLHS refRHS =
   TernaryOp(
      BinaryOp(refLHS, "<", refRHS),
      "?",
      Number -1.,
      ":",
      TernaryOp(
         BinaryOp(refLHS, "==", refRHS),
         "?",
         Number 0.,
         ":",
         Number 1.))

let private compareCustom refLHS refRHS =
   Apply(PropertyGet(refLHS, "CompareTo"), [refRHS])

let rec private compareWithKind diff kind refLHS refRHS =
   match kind with
   | Primitive -> [Assign(Reference diff, comparePrimitive refLHS refRHS)]
   | Custom -> [Assign(Reference diff, compareCustom refLHS refRHS)]
   | Array innerKind ->
      let index = Var("i", typeof<obj>)
      let innerLHS = IndexGet(refLHS, Reference index)
      let innerRHS = IndexGet(refRHS, Reference index)
      let lengthLHS = PropertyGet(refLHS, "length")
      let lengthRHS = PropertyGet(refRHS, "length")
      let assignDiff = compareWithKind diff innerKind innerLHS innerRHS
      [
         DeclareAndAssign(index, Number 0.)
         WhileLoop(
            BinaryOp(
               BinaryOp(Reference diff, "==", Number 0.),
               "&&",
               BinaryOp(
                  BinaryOp(Reference index, "<", lengthLHS),
                  "&&",
                  BinaryOp(Reference index, "<", lengthRHS))),
            Block [
               yield! assignDiff
               yield Assign(Reference index, BinaryOp(Reference index, "+", Number 1.))  
            ])
         Assign(
            Reference diff,
            TernaryOp(
               BinaryOp(Reference diff, "==", Number 0.), 
               "?",
               BinaryOp(lengthLHS, "-", lengthRHS),
               ":",
               Reference diff))
      ]

let compareCall typeLHS refLHS typeRHS refRHS =
   let leftKind = kindOf typeLHS
   let rightKind = kindOf typeRHS
   if leftKind = rightKind then
      let diff = Var("diff", typeof<obj>)
      let declaration = DeclareAndAssign(diff, Number 0.)
      let assignment = compareWithKind diff leftKind refLHS refRHS
      Some(declaration :: assignment, Reference diff)
   else None

let private primitiveInfix op _ lhsT lhs rhsT rhs =
   match isPrimitiveType lhsT, isPrimitiveType rhsT with
   | true, true ->
      Some([], BinaryOp(lhs, op, rhs))
   | false, false ->
      match compareCall lhsT lhs rhsT rhs with
      | Some (decls, expr) ->
         Some(decls, BinaryOp(expr, op, Number 0.))
      | None -> None
   | true, false | false, true -> None

let components = [ 
   CompilerComponent.binaryTyped <@ (=) @> (primitiveInfix "==")
   CompilerComponent.binaryTyped <@ (<>) @> (primitiveInfix "!=")
   CompilerComponent.binaryTyped <@ (>) @> (primitiveInfix ">")
   CompilerComponent.binaryTyped <@ (>=) @> (primitiveInfix ">=")
   CompilerComponent.binaryTyped <@ (<) @> (primitiveInfix "<")
   CompilerComponent.binaryTyped <@ (<=) @> (primitiveInfix "<=")
   CompilerComponent.binaryTyped <@ compare @> (fun _ -> compareCall)
   CompilerComponent.binaryTyped <@ Unchecked.compare @> (fun _ -> compareCall)
   ExpressionReplacer.create <@ min @> <@ Replacements.min @>
   ExpressionReplacer.create <@ max @> <@ Replacements.max @>
]