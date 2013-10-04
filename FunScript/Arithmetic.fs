module internal FunScript.Arithmetic

open AST
open System.Reflection
open Microsoft.FSharp.Quotations

let private numericInfix op (mi : MethodBase) lhsT lhs rhsT rhs =
   if Reflection.isPrimitive lhsT && Reflection.isPrimitive rhsT then
      Some([], BinaryOp(lhs, op, rhs))
   else
      None
      //Some(Apply(PropertyGet(lhs, mi.Name), [rhs]))

let components = [ 
   CompilerComponent.binaryTyped <@ (+) @> (numericInfix "+")
   CompilerComponent.binaryTyped <@ (-) @> (numericInfix "-")
   CompilerComponent.binaryTyped <@ (*) @> (numericInfix "*")
   CompilerComponent.binaryTyped <@ (/) @> (numericInfix "/")
   CompilerComponent.binaryTyped <@ (%) @> (numericInfix "%")
   CompilerComponent.binaryTyped <@ (>>>) @> (numericInfix ">>")
   CompilerComponent.binaryTyped <@ (<<<) @> (numericInfix "<<")
   CompilerComponent.binaryTyped <@ (&&&) @> (numericInfix "&")
   CompilerComponent.binaryTyped <@ (|||) @> (numericInfix "|")
   CompilerComponent.unaryOp <@ op_UnaryNegation @> "-"
]