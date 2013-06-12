module internal FunScript.Arithmetic

open AST
open System.Reflection
open Microsoft.FSharp.Quotations

let private isNumericType (t:System.Type) =
   Reflection.primitiveTypes.Contains t.FullName

let private numericInfix op (mi : MethodBase) lhsT lhs rhsT rhs =
   if isNumericType lhsT && isNumericType rhsT then
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
   CompilerComponent.unaryOp <@ op_UnaryNegation @> "-"
]