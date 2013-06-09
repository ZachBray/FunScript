module internal FunScript.Arithmetic

open AST
open Microsoft.FSharp.Quotations

let private numericTypes =
   set [
      typeof<sbyte>.Name
      typeof<byte>.Name
      typeof<int16>.Name
      typeof<uint16>.Name
      typeof<int32>.Name
      typeof<uint32>.Name
      typeof<int64>.Name
      typeof<uint64>.Name
      typeof<float>.Name
      typeof<single>.Name
      typeof<float32>.Name

      // Huh?
      typeof<string>.Name
      typeof<char>.Name
      typeof<bool>.Name
   ]

let private isNumericType (t:System.Type) =
   numericTypes.Contains t.Name

// TODO: Will this code break under some generic conditions?
// Particularly when we use the same generic code with numeric
// and reference types. E.g., Seq.sum?
let private numericInfix op lhsT lhs rhsT rhs =
   if isNumericType lhsT && isNumericType rhsT then
      Some(BinaryOp(lhs, op, rhs))
   else None

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