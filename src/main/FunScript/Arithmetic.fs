module FunScript.Arithmetic

open AST
open System
open System.Reflection
open Microsoft.FSharp.Quotations

let private numericInfix op (mi : MethodBase) lhsT lhs rhsT rhs =
   if Reflection.isPrimitive lhsT && Reflection.isPrimitive rhsT then
      Some([], BinaryOp(lhs, op, rhs))
   else
      None
      //Some(Apply(PropertyGet(lhs, mi.Name), [rhs]))

[<JS>]
type MathJS() =
    
    [<JSEmit("return Math.abs({0});")>]
    static member Abs(x : float) : float = failwith "never"

    [<JSEmit("return Math.acos({0});")>]
    static member Acos(x : float) : float = failwith "never"

    [<JSEmit("return Math.asin({0});")>]
    static member Asin(x : float) : float = failwith "never"

    [<JSEmit("return Math.atan({0});")>]
    static member Atan(x : float) : float = failwith "never"

    [<JSEmit("return Math.atan2({0}, {1});")>]
    static member Atan2(x : float, y : float) : float = failwith "never"

    [<JSEmit("return Math.ceil({0});")>]
    static member Ceiling(x : float) : float = failwith "never"

    [<JSEmit("return Math.cos({0});")>]
    static member Cos(x : float) : float = failwith "never"

    [<JSEmit("return Math.exp({0});")>]
    static member Exp(x : float) : float = failwith "never"

    [<JSEmit("return Math.floor({0});")>]
    static member Floor(x : float) : float = failwith "never"

    [<JSEmit("return Math.log({0});")>]
    static member Log(x : float) : float = failwith "never"

    [<JSEmit("return Math.pow({0}, {1});")>]
    static member Pow(x : float, y : float) : float = failwith "never"

    [<JSEmit("return Math.round({0});")>]
    static member Round(x : float) : float = failwith "never"

    [<JSEmit("return Math.sin({0});")>]
    static member Sin(x : float) : float = failwith "never"

    [<JSEmit("return Math.sqrt({0});")>]
    static member Sqrt(x : float) : float = failwith "never"

    [<JSEmit("return Math.tan({0});")>]
    static member Tan(x : float) : float = failwith "never"
    
    [<JSEmit("return Math.LN10;")>]
    static member LN10() : float = failwith "never"

    static member Log10 x = MathJS.Log x / MathJS.LN10()

let components = [ 
    yield! [
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

        ExpressionReplacer.createUnsafe <@ sqrt @> <@ MathJS.Sqrt @>
        ExpressionReplacer.createUnsafe <@ abs @> <@ MathJS.Abs @>
        ExpressionReplacer.createUnsafe <@ acos @> <@ MathJS.Acos @>
        ExpressionReplacer.createUnsafe <@ asin @> <@ MathJS.Asin @>
        ExpressionReplacer.createUnsafe <@ atan @> <@ MathJS.Atan @>
        ExpressionReplacer.createUnsafe <@ atan2 @> <@ MathJS.Atan2 @>
        ExpressionReplacer.createUnsafe <@ ceil @> <@ MathJS.Ceiling @>
        ExpressionReplacer.createUnsafe <@ cos @> <@ MathJS.Cos @>
        ExpressionReplacer.createUnsafe <@ exp @> <@ MathJS.Exp @>
        ExpressionReplacer.createUnsafe <@ floor @> <@ MathJS.Floor @>
        ExpressionReplacer.createUnsafe <@ log @> <@ MathJS.Log @>
        ExpressionReplacer.createUnsafe <@ log10 @> <@ MathJS.Log10 @>
        ExpressionReplacer.createUnsafe <@ pown @> <@ MathJS.Pow @>
        ExpressionReplacer.createUnsafe <@ round @> <@ MathJS.Round @>
        ExpressionReplacer.createUnsafe <@ sin @> <@ MathJS.Sin @>
        ExpressionReplacer.createUnsafe <@ sqrt @> <@ MathJS.Sqrt @>
        ExpressionReplacer.createUnsafe <@ tan @> <@ MathJS.Tan @>
    ]

    yield! ExpressionReplacer.createTypeMethodMappings typeof<Math> typeof<MathJS>
  
]