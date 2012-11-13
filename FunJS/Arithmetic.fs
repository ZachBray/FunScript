module internal FunJS.Arithmetic

open AST
open Microsoft.FSharp.Quotations

let components = [ 
   CompilerComponent.binaryOp <@ (+) @> "+"
   CompilerComponent.binaryOp <@ (-) @> "-"
   CompilerComponent.binaryOp <@ (*) @> "*"
   CompilerComponent.binaryOp <@ (/) @> "/"
   CompilerComponent.binaryOp <@ (%) @> "%"
   CompilerComponent.binaryOp <@ (>>>) @> ">>"
   CompilerComponent.binaryOp <@ (<<<) @> "<<"
   CompilerComponent.unaryOp <@ op_UnaryNegation @> "-"
]