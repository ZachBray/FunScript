module internal FunScript.Logic

open AST
open Microsoft.FSharp.Quotations

let private toShortCircuit (|Split|) (compiler : InternalCompiler.ICompiler) rhs =
    // Because of short circuiting we have to wrap the rhs inside a function.
    // Unless it is compilable to a javascript expression.
    match compiler.Compile ReturnStrategies.returnFrom rhs with
    | [ AST.Return rhs ] -> rhs
    | rhsBody -> Apply(Lambda([], Block rhsBody), [])

let private operators =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      function
      | DerivedPatterns.AndAlso(Split(declLHS, refLHS), rhs) -> 
         [  yield! declLHS
            let refRHS = toShortCircuit (|Split|) compiler rhs
            yield returnStrategy.Return <| BinaryOp(refLHS, "&&", refRHS)
         ]
      | DerivedPatterns.OrElse(Split(declLHS, refLHS), rhs) -> 
         [  yield! declLHS
            let refRHS = toShortCircuit (|Split|) compiler rhs
            yield returnStrategy.Return <| BinaryOp(refLHS, "||", refRHS)
         ]
      | _ -> []

let components = [ 
   CompilerComponent.unaryOp <@ not @> "!"
   operators
]