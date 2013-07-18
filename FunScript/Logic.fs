module internal FunScript.Logic

open AST
open Microsoft.FSharp.Quotations

let private toShortCircuit (|Split|) (compiler : InternalCompiler.ICompiler) rhs =
    // Because of short circuiting we have to wrap the rhs inside a function.
    // Unless it is a value or reference.
    match rhs with
    | Patterns.Value _ 
    | Patterns.Var _ ->
        let (Split(declRHS, refRHS)) = rhs
        declRHS, refRHS
    | _ -> 
        let rhsBody = compiler.Compile ReturnStrategies.returnFrom rhs
        [], Apply(Lambda([], Block rhsBody), [])

let private operators =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
      function
      | DerivedPatterns.AndAlso(Split(declLHS, refLHS), rhs) -> 
         [  yield! declLHS
            let declRHS, refRHS = toShortCircuit (|Split|) compiler rhs
            yield! declRHS
            yield returnStategy.Return <| BinaryOp(refLHS, "&&", refRHS)
         ]
      | DerivedPatterns.OrElse(Split(declLHS, refLHS), rhs) -> 
         [  yield! declLHS
            let declRHS, refRHS = toShortCircuit (|Split|) compiler rhs
            yield! declRHS
            yield returnStategy.Return <| BinaryOp(refLHS, "||", refRHS)
         ]
      | _ -> []

let components = [ 
   CompilerComponent.unaryOp <@ not @> "!"
   operators
]