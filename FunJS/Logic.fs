module internal FunJS.Logic

open AST
open Microsoft.FSharp.Quotations

let private operators =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | DerivedPatterns.AndAlso(Split(declLHS, refLHS), Split(declRHS, refRHS)) -> 
         [  yield! declLHS
            yield! declRHS
            yield returnStategy.Return <| BinaryOp(refLHS, "&&", refRHS)
         ]
      | DerivedPatterns.OrElse(Split(declLHS, refLHS), Split(declRHS, refRHS)) -> 
         [  yield! declLHS
            yield! declRHS
            yield returnStategy.Return <| BinaryOp(refLHS, "||", refRHS)
         ]
      | _ -> []

let components = [ 
   CompilerComponent.unaryOp <@ not @> "!"
   operators
]