module internal FunJS.ControlFlow

open AST
open Microsoft.FSharp.Quotations

let private forLoop =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
      let (|Return|) = compiler.Compile
      function
      | Patterns.ForIntegerRangeLoop(var, Split(fromDecl, fromRef), Split(toDecl, toRef), (Return ReturnStrategies.inplace block as bodyExpr)) ->
         [  yield! fromDecl
            yield! toDecl
            let hasClosure =
               bodyExpr |> Expr.exists (function
                  | Patterns.Lambda _ -> true
                  | _ -> false)
            let countVar, block =
               if hasClosure then
                  let mutableName = compiler.NextTempVar()
                  let block = Block [Do <| Apply(Lambda([var], Block block), [Reference mutableName])]
                  mutableName, block
               else var, Block block
               // Messiness here is to get around the closure problem in javascript for loops.
            yield ForLoop(countVar, fromRef, toRef, block)
         ]
      | _ -> []

let private whileLoop =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
      let (|Return|) = compiler.Compile
      function
      | Patterns.WhileLoop(Split(condDecl, condRef), Return ReturnStrategies.inplace block) ->
         [ yield! condDecl
           yield WhileLoop(condRef, Block block)
         ]
      | _ -> []

let private ifThenElse =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
      let (|Return|) = compiler.Compile
      function
      | Patterns.IfThenElse(Split(condDecl, condRef), Return returnStategy trueBlock, Return returnStategy falseBlock) ->
         [ yield! condDecl
           yield IfThenElse(condRef, Block trueBlock, Block falseBlock)
         ]
      | _ -> []

let private sequential =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
      let (|Return|) = compiler.Compile
      function
      | Patterns.Sequential(Return ReturnStrategies.inplace firstBlock, Return returnStategy secondBlock) ->
         [ yield! firstBlock
           yield! secondBlock
         ]
      | _ -> []

let components = [ 
   forLoop
   whileLoop
   ifThenElse
   sequential
]