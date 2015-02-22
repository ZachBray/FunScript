module internal FunScript.ControlFlow

open AST
open Microsoft.FSharp.Quotations

let private forLoop =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
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
    CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
        let (|Return|) = compiler.Compile
        function
        | Patterns.WhileLoop(condExpr, Return ReturnStrategies.inplace block) ->
            match compiler.Compile ReturnStrategies.returnFrom condExpr with
            | [AST.Return condExpr] ->
                [ WhileLoop(condExpr, Block block) ]
            | condBody ->
                let condLambda = Lambda([], Block condBody)
                let condVar = compiler.NextTempVar()
                [ 
                    yield DeclareAndAssign(condVar, condLambda)
                    yield WhileLoop(Apply(Reference condVar, []), Block block)
                ]
        | _ -> []

let private ifThenElse =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      let (|Return|) = compiler.Compile
      function
      | Patterns.IfThenElse(Split(condDecl, condRef), Return returnStrategy trueBlock, Return returnStrategy falseBlock) ->
         [ yield! condDecl
           yield IfThenElse(condRef, Block trueBlock, Block falseBlock)
         ]
      | _ -> []

let private tryFinally =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
   function
   | Patterns.TryFinally(tryExpr, finallyExpr) ->
      let tryStmts = compiler.Compile returnStrategy tryExpr
      let finallyStmts = compiler.Compile ReturnStrategies.inplace finallyExpr
      [ TryFinally(Block tryStmts, Block finallyStmts) ]
   | _ -> []

let private tryWith =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      function
      // TODO: Type testing...
      | Patterns.TryWith(body, _, _, catchVar, catchBody) ->
         let bodyBlock = compiler.Compile returnStrategy body
         let catchBlock = compiler.Compile returnStrategy catchBody
         [TryCatch(Block bodyBlock, catchVar, Block catchBlock)]
      | _ -> []

let private sequential =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      let (|Return|) = compiler.Compile
      function
      | Patterns.Sequential(Return ReturnStrategies.inplace firstBlock, Return returnStrategy secondBlock) ->
         [ yield! firstBlock
           yield! secondBlock
         ]
      | _ -> []

let components = [ 
   forLoop
   whileLoop
   ifThenElse
   sequential
   tryFinally
   tryWith
]