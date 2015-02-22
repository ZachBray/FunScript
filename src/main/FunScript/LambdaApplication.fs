module internal FunScript.LambdaApplication

open AST
open Microsoft.FSharp.Quotations

let private application =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      function

      // TODO: Implement these two optimizations directly in the pipe operator?
      | Patterns.Application(Patterns.Lambda(var, (Patterns.Call _ as call)), lambdaArg) ->
        compiler.Compile returnStrategy <| call.Substitute(fun v ->
            if v.Name = var.Name then Some lambdaArg else None)

      | Patterns.Application(Patterns.Let(letVar, letVarValue, Patterns.Lambda(lambdaVar, (Patterns.Call _ as call))), lambdaArg) ->
        compiler.Compile returnStrategy <| call.Substitute(fun v ->
            if v.Name = letVar.Name then Some letVarValue
            elif v.Name = lambdaVar.Name then Some lambdaArg
            else None)

      | Patterns.Application(Split(lambdaDecl, lambdaRef), Split(argDecl, argRef)) ->
         [ yield! lambdaDecl
           yield! argDecl
           yield returnStrategy.Return <| Apply(lambdaRef, [argRef])
         ]

      | Patterns.Call(Some (Split(delDecl, delRef)), mi, argExprs) 
        when typeof<System.Delegate>.IsAssignableFrom mi.DeclaringType ->
         let argDecls, argRefs =
            Reflection.getDeclarationAndReferences (|Split|) argExprs
         [ yield! delDecl
           yield! argDecls |> List.concat
           yield returnStrategy.Return <| Apply(delRef, argRefs)
         ]
      | _ -> []

let private definition =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      let (|Return|) = compiler.Compile
      function
      | Patterns.Lambda(var, expr) ->
         let block = compiler.Compile ReturnStrategies.returnFrom expr
         [ yield returnStrategy.Return <| Lambda([var], Block block) ]
      | Patterns.NewDelegate(_, vars, expr) ->
         let block = compiler.Compile ReturnStrategies.returnFrom expr
         [ yield returnStrategy.Return <| Lambda(vars, Block block) ]
      | _ -> []

let components = [ 
   application
   definition
]