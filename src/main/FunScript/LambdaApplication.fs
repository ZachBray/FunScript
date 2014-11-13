module internal FunScript.LambdaApplication

open AST
open Microsoft.FSharp.Quotations

let private application =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.Application(Split(lambdaDecl, lambdaRef), (Split(argDecl, argRef) as argExpr)) ->
         [ yield! lambdaDecl
           yield! argDecl
           yield returnStategy.Return <| Apply(lambdaRef, if argExpr.Type = typeof<unit> then [] else [argRef])
         ]
      | Patterns.Call(Some (Split(delDecl, delRef)), mi, argExprs) 
        when typeof<System.Delegate>.IsAssignableFrom mi.DeclaringType ->
         let argDecls, argRefs = 
            argExprs 
            |> List.map (fun (Split(valDecl, valRef)) -> valDecl, valRef)
            |> List.unzip
         [ yield! delDecl
           yield! argDecls |> List.concat
           yield returnStategy.Return <| Apply(delRef, argRefs)
         ]
      | _ -> []

let private definition =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
      let (|Return|) = compiler.Compile
      function
      | Patterns.Lambda(var, expr) ->
         let expr = expr.Substitute(fun var -> if var.Type = typeof<unit> then Some(Expr.Value(())) else None)
         let block = compiler.Compile ReturnStrategies.returnFrom expr
         let vars = if var.Type = typeof<unit> then [] else [var]
         [ yield returnStategy.Return <| Lambda(vars, Block block) ]
      | Patterns.NewDelegate(_, vars, expr) ->
         let block = compiler.Compile ReturnStrategies.returnFrom expr
         [ yield returnStategy.Return <| Lambda(vars, Block block) ]
      | _ -> []

let components = [ 
   application
   definition
]