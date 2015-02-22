module (*internal*) FunScript.ReturnStrategies

open AST
open InternalCompiler

let private returnStrategy f =
   {  new IReturnStrategy with
         member __.Return expr = f expr }

let returnFrom = returnStrategy Return

let inplace = returnStrategy Do

let assignVar varName = returnStrategy (fun expr -> Assign(Reference varName, expr))

let assignTo expr = returnStrategy (fun valExpr -> Assign(expr, valExpr))