module (*internal*) FunJS.ReturnStrategies

open AST
open InternalCompiler

let private returnStategy f =
   {  new IReturnStrategy with
         member __.Return expr = f expr }

let returnFrom = returnStategy Return

let inplace = returnStategy Do

let assignVar varName = returnStategy (fun expr -> Assign(Reference varName, expr))

let assignTo expr = returnStategy (fun valExpr -> Assign(expr, valExpr))