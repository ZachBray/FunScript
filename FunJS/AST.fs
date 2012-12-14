﻿module (*internal*) FunJS.AST

open Microsoft.FSharp.Quotations

let indent n = String.init n (fun _ -> "  ")

let (|Newline|) padding =
   System.Environment.NewLine +
   indent padding

let getNameScope (var:Var) scope =
   let rec getNameScope prefix =
      let name =
         // TODO: This string magic sucks! Wrap Var in something more
         // useful. Or use propper namespacing so we don't have to remove dots.
         if var.Name.StartsWith "!!!" then prefix + var.Name.Substring 3
         else prefix + JavaScriptNameMapper.sanitize var.Name
      match scope |> Map.tryFind name with
      | Some v when v = var -> name, scope
      | Some _ -> getNameScope (prefix + "_")
      | None -> name, scope |> Map.add name var
   getNameScope ""

type JSRef = string

type JSExpr =
   | Null
   | Boolean of bool
   | Number of float
   | String of string
   | Reference of Var
   | This
   | Object of (JSRef * JSExpr) list
   | PropertyGet of JSExpr * JSRef
   | IndexGet of JSExpr * JSExpr
   | Array of JSExpr list
   | Apply of JSExpr * JSExpr list
   | New of JSRef * JSExpr list
   | Lambda of Var list * JSBlock
   | UnaryOp of string * JSExpr
   | BinaryOp of JSExpr * string * JSExpr
   | TernaryOp of JSExpr * string * JSExpr * string * JSExpr
   member value.Print((Newline newL) as padding, scope:_ ref) =
      match value with
      | Null -> "null"
      | Boolean b -> b.ToString().ToLower()
      | Number f -> sprintf "%f" f
      | String str -> sprintf @"""%s""" (System.Web.HttpUtility.JavaScriptStringEncode(str))
      | Reference ref -> getNameScope ref !scope |> fst
      | This -> "this"
      | Object propExprs ->
         let filling =
            propExprs |> List.map (fun (name, expr) ->
               sprintf "%s: %s" name (expr.Print(padding, scope)))
            |> String.concat ", "
         sprintf "{%s}" filling
      | PropertyGet(objExpr, var) ->
         sprintf "%s.%s" (objExpr.Print(padding, scope)) var
      | IndexGet(objExpr, indexExpr) ->
         sprintf "%s[%s]" (objExpr.Print(padding, scope)) (indexExpr.Print(padding, scope))
      | Array exprs -> 
         let filling = 
            exprs |> List.map (fun expr -> 
               expr.Print(padding, scope)) 
            |> String.concat ", "
         sprintf "[%s]" filling
      | Apply(lambdaExpr, argExprs) ->
         let filling =
            argExprs |> List.map (fun argExpr -> 
               argExpr.Print(padding, scope))
            |> String.concat ", "
         sprintf "%s(%s)" (lambdaExpr.Print(padding, scope)) filling
      | New(objName, argExprs) ->
         let filling =
            argExprs |> List.map (fun argExpr -> 
               argExpr.Print(padding, scope))
            |> String.concat ", "
         sprintf "(new %s(%s))" objName filling
      | Lambda(vars, block) ->
         let oldScope = !scope
         let newScope, names = 
            vars 
            |> Seq.fold (fun (scope, acc) v -> 
               let name, scope = getNameScope v scope
               scope, name::acc) (oldScope, [])
         let filling = names |> List.rev |> String.concat ", "
         scope := newScope
         let result =
            sprintf "(function (%s)" filling
            + newL
            + block.Print(padding, scope)
            + ")"
         scope := oldScope
         result
      | UnaryOp(symbol, expr) ->
         sprintf "(%s%s)" symbol (expr.Print(padding, scope))
      | BinaryOp(lhsExpr, symbol, rhsExpr) ->
         sprintf "(%s %s %s)" (lhsExpr.Print(padding, scope)) symbol (rhsExpr.Print(padding, scope))
      | TernaryOp(lhsExpr, lSymbol, midExpr, rSymbol, rhsExpr) ->
         sprintf "(%s %s %s %s %s)" 
            (lhsExpr.Print(padding, scope)) lSymbol 
            (midExpr.Print(padding, scope)) rSymbol 
            (rhsExpr.Print(padding, scope))

and JSStatement =
   | Declare of Var list
   | Assign of JSExpr * JSExpr
   | DeclareAndAssign of Var * JSExpr
   | Throw of JSExpr
   | IfThenElse of JSExpr * JSBlock * JSBlock
   | WhileLoop of JSExpr * JSBlock
   | ForLoop of Var * JSExpr * JSExpr * JSBlock
   | TryCatch of JSBlock * JSRef * JSBlock
   | TryCatchFinally of JSBlock * JSRef * JSBlock * JSBlock
   | TryFinally of JSBlock * JSBlock
   | Scope of JSBlock
   | Return of JSExpr
   | Do of JSExpr
   | Empty
   member statement.Print((Newline newL) as padding, scope) =
      match statement with
      | Declare vars ->
         let newScope, names = 
            vars 
            |> Seq.fold (fun (scope, acc) v -> 
               let name, scope = getNameScope v scope
               scope, name::acc) (!scope, [])
         scope := newScope
         sprintf "var %s" (names |> String.concat ", ")
      | DeclareAndAssign(var, valExpr) ->
         let name, newScope = getNameScope var !scope
         scope := newScope
         sprintf "var %s = %s" name (valExpr.Print(padding, scope))
      | Assign(varExpr, valExpr) ->
         sprintf "%s = %s" (varExpr.Print(padding, scope)) (valExpr.Print(padding, scope))
      | Throw valExpr ->
         sprintf "throw (%s)" (valExpr.Print(padding, scope))
      | IfThenElse(condExpr, trueBlock, falseBlock) ->
         sprintf "if (%s) " (condExpr.Print(padding, scope))
         + newL
         + trueBlock.Print(padding, scope)
         + newL + "else" + newL
         + falseBlock.Print(padding, scope)
      | WhileLoop(condExpr, block) ->
         sprintf "while (%s)" (condExpr.Print(padding, scope))
         + newL
         + block.Print(padding, scope)
      | ForLoop(var, fromExpr, toExpr, block) ->
         let name, newScope = getNameScope var !scope
         scope := newScope
         sprintf "for (var %s = %s; %s <= %s; %s++)" name (fromExpr.Print(padding, scope)) name (toExpr.Print(padding, scope)) name
         + newL
         + block.Print(padding, scope)
      | TryCatch(tryExpr, var, catchExpr) ->
         sprintf "try"
         + newL
         + tryExpr.Print(padding, scope)
         + newL
         + sprintf "catch(%s)" var
         + catchExpr.Print(padding, scope)
      | TryCatchFinally(tryExpr, var, catchExpr, finallyExpr) ->
         sprintf "try"
         + newL
         + tryExpr.Print(padding, scope)
         + newL
         + sprintf "catch(%s)" var
         + catchExpr.Print(padding, scope)
         + newL
         + "finally"
         + finallyExpr.Print(padding, scope)
      | TryFinally(tryExpr, finallyExpr) ->
         sprintf "try"
         + newL
         + tryExpr.Print(padding, scope)
         + newL
         + "finally"
         + finallyExpr.Print(padding, scope)
      | Scope block -> block.Print(padding, scope)
      | Return expr ->
         sprintf "return %s" (expr.Print(padding, scope))
      | Do expr -> expr.Print(padding, scope)
      | Empty -> ""

and JSBlock = 
   | Block of JSStatement list
   | EmitBlock of string
   member block.Print((Newline newL) as padding, scope) =
      let (Newline paddedNewL) = padding + 1
      match block with
      | Block statements ->
         if statements |> List.isEmpty then ""
         else
            let filling =
               statements 
               |> List.map (fun smt -> smt.Print(padding + 1, scope))
               |> List.filter ((<>) "")
               |> String.concat (sprintf ";%s" paddedNewL)
            if padding = 0 then filling
            else sprintf "{%s%s;%s}" paddedNewL filling newL
      | EmitBlock code ->
         sprintf "{%s%s%s}" paddedNewL code newL

   member block.Print() =
      let keyword = Var.Global("__keyword__", typeof<obj>)
      let reservedWords =
         Map [
            "enum", keyword
         ]
      block.Print(0, ref reservedWords)