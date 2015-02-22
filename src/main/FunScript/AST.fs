module (*internal*) FunScript.AST

open System.Text
open Microsoft.FSharp.Quotations

type Site =
   | FromDeclaration
   | FromReference

let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"

type VariableScope =
   {  ByName : Map<string, Var>
      ByVar : Map<Var, string>
      ShouldCompressNames : bool }

      static member Empty =
         { ByName = Map.empty; ByVar = Map.empty; ShouldCompressNames = false }

      static member EmptyCompressing =
         { VariableScope.Empty with ShouldCompressNames = true }

      member scope.ObtainFreeCompressedName(var : Var) =
         if var.Name.StartsWith "Tag" || var.Name.StartsWith "Item" then
             scope.ObtainFreeName var
         else
             let rec buildName (acc : StringBuilder) i =
                let letterCode = i % alphabet.Length
                acc.Append alphabet.[letterCode] |> ignore
                let nextI = (i - letterCode) / alphabet.Length
                if nextI <> 0 then
                    buildName acc nextI
                else acc.ToString()
             let proposedName = buildName (StringBuilder()) scope.ByVar.Count
             if proposedName.StartsWith "Tag" || 
                proposedName.StartsWith "Item" ||
                JavaScriptNameMapper.unsafeWords.Contains proposedName then
                "$$" + proposedName
             else proposedName

      member scope.ObtainFreeName(var : Var) =
         let baseName =
             match var.Name with
             | "this" -> "_this"
             | name -> JavaScriptNameMapper.sanitizeAux name
         let rec obtainFreeName name =
             match scope.ByName.TryFind name with
             | None -> name
             | Some _ -> obtainFreeName ("_" + name)
         obtainFreeName baseName

      member scope.ObtainNameScope var site =
         match scope.ByVar.TryFind var with
         | Some name -> name, scope
         | None ->
            match site with
            | FromReference -> 
               match var.Name with
               | "this" -> "this", scope
               | _ -> failwithf "Variable '%A' is not within scope." var
            | FromDeclaration ->
               let finalName = 
                   if scope.ShouldCompressNames then
                      scope.ObtainFreeCompressedName var
                   else scope.ObtainFreeName var
                   
               let newScope =
                  {  ByName = scope.ByName |> Map.add finalName var
                     ByVar = scope.ByVar |> Map.add var finalName
                     ShouldCompressNames = scope.ShouldCompressNames }
               finalName, newScope

let indent n = String.init n (fun _ -> "  ")

let getNewlineAndOffset (padding, scope : VariableScope ref) =
   if (!scope).ShouldCompressNames then ""
   else System.Environment.NewLine + (if padding > 1 then indent padding else "")

let addVarsToScope vars oldScope =
    let newScope, names =
        vars |> Seq.fold (fun (scope : VariableScope, acc) v -> 
            let name, scope = scope.ObtainNameScope v FromDeclaration
            scope, name::acc) (oldScope, [])
    newScope, names |> List.rev

type JSRef = string

type JSExpr =
   | Null
   | Boolean of bool
   | Number of float
   | Integer of int
   | String of string
   | Reference of Var
   | Object of (JSRef * JSExpr) list
   | PropertyGet of JSExpr * JSRef
   | IndexGet of JSExpr * JSExpr
   | Array of JSExpr list
   | Apply of JSExpr * JSExpr list
   | New of Var * JSExpr list
   | Lambda of Var list * JSBlock
   | UnaryOp of string * JSExpr
   | BinaryOp of JSExpr * string * JSExpr
   | TernaryOp of JSExpr * string * JSExpr * string * JSExpr
   | EmitExpr of (int * VariableScope ref -> string)
   member value.Print(padding, scope:VariableScope ref) =
      let newL = getNewlineAndOffset(padding, scope)
      match value with
      | Null -> "null"
      | Boolean b -> b.ToString().ToLower()
      | Integer i -> sprintf "%d" i
      | Number f -> sprintf "%f" f
      | String str -> sprintf @"""%s""" (System.Web.HttpUtility.JavaScriptStringEncode(str))
      | Reference ref -> (!scope).ObtainNameScope ref FromReference |> fst
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
      | New(ref, argExprs) ->
         let filling =
            argExprs |> List.map (fun argExpr -> 
               argExpr.Print(padding, scope))
            |> String.concat ", "
         sprintf "(new %s(%s))" ((!scope).ObtainNameScope ref FromReference |> fst) filling
      | Lambda(vars, block) ->
         let oldScope = !scope
         let newScope, names = oldScope |> addVarsToScope vars
         let filling = names |> String.concat ","
         scope := newScope
         let result =
            sprintf "(function(%s)" filling
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
      | EmitExpr code -> code(padding, scope)

and JSStatement =
   | Declare of Var list
   | Assign of JSExpr * JSExpr
   | DeclareAndAssign of Var * JSExpr
   | CopyThisToVar of Var
   | Throw of JSExpr
   | IfThenElse of JSExpr * JSBlock * JSBlock
   | WhileLoop of JSExpr * JSBlock
   | ForLoop of Var * JSExpr * JSExpr * JSBlock
   | TryCatch of JSBlock * Var * JSBlock
   | TryFinally of JSBlock * JSBlock
   | Scope of JSBlock
   | Return of JSExpr
   | Do of JSExpr
   | EmitStatement of (int * VariableScope ref -> string)
   | Empty
   member statement.Print(padding, scope) =
      let newL =  getNewlineAndOffset(padding, scope)
      match statement with
      | Declare vars ->
         let newScope, names = 
            vars 
            |> Seq.fold (fun (scope : VariableScope, acc) v -> 
               let name, scope = scope.ObtainNameScope v FromDeclaration
               scope, name::acc) (!scope, [])
         scope := newScope
         sprintf "var %s" (names |> String.concat ", ")
      | DeclareAndAssign(var, valExpr) ->
         let name, newScope = (!scope).ObtainNameScope var FromDeclaration
         scope := newScope
         sprintf "var %s = %s" name (valExpr.Print(padding, scope))
      | CopyThisToVar var ->
         let name, newScope = (!scope).ObtainNameScope var FromDeclaration
         scope := newScope
         sprintf "var %s = this" name
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
         let name, newScope = (!scope).ObtainNameScope var FromDeclaration
         scope := newScope
         sprintf "for (var %s = %s; %s <= %s; %s++)" name (fromExpr.Print(padding, scope)) name (toExpr.Print(padding, scope)) name
         + newL
         + block.Print(padding, scope)
      | TryCatch(tryExpr, var, catchExpr) ->
         let name, newScope = (!scope).ObtainNameScope var FromDeclaration
         scope := newScope
         sprintf "try"
         + newL
         + tryExpr.Print(padding, scope)
         + newL
         + sprintf "catch(%s)" name
         + catchExpr.Print(padding, scope)
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
      | EmitStatement code -> code(padding, scope)

and JSBlock = 
   | Block of JSStatement list
   member block.Print(padding, scope) =
      let newL =  getNewlineAndOffset(padding, scope)
      let paddedNewL = getNewlineAndOffset(padding + 1, scope)
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

   member block.Print() =
      block.Print(0, ref VariableScope.Empty)

   member block.PrintCompressed() =
      block.Print(0, ref VariableScope.EmptyCompressing)
      