module internal FunScript.Enumerator

open AST
open Microsoft.FSharp.Quotations

let private compileGenericMethod (compiler: InternalCompiler.ICompiler) returnStrategy (genericExpr: Expr)
                                 nonGenericQuote listQuote dicQuote keyColQuote valueColQuote =
    let genericArguments = genericExpr.Type.GetGenericArguments()
    if genericArguments.Length = 0 then
        let mi, _ = Quote.toMethodInfoFromLambdas nonGenericQuote
        compiler.Compile returnStrategy (Expr.Call(mi, [genericExpr]))
    else
        let quote =
            match genericArguments.Length with
            | 1 -> listQuote
            | 2 when genericExpr.Type.FullName.Contains("KeyCollection") -> keyColQuote
            | 2 when genericExpr.Type.FullName.Contains("ValueCollection") -> valueColQuote
            | 2 -> dicQuote
            | _ -> failwith "Enumerators with more than 2 generic arguments are not supported"

        let mi, _ =  Quote.toMethodInfoFromLambdas quote
        let specificMi = mi.MakeGenericMethod(genericArguments)
        compiler.Compile returnStrategy (Expr.Call(specificMi, [genericExpr]))

// NOTE: Using ExpressionReplacer does not tell the difference between List.Enumerator<'a> and Dictionary.Enumerator<'k,'v> which causes problems
//       when replacing the quotation because the number of generic arguments may be different. So I have to use this more complicated function.
let private enumeratorComponents =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      function
      | Patterns.Call(Some(enumerator),mi,args) when enumerator.Type.Name = "Enumerator" && mi.Name = "MoveNext" ->
            compileGenericMethod compiler returnStrategy enumerator
                <@@ Core.Enumerator.NonGenericMoveNext @@>
                <@@ Core.Enumerator.ListMoveNext @@>
                <@@ Core.Enumerator.DicMoveNext @@>
                <@@ Core.Enumerator.KeyColMoveNext @@>
                <@@ Core.Enumerator.ValueColMoveNext @@>
      | Patterns.PropertyGet(Some(enumerator),pi,args) when enumerator.Type.Name = "Enumerator" && pi.Name = "Current" ->
            compileGenericMethod compiler returnStrategy enumerator
                <@@ Core.Enumerator.NonGenericCurrent @@>
                <@@ Core.Enumerator.ListCurrent @@>
                <@@ Core.Enumerator.DicCurrent @@>
                <@@ Core.Enumerator.KeyColCurrent @@>
                <@@ Core.Enumerator.ValueColCurrent @@>
      | _ -> []

let components = 
    [
//        enumeratorComponents
    ]


