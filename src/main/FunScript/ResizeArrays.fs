module internal FunScript.ResizeArrays

open AST
open Microsoft.FSharp.Quotations
open System.Collections.Generic

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
      | Patterns.Call(Some(enumerator),mi,args) when enumerator.Type.Name = "Enumerator" ->
            compileGenericMethod compiler returnStrategy enumerator
                <@@ Core.ResizeArray.Enumerator.NonGenericMoveNext @@>
                <@@ Core.ResizeArray.Enumerator.ListMoveNext @@>
                <@@ Core.ResizeArray.Enumerator.DicMoveNext @@>
                <@@ Core.ResizeArray.Enumerator.KeyColMoveNext @@>
                <@@ Core.ResizeArray.Enumerator.ValueColMoveNext @@>
      | Patterns.PropertyGet(Some(enumerator),pi,args) when enumerator.Type.Name = "Enumerator" ->
            compileGenericMethod compiler returnStrategy enumerator
                <@@ Core.ResizeArray.Enumerator.NonGenericCurrent @@>
                <@@ Core.ResizeArray.Enumerator.ListCurrent @@>
                <@@ Core.ResizeArray.Enumerator.DicCurrent @@>
                <@@ Core.ResizeArray.Enumerator.KeyColCurrent @@>
                <@@ Core.ResizeArray.Enumerator.ValueColCurrent @@>
      | _ -> []

let components = 
    [
        enumeratorComponents
        ExpressionReplacer.createUnsafe <@ fun (xs: ResizeArray<_>) -> xs.GetEnumerator() @> <@ Core.ResizeArray.GetEnumerator @>

        ExpressionReplacer.createUnsafe <@ fun () -> ResizeArray<_>() @> <@ Core.ResizeArray.ZeroCreate @>
        ExpressionReplacer.createUnsafe <@ fun (size: int) -> ResizeArray<_>(size) @> <@ Core.ResizeArray.ZeroCreateWithSize @>
        ExpressionReplacer.createUnsafe <@ fun (initValues: seq<_>) -> ResizeArray<_>(initValues)  @> <@ Core.ResizeArray.OfSeq @>

        ExpressionReplacer.createUnsafe <@ fun (xs: ResizeArray<_>) -> xs.Count @> <@ Core.ResizeArray.Count @>
        ExpressionReplacer.createUnsafe <@ fun (xs: ResizeArray<_>, index: int) -> xs.Item(index) @> <@ Core.ResizeArray.GetItem @>
        ExpressionReplacer.createUnsafe <@ fun (xs: ResizeArray<_>, index: int, item: _) -> xs.Item(index) <- item @> <@ Core.ResizeArray.SetItem @>

        ExpressionReplacer.createUnsafe <@ fun (xs: ResizeArray<_>) -> xs.Clear() @> <@ Core.ResizeArray.Clear @>
        ExpressionReplacer.createUnsafe <@ fun (xs: ResizeArray<_>, item: _) -> xs.Add(item) @> <@ Core.ResizeArray.Add @>
        ExpressionReplacer.createUnsafe <@ fun (xs: ResizeArray<_>, items: seq<_>) -> xs.AddRange(items) @> <@ Core.ResizeArray.AddRange @>

        ExpressionReplacer.createUnsafe <@ fun (xs: ResizeArray<_>, item: _) -> xs.Contains(item) @> <@ Core.ResizeArray.Contains @>
        ExpressionReplacer.createUnsafe <@ fun (xs: ResizeArray<_>, item: _) -> xs.IndexOf(item) @> <@ Core.ResizeArray.IndexOf @>
        ExpressionReplacer.createUnsafe <@ fun (xs: ResizeArray<_>, item: _) -> xs.Remove(item) @> <@ Core.ResizeArray.Remove @>
        ExpressionReplacer.createUnsafe <@ fun (xs: ResizeArray<_>, index: int, item: _) -> xs.RemoveAt(index) @> <@ Core.ResizeArray.RemoveAt @>
        ExpressionReplacer.createUnsafe <@ fun (xs: ResizeArray<_>, index: int, item: _) -> xs.Insert(index, item) @> <@ Core.ResizeArray.Insert @>

        ExpressionReplacer.createUnsafe <@ fun (xs: ResizeArray<_>) -> xs.Reverse() @> <@ Core.ResizeArray.ReverseInPlace @>
        ExpressionReplacer.createUnsafe <@ fun (xs: ResizeArray<_>) -> xs.Sort() @> <@ Core.ResizeArray.SortInPlace @>
        ExpressionReplacer.createUnsafe
            <@ fun (xs: ResizeArray<_>, comparison: System.Comparison<_>) -> xs.Sort(comparison) @>
            <@ Core.ResizeArray.SortInPlaceWith @>
    ]
