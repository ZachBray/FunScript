module internal FunScript.ResizeArrays

open AST
open Microsoft.FSharp.Quotations

let private toSeq =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      function
      | Patterns.Coerce(expr, t) when (t.Name = typeof<seq<obj>>.Name || t.Name = typeof<System.Collections.IEnumerable>.Name) && expr.Type.Name = typeof<ResizeArray<obj>>.Name ->
            let mi, _ = Quote.toMethodInfoFromLambdas <@@ Core.ResizeArray.ToSeq @@>
            let specificMi = mi.MakeGenericMethod(expr.Type.GetGenericArguments())
            compiler.Compile returnStrategy (Expr.Call(specificMi, [expr]))
      | _ -> []

let components =
    [
        [
            toSeq
            ExpressionReplacer.createUnsafe <@ fun () -> ResizeArray<_>() @> <@ Core.ResizeArray.ResizeArray<_>.ZeroCreate @>
            ExpressionReplacer.createUnsafe <@ fun (size: int) -> ResizeArray<_>(size) @> <@ Core.ResizeArray.ResizeArray<_>.ZeroCreateWithSize @>
            ExpressionReplacer.createUnsafe <@ fun (initValues: seq<_>) -> ResizeArray<_>(initValues)  @> <@ Core.ResizeArray.ResizeArray<_>.OfSeq @>

            ExpressionReplacer.createUnsafe
                <@ fun (xs: ResizeArray<_>) -> xs.Sort() @>
                <@ fun (xs: Core.ResizeArray.ResizeArray<_>) -> xs.SortInPlace() @>
            ExpressionReplacer.createUnsafe
                <@ fun (xs: ResizeArray<_>, comparison: System.Comparison<_>) -> xs.Sort(comparison) @>
                <@ fun (xs: Core.ResizeArray.ResizeArray<_>, comparison: System.Comparison<_>) -> xs.SortInPlaceWith(comparison) @>
        ]
        ExpressionReplacer.createTypeMethodMappings typeof<ResizeArray<_>> typeof<Core.ResizeArray.ResizeArray<_>>
        ExpressionReplacer.createTypeMethodMappings typeof<ResizeArray<_>.Enumerator> typeof<Core.ResizeArray.Enumerator<_>>
    ] |> List.concat