module internal FunScript.ResizeArrays

open AST
open Microsoft.FSharp.Quotations


let components =
    [
        [
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
    ] |> List.concat