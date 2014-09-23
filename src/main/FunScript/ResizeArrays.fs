module internal FunScript.ResizeArrays

open AST
open Microsoft.FSharp.Quotations

let components = 
    [
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
