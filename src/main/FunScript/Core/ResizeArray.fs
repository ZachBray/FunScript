[<FunScript.JS>]
module FunScript.Core.ResizeArray

open FunScript
open System

[<JSEmitInline("(new Array())")>]
let ZeroCreate (): ResizeArray<'a[]> = failwith "never"

[<JSEmitInline("(new Array({0}))")>]
let ZeroCreateWithSize(size: int): ResizeArray<'a[]> = failwith "never"

[<JSEmitInline("{0}.length")>]
let Count(xs: ResizeArray<'a>) : int = failwith "never"

[<JSEmitInline("{0}[{1}]")>]
let Item(xs: ResizeArray<'a>, index: int): 'a = failwith "never"

[<JSEmitInline("{0}.splice({1}, 1, {2})")>]
let Replace(xs: ResizeArray<'a>, index: int, item: 'a): unit = failwith "never"

[<JSEmitInline("({0} = [])")>]
let Clear(xs: ResizeArray<'a>): unit = failwith "never"

[<JSEmitInline("{0}.push({1})")>]
let Add(xs: ResizeArray<'a>, item: 'a): unit = failwith "never"

let AddRange(xs: ResizeArray<'a>, items: seq<'a>): unit =
    for item in items do xs.Add(item)

[<JSEmitInline("({0}.indexOf({1}) > -1)")>]
let Contains(xs: ResizeArray<'a>, searchElement: 'a): bool = failwith "never"    

[<JSEmitInline("{0}.indexOf({1})")>]
let IndexOf(xs: ResizeArray<'a>, searchElement: 'a): int = failwith "never"

[<JSEmit("var i = {0}.indexOf({1}); if (i > -1) { {0}.splice(i, 1); return true; } else { return false; }")>]
let Remove(xs: ResizeArray<'a>, item: 'a): bool = failwith "never"

[<JSEmitInline("{0}.splice({1}, 1)")>]
let RemoveAt(xs: ResizeArray<'a>) (index: int): unit = failwith "never"

[<JSEmitInline("{0}.splice({1}, 0, {2})")>]
let Insert(xs: ResizeArray<'a>, index: int, item: 'a): unit = failwith "never"

[<JSEmitInline("{0}.reverse()")>]
let ReverseInPlace(xs: ResizeArray<'a>): unit = failwith "never"

[<JSEmitInline("{0}.sort()")>]
let SortInPlace(xs: ResizeArray<'a>): unit = failwith "never"

[<JSEmitInline("{0}.sort({1})")>]
let SortInPlaceWith(xs: ResizeArray<'a>, comparison: Comparison<'a>): unit = failwith "never"

