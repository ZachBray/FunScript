[<FunScript.JS>]
module FunScript.Core.ResizeArray

open FunScript
open System.Collections.Generic

type Enumerator<'T>(xs: 'T[]) =
    let mutable index = -1
    interface IEnumerator<'T> with
        member x.Current with get() = xs.[index]
        member x.Current with get() = xs.[index] :> obj
        member x.Dispose() = ()
        member x.Reset() = index <- -1
        member x.MoveNext() =
            index <- index + 1
            index < xs.Length

[<JSEmitInline("[]")>]
let private createUnsafe(): ResizeArray<'a> = failwith "never"

let ZeroCreate(): ResizeArray<'a> =
    let ra = createUnsafe()
    FunScript.Core.Enumerator.AddArrayEnumerator(ra)
    ra

// NOTE: If we use 'new Array(size)' the array is automatically filled with undefined values which is not
// the expected behaviour in .NET so we just create empty arrays no matter the given initial capacity.
let ZeroCreateWithSize(size: int) =
    ZeroCreate()

[<JSEmitInline("{0}.GetEnumerator()")>]
let GetEnumerator(xs: ResizeArray<'a>): List.Enumerator<'a> = failwith "never"

[<JSEmitInline("{0}.length")>]
let Count(xs: ResizeArray<'a>) : int = failwith "never"

[<JSEmitInline("{0}[{1}]")>]
let GetItem(xs: ResizeArray<'a>, index: int): 'a = failwith "never"

// NOTE: Some JS MVC frameworks override the Array.splice method to hook on array updates,
//       so I'm using it instead of a simple indexed setter.
[<JSEmitInline("{0}.splice({1}, 1, {2})")>]
let SetItem(xs: ResizeArray<'a>, index: int, item: 'a): unit = failwith "never"

[<JSEmitInline("({0} = [])")>]
let Clear(xs: ResizeArray<'a>): unit = failwith "never"

[<JSEmitInline("{0}.push({1})")>]
let Add(xs: ResizeArray<'a>, item: 'a): unit = failwith "never"

let AddRange(xs: ResizeArray<'a>, items: seq<'a>): unit =
    for item in items do xs.Add(item)

let OfSeq(items: seq<'a>) =
    let ra = ZeroCreate()
    ra.AddRange(items)
    ra

[<JSEmitInline("({0}.indexOf({1}) > -1)")>]
let Contains(xs: ResizeArray<'a>, searchElement: 'a): bool = failwith "never"    

[<JSEmitInline("{0}.indexOf({1})")>]
let IndexOf(xs: ResizeArray<'a>, searchElement: 'a): int = failwith "never"

[<JSEmit("var i = {0}.indexOf({1}); if (i > -1) { {0}.splice(i, 1); return true; } else { return false; }")>]
let Remove(xs: ResizeArray<'a>, item: 'a): bool = failwith "never"

[<JSEmitInline("{0}.splice({1}, 1)")>]
let RemoveAt(xs: ResizeArray<'a>, index: int): unit = failwith "never"

[<JSEmitInline("{0}.splice({1}, 0, {2})")>]
let Insert(xs: ResizeArray<'a>, index: int, item: 'a): unit = failwith "never"

[<JSEmitInline("{0}.reverse()")>]
let ReverseInPlace(xs: ResizeArray<'a>): unit = failwith "never"

[<JSEmitInline("{0}.sort()")>]
let SortInPlace(xs: ResizeArray<'a>): unit = failwith "never"

[<JSEmitInline("{0}.sort({1})")>]
let SortInPlaceWith(xs: ResizeArray<'a>, comparison: System.Comparison<'a>): unit = failwith "never"

