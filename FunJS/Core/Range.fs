[<FunJS.JS>]
module FunJS.Core.Range

open System.Collections
open System.Collections.Generic

type 'a RangeEnumerator(first, step) =
   let mutable current = None
   let mutable isBeforeFirst = true

   interface IEnumerator<'a> with

      member __.Reset() = 
         current <- None
         isBeforeFirst <- true

      member __.Current: 'a = current.Value

      member __.Current: obj = current.Value :> obj

      member __.MoveNext() =
         match current with
         | None when isBeforeFirst ->
            current <- Some first
            isBeforeFirst <- false
            true
         | None -> failwith "never"
         | Some value ->
            current <- step value
            current.IsSome
         
      member __.Dispose() = ()

type 'a RangeEnumerable(first, step) =
   interface IEnumerable<'a> with
      member __.GetEnumerator() = new RangeEnumerator<'a>(first, step) :> 'a IEnumerator
      member __.GetEnumerator() = new RangeEnumerator<'a>(first, step) :> IEnumerator

let inline customStep (first: ^a) (stepping: ^b) (last: ^a) : ^a seq =
   RangeEnumerable(
      first, 
      fun x -> if x < last then Some <| x + stepping
               else None
   ) :> _ IEnumerable

let inline oneStep (first: ^a) (last: ^a) : ^a seq =
   customStep first GenericConstants.One< ^a> last