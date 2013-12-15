[<FunScript.JS>]
module FunScript.Core.Seq

open System
open System.Collections
open System.Collections.Generic

let Enumerator (xs:_ seq) =   
   xs.GetEnumerator()

let FoldIndexedAux f acc (xs:_ IEnumerator) =
   let i = ref 0
   let acc = ref acc
   while xs.MoveNext() do
      acc := f !i !acc xs.Current
      i := !i + 1
   !acc

let FoldIndexed<'a,'acc> f (seed:'acc) (xs: 'a seq) = 
   FoldIndexedAux f seed (Enumerator xs)

let Fold<'a,'acc> f (seed:'acc) (xs: 'a seq) = 
   FoldIndexed (fun _ acc x -> f acc x) seed xs

let FoldIndexed2Aux f acc (xs:_ IEnumerator) (ys:_ IEnumerator) =
   let i = ref 0
   let acc = ref acc
   while xs.MoveNext() && ys.MoveNext() do
      acc := f !i !acc xs.Current ys.Current
      i := !i + 1
   !acc

let FoldIndexed2<'a, 'b, 'acc> f (seed:'acc) (xs: 'a seq) (ys: 'b seq) = 
   FoldIndexed2Aux f seed (Enumerator xs) (Enumerator ys)

let Fold2<'a, 'b, 'acc> f (seed:'acc) (xs: 'a seq) (ys: 'b seq) = 
   FoldIndexed2 (fun _ acc x y -> f acc x y) seed xs ys

let Head (xs:_ seq) =
   let enumerator = Enumerator xs
   if enumerator.MoveNext() then
      enumerator.Current
   else failwith "Seq was empty"

type UnfoldEnumerator<'acc, 'a>(seed:'acc, unfold) =
   let mutable acc = Some seed
   let mutable current = Unchecked.defaultof<'a>

   interface IEnumerator<'a> with

      member __.Reset() = 
         acc <- Some seed
         current <- Unchecked.defaultof<_>

      member __.Current: 'a = current

      member __.Current: obj = current :> obj

      member __.MoveNext() =
         let next() =
            let currAcc = acc.Value
            let x = unfold currAcc
            match x with
            | None -> 
               acc <- None
               current <- Unchecked.defaultof<_>
               false
            | Some (value, nextAcc) ->
               acc <- Some nextAcc
               current <- value
               true
         if acc.IsSome then next()
         else false

// TODO: Work out problem with this:-
//
//      member __.MoveNext() =
//         match acc with
//         | None -> false
//         | Some currAcc ->
//            match unfold currAcc with
//            | None -> 
//               acc <- None
//               current <- Unchecked.defaultof<_>
//               false
//            | Some (value, nextAcc) ->
//               acc <- Some nextAcc
//               current <- value
//               true
            
      member __.Dispose() = ()


type CreateEnumerable<[<EqualityConditionalOnAttribute; ComparisonConditionalOnAttribute>] 'a>(factory) =

   interface System.IComparable<'a seq> with 
      member this.CompareTo ys =
         let xs = this :> IEnumerable<'a>
         Seq.compareWith (fun x y -> Unchecked.compare x y) 
            xs ys

   interface IEnumerable<'a> with
      member __.GetEnumerator() = factory() :> 'a IEnumerator
      member __.GetEnumerator() = factory() :> IEnumerator

let FromFactory f =
   CreateEnumerable(f) :> _ IEnumerable

let Delay (f:unit -> 'a seq): 'a seq =
   FromFactory(fun () -> Enumerator(f()))

let Unfold f seed =
   FromFactory(fun () -> upcast new UnfoldEnumerator<'acc, 'a>(seed, f))

let Append xs ys =
   Delay <| fun () ->
      let enums = lazy Enumerator xs, lazy Enumerator ys
      enums |> Unfold (function
         | (curr, next) as enums when curr.Value.MoveNext() ->
            Some (curr.Value.Current, enums)
         | (curr, next) when next.Value.MoveNext() ->
            Some (next.Value.Current, (next, curr))
         | _ -> None)

let Skip n xs =
   FromFactory(fun () ->
      let enum = Enumerator xs
      for i = 1 to n do enum.MoveNext() |> ignore<bool>
      enum)

let Empty<'a> : 'a seq =
    Unfold (fun _ -> None) false

let Length xs =
   Fold (fun count _ -> count + 1) 0 xs

let IsEmpty xs =
   not ((Enumerator xs).MoveNext())

let inline Sum (xs: ^a seq) : ^a =
   Fold (fun acc x -> acc + x) GenericConstants.Zero xs

let inline SumBy (f:^a -> ^b) (xs: ^a seq) : ^b =
   Fold (fun acc x -> acc + f x) GenericConstants.Zero xs

let Reduce f xs =
   let first = Head xs
   let rest = Skip 1 xs
   Fold f first rest

let Filter f xs =
   let rec trySkipToNext(enum:_ IEnumerator) =
      if enum.MoveNext() then
         if f enum.Current then
            Some(enum.Current, enum)
         else trySkipToNext enum
      else None
   Delay <| fun () ->
      Enumerator xs
      |> Unfold trySkipToNext

let SkipWhile f xs =
   Delay <| fun () ->
      let hasPassed = ref false
      xs |> Filter (fun x ->
         !hasPassed || (
            hasPassed := not (f x)
            !hasPassed
         ))

let Choose f xs =
   let rec trySkipToNext(enum:_ IEnumerator) =
      if enum.MoveNext() then
         match f enum.Current with
         | None -> trySkipToNext enum
         | Some value -> Some(value, enum)
      else None
   Delay <| fun () ->
      Enumerator xs
      |> Unfold trySkipToNext

let inline MaxBy f xs =
   Reduce (fun x y -> if f y > f x then y else x) xs

let inline Max xs = 
   Reduce max xs
            
let inline MinBy f xs =
   Reduce (fun x y -> if f y > f x then x else y) xs

let inline Min xs = 
   Reduce min xs
            
let inline Average (zs: ^a seq) : ^a =
   let total = Sum zs
   let count = SumBy (fun _ -> GenericConstants.One< ^a >) zs
   total / count

let inline AverageBy (g: ^a -> ^b ) (zs: ^a seq) : ^b =
   let total = SumBy g zs
   let count = SumBy (fun _ -> GenericConstants.One< ^a >) zs
   total / count

let ForAll f xs =
   Fold (fun acc x -> acc && f x) true xs

let ForAll2 f xs ys =
   Fold2 (fun acc x y -> acc && f x y) true xs ys

let rec ExistsAux f (xs:_ IEnumerator) =
   xs.MoveNext() && (f xs.Current || ExistsAux f xs)

let Exists f xs =
   ExistsAux f (Enumerator xs)

let rec Exists2Aux f (xs:_ IEnumerator) (ys:_ IEnumerator) =
   xs.MoveNext() && ys.MoveNext() && (f xs.Current ys.Current || Exists2Aux f xs ys)

let Exists2 f xs ys =
   Exists2Aux f (Enumerator xs) (Enumerator ys)

let Iterate f xs =
   Fold (fun () x -> f x) () xs

let Iterate2 f xs ys =
   Fold2 (fun () x y -> f x y) () xs ys

let IterateIndexed f xs =
   FoldIndexed (fun i () x -> f i x) () xs

let Map f xs =
   Delay <| fun () ->
      Enumerator xs
      |> Unfold (fun enum ->
         if enum.MoveNext() then Some(f enum.Current, enum)
         else None)

let MapIndexed f xs =
   Delay <| fun () ->
      (Enumerator xs, 0)
      |> Unfold (fun (enum, i) ->
         if enum.MoveNext() then Some(f i enum.Current, (enum, i+1))
         else None)

let Map2 f xs ys =
   Delay <| fun () ->
      let xs = Enumerator xs
      let ys = Enumerator ys
      ()
      |> Unfold (fun () ->
         if xs.MoveNext() && ys.MoveNext() then
            Some(f xs.Current ys.Current, ())
         else None)

let Map3 f xs ys zs =
   Delay <| fun () ->
      let xs = Enumerator xs
      let ys = Enumerator ys
      let zs = Enumerator zs
      ()
      |> Unfold (fun () ->
         if xs.MoveNext() && ys.MoveNext() && zs.MoveNext() then
            Some(f xs.Current ys.Current zs.Current, ())
         else None)

let Zip xs ys =
   Map2 (fun x y -> x, y) xs ys

let Zip3 xs ys zs =
   Map3 (fun x y z -> x, y, z) xs ys zs

let rec TryPickIndexedAux f i (xs:_ IEnumerator) =
   if xs.MoveNext() then 
      let result = f i xs.Current
      match result with
      | Some _ -> result
      | None -> TryPickIndexedAux f (i+1) xs
   else None

let TryPickIndexed f xs =
   TryPickIndexedAux f 0 (Enumerator xs)

let TryPick f xs =
   TryPickIndexed (fun _ x -> f x) xs

let Pick f xs =
   match TryPick f xs with
   | None -> invalidOp "List did not contain any matching elements"
   | Some x -> x 

let TryFindIndexed f xs =
   TryPickIndexed (fun i x -> if f i x then Some x else None) xs

let TryFind f xs =
   TryPickIndexed (fun _ x -> if f x then Some x else None) xs

let FindIndexed f xs =
   match TryFindIndexed f xs with
   | None -> invalidOp "List did not contain any matching elements"
   | Some x -> x
      
let Find f xs =
   FindIndexed (fun _ x -> f x) xs

let TryFindIndex f xs =
   TryPickIndexed (fun i x -> if f x then Some i else None) xs

let FindIndex f xs =
   match TryFindIndex f xs with
   | None -> invalidOp "List did not contain any matching elements"
   | Some x -> x

let Get n xs =
   let xs = Enumerator xs
   for i = 0 to n do
      if not (xs.MoveNext()) then
         failwith "index out of range"
   xs.Current

let Scan f seed xs =
   Delay <| fun () ->
      let xs = Enumerator xs
      None |> Unfold (function
         | None -> Some(seed, Some seed)
         | Some acc -> 
            if xs.MoveNext() then 
               let acc = f acc xs.Current
               Some (acc, Some acc)
            else None)

let ToList xs =
   Fold (fun acc x -> x::acc) [] xs
   |> List.rev

let OfList xs =
   xs |> Unfold (function
      | [] -> None
      | x::xs -> Some(x, xs))

let ToArray xs =
   let ys = FunScript.Core.Array.ZeroCreate 0// (Length xs)
   xs |> IterateIndexed (fun i x -> ys.[i] <- x)
   ys

let OfArray (xs:_ []) =
   0 |> Unfold (fun i ->
      if i < xs.Length then Some(xs.[i], i+1)
      else None)

let Initialize n f =
   0 |> Unfold (fun i ->
      if i < n then Some(f i, i+1)
      else None)

let Cast<'a> (xs:IEnumerable) =
   xs :?> 'a seq

let CompareWith f (xs:'a seq) (ys:'a seq) =
   let nonZero =
      Map2 (fun x y -> f x y) xs ys
      |> TryFind (fun i -> i <> 0)
   match nonZero with
   | Some diff -> diff
   | None -> Length xs - Length ys

let ExactlyOne xs =
   let xs = Enumerator xs
   if not <| xs.MoveNext() then failwith "Sequence was empty"
   let result = xs.Current
   if xs.MoveNext() then failwith "Sequence had multiple items"
   result

let InitializeInfinite f =
   Delay <| fun () ->
      0 |> Unfold (fun i ->
         Some(f i, i+1))

let Where f xs = 
   Filter f xs

let Take n xs =
   Delay <| fun () ->
      let xs = Enumerator xs
      0 |> Unfold (fun i ->
         if i < n && xs.MoveNext() then
            Some(xs.Current, i+1)
         else None)

let Truncate n xs =
   Take n xs

let TakeWhile f xs =
   Delay <| fun () ->
      let xs = Enumerator xs
      () |> Unfold (fun () ->
         if xs.MoveNext() && f xs.Current then Some(xs.Current, ())
         else None)

let Last xs =
   Reduce (fun _ x -> x) xs

let Pairwise xs =
   Scan 
      (fun (_, last) next -> last, next) 
      (Unchecked.defaultof<_>, Unchecked.defaultof<_>)
      xs
   |> Skip 1

let ReadOnly xs =
   Map id xs

let Singleton x =
   Some x |> Unfold (function
      | Some x -> Some(x, None)
      | None -> None)

let Compare xs ys =
   CompareWith compare xs ys

let SortBy f xs =
   let ys = xs |> ToArray
   Array.sortInPlaceBy f ys
   ys |> OfArray

let DistinctBy f xs =
   Scan (fun (_, acc) x ->
      let y = f x
      if acc |> Set.contains y then
         None, acc
      else Some x, acc |> Set.add y) 
      (None, Set.empty) xs
   |> Choose fst

let Distinct xs =
   DistinctBy id xs

let GroupBy (f : 'a -> 'b) (xs : 'a seq) =
   Fold (fun (acc:Map<_,_>) x ->
      let k = f x
      match acc.TryFind k with
      | Some vs -> acc.Add(k, x::vs)
      | None -> acc.Add(k, [x])) Collections.Map.empty xs
   |> Collections.Map.toSeq
   |> Map (fun (k, vs) -> k, vs :> 'a seq)

let CountBy f xs =
   GroupBy f xs
   |> Map (fun (k, vs) -> k, Length vs)

let Concat xs =
    Delay <| fun () ->
        let enum = Enumerator xs
        let tryGetNext innerEnum =
            let innerEnum = ref innerEnum
            let output = ref None
            let hasFinished = ref false
            while not !hasFinished do
                match !innerEnum with
                | None -> 
                    if enum.MoveNext() then
                        innerEnum := Some(Enumerator enum.Current)
                    else hasFinished := true
                | Some currentEnum ->
                    if currentEnum.MoveNext() then
                        output := Some currentEnum.Current
                        hasFinished := true
                    else innerEnum := None
            match !innerEnum, !output with
            | Some e, Some x -> Some(x, Some e)
            | _ -> None
        Unfold (fun x -> tryGetNext x) None


   //Fold Append Empty<_> xs

let Collect f xs =
   Map f xs |> Concat


module RuntimeHelpers =
    let EnumerateWhile (g : unit -> bool) (b: seq<'T>) : seq<'T> =
        Unfold(fun () ->
            if g() then Some(b, ())
            else None) ()
        |> Concat

    let EnumerateThenFinally (rest : seq<'T>) (compensation : unit -> unit)  =
        Delay <| fun () ->
          let enum = 
             try Enumerator rest
             finally compensation()
          enum
          |> Unfold (fun enum ->
             try
                if enum.MoveNext() then Some(enum.Current, enum)
                else None
             finally compensation())

    let EnumerateUsing (resource : 'T :> System.IDisposable) (rest: 'T -> #seq<'U>) =
        let isDisposed = ref false
        let disposable = resource :> IDisposable
        let disposeOnce() =
            if not !isDisposed then
                isDisposed := true
                disposable.Dispose()
        try EnumerateThenFinally (rest resource) disposeOnce
        finally disposeOnce()
