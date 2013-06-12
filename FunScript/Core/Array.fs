[<FunScript.JS>]
module FunScript.Core.Array

open FunScript
open System
   
[<JSEmit("return {0}.length;")>]
let BoxedLength (xs:System.Array): int =
   failwith "never"

[<JSEmit("return {0}.length;")>]
let Length (xs:'a[]): int =
   failwith "never"

[<JSEmit("return {0}.slice({1}, {1} + {2});")>]
let GetSubArray (xs:'a[]) (offset:int) (length:int): 'a[] =
   failwith "never"
      
[<JSEmit("return {0}.slice(0);")>]
let Copy (xs:'a[]): 'a[] =
   failwith "never"

[<JSEmit("{1}.sort(function(a,b) { return {0}(a)(b); });")>]
let SortInPlaceWith (f:'a -> 'a -> int) (xs:'a[]): unit =
   failwith "never"

let SortInPlaceBy f xs =
   SortInPlaceWith (fun (x:'a) (y:'a) -> 
      let x = f x
      let y = f y
      compare x y) xs

let SortInPlace xs =
   SortInPlaceWith (fun (x:'a) (y:'a) -> 
      compare x y) xs

let SortBy f xs =
   let ys = Copy xs
   SortInPlaceBy f ys
   ys

let Sort xs =
   let ys = Copy xs
   SortInPlace ys
   ys

let SortWith f xs =
   let ys = Copy xs
   SortInPlaceWith f ys
   ys

[<JSEmit("return new Array({0});")>]
let ZeroCreate (size:int) : 'a[] = failwith "never"

let Fill (xs:'a []) offset count value =
   for i = offset to offset + count - 1 do
      xs.[i] <- value

let CopyTo (xs:'a []) fromOffset (ys:'a []) toOffset count =
   let diff = toOffset - fromOffset
   for i = fromOffset to fromOffset + count - 1 do
      ys.[i + diff] <- xs.[i]
   ys

let IsEmpty xs = 
   Length xs = 0

let FoldIndexed f seed xs =
   let mutable acc = seed
   for i = 0 to Length xs - 1 do
      acc <- f i acc xs.[i]
   acc

let Fold<'a,'acc> f (seed:'acc) (xs: 'a []) = 
   FoldIndexed (fun _ acc x -> f acc x) seed xs

let FoldBackIndexed<'a,'acc> f (xs: 'a []) (seed:'acc) =
   let mutable acc = seed
   let size = Length xs
   for i = 1 to size do
      acc <- f (i-1) xs.[size - i] acc
   acc

let FoldBack<'a,'acc> f (xs: 'a []) (seed:'acc) = 
   FoldBackIndexed (fun _ x acc -> f x acc) xs seed 

let FoldIndexed2 f seed xs ys =
   let mutable acc = seed
   if Length xs <> Length ys then failwith "Arrays had different lengths"
   for i = 0 to Length xs - 1 do
      acc <- f i acc xs.[i] ys.[i]
   acc

let Fold2<'a, 'b, 'acc> f (seed:'acc) (xs: 'a []) (ys: 'b [])  = 
   FoldIndexed2 (fun _ acc x y -> f acc x y) seed xs ys

let FoldBackIndexed2<'a, 'b, 'acc> f (xs: 'a []) (ys: 'b []) (seed:'acc) =
   let mutable acc = seed
   if Length xs <> Length ys then failwith "Arrays had different lengths"
   let size = Length xs
   for i = 1 to size do
      acc <- f (i-1) xs.[size - i] ys.[size - i] acc
   acc

let FoldBack2<'a, 'b, 'acc> f (xs: 'a []) (ys: 'b []) (seed:'acc) = 
   FoldBackIndexed2 (fun _ x y acc -> f x y acc) xs ys seed 

let Iterate f xs =
   Fold (fun () x -> f x) () xs

let IterateIndexed f xs =
   FoldIndexed (fun i () x -> f i x) () xs

let ForAll f xs =
   Fold (fun acc x -> f x && acc) true xs

let Permute f xs = 
   let size = Length xs
   let ys  = ZeroCreate size
   let checkFlags = ZeroCreate size
   IterateIndexed (fun i x ->
      let j = f i 
      if j < 0 || j >= size then 
         invalidOp "Not a valid permutation"
      ys.[j] <- x
      checkFlags.[j] <- 1) xs
   let isValid = ForAll ((=) 1) checkFlags
   if not isValid then
      invalidOp "Not a valid permutation"
   ys

let Reverse xs =
   let size = Length xs
   let ys = ZeroCreate size
   for i = 0 to size - 1 do
      ys.[i] <- xs.[size - 1 - i]
   ys

let Scan<'a, 'acc> f (seed:'acc) (xs: 'a []) =
   let ys = ZeroCreate (Length xs + 1)
   ys.[0] <- seed
   for i = 0 to Length xs - 1 do
      ys.[i + 1] <- f ys.[i] xs.[i]
   ys

let ScanBack<'a, 'acc> f (xs: 'a []) (seed:'acc) =
   let ys = ZeroCreate (Length xs + 1)
   let size = Length xs
   ys.[Length xs] <- seed
   for i = 1 to Length xs do
      ys.[size - i] <- f xs.[size - i] ys.[size - i + 1]
   ys

[<JSEmit("return {0}.concat({1});")>]
let Append (xs:'a []) (ys:'a []) : 'a [] = 
   failwith "never"

[<JSEmit("return [].concat.apply([], {0});")>]
let ConcatImpl (xss: 'a [][]) : 'a [] = failwith "never"

let MapIndexed f (xs:'a []) =
   let ys = ZeroCreate (Length xs)
   for i = 0 to Length xs - 1 do
      ys.[i] <- f i xs.[i]
   ys

let Map f xs =
   MapIndexed (fun _ x -> f x) xs

let MapIndexed2 f (xs:'a []) (ys:'b []) =
   if Length xs <> Length ys then failwith "Arrays had different lengths"
   let zs = ZeroCreate(Length xs)
   for i = 0 to Length xs - 1 do
      zs.[i] <- f i xs.[i] ys.[i]
   zs

let Map2 f xs ys =
   MapIndexed2 (fun _ x y -> f x y) xs ys

let MapIndexed3 f (xs:'a []) (ys:'b []) (zs:'c []) =
   if Length xs <> Length ys || Length ys <> Length zs then failwith "Arrays had different lengths"
   let rs = ZeroCreate(Length xs)
   for i = 0 to Length xs - 1 do
      rs.[i] <- f i xs.[i] ys.[i] zs.[i]
   rs

let Map3 f xs ys zs =
   MapIndexed3 (fun _ x y z -> f x y z) xs ys zs

let Concat (xs:'a [] seq) : 'a[] =
   xs 
   |> Array.ofSeq
   |> ConcatImpl

let Collect f xs =
   Map f xs
   |> ConcatImpl

let Iterate2 f xs ys =
   Fold2 (fun () x y -> f x y) () xs ys

let IterateIndexed2 f xs ys =
   FoldIndexed2 (fun i () x y -> f i x y) () xs ys
      
let Empty<'a> : 'a [] = [||]

let rec TryPickIndexedAux f i xs =
   if i = Length xs then None
   else 
      let result = f i xs.[i]
      match result with
      | Some _ -> result
      | None -> TryPickIndexedAux f (i+1) xs

let TryPickIndexed f xs =
   TryPickIndexedAux f 0 xs

let TryPick f xs =
   TryPickIndexed (fun _ x -> f x) xs

let Pick f xs =
   match TryPick f xs with
   | None -> invalidOp "Array did not contain any matching elements"
   | Some x -> x 

let TryFindIndexed f xs =
   TryPickIndexed (fun i x -> if f i x then Some x else None) xs

let TryFind f xs =
   TryPickIndexed (fun _ x -> if f x then Some x else None) xs

let FindIndexed f xs =
   match TryFindIndexed f xs with
   | None -> invalidOp "Array did not contain any matching elements"
   | Some x -> x
      
let Find f xs =
   FindIndexed (fun _ x -> f x) xs

let Get xs n =
   FindIndexed (fun i _ -> n = i) xs
      
let TryFindIndex f xs =
   TryPickIndexed (fun i x -> if f x then Some i else None) xs

let FindIndex f xs =
   match TryFindIndex f xs with
   | None -> invalidOp "Array did not contain any matching elements"
   | Some x -> x

let Filter f xs =
   let ys = ZeroCreate 0
   let mutable j = 0
   for i = 0 to Length xs - 1 do
      if f xs.[i] then 
         ys.[j] <- xs.[i]
         j <- j + 1
   ys

let Partition f xs =
   let ys = ZeroCreate 0
   let zs = ZeroCreate 0
   let mutable j = 0
   let mutable k = 0
   for i = 0 to Length xs - 1 do
      if f xs.[i] then 
         ys.[j] <- xs.[i]
         j <- j + 1
      else 
         zs.[k] <- xs.[i]
         k <- k + 1
   ys, zs

let Choose f xs =
   let ys = ZeroCreate 0
   let mutable j = 0
   for i = 0 to Length xs - 1 do
      match f xs.[i] with
      | Some y -> 
         ys.[j] <- y
         j <- j + 1
      | None -> ()
   ys

let Initialize n f =
   let mutable xs = ZeroCreate 0
   for i = 0 to n - 1 do xs.[i] <- f i
   xs

let Create n x =
   let mutable xs = ZeroCreate 0
   for i = 0 to n - 1 do xs.[i] <- x
   xs    

let Replicate n x =
   Initialize n (fun _ -> x)

let Reduce f xs =
   if Length xs = 0 then invalidOp "Array was empty"
   else FoldIndexed (fun i acc x -> if i = 0 then x else f acc x) Unchecked.defaultof<_> xs

let ReduceBack f xs =
   if Length xs = 0 then invalidOp "Array was empty"
   else FoldBackIndexed (fun i x acc -> if i = 0 then x else f acc x) xs Unchecked.defaultof<_>

let ForAll2 f xs ys =
   Fold2 (fun acc x y -> acc && f x y) true xs ys

let rec ExistsOffset f xs i =
   if i = Length xs then false
   else f xs.[i] || ExistsOffset f xs (i+1)

let Exists f xs = 
   ExistsOffset f xs 0
      
let rec ExistsOffset2 f xs (ys:_ []) i =
   if i = Length xs then false
   else f xs.[i] ys.[i] || ExistsOffset2 f xs ys (i+1)

let rec Exists2 f xs ys =
   if Length xs <> Length ys then failwith "Arrays had different lengths"
   ExistsOffset2 f xs ys 0

let Unzip xs =
   let bs = ZeroCreate 0
   let cs = ZeroCreate 0
   IterateIndexed (fun i (b, c) ->
      bs.[i] <- b
      cs.[i] <- c
   ) xs
   bs, cs

let Unzip3 xs =
   let bs = ZeroCreate 0
   let cs = ZeroCreate 0
   let ds = ZeroCreate 0
   IterateIndexed (fun i (b, c, d) ->
      bs.[i] <- b
      cs.[i] <- c
      ds.[i] <- d
   ) xs
   bs, cs, ds

let Zip xs ys =
   Map2 (fun x y -> x, y) xs ys

let Zip3 xs ys zs =
   Map3 (fun x y z -> x, y, z) xs ys zs

let inline Sum (xs: ^a []) : ^a =
   Fold (fun acc x -> acc + x) GenericConstants.Zero xs

let inline SumBy (f:^a -> ^b) (xs: ^a []) : ^b =
   Fold (fun acc x -> acc + f x) GenericConstants.Zero xs

let inline MaxBy f xs =
   Reduce (fun x y -> if f y > f x then y else x) xs

let inline Max xs = 
   Reduce max xs
            
let inline MinBy f xs =
   Reduce (fun x y -> if f y > f x then x else y) xs

let inline Min xs = 
   Reduce min xs
            
let inline Average (zs: ^a []) : ^a =
   let total = Sum zs
   let count = SumBy (fun _ -> GenericConstants.One< ^a >) zs
   total / count

let inline AverageBy (g: ^a -> ^b ) (zs: ^a []) : ^b =
   let total = SumBy g zs
   let count = SumBy (fun _ -> GenericConstants.One< ^a >) zs
   total / count

   