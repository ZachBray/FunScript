[<FunJS.JS>]
module FunJS.Core.List

open FunJS

type 'a list =
   | Nil
   | Cons of 'a * 'a list

let CreateCons x xs = Cons(x, xs)

let Head = function
   | Nil -> failwith "List was empty"
   | Cons(x,xs) -> x

let Tail = function
   | Nil -> failwith "List was empty"
   | Cons(x,xs) -> xs

let rec FoldIndexedAux f i acc = function
   | Nil -> acc
   | Cons(x,xs) -> FoldIndexedAux f (i+1) (f i acc x) xs

let FoldIndexed<'a,'acc> f (seed:'acc) (xs: 'a list) = 
   FoldIndexedAux f 0 seed xs

let Fold<'a,'acc> f (seed:'acc) (xs: 'a list) = 
   FoldIndexed (fun _ acc x -> f acc x) seed xs

let Reverse xs =
   Fold (fun acc x -> Cons(x,acc)) Nil xs

let FoldBack<'a,'acc> f (xs: 'a list) (seed:'acc) = 
   Fold (fun acc x -> f x acc) seed (Reverse xs)

let rec FoldIndexed2Aux f i acc bs cs =
   match bs, cs with
   | Nil, Nil -> acc
   | Cons(x,xs), Cons(y,ys) -> FoldIndexed2Aux f (i+1) (f i acc x y) xs ys
   | _ -> invalidOp "Lists had different lengths"

let FoldIndexed2<'a, 'b, 'acc> f (seed:'acc) (xs: 'a list) (ys: 'b list) = 
   FoldIndexed2Aux f 0 seed xs ys

let Fold2<'a, 'b, 'acc> f (seed:'acc) (xs: 'a list) (ys: 'b list) = 
   FoldIndexed2 (fun _ acc x y -> f acc x y) seed xs ys

let FoldBack2<'a, 'b, 'acc> f (xs: 'a list) (ys: 'b list) (seed:'acc) = 
   Fold2 (fun acc x y -> f acc x y) seed (Reverse xs) (Reverse ys)

let rec FoldIndexed3Aux f i acc bs cs ds =
   match bs, cs, ds with
   | Nil, Nil, Nil -> acc
   | Cons(x,xs), Cons(y,ys), Cons(z,zs) -> FoldIndexed3Aux f (i+1) (f i acc x y z) xs ys zs
   | _ -> invalidOp "Lists had different lengths"

let FoldIndexed3<'a, 'b, 'c, 'acc> f (seed:'acc) (xs: 'a list) (ys: 'b list) (zs: 'c list) = 
   FoldIndexed3Aux f 0 seed xs ys zs

let Fold3<'a, 'b, 'c, 'acc> f (seed:'acc) (xs: 'a list) (ys: 'b list) (zs: 'c list) = 
   FoldIndexed3 (fun _ acc x y -> f acc x y) seed xs ys zs

let Scan<'a, 'acc> f (seed:'acc) (xs: 'a list) =
   Fold (fun acc x ->
      match acc with
      | Nil -> failwith "never"
      | Cons(y,ys) -> Cons(f y x, acc)) (Cons(seed,Nil)) xs
   |> Reverse

let ScanBack<'a, 'acc> f (xs: 'a list) (seed:'acc) =
   Scan (fun acc x -> f x acc) seed (Reverse xs)
   |> Reverse

let Length xs = 
   Fold (fun acc _ -> acc + 1) 0 xs

let Append xs ys =
   Fold (fun acc x -> Cons(x,acc)) ys (Reverse xs)

let Collect f xs =
   Fold (fun acc x -> Append (f x) acc) Nil (Reverse xs)

let Map f xs =
   Fold (fun acc x -> (Cons(f x,acc))) Nil xs
   |> Reverse

let MapIndexed f xs =
   FoldIndexed (fun i acc x -> Cons(f i x,acc)) Nil xs
   |> Reverse

let Map2 f xs ys =
   Fold2 (fun acc x y -> Cons(f x y,acc)) Nil xs ys
   |> Reverse

let MapIndexed2 f xs ys =
   FoldIndexed2 (fun i acc x y  -> Cons(f i x y, acc)) Nil xs ys
   |> Reverse

let Map3 f xs ys zs =
   Fold3 (fun acc x y z -> Cons(f x y z,acc)) Nil xs ys zs
   |> Reverse

let MapIndexed3 f xs ys zs =
   FoldIndexed3 (fun i acc x y z -> Cons(f i x y z, acc)) Nil xs ys zs
   |> Reverse

let Iterate f xs =
   Fold (fun () x -> f x) () xs

let Iterate2 f xs ys =
   Fold2 (fun () x y -> f x y) () xs ys

let IterateIndexed f xs =
   FoldIndexed (fun i () x -> f i x) () xs

let IterateIndexed2 f xs ys =
   FoldIndexed2 (fun i () x y -> f i x y) () xs ys

let OfArray xs =
   Array.FoldBack (fun x acc -> Cons(x,acc)) xs Nil

let ToArray xs =
   let size = Length xs
   let ys = Array.ZeroCreate size
   IterateIndexed (fun i x -> ys.[i] <- x) xs
   ys

let Empty<'a> : 'a list = Nil

let IsEmpty = function
   | Nil -> true
   | _ -> false

let rec TryPickIndexedAux f i = function
   | Nil -> None
   | Cons(x,xs) -> 
      let result = f i x
      match result with
      | Some _ -> result
      | None -> TryPickIndexedAux f (i+1) xs

let TryPickIndexed f xs =
   TryPickIndexedAux f 0 xs

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

let Get xs n =
   FindIndexed (fun i _ -> n = i) xs
      
let Filter f xs =
   Fold (fun acc x ->
      if f x then Cons(x,acc)
      else acc) Nil xs

let Partition f xs =
   Fold (fun (lacc, racc) x ->
      if f x then Cons(x, lacc), racc
      else lacc,Cons(x,racc)) (Nil,Nil) xs

let Choose f xs =
   Fold (fun acc x ->
      match f x with
      | Some y -> Cons(y, acc)
      | None -> acc) Nil xs

let Initialize n f =
   let mutable xs = Nil
   for i = 1 to n do xs <- Cons(f (n - i), xs)
   xs

let Replicate n x =
   Initialize n (fun _ -> x)

let Reduce f = function
   | Nil -> invalidOp "List was empty"
   | Cons(h,t) -> Fold f h t

let ReduceBack f = function
   | Nil -> invalidOp "List was empty"
   | Cons(h,t) -> FoldBack f t h
   
let ForAll f xs =
   Fold (fun acc x -> acc && f x) true xs

let ForAll2 f xs ys =
   Fold2 (fun acc x y -> acc && f x y) true xs ys

let rec Exists f = function
   | Nil -> false
   | Cons(x,xs) -> f x || Exists f xs

let rec Exists2 f bs cs =
   match bs, cs with
   | Nil, Nil -> false
   | Cons(x,xs), Cons(y,ys) -> f x y || Exists2 f xs ys
   | _ -> invalidOp "Lists had different lengths"

let Unzip xs =
   Fold (fun (lacc, racc) (x, y) -> Cons(x,lacc), Cons(y,racc)) (Nil,Nil) xs

let Unzip3 xs =
   Fold (fun (lacc, macc, racc) (x, y, z) -> Cons(x,lacc), Cons(y,macc), Cons(z,racc)) (Nil,Nil,Nil) xs

let Zip xs ys =
   Map2 (fun x y -> x, y) xs ys

let Zip3 xs ys zs =
   Map3 (fun x y z -> x, y, z) xs ys zs

let Sort xs =
   xs
   |> ToArray 
   |> Array.Sort
   |> OfArray

let SortWith f xs =
   xs
   |> ToArray 
   |> Array.SortWith f
   |> OfArray

let inline Sum (xs: ^a list) : ^a =
   Fold (fun acc x -> acc + x) GenericConstants.Zero xs

let inline SumBy (f:^a -> ^b) (xs: ^a list) : ^b =
   Fold (fun acc x -> acc + f x) GenericConstants.Zero xs

let inline MaxBy f xs =
   Reduce (fun x y -> if f y > f x then y else x) xs

let inline Max xs = 
   Reduce max xs
            
let inline MinBy f xs =
   Reduce (fun x y -> if f y > f x then x else y) xs

let inline Min xs = 
   Reduce min xs
            
let inline Average (zs: ^a list) : ^a =
   let total = Sum zs
   let count = SumBy (fun _ -> GenericConstants.One< ^a >) zs
   total / count

let inline AverageBy (g: ^a -> ^b ) (zs: ^a list) : ^b =
   let total = SumBy g zs
   let count = SumBy (fun _ -> GenericConstants.One< ^a >) zs
   total / count

let Permute f xs =
   xs  
   |> ToArray
   |> Array.Permute f 
   |> OfArray

let SortBy f xs =
   let ys = xs |> ToArray
   Array.sortInPlaceBy f ys
   ys |> OfArray
