[<FunJS.JS>]
module FunJS.Core.Seq

open System.Collections.Generic

let rec FoldIndexedAux f i acc (xs:_ IEnumerator) =
   if xs.MoveNext() then FoldIndexedAux f (i+1) (f i acc xs.Current) xs
   else acc

let FoldIndexed<'a,'acc> f (seed:'acc) (xs: 'a seq) = 
   FoldIndexedAux f 0 seed (xs.GetEnumerator())

let Fold<'a,'acc> f (seed:'acc) (xs: 'a seq) = 
   FoldIndexed (fun _ acc x -> f acc x) seed xs

let ToList xs =
   Fold (fun acc x -> x::acc) [] xs
   |> List.Reverse