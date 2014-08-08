[<FunScript.JS>]
module FunScript.Core.Range

open System.Collections
open System.Collections.Generic

let inline customStep (first: ^a) (stepping: ^b) (last: ^a) : ^a seq =
   let zero: ^b = FunScript.Core.GenericConstants.Zero
   first |> FunScript.Core.Seq.Unfold (fun x -> 
      if (stepping > zero && x <= last) || (stepping < zero && x >= last) then Some(x, x + stepping)
      else None)

let inline oneStep (first: ^a) (last: ^a) : ^a seq =
   customStep first GenericConstants.One< ^a> last