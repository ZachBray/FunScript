[<FunJS.JS>]
module FunJS.Core.Range

open System.Collections
open System.Collections.Generic

let inline customStep (first: ^a) (stepping: ^b) (last: ^a) : ^a seq =
   first |> FunJS.Core.Seq.Unfold (fun x -> 
      if x <= last then Some(x, x + stepping)
      else None)

let inline oneStep (first: ^a) (last: ^a) : ^a seq =
   customStep first GenericConstants.One< ^a> last