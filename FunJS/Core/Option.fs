[<FunJS.JS>]
module FunJS.Core.Option

open FunJS

let GetValue option = 
   match option with 
   | None -> invalidArg "option" "Expected Some got None"
   | Some x -> x

let IsSome option = 
   match option with  
   | None -> false 
   | Some _ -> true

let IsNone option = not (IsSome option)

let Count option = 
   match option with  
   | None -> 0 
   | Some _ -> 1

let Fold<'a, 'acc> (f:'acc -> 'a -> 'acc) (s:'acc) (inp:'a option) = 
   match inp with 
   | None -> s 
   | Some x -> f s x

let FoldBack<'a, 'acc> (f:'a -> 'acc -> 'acc) (inp:'a option) (s:'acc) = 
   match inp with 
   | None -> s 
   | Some x -> f x s

let Exists p inp = 
   match inp with 
   | None -> false 
   | Some x -> p x

let ForAll p inp = 
   match inp with 
   | None -> true 
   | Some x -> p x

let Iterate f inp = 
   match inp with 
   | None -> () 
   | Some x -> f x

let Map f inp = 
   match inp with 
   | None -> None 
   | Some x -> Some (f x)

let Bind f inp = 
   match inp with 
   | None -> None 
   | Some x -> f x

let ToArray option = 
   match option with 
   | None -> [||] 
   | Some x -> [| x |]

let ToList option = 
   match option with
   | None -> [] 
   | Some x -> [ x ]