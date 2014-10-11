[<FunScript.JS>]
[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.SeqExpressions

open NUnit.Framework

[<Test>]
let ``empty seq expressions work``() =
   check  
      <@@ 
         let xs = seq { do () }
         xs |> Seq.length |> float
      @@>

[<Test>]
let ``yield in seq expressions works``() =
   check  
      <@@ 
         let xs = seq { yield 1 }
         xs |> Seq.length |> float
      @@>

[<Test>]
let ``yield from in seq expressions works``() =
   check  
      <@@ 
         let ys = seq { yield 1 }
         let xs = seq { yield! ys }
         xs |> Seq.length |> float
      @@>

[<Test>]
let ``infinite seq expressions work``() =
   check  
      <@@ 
        let rec green () = seq { 
            yield "green"
            yield! yellow() }
        and yellow () = seq { 
            yield "yellow"
            yield! red() }
        and red () = seq { 
            yield "red" 
            yield! green() }
        green() |> Seq.nth 12
      @@>

[<Test>]
let ``multiple yields in seq expressions work``() =
   check  
      <@@ 
        let upTo n = seq {
            for i = 1 to n do
                yield n
        }
        let numbers () = seq { 
            yield 8
            yield! upTo 5 
            yield 4
            yield! upTo 3
            yield 2
         }
        numbers() |> Seq.sumBy float
      @@>

type 'a Tree =
    | Leaf
    | Node of 'a Tree * 'a * 'a Tree

[<Test>]
let ``recursive seq expressions work``() =
   check  
      <@@ 
        let rec traverse t = seq { 
            match t with
            | Leaf -> ()
            | Node(xs, y, zs) ->
                yield y
                yield! traverse xs
                yield! traverse zs |> Seq.map ((*) 2.)
         }
        let t =
            Node(Node(Leaf, 1., Leaf), 2., Node(Leaf, 3., Leaf))
        traverse t |> Seq.sumBy float
      @@>

[<Test>]
let ``combine in seq expressions works``() =
   check  
      <@@ 
         let xs = seq { yield 1; yield 2 }
         xs |> Seq.length |> float
      @@>

[<Test>]
let ``for in seq expressions works``() =
   check  
      <@@ 
         let xs = seq { 
            for x in 1 .. 10 do
               yield x 
         }
         xs |> Seq.length |> float
      @@>

[<Test>]
let ``while in seq expressions works``() =
   check  
      <@@
         let n = ref 0.0
         seq {
            while !n < 10.0 do
                n := !n + 1.0
                yield !n
         } |> Seq.sum
      @@>

[<Test>]
let ``try...finally in seq expressions works``() =
   check  
      <@@
         let n = ref 0.0
         try
             seq { 
                try 
                    raise (exn "My message")
                finally 
                    n := !n + 1.0
             } |> Seq.iter ignore
         with _ -> ()
         !n
      @@>

open System
type DisposableAction(f) =
    interface IDisposable with
        member __.Dispose() = f()

[<Test>]
let ``use in seq expressions works``() =
   check  
      <@@
         let n = ref 0.0
         seq { 
            use x = new DisposableAction(fun () ->
                n := !n + 1.0)
            ignore x
         } |> Seq.iter ignore
         !n
      @@>

[<Test>]
let ``array expressions work``() =
   check  
      <@@ 
         let xs = [| 
            for x in 1 .. 10 do
               yield x 
         |]
         xs |> Array.length |> float
      @@>

[<Test>]
let ``list expressions work``() =
   check  
      <@@ 
         let xs = [
            for x in 1 .. 10 do
               yield x 
         ]
         xs |> List.length |> float
      @@>