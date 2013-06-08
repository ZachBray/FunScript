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