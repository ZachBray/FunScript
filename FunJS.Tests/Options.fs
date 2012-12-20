[<NUnit.Framework.TestFixture>] 
module FunJS.Tests.Options

open NUnit.Framework


[<Test>]
let ``Option.Value works``() =
   check  
      <@@ 
         let x = Some 1.
         x.Value
      @@>

[<Test>]
let ``Option.IsSome works``() =
   check  
      <@@ 
         let x = Some 1.
         x.IsSome
      @@>

[<Test>]
let ``Option.IsNone works``() =
   check  
      <@@ 
         let x = Some 1.
         x.IsNone
      @@>

[<Test>]
let ``Option.bind works``() =
   check  
      <@@ 
         let f x =
            if x = 1. then Some x
            else None
         let r = Some 1. |> Option.bind f
         r.Value
      @@>

[<Test>]
let ``Option.count works``() =
   check  
      <@@ 
         Some 1. 
         |> Option.count
         |> float
      @@>

[<Test>]
let ``Option.exists works``() =
   check  
      <@@ 
         Some 1. |> Option.exists ((=) 1.)
      @@>

[<Test>]
let ``Option.fold works``() =
   check  
      <@@ 
         Some 1. |> Option.fold (+) 1.
      @@>

[<Test>]
let ``Option.foldBack works``() =
   check  
      <@@ 
         Option.foldBack (+) (Some 1.) 1.
      @@>

[<Test>]
let ``Option.forall works``() =
   check  
      <@@ 
         Some 1. |> Option.forall ((=) 1.)
      @@>


[<Test>]
let ``Option.get works``() =
   check  
      <@@ 
         Some 1. |> Option.get
      @@>

[<Test>]
let ``Option.isNone works``() =
   check  
      <@@ 
         Some 1. |> Option.isNone
      @@>

[<Test>]
let ``Option.isSome works``() =
   check  
      <@@ 
         Some 1. |> Option.isSome
      @@>

[<Test>]
let ``Option.iter works``() =
   check  
      <@@ 
         let x = ref 0.
         Some 1. |> Option.iter (fun y -> x := !x + y)
         !x
      @@>

[<Test>]
let ``Option.map works``() =
   check  
      <@@ 
         Some 1. |> Option.map ((*) 2.)
         |> Option.get
      @@>

[<Test>]
let ``Option.toArray works``() =
   check  
      <@@ 
         let xs = Some 1. |> Option.toArray
         xs.[0]
      @@>

[<Test>]
let ``Option.toList works``() =
   check  
      <@@ 
         let xs = Some 1. |> Option.toList
         xs.Head
      @@>