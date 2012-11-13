module FunJS.Tests.Options

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Option.Value works``() =
   check  
      <@@ 
         let x = Some 1.
         x.Value
      @@>

[<Fact>]
let ``Option.IsSome works``() =
   check  
      <@@ 
         let x = Some 1.
         x.IsSome
      @@>

[<Fact>]
let ``Option.IsNone works``() =
   check  
      <@@ 
         let x = Some 1.
         x.IsNone
      @@>

[<Fact>]
let ``Option.bind works``() =
   check  
      <@@ 
         let f x =
            if x = 1. then Some x
            else None
         let r = Some 1. |> Option.bind f
         r.Value
      @@>

[<Fact>]
let ``Option.count works``() =
   check  
      <@@ 
         Some 1. 
         |> Option.count
         |> float
      @@>

[<Fact>]
let ``Option.exists works``() =
   check  
      <@@ 
         Some 1. |> Option.exists ((=) 1.)
      @@>

[<Fact>]
let ``Option.fold works``() =
   check  
      <@@ 
         Some 1. |> Option.fold (+) 1.
      @@>

[<Fact>]
let ``Option.foldBack works``() =
   check  
      <@@ 
         Option.foldBack (+) (Some 1.) 1.
      @@>

[<Fact>]
let ``Option.forall works``() =
   check  
      <@@ 
         Some 1. |> Option.forall ((=) 1.)
      @@>


[<Fact>]
let ``Option.get works``() =
   check  
      <@@ 
         Some 1. |> Option.get
      @@>

[<Fact>]
let ``Option.isNone works``() =
   check  
      <@@ 
         Some 1. |> Option.isNone
      @@>

[<Fact>]
let ``Option.isSome works``() =
   check  
      <@@ 
         Some 1. |> Option.isSome
      @@>

[<Fact>]
let ``Option.iter works``() =
   check  
      <@@ 
         let x = ref 0.
         Some 1. |> Option.iter (fun y -> x := !x + y)
         !x
      @@>

[<Fact>]
let ``Option.map works``() =
   check  
      <@@ 
         Some 1. |> Option.map ((*) 2.)
         |> Option.get
      @@>

[<Fact>]
let ``Option.toArray works``() =
   check  
      <@@ 
         let xs = Some 1. |> Option.toArray
         xs.[0]
      @@>

[<Fact>]
let ``Option.toList works``() =
   check  
      <@@ 
         let xs = Some 1. |> Option.toList
         xs.Head
      @@>