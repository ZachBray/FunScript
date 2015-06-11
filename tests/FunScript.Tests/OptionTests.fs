[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Options

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
let ``pattern matching on None works``() =
   check  
      <@@ 
         match None with
         | Some _ -> "foo"
         | None -> "bar"
      @@>

[<Test>]
let ``pattern matching on Some works``() =
   check  
      <@@ 
         match Some 1 with
         | None -> "bar"
         | Some _ -> "foo"
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

[<Test>]
let ``Some unit works``() =
   check  
      <@@ 
         let xs = Some()
         match xs with
         | None -> 0.0
         | Some() -> 1.0
      @@>
      
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations

[<Test>] // Note: this is for some HashBang functionality.
let ``Some unit via reflection works``() =
   let genericOption = typedefof<_ option>
   let unitOption = genericOption.MakeGenericType [| typeof<unit> |]
   let someMethod = unitOption.GetMethod("Some")
   let someUnit = Expr.Call(someMethod, [Expr.Value(())])
   check  
      <@@ 
         let xs = %%someUnit
         match xs with
         | None -> 0.0
         | Some() -> 1.0
      @@>

open System

[<Test>]
let ``Empty Nullable construction works``() =
   check  
      <@@ 
         let x = Nullable<int>()
         x.HasValue
      @@>

[<Test>]
let ``Filled Nullable construction works``() =
   check  
      <@@ 
         let x = Nullable(12)
         x.HasValue
      @@>

[<Test>]
let ``Filled Nullable Value access works``() =
   check  
      <@@ 
         let x = Nullable(12.)
         x.Value
      @@>