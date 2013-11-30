[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.ControlFlow

open NUnit.Framework


[<Test>]
let ``If then else expressions work``() =
   check 
      <@@ 
         let x = true
         if x then false
         else true
      @@>

[<Test>]
let ``If then else expressions work inline``() =
   check 
      <@@ 
         let f (x:string) = x
         let y = true
         f (if y then "foo" else "bar")
      @@>

[<Test>]
let ``While expressions work``() =
   checkAreEqual
      10. 
      <@@ 
         let mutable x = 1.
         while x < 10. do
            x <- x + 1.
         x
      @@>

[<Test>]
let ``For i = 0 to N expressions work``() =
   checkAreEqual
      10. 
      <@@ 
         let mutable x = 0.
         for i = 1 to 10 do
            x <- x + 1.
         x
      @@>

[<Test>]
let ``Lambdas inside for loops create closures``() =
   checkAreEqual
      1. 
      <@@ 
         let mutable f = fun () -> 0.
         for i = 1 to 10 do
            if i = 1 then f <- fun () -> float i
         f()
      @@>

[<Test>]
let ``Matching when the matched variable name is re-used in the case works``() =
   check
      <@@
         let xs = [1.; 2.; 3.]
         match xs with
         | x::xs -> x
         | _ -> failwith "never"
      @@>

[<Test>]
let ``Catching any exception works``() =
   checkAreEqual 100.
      <@@ 
         let f x =
            try
                if x then  raise (exn())
                else 1.0
            with _ -> 99.0
         f true + f false
      @@>