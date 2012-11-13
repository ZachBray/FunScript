module FunJS.Tests.LetBindings

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Let bindings can be generated``() =
   check 
      <@@
         let x = true
         x
      @@>

[<Fact>]
let ``Let bindings inline can be generated``() =
   check 
      <@@
         let f (x:string) = x
         f (let y = "foo" in y)
      @@>

[<Fact>]
let ``Recursive let bindings can be generated``() =
   check 
      <@@
         let rec even i =
            if i < 10. then
               odd (i+1.)
            else i
         and odd i =
            even (i+1.)
         even 0.
      @@>


[<Fact>]
let ``Mutable let bindings can be generated``() =
   check 
      <@@
         let mutable x = true
         x
      @@>

[<Fact>]
let ``Mutations of let bindings can be generated``() =
   checkAreEqual
      false 
      <@@
         let mutable x = true
         x <- false
         x
      @@>


[<Fact>] // see: http://www.adequatelygood.com/2010/2/JavaScript-Scoping-and-Hoisting
let ``Inner-scope let-bindings do not destroy outer-scope let-bindings``() =
   check 
      <@@
         let x = 10.
         let y =
            let x = 1.
            x * 1.
         y * x
      @@>