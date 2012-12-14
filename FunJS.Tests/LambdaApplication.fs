[<FunJS.JS>]
module FunJS.Tests.LambdaApplication

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Defining lambdas works``() =
   check <@@ (fun x -> x % 2. = 0.)(2.) @@>

[<Fact>]
let ``Applying let bound functions works``() =
   check 
      <@@ 
         let isOdd x = x % 2. <> 0.
         isOdd 2.
      @@>

type MapDelegate<'a,'b> = delegate of 'a -> 'b

[<Fact>]
let ``Constructing delegates works``() =
   check 
      <@@ 
         let squ = MapDelegate(fun x -> x * x)
         ()
      @@>

[<Fact>]
let ``Invoking delegates works``() =
   check 
      <@@ 
         let squ = MapDelegate(fun x -> x * x)
         squ.Invoke 2.
      @@>

type TupledMapDelegate<'a,'b,'c> = delegate of 'a * 'b -> 'c

[<Fact>]
let ``Constructing tupled delegates works``() =
   check 
      <@@ 
         let diff = TupledMapDelegate(fun x y -> x - y)
         ()
      @@>

[<Fact>]
let ``Invoking tupled delegates works``() =
   check 
      <@@ 
         let diff = TupledMapDelegate(fun x y -> x - y)
         diff.Invoke(4.,2.)
      @@>