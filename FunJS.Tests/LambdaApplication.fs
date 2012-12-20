[<FunJS.JS>]
[<NUnit.Framework.TestFixture>] 
module FunJS.Tests.LambdaApplication

open NUnit.Framework


[<Test>]
let ``Defining lambdas works``() =
   check <@@ (fun x -> x % 2. = 0.)(2.) @@>

[<Test>]
let ``Applying let bound functions works``() =
   check 
      <@@ 
         let isOdd x = x % 2. <> 0.
         isOdd 2.
      @@>

type MapDelegate<'a,'b> = delegate of 'a -> 'b

[<Test>]
let ``Constructing delegates works``() =
   check 
      <@@ 
         let squ = MapDelegate(fun x -> x * x)
         ()
      @@>

[<Test>]
let ``Invoking delegates works``() =
   check 
      <@@ 
         let squ = MapDelegate(fun x -> x * x)
         squ.Invoke 2.
      @@>

type TupledMapDelegate<'a,'b,'c> = delegate of 'a * 'b -> 'c

[<Test>]
let ``Constructing tupled delegates works``() =
   check 
      <@@ 
         let diff = TupledMapDelegate(fun x y -> x - y)
         ()
      @@>

[<Test>]
let ``Invoking tupled delegates works``() =
   check 
      <@@ 
         let diff = TupledMapDelegate(fun x y -> x - y)
         diff.Invoke(4.,2.)
      @@>