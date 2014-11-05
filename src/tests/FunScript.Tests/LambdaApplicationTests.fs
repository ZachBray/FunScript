[<FunScript.JS>]
[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.LambdaApplication

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

[<Test>]
let ``Currying works``() =
   check 
      <@@ 
         let isDivBy divisor n = n % divisor = 0.
         let isEven = isDivBy 2.
         isEven 21.
      @@>

[<Test>]
let ``Returning lambdas works``() =
   check 
      <@@ 
         let isDivBy divisor = 
            fun n -> n % divisor = 0.
         let isEven = isDivBy 2.
         isEven 21.
      @@>

let isDivBy2 n = n % 2. = 0.

[<FunScript.JSEmit("return {0} % 3 == 0;")>]
let isDivBy3 n : bool = failwith "never"



type LambdaReturner() =

    static member Check(which, other) : float -> bool =
        if which then isDivBy2
        else isDivBy3

[<Test>]
let ``Returning lambdas from tupled methods works``() =
   check 
      <@@
         LambdaReturner.Check(true, false) 21.
      @@>


[<Test>]
let ``Returning emit lambdas from tupled methods works``() =
   checkAreEqual true 
      <@@
         let f = LambdaReturner.Check(false, false) 
         f 21.
      @@>


let double x = x * 2.
let doubleReplacement x = x * 3.

type DoublerProvider() =
    static member Get() = double

type DoublerProviderReplacement() =
    static member Get() = doubleReplacement

[<Test>]
let ``Replacing methods returning lambdas works``() =
    checkAreEqualWithComponents 
        [
            FunScript.ExpressionReplacer.create <@ DoublerProvider.Get @> <@ DoublerProviderReplacement.Get @>
        ] 
        false 
        <@@
            let f = DoublerProvider.Get()
            f 2. = 4.
        @@>

[<Test>]
let ``Replacing types with methods returning lambdas works``() =
    checkAreEqualWithComponents 
        (FunScript.ExpressionReplacer.createTypeMethodMappings typeof<DoublerProvider> typeof<DoublerProviderReplacement>)
        false 
        <@@
            let f = DoublerProvider.Get()
            f 2. = 4.
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

[<FunScript.JSEmitInline("arguments.length")>]
let getArgCount() = failwith "JavaScript only"

// TODO:
//[<Test>]
//let ``unit is erased from lambdas``() =
//    checkAreEqual 0.0
//        <@
//            (fun () -> getArgCount())()
//        @>