[<FunScript.JS>]
[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.ReflectedDefinitions

open NUnit.Framework


module Calculator =

   let zero = 0.

   let add x y = x + y

   let subtract(x, y) = x - y

   let addSub x yz = x + subtract yz

   let addSub2(x, yz) = x + subtract yz

   let mutable workings = 1.

[<Test>]
let ``application of a curried module method works``() =
   check  <@@ Calculator.add 1. 2. @@>

[<Test>]
let ``partial application of a curried module method works``() =
   check  
      <@@ 
         let f = Calculator.add 1. 
         f 2. 
      @@>

[<Test>]
let ``application of a tupled module method works``() =
   check  <@@ Calculator.subtract(1., 2.) @@>

[<Test>]
let ``application of a partially tupled curried module method works``() =
   check  <@@ Calculator.addSub 1. (2., 3.) @@>

[<Test>]
let ``application of a partially tupled tupled module method works``() =
   check  <@@ Calculator.addSub2(1., (2., 3.)) @@>

[<Test>]
let ``getting a module property works``() =
   check <@@ Calculator.zero @@>

[<Test>]
let ``setting a mutable module property works``() =
   check  
      <@@ 
         Calculator.workings <- 10. + Calculator.workings 
         Calculator.workings
      @@>

[<ReflectedDefinition>]
module StaticBackingFields =
    let mutable x = 0.

[<ReflectedDefinition>]
type StaticCalculator() =
   static member zero = 0.
   static member add x y = x + y
   static member subtract(x, y) = x - y
   static member currentX2
      with get() = StaticBackingFields.x
      and set value = StaticBackingFields.x <- 2. * value
   //NOTE: not supported by ReflectedDefinition/quotations because of static field
   //static member val current = 0 with get, set

[<Test>]
let ``application of a static curried method works``() =
   check  <@@ StaticCalculator.add 1. 2. @@>

[<Test>]
let ``partial application of a static method works``() =
   check  
      <@@ 
         let f = StaticCalculator.add 1. 
         f 2. 
      @@>

[<Test>]
let ``application of a static tupled method works``() =
   check  <@@ StaticCalculator.subtract(1., 2.) @@>

[<Test>]
let ``getting a static property works``() =
   check  <@@ StaticCalculator.zero @@>

[<Test>]
let ``setting a static property works``() =
   check  <@@ StaticCalculator.currentX2 <- 1.; StaticCalculator.currentX2 @@>

//[<Test>]
//let ``setting a static auto-property works``() =
//   check  
//      <@@ StaticCalculator.current <- 3; StaticCalculator.current @@>

type InstanceCalculator(x) =
   let y = x * 2.
   let mutable x = 0.
   member val current = x with get, set
   member __.currentX2 
      with get() = x
      and set v = x <- v * 2.
   member __.getY = y
   member __.zero = 0.
   member __.add x y = x + y
   member __.subtract(x, y) = x - y
   member __.genericIdentity x = x
   member __.addSub x yz = x + __.subtract yz
   member __.addSub2(x, yz) = x + __.subtract yz

[<Test>]
let ``constructing instances works``() =
   check
      <@@
         let calc = new InstanceCalculator(10.)
         () @@>
      

[<Test>]
let ``application of a curried instance method works``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         calc.add 1. 2. @@>

[<Test>]
let ``partial application of a curried instance method works``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         let f = calc.add 1. 
         f 2. 
      @@>

[<Test>]
let ``application of a tupled instance method works``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         calc.subtract(1., 2.) @@>

[<Test>]
let ``application of a partially tupled curried instance method works``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         calc.addSub 1. (2., 3.) @@>

[<Test>]
let ``application of a partially tupled tupled instance method works``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         calc.addSub2(1., (2., 3.)) @@>

[<Test>]
let ``getting an instance property works``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         calc.zero @@>

[<Test>]
let ``setting an instance property works``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         calc.currentX2 <- 3.
         calc.currentX2 @@>

[<Test>]
let ``setting an instance auto-property works``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         calc.current <- 1.
         calc.current @@>

[<Test>]
let ``let bound fields on instances work``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         calc.getY @@>

[<Test>]
let ``generic methods on instances work``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         calc.genericIdentity 11. @@>


type CalculatorUnion =
   | Calculator of float
        
   member calc.getSeed =
      match calc with Calculator seed -> seed
   member __.zero = 0.
   member __.add x y = x + y
   member __.subtract(x, y) = x - y
   member __.genericIdentity x = x

[<Test>]
let ``application of a curried union method works``() =
   check  
      <@@ 
         let calc = Calculator(10.)
         calc.add 1. 2. @@>

[<Test>]
let ``partial application of a curried union method works``() =
   check  
      <@@ 
         let calc = Calculator(10.)
         let f = calc.add 1. 
         f 2. 
      @@>

[<Test>]
let ``application of a tupled union method works``() =
   check  
      <@@ 
         let calc = Calculator(10.)
         calc.subtract(1., 2.) @@>

[<Test>]
let ``getting an union property works``() =
   check  
      <@@ 
         let calc = Calculator(10.)
         calc.zero @@>

[<Test>]
let ``let bound fields on unions work``() =
   check  
      <@@ 
         let calc = Calculator(10.)
         calc.getSeed @@>

[<Test>]
let ``generic methods on unions work``() =
   check  
      <@@ 
         let calc = Calculator(10.)
         calc.genericIdentity 11. @@>

type RecordCalculator =
 { mutable y: float }
   member r.getY = r.y
   member __.zero = 0.
   member __.add x y = x + y
   member __.subtract(x, y) = x - y
   member __.genericIdentity x = x
     
[<Test>]
let ``application of a curried record method works``() =
   check  
      <@@ 
         let calc = { y = 10. }
         calc.add 1. 2. @@>

[<Test>]
let ``partial application of a curried record method works``() =
   check  
      <@@ 
         let calc = { y = 10. }
         let f = calc.add 1. 
         f 2. 
      @@>

[<Test>]
let ``application of a tupled record method works``() =
   check  
      <@@ 
         let calc = { y = 10. }
         calc.subtract(1., 2.) @@>

[<Test>]
let ``getting an record property works``() =
   check  
      <@@ 
         let calc = { y = 10. }
         calc.zero @@>

[<Test>]
let ``setting an record property works``() =
   check  
      <@@ 
         let calc = { y = 10. }
         calc.y <- calc.y + 1.
         calc.y @@>

[<Test>]
let ``let bound fields on records work``() =
   check  
      <@@ 
         let calc = { y = 10. }
         calc.getY @@>

[<Test>]
let ``generic methods on records work``() =
   check  
      <@@ 
         let calc = { y = 10. }
         calc.genericIdentity 11. @@>

module ValueWrapperCompanion =
    let mutable secondaryCount = 0.

type ValueWrapper(x) =
    member __.Value = x 
    new () = 
        ValueWrapperCompanion.secondaryCount <- ValueWrapperCompanion.secondaryCount + 1.
        let y = 1.
        ValueWrapper(y * 2.)

[<Test>]
let ``multiple constructors work``() =
   check  
      <@@ 
         let a = ValueWrapper()
         let b = ValueWrapper(10.)
         a.Value + b.Value + ValueWrapperCompanion.secondaryCount @@>


[<FunScript.JS>]
type TestClass() =
    let f = ref (fun x -> x * 2.)

    let g y = !f y

    member val G = g

[<Test>]
let ``references to fields inside exported lambdas produce the same result``() =
    check
        <@@
            let z = TestClass()
            z.G 10.
        @@>