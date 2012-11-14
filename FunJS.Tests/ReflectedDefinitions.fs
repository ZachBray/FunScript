module FunJS.Tests.ReflectedDefinitions

open Xunit
open FsUnit.Xunit

[<ReflectedDefinition>]
module Calculator =
   let zero = 0.

   let add x y = x + y

   let subtract(x, y) = x - y

[<Fact>]
let ``application of a curried module method works``() =
   check  <@@ Calculator.add 1. 2. @@>

[<Fact>]
let ``partial application of a curried module method works``() =
   check  
      <@@ 
         let f = Calculator.add 1. 
         f 2. 
      @@>

[<Fact>]
let ``application of a tupled module method works``() =
   check  <@@ Calculator.subtract(1., 2.) @@>

[<Fact>]
let ``getting a module property works``() =
   check  <@@ Calculator.zero @@>


[<ReflectedDefinition>]
type StaticCalculator() =
   static member zero = 0.
   static member add x y = x + y
   static member subtract(x, y) = x - y
   static member current
      with get() = 0
      and set (value:int) = ()

[<Fact>]
let ``application of a static curried method works``() =
   check  <@@ StaticCalculator.add 1. 2. @@>

[<Fact>]
let ``partial application of a static method works``() =
   check  
      <@@ 
         let f = StaticCalculator.add 1. 
         f 2. 
      @@>

[<Fact>]
let ``application of a static tupled method works``() =
   check  <@@ StaticCalculator.subtract(1., 2.) @@>

[<Fact>]
let ``getting a static property works``() =
   check  <@@ StaticCalculator.zero @@>

[<Fact>]
let ``setting a static property works``() =
   check  <@@ StaticCalculator.current <- 1 @@>


[<ReflectedDefinition>]
type InstanceCalculator(x) =
   member val current = x with get, set
   member __.zero = 0.
   member __.add x y = x + y
   member __.subtract(x, y) = x - y
   member __.genericIdentity x = x

[<Fact>]
let ``constructing instances works``() =
   check
      <@@
         let calc = new InstanceCalculator(10.)
         () @@>
      

[<Fact>]
let ``application of a curried instance method works``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         calc.add 1. 2. @@>

[<Fact>]
let ``partial application of a curried instance method works``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         let f = calc.add 1. 
         f 2. 
      @@>

[<Fact>]
let ``application of a tupled instance method works``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         calc.subtract(1., 2.) @@>

[<Fact>]
let ``getting an instance property works``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         calc.zero @@>

[<Fact>]
let ``setting an instance property works``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         calc.current <- calc.current + 1. @@>

[<Fact>]
let ``generic methods on instances work``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         calc.genericIdentity 11. @@>