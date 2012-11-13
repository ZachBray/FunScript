module FunJS.Tests.ReflectedDefinitions

open Xunit
open FsUnit.Xunit

[<ReflectedDefinition>]
module Calculator =
   let zero = 0.

   let add x y = x + y

   let subtract(x, y) = x - y

[<Fact>]
let ``Curried methods can be generated from modules``() =
   check  <@@ Calculator.add 1. 2. @@>

[<Fact>]
let ``Tupled methods can be generated from modules``() =
   check  <@@ Calculator.subtract(1., 2.) @@>

[<Fact>]
let ``Property getters can be generated from modules``() =
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
let ``Curried methods can be generated from static types``() =
   check  <@@ StaticCalculator.add 1. 2. @@>

[<Fact>]
let ``Tupled methods can be generated from static types``() =
   check  <@@ StaticCalculator.subtract(1., 2.) @@>

[<Fact>]
let ``Property getters can be generated from static types``() =
   check  <@@ StaticCalculator.zero @@>

[<Fact>]
let ``Property setters can be generated from static types``() =
   check  <@@ StaticCalculator.current <- 1 @@>


[<ReflectedDefinition>]
type InstanceCalculator(x) =
   member val current = x with get, set
   member __.zero = 0.
   member __.add x y = x + y
   member __.subtract(x, y) = x - y
   member __.genericIdentity x = x

[<Fact>]
let ``Instance types can be generated``() =
   check
      <@@
         let calc = new InstanceCalculator(10.)
         () @@>
      

[<Fact>]
let ``Curried methods can be generated from instance types``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         calc.add 1. 2. @@>

[<Fact>]
let ``Tupled methods can be generated from instance types``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         calc.subtract(1., 2.) @@>

[<Fact>]
let ``Property getters can be generated from instance types``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         calc.zero @@>

[<Fact>]
let ``Property setters can be generated from instance types``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         calc.current <- calc.current + 1. @@>

[<Fact>]
let ``Generic methods can be generated from instance types``() =
   check  
      <@@ 
         let calc = InstanceCalculator(10.)
         calc.genericIdentity 11. @@>