[<FunScript.JS>]
[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Reflection

open NUnit.Framework

[<FunScript.JS>]
module X =
   let printName<'a>() = typeof<'a>.Name

   let printName2 (x : 'a) = x.GetType().Name

   let printNames<'a, 'b>() =
      printName2 "abc" + printName<'a>() + printName<'b>()

   let printName3 (x : 'a) (f : 'a -> 'b) =
      printName<'b>() + printName<'a>()

[<Test>]
let ``typeof<ConcreteT>.Name works``() =
   check  
      <@@ 
         typeof<float>.Name
      @@>

[<Test>]
let ``typeof<'genericT>.Name works``() =
   check  
      <@@ 
         X.printName<float>() + X.printName<bool>()
      @@>

[<Test>]
let ``threaded typeof<'genericT>.Name works``() =
   check  
      <@@ 
         X.printNames<float,bool>()
      @@>

[<Test>]
let ``threaded partially applied typeof<'genericT>.Name works``() =
   check  
      <@@ 
         X.printName3 1 (fun x -> float x)
      @@>
//
//[<Test>]
//let ``type test works``() =
//   check  
//      <@@ 
//         match box 1 with
//         | :? int as x -> x
//         | _ -> 99
//      @@>

[<Test>]
let ``GetType on concrete argument works``() =
   check  
      <@@ 
         (1.).GetType().Name + (true).GetType().Name
      @@>

[<Test>]
let ``GetType on generic argument works``() =
   check  
      <@@ 
         X.printName2 1. + X.printName2 true
      @@>
