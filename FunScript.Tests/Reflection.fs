[<FunScript.JS>]
[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Reflection

open NUnit.Framework

[<FunScript.JS>]
module X =
   let printName<'a>() = typeof<'a>.Name

   let printName2 (x : 'a) = x.GetType().Name

//[<Test>]
//let ``typeof<ConcreteT>.Name works``() =
//   check  
//      <@@ 
//         typeof<int>.Name
//      @@>
//
//[<Test>]
//let ``typeof<'genericT>.Name works``() =
//   check  
//      <@@ 
//         X.printName<int>()
//      @@>
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
