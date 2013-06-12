[<FunScript.JS>]
[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Reflection

open NUnit.Framework

[<FunScript.JS>]
module X =
   let printName<'a>() = 
      typeof<'a>.FullName
   
   let printListName<'a>() = typeof<'a list>.FullName

   let printName2 (x : 'a) = x.GetType().FullName

   let printNames<'a, 'b>() =
      printName2 "abc" + printName<'a>() + printName<'b>()

   let printName3 (x : 'a) (f : 'a -> 'b) =
      printName<'b>() + printName<'a>()

[<Test>]
let ``typeof<ConcreteT>.FullName works``() =
   check  
      <@@ 
         typeof<float>.FullName
      @@>

[<Test>]
let ``typeof<ConcreteCollection<ConcreteT>>.FullName works``() =
   check  
      <@@ 
         typeof<list<float>>.FullName
      @@>

[<Test>]
let ``typeof<'genericT>.FullName works``() =
   check  
      <@@ 
         X.printName<float>() + X.printName<bool>()
      @@>

[<Test>]
let ``typeof<'genericT list>.FullName works``() =
   check  
      <@@ 
         X.printListName<float>() + X.printListName<bool>()
      @@>

[<Test>]
let ``typeof<'genericT>.FullName works when 'genericT is a generic collection``() =
   check  
      <@@ 
         X.printName<float list>() + X.printName<bool list>()
      @@>

[<Test>]
let ``threaded typeof<'genericT>.FullName works``() =
   check  
      <@@ 
         X.printNames<float,bool>()
      @@>

[<Test>]
let ``threaded partially applied typeof<'genericT>.FullName works``() =
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
         (1.).GetType().FullName + (true).GetType().FullName
      @@>

[<Test>]
let ``GetType on generic argument works``() =
   check  
      <@@ 
         X.printName2 1. + X.printName2 true
      @@>

[<Test>]
let ``GetType on generic collection works``() =
   check  
      <@@ 
         [1.].GetType().FullName + [true].GetType().FullName
      @@>