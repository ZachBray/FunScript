[<NUnit.Framework.TestFixture>] 
module FunJS.Tests.Tuples

open NUnit.Framework


[<Test>]
let ``Tuple declarations can be generated``() =
   check 
      <@@ 
         let x = 10, true
         true
      @@>

[<Test>]
let ``Tuple dereferencing can be generated``() =
   check 
      <@@ 
         let x, y = 10, true
         y
      @@>

[<Test>]
let ``fst function can be generated``() =
   check 
      <@@ 
         let xy = 10., true
         fst xy
      @@>

[<Test>]
let ``snd function can be generated``() =
   check 
      <@@ 
         let xy = 10., true
         snd xy
      @@>