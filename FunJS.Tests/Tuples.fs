module FunJS.Tests.Tuples

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Tuple declarations can be generated``() =
   check 
      <@@ 
         let x = 10, true
         true
      @@>

[<Fact>]
let ``Tuple dereferencing can be generated``() =
   check 
      <@@ 
         let x, y = 10, true
         y
      @@>

[<Fact>]
let ``fst function can be generated``() =
   check 
      <@@ 
         let xy = 10., true
         fst xy
      @@>

[<Fact>]
let ``snd function can be generated``() =
   check 
      <@@ 
         let xy = 10., true
         snd xy
      @@>