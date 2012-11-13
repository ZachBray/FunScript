module FunJS.Tests.UnionTypes

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Union case constructions with no arguments can be generated``() =
   check 
      <@@ 
         let x: unit Option = None
         true
      @@>

[<Fact>]
let ``Union cases matches with no arguments can be generated``() =
   check 
      <@@ 
         let x: bool Option = None
         match x with
         | None -> true
         | Some y -> y
      @@>

[<Fact>]
let ``Union case constructions with one argument can be generated``() =
   check 
      <@@ 
         let x = Some ""
         true
      @@>

[<Fact>]
let ``Union cases matches with one argument can be generated``() =
   check 
      <@@ 
         let x = Some false
         match x with
         | None -> true
         | Some y -> y
      @@>


type TestUnion =
   | Case0
   | Case1 of string
   | Case2 of string * string
   | Case3 of string * string * string


[<Fact>]
let ``Union case constructions with many arguments can be generated``() =
   check 
      <@@ 
         let x = Case2("", "")
         true
      @@>

[<Fact>]
let ``Union cases matches with many arguments can be generated``() =
   check 
      <@@ 
         let x = Case3("a", "b", "c")
         match x with
         | Case3(a, b, c) -> a + b + c
         | _ -> ""
      @@>