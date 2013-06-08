[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.UnionTypes

open NUnit.Framework

type Gender = Male | Female

[<Test>]
let ``Union case constructions with no arguments can be generated (user)``() =
   check 
      <@@ 
         let x = Male
         let y = Female
         true
      @@>

[<Test>]
let ``Union cases matches with no arguments can be generated (user)``() =
   check 
      <@@ 
         let x = Male
         match x with
         | Female -> true
         | Male -> false
      @@>


[<Test>]
let ``Union case constructions with no arguments can be generated (option)``() =
   check 
      <@@ 
         let x: unit Option = None
         true
      @@>

[<Test>]
let ``Union cases matches with no arguments can be generated (option)``() =
   check 
      <@@ 
         let x: bool Option = None
         match x with
         | None -> true
         | Some y -> y
      @@>

[<Test>]
let ``Union case constructions with one argument can be generated``() =
   check 
      <@@ 
         let x = Some ""
         true
      @@>

[<Test>]
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


[<Test>]
let ``Union case constructions with many arguments can be generated``() =
   check 
      <@@ 
         let x = Case2("", "")
         true
      @@>

[<Test>]
let ``Union cases matches with many arguments can be generated``() =
   check 
      <@@ 
         let x = Case3("a", "b", "c")
         match x with
         | Case3(a, b, c) -> a + b + c
         | _ -> ""
      @@>