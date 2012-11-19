[<FunJS.JS>]
module FunJS.Tests.RecordTypes

open Xunit
open FsUnit.Xunit

type Person = { Name: string; Age: float } 

[<Fact>]
let ``Record constructors can be generated``() =
   check 
      <@@ 
         let x = { Name = "Zach"; Age = 24. }
         true
      @@>


[<Fact>]
let ``Record property access can be generated``() =
   check 
      <@@ 
         let x = { Name = "Zach"; Age = 24. }
         x.Name
      @@>

[<Fact>]
let ``Record expression constructors can be generated``() =
   check 
      <@@ 
         let now = { Name = "Zach"; Age = 24. }
         let soon = { now with Age = 25. }
         soon.Age
      @@>