[<FunScript.JS>]
[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.RecordTypes

open NUnit.Framework


type Person = { Name: string; Age: float } 

[<Test>]
let ``Record constructors can be generated``() =
   check 
      <@@ 
         let x = { Name = "Zach"; Age = 24. }
         true
      @@>


[<Test>]
let ``Record property access can be generated``() =
   check 
      <@@ 
         let x = { Name = "Zach"; Age = 24. }
         x.Name
      @@>

[<Test>]
let ``Record expression constructors can be generated``() =
   check 
      <@@ 
         let now = { Name = "Zach"; Age = 24. }
         let soon = { now with Age = 25. }
         soon.Age
      @@>

type JSKiller =
   { ``for`` : float
     ``class`` : float }

[<Test>]
let ``Records with key/reserved words are mapped correctly``() =
   check 
      <@@ 
         let x = { ``for`` = 1.0; ``class`` = 2.0 }
         x.``class``
      @@>