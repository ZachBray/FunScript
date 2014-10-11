[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.StringWriter

open NUnit.Framework
open System.IO


[<Test>]
let ``should return empty when nothing has been written``() =
   check 
      <@@
         let x = new StringWriter()
         x.ToString()
      @@>

[<Test>]
let ``should return the string when a string has been written to it``() =
   check 
      <@@
         let x = new StringWriter()
         x.Write "blah"
         x.ToString().ToLower()
      @@>

[<Test>]
let ``should concat the things that are written to it``() =
   check 
      <@@
         let x = new StringWriter()
         x.Write "Blah1."
         x.Write "Blah2."
         x.Write "Blah3."
         x.ToString().ToLower()
      @@>

[<Test>]
let ``should return a string with the float when a float has been written to it``() =
   check 
      <@@
         let x = new StringWriter()
         x.Write 99.0
         x.ToString().ToLower()
      @@>

[<Test>]
let ``should return a string the bool when a bool has been written to it``() =
   check 
      <@@
         let x = new StringWriter()
         x.Write true
         x.ToString().ToLower()
      @@>