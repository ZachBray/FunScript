module FunJS.Tests.LambdaApplication

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Defining lambdas works``() =
   check <@@ (fun x -> x % 2. = 0.)(2.) @@>

[<Fact>]
let ``Applying let bound functions works``() =
   check 
      <@@ 
         let isOdd x = x % 2. <> 0.
         isOdd 2.
      @@>