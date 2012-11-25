module FunJS.Tests.PrimitiveTypes

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Unit is generated as null``() =
   checkAreEqual null <@@ () @@>

[<Fact>]
let ``Ints are generated as numbers``() =
   checkAreEqual 1. <@@ 1 @@>

[<Fact>]
let ``Chars are generated as strings``() =
   checkAreEqual "a" <@@ 'a' @@>

[<Fact>]
let ``Floats are generated as numbers``() =
   checkAreEqual 1. <@@ 1. @@>

[<Fact>]
let ``Strings are generated as strings``() =
   check <@@ "test" @@>

[<Fact>]
let ``Bools are generated as bools``() =
   check <@@ true @@>