module FunJS.Tests.Logic

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Infix AND can be generated``() =
   check <@@ true && false @@>

[<Fact>]
let ``Infix OR can be generated``() =
   check <@@ true && false @@>

[<Fact>]
let ``Not can be generated``() =
   check <@@ not false @@>

[<Fact>]
let ``Evaluation order is preserved by generated code``() =
   check <@@ (true && false) || false  @@>
