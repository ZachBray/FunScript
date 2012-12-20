[<NUnit.Framework.TestFixture>]
module FunJS.Tests.Logic

open NUnit.Framework


[<Test>]
let ``Infix AND can be generated``() =
   check <@@ true && false @@>

[<Test>]
let ``Infix OR can be generated``() =
   check <@@ true && false @@>

[<Test>]
let ``Not can be generated``() =
   check <@@ not false @@>

[<Test>]
let ``Evaluation order is preserved by generated code``() =
   check <@@ (true && false) || false  @@>
