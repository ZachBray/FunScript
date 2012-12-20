[<NUnit.Framework.TestFixture>] 
module FunJS.Tests.Arithmetic

open NUnit.Framework

[<Test>]
let ``Infix add can be generated``() =
   check <@@ 4. + 2. @@>

[<Test>]
let ``Infix subtract can be generated``() =
   check <@@ 4. - 2. @@>

[<Test>]
let ``Infix multiply can be generated``() =
   check <@@ 4. * 2. @@>

[<Test>]
let ``Infix divide can be generated``() =
   check <@@ 4. / 2. @@>

[<Test>]
let ``Infix modulo can be generated``() =
   check <@@ 4. % 2. @@>

[<Test>]
let ``Evaluation order is preserved by generated code``() =
   check <@@ (4. - 2.) * 2. + 1. @@>

